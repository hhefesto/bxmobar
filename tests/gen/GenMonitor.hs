-- The monitors' common part's differential test cases: random monitor
-- arguments (pluginOptions in every spelling, with their errors), each
-- with one of Common.Output's helpers or the monitor's template, run as a
-- monitor runs them (doArgs, then the helper, the exception shown as
-- runM shows it), with what xmobar 0.50 answers. Written as
-- tests/MonitorCases.bend for tests/MonitorDiff.bend.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad.Reader (runReaderT)
import Data.Char (ord)
import Data.List (intercalate)
import GHC.Float (castFloatToWord32)
import Numeric (showHex)
import System.Environment (getArgs)
import System.Console.GetOpt (ArgOrder (Permute), getOpt)
import Test.QuickCheck
import Text.Read (readMaybe)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Xmobar.Plugins.Monitors.Common

exports :: [String]
exports = ["a", "b", "c", "total", "x1"]

gNumber :: Gen String
gNumber = frequency
  [ (8, show <$> (choose (-20, 120) :: Gen Int))
  , (1, elements ["0x10", "(5)", " 7 ", "-3", "( -4 )", "- 2", "0o17", "007", "1000"])
  ]

gColor :: Gen String
gColor = elements ["red", "#ff0000", "darkgreen", "#00FF00", "blue", "x"]

gTemplate :: Gen String
gTemplate = concat <$> (choose (0, 4) >>= flip vectorOf (frequency
  [ (4, elements ["<a>", "<b>", "<c>", "<total>", "<x1>", "<zz>"])
  , (3, elements ["Cpu: ", "%", "|", " ", "Mem", "<fc=red>", "</fc>", "<icon=i.xbm/>", "\233", "x"])
  , (1, elements ["<", ">", "<fc=", "<icon=a", "<a"])
  ]))

-- one option, any spelling
gOpt :: Gen [String]
gOpt = do
  (s, l, v) <- frequency
    [ (3, (,,) 'H' "High" <$> gNumber)
    , (3, (,,) 'L' "Low" <$> gNumber)
    , (2, (,,) 'h' "high" <$> gColor)
    , (2, (,,) 'n' "normal" <$> gColor)
    , (2, (,,) 'l' "low" <$> gColor)
    , (4, (,,) 't' "template" <$> gTemplate)
    , (2, (,,) 'S' "suffix" <$> elements ["True", "true", "yes", "On", "False", "no", "x"])
    , (2, (,,) 'd' "ddigits" <$> (show <$> (choose (0, 4) :: Gen Int)))
    , (2, (,,) 'p' "ppad" <$> (show <$> (choose (-1, 6) :: Gen Int)))
    , (2, (,,) 'm' "minwidth" <$> (show <$> (choose (-1, 9) :: Gen Int)))
    , (2, (,,) 'M' "maxwidth" <$> (show <$> (choose (-1, 9) :: Gen Int)))
    , (1, (,,) 'w' "width" <$> (show <$> (choose (0, 9) :: Gen Int)))
    , (1, (,,) 'e' "maxwidthellipsis" <$> elements ["", "...", "\8230"])
    , (2, (,,) 'c' "padchars" <$> elements [" ", "0", "ab", "\183"])
    , (2, (,,) 'a' "align" <$> elements ["l", "r", "right", "left", "x"])
    , (2, (,,) 'b' "bback" <$> elements [":", "-", "ab", "\9617"])
    , (2, (,,) 'f' "bfore" <$> elements ["#", "=", "xy", "\9608"])
    , (2, (,,) 'W' "bwidth" <$> (show <$> (choose (0, 15) :: Gen Int)))
    , (1, (,,) 'x' "nastring" <$> elements ["N/A", "-"])
    , (2, (,,) 'T' "maxtwidth" <$> (show <$> (choose (0, 25) :: Gen Int)))
    , (1, (,,) 'E' "maxtwidthellipsis" <$> elements ["", "..", "\8230"])
    ]
  frequency
    [ (4, pure ['-' : [s], v])
    , (3, pure ["--" ++ l, v])
    , (2, pure ["--" ++ l ++ "=" ++ v])
    , (1, pure ['-' : s : v])
    , (1, pure ["--" ++ take (max 1 (length l - 1)) l, v])
    ]

-- an argument list: options, sometimes an error (an unknown option, an
-- ambiguous prefix, a missing argument), sometimes "--" and the rest
gArgs :: Gen [String]
gArgs = do
  os <- concat <$> (choose (0, 5) >>= flip vectorOf gOpt)
  junk <- frequency
    [ (12, pure [])
    , (1, elements [["-z"], ["--zz"], ["--max", "3"], ["--b", "x"], ["-t"], ["--Low"], ["--nastring"], ["word"], ["-"], ["--high=red", "--suffix=True"], ["-St"], ["--template"]])
    ]
  tl <- frequency [(6, pure []), (1, elements [["--", "-x", "y"], ["--"]])]
  pure (os ++ junk ++ tl)

gFloat :: Gen Float
gFloat = frequency
  [ (6, (/ 1000) . fromIntegral <$> (choose (0, 1200) :: Gen Int))
  , (2, (/ 10) . fromIntegral <$> (choose (-50, 1500) :: Gen Int))
  , (1, (/ 1000) . fromIntegral <$> (choose (-300, 0) :: Gen Int))
  , (1, elements [0, 0.5, 0.125, 0.335, 0.665, 1, 2, 1e-3, 33, 66, 33.5, 66.5])
  , (1, fromIntegral <$> (choose (0, 20000000) :: Gen Int))
  ]

gValue :: Gen String
gValue = frequency
  [ (5, show <$> (choose (0, 100) :: Gen Int))
  , (2, elements ["", "<fc=red>12</fc>", "<zz>", "a<b c", "N/A", "\233t\233", "<icon=x/>5"])
  ]

gStr :: Gen String
gStr = elements ["12", "", "abc", "\233", "<fc=x>y</fc>", "a%%b", "%%", "i%%.xbm"]

-- every numeric option reads as an Int: where one does not, xmobar fails
-- the monitor lazily (when the text needs the value, sometimes in another
-- thread) and xmobar-bend at once, a difference README.md names
numbersRead :: [String] -> Bool
numbersRead args = all ok os
  where
    (os, _, _) = getOpt Permute pluginOptions args
    num s = (readMaybe s :: Maybe Int) /= Nothing
    ok o = case o of
      High s -> num s
      Low s -> num s
      PercentPad s -> num s
      DecDigits s -> num s
      MinWidth s -> num s
      MaxWidth s -> num s
      Width s -> num s
      BarWidth s -> num s
      MaxTotalWidth s -> num s
      _ -> True

data Case = Case [String] Int Float Float String [String]

gCase :: Gen Case
gCase = Case <$> (gArgs `suchThat` numbersRead) <*> choose (0, 12) <*> gFloat <*> gFloat <*> gStr <*> vectorOf 5 gValue

answer :: Case -> IO String
answer (Case args k a b s vs) = do
  r <- try $ do
    c <- mkMConfig "<a>|<b>" exports
    out <- runReaderT (doArgs args (\_ -> act) (\_ -> return True)) c
    _ <- evaluate (length out)
    return out
  return $ case r of
    Right out -> out
    Left e -> "error: " ++ show (e :: SomeException)
  where
    act = case k of
      0 -> showPercentWithColors a
      1 -> showPercentBar a b
      2 -> showVerticalBar a b
      3 -> showWithColors' s a
      4 -> showLogBar a b
      5 -> floatToPercent a
      6 -> parseTemplate vs
      7 -> showIconPattern (Just (parseIconPattern s)) a
      8 -> return (showWithUnits 2 1 a)
      9 -> return (showWithUnits 3 0 a)
      10 -> return (show (takeDigits 2 a))
      11 -> showLogVBar a b
      _ -> showLogIconPattern (Just (parseIconPattern s)) a b

-- a Bend string literal: printable ASCII as itself, " and \ escaped,
-- anything else as \u{hex}
bStr :: String -> String
bStr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc c
      | ord c >= 32 && ord c < 127 = [c]
      | otherwise = "\\u{" ++ showHex (ord c) "" ++ "}"

bList :: [String] -> String
bList xs = "[" ++ intercalate ", " (map bStr xs) ++ "]"

main :: IO ()
main = do
  [seedS, countS] <- getArgs
  let seed = read seedS
      count = read countS :: Int
      cases = [unGen gCase (mkQCGen (seed * 1000003 + i)) 30 | i <- [1 .. count]]
  outs <- mapM answer cases
  let line (Case args k a b s vs, w) = "    MCase{" ++ bList args ++ ", " ++ show k ++ ", " ++ show (castFloatToWord32 a) ++ ", " ++ show (castFloatToWord32 b) ++ ", " ++ bStr s ++ ", " ++ bList vs ++ ", " ++ bStr w ++ "}"
  putStrLn "# Written by tests/gen/GenMonitor.hs: monitor arguments, a helper of"
  putStrLn "# Common.Output or the monitor's template, and xmobar 0.50's answer."
  putStrLn "import Base"
  putStrLn ""
  putStrLn "type Case is Data:"
  putStrLn "  MCase{args: List<&2, String>, kind: U32, a: U32, b: U32, s: String, values: List<&2, String>, want: String}"
  putStrLn ""
  putStrLn "def cases() -> List<&2, Case>:"
  putStrLn ("  [\n" ++ intercalate ",\n" (map line (zip cases outs)) ++ "\n  ]")
