-- The markup's and templates' differential test cases: random strings of
-- xmobar's markup (mostly well nested, some cut, some soup), each with
-- what xmobar 0.50's own parseString answers (show of its segments); and
-- random templates with what splitTemplate and parseTemplate answer.
-- Written as tests/TemplateCases.bend for tests/TemplateDiff.bend.
--
--   ghc tests/gen/GenTemplate.hs -o gen   (a ghc with .#xmobar-lib)
--   ./gen SEED COUNT > tests/TemplateCases.bend
module Main (main) where

import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)
import System.Environment (getArgs)
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Xmobar.App.Config (defaultConfig)
import Xmobar.Config.Template (parseString)
import Xmobar.Config.Types (Config (..))
import Xmobar.Run.Exec (alias)
import Xmobar.Run.Template (parseTemplate, splitTemplate)

-- characters text is made of: ASCII, the markup's own, some non-ASCII
-- letters and numbers (isAlphaNum's other scripts), spaces of every kind
gChar :: Gen Char
gChar = frequency
  [ (20, elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))
  , (6, elements " <>/=`,:#%{}()!-_.")
  , (2, elements "\233\178\189\20013\1488\1633\8544\12354\223\955")
  , (1, elements "\t\n\160\8195\12288\1\127\200")
  ]

gText :: Gen String
gText = do
  n <- frequency [(3, choose (1, 4)), (1, choose (5, 12))]
  vectorOf n gChar

gDigits :: Gen String
gDigits = frequency
  [ (8, show <$> (choose (0, 20) :: Gen Int))
  , (2, show <$> (choose (0, 5000000000) :: Gen Integer))
  , (1, show <$> (choose (0, 30000000000000000000) :: Gen Integer))
  , (1, pure "")
  , (1, ('0' :) . show <$> (choose (0, 99) :: Gen Int))
  ]

-- a number as readMaybe sees it: decimal, hex, octal, junk
gNum :: Gen String
gNum = frequency
  [ (6, show <$> (choose (0, 300) :: Gen Int))
  , (1, show <$> (choose (0, 9000000000) :: Gen Integer))
  , (1, ("0x" ++) . flip showHex "" <$> (choose (0, 100000) :: Gen Int))
  , (1, elements ["0X1F", "0o17", "0O7", "0o8", "0x", "0b101", "1e3", "12a", "", "a", "00x5", "0xg", "007"])
  ]

gColorName :: Gen String
gColorName = elements ["red", "#ff0000", "blue", "#00ff00", "green", "#0a0B0c", "x", "\233t\233"]

-- an fc tag's inside: fg, fg,bg, fg:off, fg:top,bottom, junk
gColor :: Gen String
gColor = frequency
  [ (4, gColorName)
  , (3, (\a b -> a ++ "," ++ b) <$> gColorName <*> gColorName)
  , (2, (\a o -> a ++ ":" ++ o) <$> gColorName <*> gNum)
  , (2, (\a o p -> a ++ ":" ++ o ++ "," ++ p) <$> gColorName <*> gNum <*> gNum)
  , (1, (\a o p -> a ++ "," ++ a ++ ":" ++ o ++ "," ++ p ++ ":" ++ o) <$> gColorName <*> gNum <*> gNum)
  , (1, pure "")
  ]

gBoxParam :: Gen String
gBoxParam = frequency
  [ (3, ("type=" ++) <$> elements ["Top", "Bottom", "VBoth", "Left", "Right", "HBoth", "Full", "Middle", "", "top", "Top,"])
  , (2, (\a o -> "offset=" ++ a : o) <$> elements "LRCX5" <*> gNum)
  , (2, ("width=" ++) <$> gNum)
  , (2, ("color=" ++) <$> gColorName)
  , (2, (\p v -> 'm' : p ++ "=" ++ v) <$> elements ["t", "r", "b", "l", "x", ""] <*> gNum)
  , (1, elements ["", "type", "=", "type=Top=Bottom", "x=1"])
  ]

gBoxParams :: Gen String
gBoxParams = frequency
  [ (1, pure "")
  , (4, (" " ++) . unwords <$> (choose (1, 4) >>= flip vectorOf gBoxParam))
  , (1, (\ps -> "  " ++ intercalate "   " ps ++ " ") <$> (choose (1, 3) >>= flip vectorOf gBoxParam))
  , (1, (\ps -> "es" ++ unwords ps) <$> (choose (0, 2) >>= flip vectorOf gBoxParam))
  ]

gCommand :: Gen String
gCommand = frequency [(4, elements ["xterm", "x y", "notify-send hi", "mpc toggle", "\233"]), (1, elements ["a>b", "", "`", "echo `x`"])]

gAction :: Gen String
gAction = do
  c <- gCommand
  frequency
    [ (3, pure ("<action=`" ++ c ++ "`>"))
    , (3, pure ("<action=" ++ c ++ ">"))
    , (2, (\sp bs -> "<action=`" ++ c ++ "`" ++ sp ++ "button=" ++ bs ++ ">") <$> elements [" ", "  ", "\t", "\160 "] <*> elements ["1", "3", "45", "12345", "", "6", "1 "])
    , (1, (\bs -> "<action=" ++ c ++ " button=" ++ bs ++ ">") <$> elements ["1", "3"])
    , (1, pure ("<action=`" ++ c ++ "`x>"))
    ]

-- one piece of markup at depth d (clean: no piece meant to fail)
gPiece :: Bool -> Int -> Gen String
gPiece clean d = frequency $
  [ (10, gText)
  , (2, (\i -> "<icon=" ++ i ++ "/>") <$> elements ["a.xbm", "dir/b.xpm", "a/b/c", "", "x>y", "a/"])
  , (2, (\n -> "<hspace=" ++ n ++ "/>") <$> gDigits)
  , (2, gRaw)
  , (if clean then 0 else 1, elements ["<", ">", "</fc>", "</box>", "</fn>", "</action>", "<fc=", "<box", "<boxy", "<fn=x>", "<icon=a", "<raw=", "<raw=2", "<hspace=1", "</fc", "<fcx>", "<b>"])
  ] ++ (if d > 0 then
  [ (3, (\c s -> "<fc=" ++ c ++ ">" ++ s ++ "</fc>") <$> gColor <*> gMarkup clean (d - 1))
  , (2, (\p s -> "<box" ++ p ++ ">" ++ s ++ "</box>") <$> gBoxParams <*> gMarkup clean (d - 1))
  , (2, (\n s -> "<fn=" ++ n ++ ">" ++ s ++ "</fn>") <$> gDigits <*> gMarkup clean (d - 1))
  , (2, (\a s -> a ++ s ++ "</action>") <$> gAction <*> gMarkup clean (d - 1))
  ] else [])

gRaw :: Gen String
gRaw = do
  s <- frequency [(3, gText), (1, elements ["<fc=red>", "</fc>", "/>", "a/>b", ""])]
  k <- frequency [(6, pure 0), (1, choose (-3, 3))]
  let n = max 0 (length s + k)
  closer <- frequency [(8, pure "/>"), (1, elements ["/", ">", ""])]
  pure ("<raw=" ++ show n ++ ":" ++ s ++ closer)

gMarkup :: Bool -> Int -> Gen String
gMarkup clean d = do
  n <- frequency [(1, pure 0), (4, choose (1, 3)), (1, choose (4, 6))]
  concat <$> vectorOf n (gPiece clean d)

-- mostly markup; some of it cut short or with a piece dropped in
gString :: Gen String
gString = frequency
  [ (5, gMarkup True 3)
  , (3, gMarkup False 3)
  , (1, do m <- gMarkup True 2; k <- choose (0, length m); pure (take k m))
  , (1, do a <- gMarkup True 2; b <- gPiece False 0; c <- gMarkup True 1; pure (a ++ b ++ c))
  ]

-- templates: text, %commands%, the align characters, other separators
gTemplateChar :: Gen Char
gTemplateChar = frequency [(8, gChar), (3, elements "%}{$|"), (1, elements "\233 ")]

gTemplate :: Gen String
gTemplate = frequency
  [ (3, do n <- choose (0, 25); vectorOf n gTemplateChar)
  , (4, concat <$> (choose (0, 5) >>= flip vectorOf (frequency [(3, gText), (2, (\c -> "%" ++ c ++ "%") <$> elements ["date", "cpu", "StdinReader", "uname", "", "a b", "\233"]), (1, elements ["}", "{", "}{", "%", "%%", "$", "$x$"])])))
  ]

gSep :: Gen String
gSep = frequency [(6, pure "%"), (2, elements ["$", "|", "%$", "$%", "ab"]), (1, elements ["%%", "x"])]

gAlignSep :: Gen String
gAlignSep = frequency [(6, pure "}{"), (2, elements ["{}", "||", "<>", "ab"]), (1, elements ["", "}", "}{x", "%"])]

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

main :: IO ()
main = do
  [seedS, countS] <- getArgs
  let seed = read seedS
      count = read countS :: Int
      gen g i = unGen g (mkQCGen (seed * 1000003 + i)) 30
      strings = [(gen (elements ["#BFBFBF", "#646464", "red", "a,b", "\233"]) i, gen gString i) | i <- [1 .. count]]
      splits = [(gen gAlignSep (i + count), gen gTemplate (i + count)) | i <- [1 .. count `div` 4 + 1]]
      templates = [(gen gSep (i + 2 * count), gen gTemplate (i + 2 * count)) | i <- [1 .. count `div` 4 + 1]]
  parts <- mapM (\(sep, t) -> map (\(r, s, ss) -> (alias r, s, ss)) <$> parseTemplate [] sep t) templates
  let pLine (fg, s) = "    PCase{" ++ bStr fg ++ ", " ++ bStr s ++ ", " ++ bStr (show (parseString defaultConfig {fgColor = fg} s)) ++ "}"
      sLine (a, t) = "    SCase{" ++ bStr a ++ ", " ++ bStr t ++ ", " ++ bStr (show (splitTemplate a t)) ++ "}"
      tLine ((sep, t), ps) = "    TCase{" ++ bStr sep ++ ", " ++ bStr t ++ ", " ++ bStr (show ps) ++ "}"
  putStrLn "# Written by tests/gen/GenTemplate.hs: markup strings and templates, each"
  putStrLn "# with what xmobar 0.50 answers (parseString, splitTemplate, parseTemplate)."
  putStrLn "import Base"
  putStrLn ""
  putStrLn "type Case is Data:"
  putStrLn "  PCase{fg: String, s: String, want: String}"
  putStrLn "  SCase{sep: String, t: String, want: String}"
  putStrLn "  TCase{sep: String, t: String, want: String}"
  putStrLn ""
  putStrLn "def cases() -> List<&2, Case>:"
  putStrLn ("  [\n" ++ intercalate ",\n" (map pLine strings ++ map sLine splits ++ map tLine (zip templates parts)) ++ "\n  ]")
