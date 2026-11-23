-- The Date plugin's differential test cases: random local times and zones
-- (1902-2037), random formats (every specifier, the modifiers, widths, E,
-- unknown ones, a final %), each with what time 1.12.2's formatTime with
-- defaultTimeLocale answers. Written as tests/DateCases.bend for
-- tests/DateDiff.bend.
module Main (main) where

import Data.Char (ord)
import Data.List (intercalate)
import Data.Time
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Numeric (showHex)
import System.Environment (getArgs)
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

gTime :: Gen ZonedTime
gTime = do
  d <- fromGregorian <$> choose (1902, 2037) <*> choose (1, 12) <*> choose (1, 31)
  h <- choose (0, 23)
  m <- choose (0, 59)
  s <- choose (0, 59) :: Gen Int
  ns <- frequency [(3, pure 0), (3, choose (0, 999999999)), (1, (* 1000000) <$> choose (0, 999))] :: Gen Integer
  off <- frequency [(3, pure (-360)), (2, choose (-720, 840)), (1, elements [0, 330, -210, 345])]
  name <- elements ["CST", "UTC", "", "JST", "-03"]
  let tod = TimeOfDay h m (fromIntegral s + fromIntegral ns / 1000000000)
  pure (ZonedTime (LocalTime d tod) (TimeZone off False name))

gSpec :: Gen String
gSpec = do
  modi <- frequency [(6, pure ""), (1, elements ["_", "-", "0", "^", "#"])]
  w <- frequency [(6, pure ""), (1, show <$> (choose (0, 15) :: Gen Int))]
  e <- frequency [(8, pure ""), (1, pure "E")]
  c <- elements "%tnzZcRTXrPpHIklMSqQsDFxYyCBbhmdejGgfVuaAUwWvEKiLoO1"
  pure ("%" ++ modi ++ w ++ e ++ [c])

gFormat :: Gen String
gFormat = do
  ps <- choose (1, 6) >>= flip vectorOf (frequency [(4, gSpec), (2, elements ["<fc=#ABABAB>", " (", ") ", ":", "-", "date ", "\233"]), (1, elements ["%a %b %_d %Y * %H:%M:%S", "%F (%a) %T", "<fc=#ABABAB>%F (%a) %T</fc>"])])
  end <- frequency [(8, pure ""), (1, elements ["%", "%_", "%-3", "%E"])]
  pure (concat ps ++ end)

bStr :: String -> String
bStr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc c
      | ord c >= 32 && ord c < 127 = [c]
      | otherwise = "\\u{" ++ showHex (ord c) "" ++ "}"

-- the time as Date.bend's Tm takes it
tm :: ZonedTime -> String
tm zt@(ZonedTime (LocalTime d (TimeOfDay h m s)) (TimeZone off _ name)) =
  let (y, mo, dd) = toGregorian d
      (_, yd) = toOrdinalDate d
      wd = fromEnum (dayOfWeek d) `mod` 7
      secs = floor s :: Int
      ns = floor ((s - fromIntegral secs) * 1000000000) :: Integer
      ep = floor (utcTimeToPOSIXSeconds (zonedTimeToUTC zt)) :: Integer
      i32 n = show (n `mod` 4294967296)
  in "D.Tm{" ++ intercalate ", " [i32 y, show mo, show dd, show h, show m, show secs, show ns, show wd, show (yd - 1), i32 (fromIntegral off), bStr name, i32 ep] ++ "}"

main :: IO ()
main = do
  [seedS, countS] <- getArgs
  let seed = read seedS
      count = read countS :: Int
      gen g i = unGen g (mkQCGen (seed * 1000003 + i)) 30
      cases = [(gen gFormat i, gen gTime (i + count)) | i <- [1 .. count]]
  putStrLn "# Written by tests/gen/GenDate.hs: formats and times with what time's"
  putStrLn "# formatTime defaultTimeLocale answers."
  putStrLn "import Base"
  putStrLn "import ../Date.bend as D"
  putStrLn ""
  putStrLn "type Case is Data:"
  putStrLn "  Case{f: String, t: D.Tm, want: String}"
  putStrLn ""
  putStrLn "def cases() -> List<&2, Case>:"
  putStrLn ("  [\n" ++ intercalate ",\n" ["    Case{" ++ bStr f ++ ", " ++ tm t ++ ", " ++ bStr (formatTime defaultTimeLocale f t) ++ "}" | (f, t) <- cases] ++ "\n  ]")
