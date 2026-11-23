-- StdinReader's stripActions, differential cases: random lines made of
-- the regex's pieces (<action=, backticks, button=, digits, >,
-- </action>, spaces) and other text (non-ASCII too), and what xmobar
-- 0.50's stripActions answers. Written as tests/StripCases.bend for
-- tests/StripDiff.bend.
--
--   genstrip SEED COUNT > tests/StripCases.bend
module Main (main) where

import Data.List (intercalate)
import Gens (bList)
import System.Environment (getArgs)
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Xmobar.Run.Actions (stripActions)

gPiece :: Gen String
gPiece = frequency
  [ (4, pure "<action=")
  , (3, pure "`")
  , (3, pure ">")
  , (4, pure "</action>")
  , (2, pure " button=")
  , (2, (: []) <$> elements "0123456789")
  , (3, elements ["cmd", "xdotool key super+1", "a", "x y", "=", "<", "/", "action"])
  , (2, pure " ")
  , (1, elements ["é", "Ω", "日本", "\t", "ÿ", "€", "Ã©", "\x13e", "\x160", "\x13c", "\x0", "\xc3", "\xa9", "\xe2\x82\xac", "\xf0\x9f\x98\x80", "\xed\xa0\x80", "\xc0\xaf"])
  , (1, pure "<action=`echo hi` button=13>")
  , (1, pure "<action=cmd>")
  ]

gLine :: Gen String
gLine = concat <$> (choose (0, 14) >>= flip vectorOf gPiece)

main :: IO ()
main = do
  [seedS, countS] <- getArgs
  let seed = read seedS
      cases = [unGen gLine (mkQCGen (seed * 1000003 + i)) 30 | i <- [1 .. read countS :: Int]]
  putStrLn "# Written by tests/gen/GenStrip.hs: lines, and xmobar 0.50's stripActions of each."
  putStrLn "import Base"
  putStrLn ""
  putStrLn "type Case is Data:"
  putStrLn "  Case{line: String, want: String}"
  putStrLn ""
  putStrLn "def cases() -> List<&2, Case>:"
  putStrLn ("  [\n" ++ intercalate ",\n" ["    Case{" ++ tail (init (bList [l])) ++ ", " ++ tail (init (bList [stripActions l])) ++ "}" | l <- cases] ++ "\n  ]")
