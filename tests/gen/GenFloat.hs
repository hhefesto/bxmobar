-- Float's differential test cases: random Floats (any bits, and values a
-- monitor prints: percentages, halves, rates, denormals, powers of two),
-- each with what GHC answers for showFFloat (Just d), show, show . round,
-- and random Integers with the bits of fromInteger at Float. Written as
-- tests/FloatCases.bend for tests/FloatDiff.bend.
module Main (main) where

import Data.List (intercalate)
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import Numeric (showFFloat)
import System.Environment (getArgs)
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Text.Read (readMaybe)

gFloat :: Gen Float
gFloat = frequency
  [ (3, castWord32ToFloat <$> arbitrary)
  , (3, (/ 100) . fromIntegral <$> (choose (0, 100000) :: Gen Int))
  , (2, (\n -> fromIntegral n + 0.5) <$> (choose (-50, 200) :: Gen Int))
  , (2, (\a b -> fromIntegral (a :: Int) / fromIntegral (max 1 (b :: Int))) <$> choose (0, 1000000) <*> choose (1, 100000))
  , (1, (\n -> 2 ^^ (n :: Int)) <$> choose (-149, 127))
  , (1, castWord32ToFloat <$> choose (0, 8388607))
  , (1, elements [0, -0, 0.05, 0.35, 12.45, 99.5, 1/0, -1/0, 0/0, 1e7, 1e8, 12345678, 0.1, 3.4028235e38, 1.0e-45])
  , (1, negate <$> (/ 7) . fromIntegral <$> (choose (0, 100000) :: Gen Int))
  ]

gInteger :: Gen Integer
gInteger = frequency
  [ (3, choose (0, 2 ^ (32 :: Int)))
  , (3, choose (0, 2 ^ (64 :: Int)))
  , (1, choose (0, 2 ^ (130 :: Int)))
  , (1, (\k -> 2 ^ (k :: Int)) <$> choose (0, 140))
  , (1, (\k d -> 2 ^ (k :: Int) + d) <$> choose (24, 80) <*> choose (-3, 3))
  , (1, (\k -> 2 ^ (k :: Int) * 3 + 2 ^ (k - 24)) <$> choose (30, 100))
  ]

-- strings as read at Float sees them: integers, decimals, exponents, the
-- tokens it also takes (NaN, Infinity, hexadecimal), and what it refuses
gReadText :: Gen String
gReadText = frequency
  [ (3, show <$> (choose (0, 100000000) :: Gen Integer))
  , (3, (\a b -> show a ++ "." ++ show b) <$> (choose (0, 100000) :: Gen Int) <*> (choose (0, 99999) :: Gen Int))
  , (2, (\a e -> show a ++ "e" ++ show e) <$> (choose (0, 99999) :: Gen Int) <*> (choose (-60, 45) :: Gen Int))
  , (1, (\a b e -> show a ++ "." ++ show b ++ "E+" ++ show e) <$> (choose (0, 999) :: Gen Int) <*> (choose (0, 999) :: Gen Int) <*> (choose (0, 40) :: Gen Int))
  , (1, show <$> (choose (0, 10 ^ (60 :: Int)) :: Gen Integer))
  , (1, (\k -> "0." ++ replicate k '0' ++ "1") <$> choose (0, 50))
  , (2, elements ["-5", "(3.5)", " 7 ", "( -2.25 )", "- 1", "NaN", "Infinity", "-Infinity", "0x1F", "0o17", "1.", ".5", "1e", "1e+", "5a", "", "--1", "1e400", "1e-400", "3.4028235e38", "3.4028236e38", "1.4e-45", "7.0e-46", "0.1", "16303164", "100", "1_000"])
  ]

main :: IO ()
main = do
  [seedS, countS] <- getArgs
  let seed = read seedS
      count = read countS :: Int
      gen g i = unGen g (mkQCGen (seed * 1000003 + i)) 30
      xs = [gen gFloat i | i <- [1 .. count]]
      ns = [max 0 (gen gInteger (i + count)) | i <- [1 .. count `div` 2]]
      rs = [gen gReadText (i + 2 * count) | i <- [1 .. count]]
      rLine s = "    RCase{" ++ show s ++ ", " ++ (case readMaybe s :: Maybe Float of Nothing -> "None{}"; Just v -> "Some{" ++ (if isNaN v then "2143289344" else w v) ++ "}") ++ "}"
      w = show . castFloatToWord32
      fLine x = "    FCase{" ++ w x ++ ", " ++ show (map (\d -> showFFloat (Just d) x "") [0 .. 4]) ++ ", " ++ show (show x) ++ ", " ++ show (if abs x < 2 ^ (62 :: Int) && not (isNaN x) then show (round x :: Int) else "") ++ "}"
      iLine n = "    ICase{" ++ show (show n) ++ ", " ++ w (fromInteger n :: Float) ++ "}"
  putStrLn "# Written by tests/gen/GenFloat.hs: Floats with GHC's showFFloat, show and"
  putStrLn "# round; Integers with the bits of fromInteger at Float."
  putStrLn "import Base"
  putStrLn ""
  putStrLn "type Case is Data:"
  putStrLn "  FCase{bits: U32, fixed: List<&2, String>, shown: String, rounded: String}"
  putStrLn "  ICase{n: String, bits: U32}"
  putStrLn "  RCase{s: String, bits: Maybe<&2, U32>}"
  putStrLn ""
  putStrLn "def cases() -> List<&2, Case>:"
  putStrLn ("  [\n" ++ intercalate ",\n" (map fLine xs ++ map iLine ns ++ map rLine rs) ++ "\n  ]")
