-- The monitors' differential test cases: random /proc files (a few steps
-- of each, the counters growing), random arguments for MultiCpu, Memory
-- and Swap, and what xmobar 0.50's own monitor answers on them (run by
-- tests/gen/MonRef.hs inside bwrap, the files bound over /proc's).
-- Written as tests/MonCases.bend for tests/MonDiff.bend.
--
--   genmon SEED COUNT MONREF BWRAP > tests/MonCases.bend
module Main (main) where

import Control.Monad (forM, forM_)
import Data.List (intercalate)
import Gens
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

data Kind = KCpu | KMem | KSwap deriving (Eq, Show)

data Case = Case Kind [String] [String]

gCase :: Gen Case
gCase = do
  k <- frequency [(3, pure KCpu), (2, pure KMem), (1, pure KSwap)]
  steps <- choose (2, 4)
  case k of
    KCpu -> do
      files <- gStat steps
      args <- (++) <$> gCommon (cpuFields 16) <*> ((\o -> if null o then [] else "--" : o) <$> gCpuOwn)
      pure (Case k args files)
    KMem -> do
      files <- gMeminfo steps
      args <- (++) <$> gCommon memFields <*> ((\o -> if null o then [] else "--" : o) <$> gMemOwn)
      pure (Case k args files)
    KSwap -> do
      files <- gMeminfo steps
      args <- gCommon swapFields
      pure (Case k args files)

spec :: Case -> String
spec (Case KCpu args _) = "MultiCpu " ++ show args ++ " 2"
spec (Case KMem args _) = "Memory " ++ show args ++ " 2"
spec (Case KSwap args _) = "Swap " ++ show args ++ " 2"

target :: Kind -> (String, String)
target KCpu = ("proc_stat", "/proc/stat")
target _ = ("proc_meminfo", "/proc/meminfo")

-- xmobar's outputs for a case: monref in bwrap, the case's file bound
run :: FilePath -> FilePath -> FilePath -> Case -> IO [String]
run monref bwrap dir c@(Case k _ files) = do
  let (name, path) = target k
      cur = dir </> "cur"
  createDirectoryIfMissing True cur
  writeFile (cur </> name) (head files)
  stepDirs <- forM (zip [0 :: Int ..] files) $ \(i, t) -> do
    let d = dir </> show i
    createDirectoryIfMissing True d
    writeFile (d </> name) t
    pure d
  out <- readProcess bwrap (["--dev-bind", "/", "/", "--bind", cur </> name, path, monref, spec c, cur] ++ stepDirs) ""
  pure (map read (lines out))
main :: IO ()
main = do
  [seedS, countS, monref, bwrap] <- getArgs
  let seed = read seedS
      count = read countS :: Int
      cases = [unGen gCase (mkQCGen (seed * 1000003 + i)) 30 | i <- [1 .. count]]
  outs <- forM (zip [0 :: Int ..] cases) $ \(i, c) ->
    withSystemTempDirectory ("genmon" ++ show i) $ \d -> run monref bwrap d c
  putStrLn "# Written by tests/gen/GenMon.hs: /proc files, a monitor's arguments, and"
  putStrLn "# what xmobar 0.50's monitor answered at each step."
  putStrLn "import Base"
  putStrLn ""
  putStrLn "type Case is Data:"
  putStrLn "  CpuCase{args: List<&2, String>, files: List<&2, String>, want: List<&2, String>}"
  putStrLn "  MemCase{args: List<&2, String>, files: List<&2, String>, want: List<&2, String>}"
  putStrLn "  SwapCase{args: List<&2, String>, files: List<&2, String>, want: List<&2, String>}"
  putStrLn ""
  putStrLn "def cases() -> List<&2, Case>:"
  let line (Case k args files, w) = "    " ++ (case k of KCpu -> "CpuCase"; KMem -> "MemCase"; KSwap -> "SwapCase") ++ "{" ++ bList args ++ ", " ++ bList files ++ ", " ++ bList w ++ "}"
  putStrLn ("  [\n" ++ intercalate ",\n" (map line (zip cases outs)) ++ "\n  ]")
  forM_ [] (\() -> pure ())
