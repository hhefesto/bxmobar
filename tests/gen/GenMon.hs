-- The monitors' differential test cases: random /proc and /sys files (a
-- few steps of each, the counters growing), random arguments for
-- MultiCpu, Memory, Swap, Network and DynNetwork, and what xmobar 0.50's
-- own monitor answers on them (run by tests/gen/MonRef.hs inside bwrap,
-- the files bound over /proc's and /sys's). Written as tests/MonCases.bend
-- for tests/MonDiff.bend.
--
--   genmon SEED COUNT MONREF BWRAP FAKETIME_LIB > tests/MonCases.bend
--
-- FAKETIME_LIB is libfaketime's libfaketime.so.1: the reference runs with
-- its wall clock stopped, so parseNet's seconds between two readings are 0
-- (and so 1, scx'), and a rate is the counters' difference; the Bend side
-- is given 0 seconds too.
module Main (main) where

import Control.Monad (filterM, forM)
import Data.List (intercalate)
import Gens
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

data Kind = KCpu | KMem | KSwap | KNet | KDyn deriving (Eq, Show)

-- a case: its monitor, its arguments, the device (Network's), the files
-- at each step (paths under the case's directory), and where bwrap puts
-- them
data Case = Case
  { cKind :: Kind
  , cArgs :: [String]
  , cDev :: String
  , cSteps :: [[(FilePath, String)]]
  , cSys :: [String]
  }

own :: Gen [String] -> Gen [String]
own g = (\o -> if null o then [] else "--" : o) <$> g

gCase :: Gen Case
gCase = do
  k <- frequency [(3, pure KCpu), (2, pure KMem), (1, pure KSwap), (2, pure KNet), (2, pure KDyn)]
  steps <- choose (2, 4)
  let one name files = [[(name, t)] | t <- files]
  case k of
    KCpu -> do
      files <- gStat steps
      args <- (++) <$> gCommon (cpuFields 16) <*> own gCpuOwn
      pure (Case k args "" (one "proc_stat" files) [])
    KMem -> do
      files <- gMeminfo steps
      args <- (++) <$> gCommon memFields <*> own gMemOwn
      pure (Case k args "" (one "proc_meminfo" files) [])
    KSwap -> do
      files <- gMeminfo steps
      args <- gCommon swapFields
      pure (Case k args "" (one "proc_meminfo" files) [])
    _ -> do
      ns <- gNetSteps (steps + 1)
      args <- (++) <$> gCommon netFields <*> own gNetOwn
      dev <- frequency [(4, elements (if null (nsSys ns) then netNames else nsSys ns)), (1, elements netNames)]
      let step i = ("proc_net_dev", nsProc ns !! i) : [("sys_net" </> n </> "operstate", os !! i) | (n, Just os) <- nsOper ns]
      pure (Case k args dev [step i | i <- [0 .. steps]] (nsSys ns))

spec :: Case -> String
spec c = case cKind c of
  KCpu -> "MultiCpu " ++ show (cArgs c) ++ " 2"
  KMem -> "Memory " ++ show (cArgs c) ++ " 2"
  KSwap -> "Swap " ++ show (cArgs c) ++ " 2"
  KNet -> "Network " ++ show (cDev c) ++ " " ++ show (cArgs c) ++ " 2"
  KDyn -> "DynNetwork " ++ show (cArgs c) ++ " 2"

binds :: Kind -> [String]
binds KCpu = ["--bind", "proc_stat", "/proc/stat"]
binds k | k `elem` [KMem, KSwap] = ["--bind", "proc_meminfo", "/proc/meminfo"]
-- /proc/net is a symlink to /proc/self/net, where no bind reaches every
-- process: a tmpfs /proc holds the one file instead
binds _ = ["--tmpfs", "/proc", "--dir", "/proc/net", "--bind", "proc_net_dev", "/proc/net/dev", "--bind", "sys_net", "/sys/class/net"]

-- the devices startDynNet finds, in its order: /sys/class/net's entries
-- (the order the directory lists them in, the same inside bwrap) with an
-- operstate, lo left out
existing :: FilePath -> IO [String]
existing sys = getDirectoryContents sys >>= filterM isDev
  where
    isDev d
      | d `elem` [".", "..", "lo"] = pure False
      | otherwise = doesFileExist (sys </> d </> "operstate")

write :: FilePath -> [String] -> [(FilePath, String)] -> IO ()
write d sys files = do
  createDirectoryIfMissing True (d </> "sys_net")
  mapM_ (\n -> createDirectoryIfMissing True (d </> "sys_net" </> n)) sys
  mapM_ (\(p, t) -> createDirectoryIfMissing True (takeDirectory (d </> p)) >> writeFile (d </> p) t) files

-- xmobar's outputs for a case, and the devices DynNetwork found: monref
-- in bwrap, the case's files bound
run :: FilePath -> FilePath -> FilePath -> FilePath -> Case -> IO ([String], [String])
run monref bwrap faketime dir c = do
  let cur = dir </> "cur"
  write cur (cSys c) (head (cSteps c))
  stepDirs <- forM (zip [0 :: Int ..] (cSteps c)) $ \(i, fs) -> do
    let d = dir </> show i
    write d (cSys c) fs
    pure d
  devs <- existing (cur </> "sys_net")
  let bound = go (binds (cKind c))
        where
          go ("--bind" : a : p : rest) = "--bind" : (cur </> a) : p : go rest
          go (x : rest) = x : go rest
          go [] = []
      clock = ["--setenv", "LD_PRELOAD", faketime, "--setenv", "FAKETIME", "2026-01-01 00:00:00", "--setenv", "FAKETIME_DONT_FAKE_MONOTONIC", "1"]
  out <- readProcess bwrap (["--dev-bind", "/", "/"] ++ bound ++ clock ++ [monref, spec c, cur] ++ stepDirs) ""
  pure (map read (lines out), devs)

-- a step's operstate of a device, "" when it has none
oper :: [(FilePath, String)] -> String -> String
oper fs n = maybe "" id (lookup ("sys_net" </> n </> "operstate") fs)

line :: (Case, ([String], [String])) -> String
line (c, (w, devs)) = "    " ++ case cKind c of
  KCpu -> "CpuCase{" ++ bList (cArgs c) ++ ", " ++ bList files ++ ", " ++ bList w ++ "}"
  KMem -> "MemCase{" ++ bList (cArgs c) ++ ", " ++ bList files ++ ", " ++ bList w ++ "}"
  KSwap -> "SwapCase{" ++ bList (cArgs c) ++ ", " ++ bList files ++ ", " ++ bList w ++ "}"
  KNet -> "NetCase{" ++ bStr (cDev c) ++ ", " ++ bList (cArgs c) ++ ", " ++ bList procs ++ ", " ++ bList [oper fs (cDev c) | fs <- cSteps c] ++ ", " ++ bList w ++ "}"
  KDyn -> "DynCase{" ++ bList devs ++ ", " ++ bList (cArgs c) ++ ", " ++ bList procs ++ ", " ++ bLists [[oper fs n | n <- devs] | fs <- cSteps c] ++ ", " ++ bList w ++ "}"
  where
    files = map (snd . head) (cSteps c)
    procs = [maybe "" id (lookup "proc_net_dev" fs) | fs <- cSteps c]

main :: IO ()
main = do
  [seedS, countS, monref, bwrap, faketime] <- getArgs
  let seed = read seedS
      count = read countS :: Int
      cases = [unGen gCase (mkQCGen (seed * 1000003 + i)) 30 | i <- [1 .. count]]
  outs <- forM (zip [0 :: Int ..] cases) $ \(i, c) ->
    withSystemTempDirectory ("genmon" ++ show i) $ \d -> run monref bwrap faketime d c
  putStrLn "# Written by tests/gen/GenMon.hs: /proc and /sys files, a monitor's"
  putStrLn "# arguments, and what xmobar 0.50's monitor answered at each step."
  putStrLn "import Base"
  putStrLn ""
  putStrLn "type Case is Data:"
  putStrLn "  CpuCase{args: List<&2, String>, files: List<&2, String>, want: List<&2, String>}"
  putStrLn "  MemCase{args: List<&2, String>, files: List<&2, String>, want: List<&2, String>}"
  putStrLn "  SwapCase{args: List<&2, String>, files: List<&2, String>, want: List<&2, String>}"
  putStrLn "  NetCase{dev: String, args: List<&2, String>, procs: List<&2, String>, opers: List<&2, String>, want: List<&2, String>}"
  putStrLn "  DynCase{devs: List<&2, String>, args: List<&2, String>, procs: List<&2, String>, opers: List<&2, List<&2, String>>, want: List<&2, String>}"
  putStrLn ""
  putStrLn "def cases() -> List<&2, Case>:"
  putStrLn ("  [\n" ++ intercalate ",\n" (map line (zip cases outs)) ++ "\n  ]")
