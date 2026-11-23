-- The monitors' differential test cases: random /proc files (a few steps
-- of each, the counters growing), random arguments for MultiCpu, Memory
-- and Swap, and what xmobar 0.50's own monitor answers on them (run by
-- tests/gen/MonRef.hs inside bwrap, the files bound over /proc's).
-- Written as tests/MonCases.bend for tests/MonDiff.bend.
--
--   genmon SEED COUNT MONREF BWRAP > tests/MonCases.bend
module Main (main) where

import Control.Monad (forM, forM_)
import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)
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

-- the common options, with numbers that read
gCommon :: [String] -> Gen [String]
gCommon fields = concat <$> (choose (0, 4) >>= flip vectorOf one)
  where
    one = frequency
      [ (3, (\v -> ["-L", show v]) <$> (choose (0, 100) :: Gen Int))
      , (3, (\v -> ["-H", show v]) <$> (choose (0, 100) :: Gen Int))
      , (2, (\v -> ["-l", v]) <$> elements ["darkgreen", "#00ff00"])
      , (2, (\v -> ["-n", v]) <$> elements ["darkorange", "yellow"])
      , (2, (\v -> ["-h", v]) <$> elements ["darkred", "red"])
      , (6, (\t -> ["-t", t]) <$> gTemplate fields)
      , (1, (\v -> ["-S", v]) <$> elements ["True", "False"])
      , (2, (\v -> ["-d", show v]) <$> (choose (0, 3) :: Gen Int))
      , (1, (\v -> ["-p", show v]) <$> (choose (0, 4) :: Gen Int))
      , (1, (\v -> ["-m", show v]) <$> (choose (0, 6) :: Gen Int))
      , (1, (\v -> ["-W", show v]) <$> (choose (0, 12) :: Gen Int))
      , (1, (\v -> ["-b", v, "-f", v]) <$> elements ["-", ":#"])
      , (1, (\v -> ["-T", show v]) <$> (choose (0, 30) :: Gen Int))
      ]

gTemplate :: [String] -> Gen String
gTemplate fields = concat <$> (choose (1, 5) >>= flip vectorOf (frequency
  [ (5, (\f -> "<" ++ f ++ ">") <$> elements fields)
  , (2, elements ["Cpu: ", "%", "|", " ", "Mem: ", "M", "<fc=red>", "</fc>"])
  , (1, elements ["<zz>", "x"])
  ]))

cpuFields :: Int -> [String]
cpuFields n = [p ++ v ++ s | v <- vars, (p, s) <- ("", "") : ("auto", "") : [("", show i) | i <- [0 .. n]]]
  where vars = ["bar", "vbar", "ipat", "total", "user", "nice", "system", "idle"]

memFields :: [String]
memFields = ["usedbar", "usedvbar", "usedipat", "freebar", "freevbar", "freeipat", "availablebar", "availablevbar", "availableipat", "usedratio", "freeratio", "availableratio", "total", "free", "buffer", "cache", "available", "used"]

swapFields :: [String]
swapFields = ["usedratio", "total", "used", "free"]

gPattern :: Gen String
gPattern = elements ["<icon=cpu_%%.xbm/>", "c%%", "", "%%%%"]

gCpuOwn :: Gen [String]
gCpuOwn = frequency
  [ (5, pure [])
  , (3, concat <$> (choose (1, 3) >>= flip vectorOf (frequency
      [ (2, (\p -> ["--load-icon-pattern", p]) <$> gPattern)
      , (2, (\p -> ["--load-icon-patterns", p]) <$> gPattern)
      , (1, (\p -> ["--fallback-icon-pattern", p]) <$> gPattern)
      , (1, pure ["--contiguous-icons"])
      ])))
  , (1, elements [["--bogus"], ["--load-icon-pattern"], ["-x"]])
  ]

gMemOwn :: Gen [String]
gMemOwn = frequency
  [ (5, pure [])
  , (3, concat <$> (choose (1, 3) >>= flip vectorOf (frequency
      [ (1, (\p -> ["--used-icon-pattern", p]) <$> gPattern)
      , (1, (\p -> ["--free-icon-pattern", p]) <$> gPattern)
      , (1, (\p -> ["--available-icon-pattern", p]) <$> gPattern)
      , (2, (\s -> ["--scale", s]) <$> elements ["1024", "0.5", "2", "1e3", "1"])
      ])))
  , (1, elements [["--bogus"], ["--scale"]])
  ]

-- /proc/stat: the aggregate line, one per cpu, then the rest; the
-- counters grow by up to d each step
gStat :: Int -> Gen [String]
gStat steps = do
  ncpu <- elements [1, 2, 4, 16]
  nf <- frequency [(6, pure 10), (1, elements [4, 7, 8])]
  base <- vectorOf (ncpu * nf) (frequency [(3, choose (0, 100000)), (1, choose (0, 2 ^ (40 :: Int)))] :: Gen Integer)
  incs <- vectorOf (steps * ncpu * nf) (frequency [(4, choose (0, 300)), (1, pure 0)] :: Gen Integer)
  let per k = [[base !! (c * nf + f) + sum [incs !! (s * ncpu * nf + c * nf + f) | s <- [0 .. k - 1]] | f <- [0 .. nf - 1]] | c <- [0 .. ncpu - 1]]
      agg k = foldr1 (zipWith (+)) (per k)
      line name xs = name ++ " " ++ unwords (map show xs)
      text k = unlines ([line "cpu " (agg k)] ++ [line ("cpu" ++ show c) xs | (c, xs) <- zip [0 :: Int ..] (per k)] ++ ["intr 123 4 5", "ctxt 999", "btime 1700000000", "processes 42", "procs_running 1", "procs_blocked 0"])
  pure [text k | k <- [0 .. steps - 1]]

gMeminfo :: Int -> Gen [String]
gMeminfo steps = forM [1 .. steps] $ \_ -> do
  total <- choose (100000, 64000000) :: Gen Integer
  free <- choose (0, total)
  avail <- choose (free, total)
  buffers <- choose (0, total `div` 10)
  cached <- choose (0, total `div` 2)
  swapT <- frequency [(4, choose (1, 32000000)), (1, pure 0)] :: Gen Integer
  swapF <- choose (0, swapT)
  hasAvail <- frequency [(5, pure True), (1, pure False)]
  let kv k v = k ++ ":" ++ replicate (16 - length k) ' ' ++ show v ++ " kB"
  pure (unlines ([kv "MemTotal" total, kv "MemFree" free] ++ [kv "MemAvailable" avail | hasAvail] ++ [kv "Buffers" buffers, kv "Cached" cached, kv "SwapCached" (0 :: Int), kv "Active" (total `div` 3), kv "Inactive" (total `div` 4), kv "Active(anon)" (1 :: Int), kv "SwapTotal" swapT, kv "SwapFree" swapF, kv "Dirty" (12 :: Int)]))

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
