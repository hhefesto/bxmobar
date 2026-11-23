-- The bar end to end: xmobar-bend (the binary Main.bend builds) against
-- xmobar 0.50's own monitors, both in one bwrap, with the same random
-- files in the places the monitors read (/proc/stat, /proc/meminfo,
-- /proc/net/dev, /sys/class/net, /sys/bus/platform/devices,
-- /sys/class/hwmon, /etc/mtab, /dev and tmpfs mounts of set sizes).
--
-- A case is one monitor with random arguments and random files. The bar
-- runs on a config whose template is an UnsafeStdinReader then the
-- monitor's alias, with standard input at its end: it writes its first
-- line and stops. The reference is tests/gen/MonRef.hs, which starts the
-- monitor as xmobar does and, with PLAIN set, writes the line as xmobar -T
-- would (Text.Output.format). The files do not change between the
-- monitor's two readings, so every rate and cpu share is 0 and the two
-- answers must be equal to the character; tests/mon-diff.sh tests the
-- changing readings of MultiCpu, Memory and Swap.
--
--   gene2e SEED COUNT MONREF BWRAP XMOBAR_BEND
module Main (main) where

import Control.Monad (forM, forM_)
import Data.List (intercalate, isPrefixOf)
import Gens
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Xmobar.Plugins.Monitors (Monitors)
import Xmobar.Run.Exec (alias)

-- what a case puts in place, its paths under the case's directory; "@D@"
-- in a text stands for that directory
data Mount
  = Bind FilePath FilePath -- the case's file or directory, over that path
  | DevFile FilePath String -- the case's file as /dev/NAME
  | DevLink String String -- a symlink in /dev: its target, its path
  | Tmpfs Integer FilePath -- a tmpfs of that many bytes at the case's path

data Case = Case
  { cSpec :: String
  , cDirs :: [FilePath]
  , cFiles :: [(FilePath, String)]
  , cMounts :: [Mount]
  , cFill :: [(FilePath, Integer)]
  }

-- Network and DynNetwork -------------------------------------------------------

gNet :: Bool -> Gen Case
gNet dyn = do
  inProc <- sublistOf netNames >>= shuffle
  inSys <- sublistOf netNames >>= shuffle
  states <- forM inSys $ \_ -> frequency
    [(4, pure (Just "up")), (2, pure (Just "unknown")), (2, pure (Just "down")), (1, pure (Just "dormant")), (1, pure Nothing)]
  nums <- forM inProc $ \_ -> vectorOf 16 (frequency [(3, choose (0, 100000)), (1, choose (0, 2 ^ (40 :: Int)))] :: Gen Integer)
  tight <- forM inProc $ \_ -> frequency [(3, pure False), (1, pure True)]
  let procText = netDevText (zip3 inProc nums tight)
  own <- gNetOwn
  args <- (++) <$> gCommon netFields <*> pure (if null own then [] else "--" : own)
  dev <- elements netNames
  let spec
        | dyn = "DynNetwork " ++ show args ++ " 10"
        | otherwise = "Network " ++ show dev ++ " " ++ show args ++ " 10"
  pure Case
    { cSpec = spec
    , cDirs = "sys_net" : ["sys_net" </> n | n <- inSys]
    , cFiles = ("proc_net_dev", procText) : [("sys_net" </> n </> "operstate", s ++ "\n") | (n, Just s) <- zip inSys states]
    , cMounts = [Bind "proc_net_dev" "/proc/net/dev", Bind "sys_net" "/sys/class/net"]
    , cFill = []
    }

-- MultiCoreTemp ------------------------------------------------------------------

ctFields :: [String]
ctFields = ["max", "maxpc", "maxbar", "maxvbar", "maxipat", "avg", "avgpc", "avgbar", "avgvbar", "avgipat", "core0", "core1", "core2", "core3"]

-- a hwmon directory's tempN_label and tempN_input files
gHwmon :: FilePath -> Gen [(FilePath, String)]
gHwmon dir = do
  n <- frequency [(1, pure 0), (6, choose (1, 6))]
  fmap concat $ forM [1 .. n :: Int] $ \i -> do
    lab <- frequency
      [(6, pure ("Core " ++ show (i - 1))), (1, pure "Package id 0"), (1, pure "Tdie"), (1, pure "Tctl"), (1, pure "edge"), (1, pure "Cor")]
    inp <- frequency [(12, show <$> (choose (0, 110000) :: Gen Int)), (1, pure "-5000"), (1, pure "45000\n12")]
    hasInp <- frequency [(12, pure True), (1, pure False)]
    pure ([(dir </> ("temp" ++ show i ++ "_label"), lab ++ "\n")] ++ [(dir </> ("temp" ++ show i ++ "_input"), inp ++ "\n") | hasInp])

gCT :: Gen Case
gCT = do
  layout <- elements [0, 1, 2 :: Int]
  hw0 <- elements ["hwmon3", "hwmon0", "hwmon12"]
  coreFiles <- gHwmon ("plat" </> "coretemp.0" </> "hwmon" </> hw0)
  others <- forM ["hwmon0", "hwmon1", "hwmon7"] $ \h -> gHwmon ("cls" </> h)
  pathFiles <- gHwmon "hp"
  noise <- sublistOf ["serial8250", "coretemp.x", "alarmtimer.0"]
  own <- frequency
    [ (3, pure [])
    , (3, concat <$> (choose (1, 3) >>= flip vectorOf (frequency
        [ (1, (\p -> ["--max-icon-pattern", p]) <$> gPattern)
        , (1, (\p -> ["--avg-icon-pattern", p]) <$> gPattern)
        , (2, (\v -> ["--mintemp", show v]) <$> (choose (0, 60) :: Gen Int))
        , (2, (\v -> ["--maxtemp", show v]) <$> (choose (61, 120) :: Gen Int))
        ])))
    ]
  let own'
        | layout == 2 = own ++ ["--hwmon-path", "@D@/hp"]
        | otherwise = own
  args <- (++) <$> gCommon ctFields <*> pure (if null own' then [] else "--" : own')
  let plat = ["plat" </> n | n <- noise] ++ (if layout == 0 then ["plat" </> "coretemp.0" </> "hwmon" </> hw0] else [])
  pure Case
    { cSpec = "MultiCoreTemp " ++ show args ++ " 10"
    , cDirs = ["plat", "cls", "hp"] ++ plat ++ ["cls" </> h | h <- ["hwmon0", "hwmon1", "hwmon7"]]
    , cFiles = (if layout == 0 then coreFiles else []) ++ concat others ++ pathFiles
    , cMounts = [Bind "plat" "/sys/bus/platform/devices", Bind "cls" "/sys/class/hwmon"]
    , cFill = []
    }

-- DiskU ----------------------------------------------------------------------------

diskFields :: [String]
diskFields = ["size", "free", "used", "freep", "usedp", "freebar", "freevbar", "freeipat", "usedbar", "usedvbar", "usedipat"]

gDisk :: Gen Case
gDisk = do
  k <- choose (1, 3)
  devs <- take k <$> shuffle ["sdz1", "sdz2", "nvme9n1p1"]
  sizes <- vectorOf k (elements [1048576, 4194304, 16777216])
  fills <- forM sizes $ \s -> choose (0, s * 9 `div` 10)
  linked <- vectorOf k (frequency [(3, pure False), (1, pure True)])
  let mnt i = "m" ++ show i
      dev i = devs !! i
      devPath i = if linked !! i then "/dev/disk/by-label/L" ++ show i else "/dev/" ++ dev i
      lines0 = [devPath i ++ " @D@/" ++ mnt i ++ " ext4 rw,relatime 0 0" | i <- [0 .. k - 1]]
      noise = ["proc /proc proc rw,nosuid 0 0", "tmpfs /run tmpfs rw 0 0", "/dev/missing0 @D@/mx ext4 rw 0 0"]
  mtab <- shuffle (lines0 ++ noise)
  keys <- forM [0 .. k - 1] $ \i -> elements [dev i, "@D@/" ++ mnt i]
  extra <- frequency [(4, pure []), (1, pure [("nope", "<used>")])]
  tpls <- forM keys $ \_ -> gTemplate diskFields
  disks <- shuffle (zip keys tpls ++ extra)
  own <- frequency
    [ (3, pure [])
    , (2, concat <$> (choose (1, 2) >>= flip vectorOf (frequency
        [ (1, (\p -> ["--free-icon-pattern", p]) <$> gPattern)
        , (1, (\p -> ["--used-icon-pattern", p]) <$> gPattern)
        , (1, pure ["--contiguous"])
        , (1, pure ["-c"])
        ])))
    ]
  args <- (++) <$> gCommon diskFields <*> pure (if null own then [] else "--" : own)
  pure Case
    { cSpec = "DiskU " ++ show disks ++ " " ++ show args ++ " 10"
    , cDirs = "mx" : [mnt i | i <- [0 .. k - 1]]
    , cFiles = ("mtab", unlines mtab) : [("dev_" ++ dev i, "") | i <- [0 .. k - 1]]
    , cMounts =
        [Bind "mtab" "/etc/mtab"]
          ++ [DevFile ("dev_" ++ dev i) (dev i) | i <- [0 .. k - 1]]
          ++ [DevLink ("../../" ++ dev i) ("disk/by-label/L" ++ show i) | i <- [0 .. k - 1], linked !! i]
          ++ [Tmpfs (sizes !! i) (mnt i) | i <- [0 .. k - 1]]
    , cFill = [(mnt i, fills !! i) | i <- [0 .. k - 1]]
    }

-- MultiCpu, Memory, Swap (one reading) ---------------------------------------------

gProc :: Gen Case
gProc = do
  k <- elements [0, 1, 2 :: Int]
  case k of
    0 -> do
      t <- head <$> gStat 1
      args <- (++) <$> gCommon (cpuFields 16) <*> ((\o -> if null o then [] else "--" : o) <$> gCpuOwn)
      pure (Case ("MultiCpu " ++ show args ++ " 10") [] [("proc_stat", t)] [Bind "proc_stat" "/proc/stat"] [])
    1 -> do
      t <- head <$> gMeminfo 1
      args <- (++) <$> gCommon memFields <*> ((\o -> if null o then [] else "--" : o) <$> gMemOwn)
      pure (Case ("Memory " ++ show args ++ " 10") [] [("proc_meminfo", t)] [Bind "proc_meminfo" "/proc/meminfo"] [])
    _ -> do
      t <- head <$> gMeminfo 1
      args <- gCommon swapFields
      pure (Case ("Swap " ++ show args ++ " 10") [] [("proc_meminfo", t)] [Bind "proc_meminfo" "/proc/meminfo"] [])

gCase :: Gen Case
gCase = frequency [(3, gNet True), (2, gNet False), (3, gCT), (3, gDisk), (2, gProc)]

-- Running ----------------------------------------------------------------------------

replace :: String -> String -> String -> String
replace a b s@(c : cs)
  | a `isPrefixOf` s = b ++ replace a b (drop (length a) s)
  | otherwise = c : replace a b cs
replace _ _ [] = []

mountArgs :: FilePath -> [Mount] -> [String]
mountArgs d ms = concatMap one ms
  where
    one m = case m of
      Bind a p -> ["--bind", d </> a, p]
      DevFile a n -> ["--bind", d </> a, "/dev" </> n]
      DevLink t p -> ["--symlink", t, "/dev" </> p]
      Tmpfs n a -> ["--size", show n, "--tmpfs", d </> a]

-- the reference's line and the bar's, for one case (/etc and /dev are
-- bwrap's own, an empty tmpfs and a minimal /dev, when a case puts a
-- file there)
runCase :: FilePath -> FilePath -> FilePath -> Int -> Case -> IO (String, String, String)
runCase monref bwrap xb i c = withSystemTempDirectory ("gene2e" ++ show i) $ \d -> do
  let sub = replace "@D@" d
      spec = sub (cSpec c)
      al = alias (read spec :: Monitors)
  forM_ (cDirs c) $ \p -> createDirectoryIfMissing True (d </> p)
  forM_ (cFiles c) $ \(p, t) -> do
    createDirectoryIfMissing True (takeDirectory (d </> p))
    writeFile (d </> p) (sub t)
  createDirectoryIfMissing True (d </> "empty")
  writeFile (d </> "spec") spec
  writeFile (d </> "xmobarrc") ("Config { template = \"%UnsafeStdinReader%|%" ++ al ++ "%\"\n       , commands = [ Run UnsafeStdinReader, Run " ++ spec ++ " ] }\n")
  let fill = intercalate "," [d </> p ++ ":" ++ show n | (p, n) <- cFill c]
      script = unlines
        [ "cd " ++ d
        , "PLAIN='Updating...|' " ++ (if null fill then "" else "FILL='" ++ fill ++ "' ") ++ monref ++ " \"$(cat spec)\" " ++ d </> "empty" ++ " " ++ d </> "empty"
        , "echo @@@"
        , "timeout 20 " ++ xb ++ " -T xmobarrc < /dev/null"
        ]
      etcDev =
        ["--tmpfs" | any isEtc (cMounts c)] ++ ["/etc" | any isEtc (cMounts c)]
          ++ ["--dev" | any isDevM (cMounts c)] ++ ["/dev" | any isDevM (cMounts c)]
          ++ concat [["--tmpfs", "/proc", "--dir", "/proc/net"] | any isNet (cMounts c)]
      -- /proc/net is a symlink to /proc/self/net, where no bind reaches
      -- every process: a tmpfs /proc holds the one file instead
      isNet (Bind _ p) = "/proc/net/" `isPrefixOf` p
      isNet _ = False
      isEtc (Bind _ p) = "/etc/" `isPrefixOf` p
      isEtc _ = False
      isDevM (DevFile _ _) = True
      isDevM (DevLink _ _) = True
      isDevM _ = False
  writeFile (d </> "run.sh") script
  (_, out, err) <- readProcessWithExitCode bwrap (["--dev-bind", "/", "/"] ++ etcDev ++ mountArgs d (cMounts c) ++ ["/bin/sh", d </> "run.sh"]) ""
  let (ref, rest) = break (== "@@@") (lines out)
      want = case drop (length (cFill c)) ref of
        w : _ -> read w
        [] -> "(no reference output) " ++ err
      -- a text with newlines (an exception's call stack) is several lines
      got = case drop 1 rest of
        [] -> "(no output) " ++ err
        gs -> intercalate "\n" gs
  pure (want, got, spec)

main :: IO ()
main = do
  [seedS, countS, monref, bwrap, xb] <- getArgs
  let seed = read seedS
      count = read countS :: Int
      cases = [unGen gCase (mkQCGen (seed * 1000003 + i)) 30 | i <- [1 .. count]]
  rs <- forM (zip [0 :: Int ..] cases) $ \(i, c) -> runCase monref bwrap xb i c
  let bad = [r | r@(w, g, _) <- rs, w /= g]
  forM_ (take 12 bad) $ \(w, g, s) -> do
    putStrLn ("differ: " ++ s)
    putStrLn ("  xmobar: " ++ show w)
    putStrLn ("  bend:   " ++ show g)
  -- how many cases of each monitor, and how many of them answered an error
  let kinds = foldr (\k ks -> if k `elem` ks then ks else k : ks) [] [takeWhile (/= ' ') s | (_, _, s) <- rs]
      count1 k = length [() | (_, _, s) <- rs, takeWhile (/= ' ') s == k]
      errs k = length [() | (w, _, s) <- rs, takeWhile (/= ' ') s == k, "Updating...|error" `isPrefixOf` w]
  putStrLn (unwords [k ++ " " ++ show (count1 k) ++ " (errors " ++ show (errs k) ++ ")" | k <- kinds])
  putStrLn ("same " ++ show (length rs - length bad) ++ ", differ " ++ show (length bad))
  hFlush stdout
