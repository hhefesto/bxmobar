-- The monitors' random inputs, shared by tests/gen/GenMon.hs (the pure
-- steps' cases) and tests/gen/GenE2E.hs (the whole bar's): the common
-- options, the templates, each monitor's own options, and /proc's files.
module Gens where

import Control.Monad (forM)
import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)
import Test.QuickCheck

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


-- Network and DynNetwork

netNames :: [String]
netNames = ["eth0", "wlp5s0", "lo", "tailscale0", "docker0", "enp3s0", "veth1a"]

netFields :: [String]
netFields = ["dev", "rx", "tx", "rxbar", "rxvbar", "rxipat", "txbar", "txvbar", "txipat", "up"]

gNetOwn :: Gen [String]
gNetOwn = frequency
  [ (4, pure [])
  , (3, concat <$> (choose (1, 3) >>= flip vectorOf (frequency
      [ (1, (\p -> ["--rx-icon-pattern", p]) <$> gPattern)
      , (1, (\p -> ["--tx-icon-pattern", p]) <$> gPattern)
      , (2, (\s -> ["--up", s]) <$> elements ["+", "UP", ""])
      , (2, (\ds -> ["--devices", intercalate "," ds]) <$> sublistOf netNames)
      ])))
  , (1, elements [["--bogus"], ["--up"]])
  ]

-- /proc/net/dev's text: its two header lines, then each device's line: its
-- name right-aligned in 6 as the kernel writes it, a colon (then a space,
-- unless tight) and its 16 counters
netDevText :: [(String, [Integer], Bool)] -> String
netDevText ds = unlines (header ++ [devLine n xs t | (n, xs, t) <- ds])
  where
    header =
      [ "Inter-|   Receive                                                |  Transmit"
      , " face |bytes    packets errs drop fifo frame compressed multicast|bytes    packets errs drop fifo colls carrier compressed"
      ]
    devLine n xs t = replicate (max 0 (6 - length n)) ' ' ++ n ++ ":" ++ (if t then "" else " ") ++ unwords (map show xs)

gCounter :: Gen Integer
gCounter = frequency [(3, choose (0, 100000)), (1, choose (0, 2 ^ (40 :: Int)))]

-- a device's counters at each step: they mostly grow, sometimes stay,
-- sometimes go back (a reset: Word64's difference wraps) or jump, and
-- now and then pass 2^64 (read at Word64 keeps them modulo 2^64)
gCounterSteps :: Int -> Gen [[Integer]]
gCounterSteps steps = do
  c0 <- vectorOf 16 gCounter
  let next xs = forM xs $ \x -> frequency
        [ (6, (x +) <$> choose (0, 200000))
        , (2, pure x)
        , (1, choose (0, x))
        , (1, (x +) <$> choose (0, 2 ^ (40 :: Int)))
        , (1, (2 ^ (64 :: Int) +) <$> choose (0, 1000))
        ]
      go 1 xs = pure [xs]
      go k xs = (xs :) <$> (next xs >>= go (k - 1))
  go steps c0

-- an operstate file's text at each step: mostly the same as the step
-- before; what xmobar compares is its first line
gOperstates :: Int -> Gen [String]
gOperstates steps = do
  let st = frequency [(4, pure "up\n"), (2, pure "unknown\n"), (3, pure "down\n"), (1, pure "dormant\n"), (1, elements ["up", "UP\n", "up x\n", " up\n", "unknown\nup\n", "down\nup\n", "up\ndown\n"])]
      go 1 _ = pure []
      go k prev = do
        s <- frequency [(3, pure prev), (2, st)]
        (s :) <$> go (k - 1) s
  s0 <- st
  (s0 :) <$> go steps s0

-- the network monitors' files at each step: /proc/net/dev (the devices in
-- it may change), and /sys/class/net/NAME/operstate for each NAME of the
-- devices in /sys (one may have no operstate, every step)
data NetSteps = NetSteps
  { nsSys :: [String] -- the devices in /sys/class/net
  , nsProc :: [String] -- /proc/net/dev at each step
  , nsOper :: [(String, Maybe [String])] -- each sys device's operstates
  }

gNetSteps :: Int -> Gen NetSteps
gNetSteps steps = do
  inProc0 <- sublistOf netNames >>= shuffle
  inSys <- sublistOf netNames >>= shuffle
  tight <- forM netNames (const (frequency [(3, pure False), (1, pure True)]))
  counters <- forM netNames (const (gCounterSteps steps))
  present <- forM [1 .. steps] $ \_ -> frequency [(5, pure inProc0), (1, sublistOf netNames >>= shuffle)]
  opers <- forM inSys $ \_ -> frequency [(9, Just <$> gOperstates steps), (1, pure Nothing)]
  let at n = head [(t, cs) | (m, t, cs) <- zip3 netNames tight counters, m == n]
      text k = netDevText [(n, snd (at n) !! k, fst (at n)) | n <- present !! k]
  pure (NetSteps inSys [text k | k <- [0 .. steps - 1]] (zip inSys opers))

bLists :: [[String]] -> String
bLists xss = "[" ++ intercalate ", " (map bList xss) ++ "]"
