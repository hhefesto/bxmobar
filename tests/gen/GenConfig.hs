-- The configuration's differential test cases: random xmobarrc texts (any
-- fields in any order, comments, continued strings, positions, borders,
-- command lists of many plugins; some broken), each with what xmobar
-- 0.50's own parseConfig answers (show of the config and the defaulted
-- fields, or ERROR). Written as tests/ConfigCases.bend for
-- tests/ConfigDiff.bend.
module Main (main) where

import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)
import System.Environment (getArgs)
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Xmobar.App.Config (defaultConfig)
import Xmobar.Config.Parse (parseConfig)

gStrText :: Gen String
gStrText = do
  n <- choose (0, 12)
  vectorOf n (frequency [(10, elements (['a' .. 'z'] ++ " %{}<>=#:,./-_()[]")), (1, elements "\233\955\\"), (1, pure '-')])

-- a string field's value: quoted text, sometimes continued over lines
gStr :: Gen String
gStr = frequency
  [ (8, (\s -> "\"" ++ s ++ "\"") <$> gStrText)
  , (1, (\a b -> "\"" ++ a ++ "\\\n      \\" ++ b ++ "\"") <$> gStrText <*> gStrText)
  , (1, elements ["\"unclosed", "noquote", "\"a\\\n  b\""])
  ]

gInt :: Gen String
gInt = frequency [(6, show <$> (choose (-5, 100) :: Gen Int)), (1, elements ["(-3)", " 7", "0x10", "1.5", "x"])]

gBool :: Gen String
gBool = frequency [(8, elements ["True", "False"]), (1, elements ["true", "(True)", "Truex", "True -- c"])]

gAlign :: Gen String
gAlign = elements ["L", "R", "C"]

gPos :: Int -> Gen String
gPos d = frequency $
  [ (2, elements ["Top", "Bottom"])
  , (2, (\h -> "TopH " ++ h) <$> gIntArg)
  , (2, (\h -> "BottomH " ++ h) <$> gIntArg)
  , (1, (\a b c e f -> unwords ["TopHM", a, b, c, e, f]) <$> gIntArg <*> gIntArg <*> gIntArg <*> gIntArg <*> gIntArg)
  , (1, (\a b -> unwords ["TopP", a, b]) <$> gIntArg <*> gIntArg)
  , (1, (\a b -> unwords ["BottomP", a, b]) <$> gIntArg <*> gIntArg)
  , (1, (\a w -> unwords ["TopW", a, w]) <$> gAlign <*> gIntArg)
  , (1, (\a w h -> unwords ["BottomSize", a, w, h]) <$> gAlign <*> gIntArg <*> gIntArg)
  , (1, (\a b c e -> "Static { xpos = " ++ a ++ " , ypos = " ++ b ++ ", width = " ++ c ++ ", height = " ++ e ++ " }") <$> gIntArg <*> gIntArg <*> gIntArg <*> gIntArg)
  , (1, elements ["TopH", "Middle", "TopW X 50", "Static {xpos = 1}"])
  ] ++ [(1, (\n p -> "OnScreen " ++ n ++ " (" ++ p ++ ")") <$> gIntArg <*> gPos (d - 1)) | d > 0]
  where gIntArg = frequency [(5, show <$> (choose (0, 1920) :: Gen Int)), (1, pure "(-2)"), (1, pure "-2")]

gBorder :: Gen String
gBorder = frequency [(4, elements ["NoBorder", "TopB", "BottomB", "FullB"]), (2, (\c n -> c ++ " " ++ show n) <$> elements ["TopBM", "BottomBM", "FullBM"] <*> (choose (0, 9) :: Gen Int)), (1, pure "Full")]

gList :: Gen String -> Gen String
gList g = (\xs -> "[" ++ intercalate ", " xs ++ "]") <$> (choose (0, 3) >>= flip vectorOf g)

gArgs :: Gen String
gArgs = gList (elements ["\"-t\"", "\"<total>%\"", "\"--Low\"", "\"20\"", "\"-L\"", "\"--\"", "\"--scale\""])

gRate :: Gen String
gRate = frequency [(5, show <$> (choose (1, 600) :: Gen Int)), (1, pure "(-1)"), (1, pure "-1")]

gRun :: Gen String
gRun = do
  p <- frequency
    [ (3, (\a r -> "Date " ++ a ++ " \"date\" " ++ r) <$> elements ["\"%F (%a) %T\"", "\"%H:%M\"", "\"<fc=#ABABAB>%F</fc>\""] <*> gRate)
    , (2, elements ["StdinReader", "UnsafeStdinReader", "XMonadLog", "UnsafeXMonadLog", "Locks"])
    , (2, (\a r -> "Com \"uname\" " ++ a ++ " \"\" " ++ r) <$> gList (elements ["\"-s\"", "\"-r\""]) <*> gRate)
    , (1, (\r -> "ComX \"x\" [] \"failed\" \"xx\" " ++ r) <$> gRate)
    , (3, (\c a r -> c ++ " " ++ a ++ " " ++ r) <$> elements ["DynNetwork", "Memory", "Swap", "Cpu", "MultiCpu", "MultiCoreTemp", "CoreTemp", "Load", "Uptime", "Battery", "Brightness", "TopProc", "TopMem", "CpuFreq"] <*> gArgs <*> gRate)
    , (1, (\a r -> "Network \"eth0\" " ++ a ++ " " ++ r) <$> gArgs <*> gRate)
    , (1, (\a r -> "DiskU [(\"/\", \"Disk: <usedp>%\")] " ++ a ++ " " ++ r) <$> gArgs <*> gRate)
    , (1, (\a r -> "BatteryP [\"BAT0\"] " ++ a ++ " " ++ r) <$> gArgs <*> gRate)
    , (1, (\a r -> "Weather \"KXYZ\" " ++ a ++ " " ++ r) <$> gArgs <*> gRate)
    , (1, (\a r -> "ThermalZone 0 " ++ a ++ " " ++ r) <$> gArgs <*> gRate)
    , (1, (\a r -> "CatInt 0 \"/sys/x\" " ++ a ++ " " ++ r) <$> gArgs <*> gRate)
    , (1, pure "Kbd [(\"us(dvorak)\", \"<fc=#00008B>DV</fc>\"), (\"us\", \"US\")]")
    , (1, pure "PipeReader \"/tmp/p\" \"pipe\"")
    , (1, elements ["Bogus 1", "Date \"%F\"", "Memory [] \"x\"", "StdinReader 3"])
    ]
  w <- frequency [(6, pure id), (1, pure (\x -> "(" ++ x ++ ")"))]
  pure ("Run " ++ w p)

gCommands :: Gen String
gCommands = do
  rs <- choose (0, 5) >>= flip vectorOf gRun
  sep <- elements [", ", "\n        , ", ",\n  "]
  pure ("[ " ++ intercalate sep rs ++ " ]")

fieldGen :: String -> Gen String
fieldGen f = case f of
  "font" -> gStr
  "additionalFonts" -> gList gStr
  "wmClass" -> gStr
  "wmName" -> gStr
  "bgColor" -> gStr
  "fgColor" -> gStr
  "borderColor" -> gStr
  "iconRoot" -> gStr
  "sepChar" -> gStr
  "alignSep" -> gStr
  "template" -> gStr
  "position" -> gPos 2
  "textOutput" -> gBool
  "textOutputFormat" -> elements ["Plain", "Ansi", "Pango", "Swaybar"]
  "textOffset" -> gInt
  "textOffsets" -> gList (show <$> (choose (-3, 30) :: Gen Int))
  "iconOffset" -> gInt
  "border" -> gBorder
  "borderWidth" -> gInt
  "alpha" -> gInt
  "commands" -> gCommands
  "dpi" -> elements ["96", "96.0", "120.5", "1e2", "x"]
  _ -> gBool

allFields :: [String]
allFields = ["font", "additionalFonts", "wmClass", "wmName", "bgColor", "fgColor", "position", "textOutput", "textOutputFormat", "textOffset", "textOffsets", "iconOffset", "border", "borderColor", "borderWidth", "alpha", "hideOnStart", "allDesktops", "overrideRedirect", "pickBroadest", "lowerOnStart", "persistent", "iconRoot", "commands", "sepChar", "alignSep", "template", "verbose", "dpi"]

gConfig :: Gen String
gConfig = do
  fs <- sublistOf allFields >>= shuffle
  dup <- frequency [(12, pure []), (1, (: []) <$> elements allFields)]
  vals <- mapM (\f -> (\v -> (f, v)) <$> fieldGen f) (fs ++ dup)
  lead <- elements ["", "\n", "-- my config\n"]
  open <- elements ["Config {", "Config{", "Config\n  { ", "Config { "]
  comment <- frequency [(4, pure ""), (1, pure "   -- a comment")]
  sep <- elements ["\n   , ", ", ", "\n,", " ,\n\n   -- between\n   "]
  close <- elements ["\n   }\n", " }", "}", "\n}\n}"]
  pure (lead ++ open ++ intercalate sep [f ++ " = " ++ v ++ comment | (f, v) <- vals] ++ close)

bStr :: String -> String
bStr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc c
      | ord c >= 32 && ord c < 127 = [c]
      | otherwise = "\\u{" ++ showHex (ord c) "" ++ "}"

answer :: String -> String
answer t = case parseConfig defaultConfig t of
  Left _ -> "ERROR"
  Right (c, defs) -> show c ++ "\n" ++ show defs

main :: IO ()
main = do
  [seedS, countS] <- getArgs
  let seed = read seedS
      count = read countS :: Int
      texts = [unGen gConfig (mkQCGen (seed * 1000003 + i)) 30 | i <- [1 .. count]]
  putStrLn "# Written by tests/gen/GenConfig.hs: xmobarrc texts and what xmobar 0.50's"
  putStrLn "# parseConfig answers (the config shown, the defaulted fields; or ERROR)."
  putStrLn "import Base"
  putStrLn ""
  putStrLn "type Case is Data:"
  putStrLn "  Case{text: String, want: String}"
  putStrLn ""
  putStrLn "def cases() -> List<&2, Case>:"
  putStrLn ("  [\n" ++ intercalate ",\n" ["    Case{" ++ bStr t ++ ", " ++ bStr (answer t) ++ "}" | t <- texts] ++ "\n  ]")
