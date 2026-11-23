-- The X11 bar's differential cases: random configurations (position,
-- border, fonts and their offsets, colours, alpha, dpi) and random
-- templates (text, <fc>, <box>, <hspace>, <fn>, <action>, the three
-- sections, texts long enough to be cut) with an UnsafeStdinReader whose
-- line is random markup too. tests/x-diff.sh runs xmobar 0.50 and
-- xmobar-bend on each, in Xvfb, and compares the screens pixel by pixel
-- and the windows' geometry and properties.
--
--   genx SEED COUNT DIR   (DIR/N/config, DIR/N/line for N = 1..COUNT)
module Main (main) where

import Control.Monad (forM_)
import Data.List (intercalate)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

-- colours: X11 and SVG names (some only Pango or X knows), #rgb, #rrggbb,
-- #rrggbbaa
gColour :: Gen String
gColour = frequency
  [ (4, elements ["red", "green", "blue", "black", "white", "darkgreen", "darkorange", "darkred", "yellow", "grey", "gray", "orange", "purple", "cyan", "magenta"])
  , (4, ('#' :) <$> vectorOf 6 hexDigit)
  , (1, ('#' :) <$> vectorOf 3 hexDigit)
  , (1, (\h a -> '#' : h ++ a) <$> vectorOf 6 hexDigit <*> elements ["00", "ff", "80"])
  , (1, elements ["dark olive green", "DarkGreen", "lightgoldenrod", "aliceblue"])
  ]
  where
    hexDigit = elements "0123456789abcdefABCDEF"

-- a colour X parses (bgColor: XParseColor)
gXColour :: Gen String
gXColour = frequency
  [ (3, elements ["black", "white", "#000000", "#1d1f21", "darkblue", "grey20", "navy"])
  , (2, ('#' :) <$> vectorOf 6 (elements "0123456789abcdef"))
  ]

gWord :: Gen String
gWord = frequency
  [ (6, elements ["cpu", "mem", "42%", "hello", "world", "net", "kB/s", "12:34", "|", "Disk:", "Ωmega", "naïve", "日本", "a&b", "x<y", "'q'"])
  , (1, choose (1, 8) >>= flip vectorOf (elements "abcxyz0123 "))
  , (1, pure "a fairly long piece of text that should not fit anywhere at all on a bar")
  ]

gText :: Gen String
gText = unwords <$> (choose (1, 3) >>= flip vectorOf gWord)

gBoxAttrs :: Gen String
gBoxAttrs = concat <$> sequence
  [ opt (\t -> " type=" ++ t) (elements ["Top", "Bottom", "VBoth", "Left", "Right", "HBoth", "Full"])
  , opt (\w -> " width=" ++ show w) (choose (0, 4 :: Int))
  , opt (\c -> " color=" ++ c) (elements ["red", "green", "#00ff00", "#123456", "blue", "dark olive green", "#abc"])
  , opt (\(a, m) -> " offset=" ++ a ++ show m) ((,) <$> elements ["L", "R", "C"] <*> choose (0, 6 :: Int))
  , opt (\m -> " mt=" ++ show m) (choose (0, 5 :: Int))
  , opt (\m -> " mb=" ++ show m) (choose (0, 5 :: Int))
  , opt (\m -> " ml=" ++ show m) (choose (0, 5 :: Int))
  , opt (\m -> " mr=" ++ show m) (choose (0, 5 :: Int))
  ]
  where
    opt f g = frequency [(1, pure ""), (1, f <$> g)]

-- markup, depth d
gMarkup :: [String] -> Int -> Gen String
gMarkup is d = concat <$> (choose (1, 3) >>= flip vectorOf (gItem is d))

-- is: the icon files there are
gItem :: [String] -> Int -> Gen String
gItem is d = frequency $
  [ (6, gText)
  , (1, (\p -> "<icon=" ++ p ++ "/>") <$> elements (is ++ ["none.xbm", "/nonexistent/x.xpm", "."]))
  , (1, (\n -> "<hspace=" ++ show n ++ "/>") <$> choose (0, 40 :: Int))
  ] ++ if d <= 0 then [] else
  [ (3, (\c m -> "<fc=" ++ c ++ ">" ++ m ++ "</fc>") <$> gColour <*> gMarkup is (d - 1))
  , (2, (\c b m -> "<fc=" ++ c ++ "," ++ b ++ ">" ++ m ++ "</fc>") <$> gColour <*> gColour <*> gMarkup is (d - 1))
  , (1, (\c b o m -> "<fc=" ++ c ++ "," ++ b ++ ":" ++ show o ++ ">" ++ m ++ "</fc>") <$> gColour <*> gColour <*> choose (0, 6 :: Int) <*> gMarkup is (d - 1))
  , (2, (\a m -> "<box" ++ a ++ ">" ++ m ++ "</box>") <$> gBoxAttrs <*> gMarkup is (d - 1))
  , (1, (\i m -> "<fn=" ++ show i ++ ">" ++ m ++ "</fn>") <$> choose (0, 3 :: Int) <*> gMarkup is (d - 1))
  , (2, (\n b m -> "<action=`echo a" ++ show n ++ " >> clicks` button=" ++ b ++ ">" ++ m ++ "</action>") <$> choose (0, 999 :: Int) <*> elements ["1", "3", "13", "45"] <*> gMarkup is (d - 1))
  ]

gFont :: Gen String
gFont = do
  fam <- elements ["DejaVu Sans Mono", "DejaVu Sans", "DejaVu Serif", "DejaVu Sans Mono Bold", "Nonexistent Font"]
  size <- elements [8, 10, 12, 16, 20 :: Int]
  frequency [(4, pure (fam ++ " " ++ show size)), (1, pure ("xft:" ++ map dash fam ++ "-" ++ show size))]
  where
    dash ' ' = '-'
    dash c = c

gPosition :: Gen String
gPosition = frequency
  [ (2, pure "Top")
  , (2, pure "Bottom")
  , (2, ("TopH " ++) . show <$> choose (0, 60 :: Int))
  , (2, ("BottomH " ++) . show <$> choose (0, 60 :: Int))
  , (1, (\a p -> "TopW " ++ a ++ " " ++ show p) <$> elements ["L", "R", "C"] <*> choose (10, 120 :: Int))
  , (1, (\a p -> "BottomW " ++ a ++ " " ++ show p) <$> elements ["L", "R", "C"] <*> choose (10, 120 :: Int))
  , (1, (\a p h -> "TopSize " ++ a ++ " " ++ show p ++ " " ++ show h) <$> elements ["L", "R", "C"] <*> choose (10, 100 :: Int) <*> choose (0, 50 :: Int))
  , (1, (\a p h -> "BottomSize " ++ a ++ " " ++ show p ++ " " ++ show h) <$> elements ["L", "R", "C"] <*> choose (10, 100 :: Int) <*> choose (0, 50 :: Int))
  , (1, (\l r -> "TopP " ++ show l ++ " " ++ show r) <$> choose (0, 200 :: Int) <*> choose (0, 200 :: Int))
  , (1, (\l r -> "BottomP " ++ show l ++ " " ++ show r) <$> choose (0, 200 :: Int) <*> choose (0, 200 :: Int))
  , (1, (\h l r t b -> "TopHM " ++ unwords (map show [h, l, r, t, b])) <$> choose (0, 50) <*> choose (0, 100) <*> choose (0, 100) <*> choose (0, 30) <*> choose (0, 30 :: Int))
  , (1, (\h l r t b -> "BottomHM " ++ unwords (map show [h, l, r, t, b])) <$> choose (0, 50) <*> choose (0, 100) <*> choose (0, 100) <*> choose (0, 30) <*> choose (0, 30 :: Int))
  , (1, (\x y w h -> "Static { xpos = " ++ show x ++ ", ypos = " ++ show y ++ ", width = " ++ show w ++ ", height = " ++ show h ++ " }") <$> choose (0, 400 :: Int) <*> choose (0, 200 :: Int) <*> choose (50, 600 :: Int) <*> choose (10, 60 :: Int))
  , (1, (\p -> "OnScreen 0 (" ++ p ++ ")") <$> elements ["Top", "BottomH 30", "TopW C 50"])
  ]

gBorder :: Gen String
gBorder = frequency
  [ (3, pure "NoBorder")
  , (2, elements ["TopB", "BottomB", "FullB"])
  , (1, (\c m -> c ++ " " ++ show m) <$> elements ["TopBM", "BottomBM", "FullBM"] <*> choose (0, 8 :: Int))
  ]

gConfig :: Gen (String, String, [(String, String)])
gConfig = do
  icons <- gIcons
  let is = map fst icons ++ map (("./" ++) . fst) icons
  ioff <- frequency [(3, pure Nothing), (1, Just <$> choose (-6, 6 :: Int))]
  font <- gFont
  nfonts <- choose (0, 3)
  fonts <- vectorOf nfonts gFont
  offs <- frequency [(3, pure Nothing), (1, Just <$> choose (0, 12 :: Int))]
  toffs <- frequency [(3, pure Nothing), (1, Just <$> vectorOf nfonts (choose (-1, 12 :: Int)))]
  bg <- gXColour
  fg <- gColour
  pos <- gPosition
  border <- gBorder
  bcol <- gColour
  bw <- frequency [(2, pure Nothing), (2, Just <$> choose (0, 4 :: Int))]
  alpha <- frequency [(5, pure Nothing), (1, Just <$> choose (0, 255 :: Int))]
  dpi <- frequency [(5, pure Nothing), (1, Just <$> elements [72, 96, 120, 144 :: Int])]
  l <- frequency [(1, pure ""), (3, gMarkup is 2)]
  c <- frequency [(2, pure ""), (2, gMarkup is 2)]
  r <- frequency [(1, pure ""), (3, gMarkup is 2)]
  slot <- elements [0, 1, 2 :: Int]
  line <- gMarkup is 2
  let stdin = "%UnsafeStdinReader%"
      safe = filter (`notElem` "%{}")
      sections = case slot of
        0 -> [safe l ++ stdin, safe c, safe r]
        1 -> [safe l, safe c ++ stdin, safe r]
        _ -> [safe l, safe c, stdin ++ safe r]
      tpl = intercalate "}" [head sections, (sections !! 1) ++ "{" ++ (sections !! 2)]
      field k v = "  , " ++ k ++ " = " ++ v
      -- xmobar's strings: raw to the next quote (none inside)
      q s = "\"" ++ filter (/= '"') s ++ "\""
      cfg = unlines $
        [ "Config { font = " ++ q font
        , field "additionalFonts" ("[" ++ intercalate ", " (map q fonts) ++ "]")
        , field "bgColor" (q bg)
        , field "fgColor" (q fg)
        , field "position" pos
        , field "border" border
        , field "borderColor" (q bcol)
        , field "template" (q tpl)
        , field "commands" "[Run UnsafeStdinReader]"
        , field "lowerOnStart" "True"
        , field "overrideRedirect" "True"
        ]
        ++ [field "textOffset" (show o) | Just o <- [offs]]
        ++ [field "iconOffset" (show o) | Just o <- [ioff]]
        ++ [field "textOffsets" (show o) | Just o <- [toffs]]
        ++ [field "borderWidth" (show w) | Just w <- [bw]]
        ++ [field "alpha" (show a) | Just a <- [alpha]]
        ++ [field "dpi" (show (fromIntegral d :: Double)) | Just d <- [dpi]]
        ++ ["  }"]
  pure (cfg, line, icons)

main :: IO ()
main = do
  [seedS, countS, dir] <- getArgs
  let seed = read seedS
  forM_ [1 .. read countS :: Int] $ \i -> do
    let (cfg, line, icons) = unGen gConfig (mkQCGen (seed * 1000003 + i)) 30
        d = dir </> show i
    createDirectoryIfMissing True d
    forM_ icons $ \(n, t) -> writeFile (d </> n) t
    writeFile (d </> "config") cfg
    writeFile (d </> "line") (filter (/= '\n') line ++ "\n")

-- Icons -------------------------------------------------------------------------------------

-- an XBM of w by h, random bits
gXbm :: Gen String
gXbm = do
  w <- choose (1, 24 :: Int)
  h <- choose (1, 24 :: Int)
  bytes <- vectorOf (((w + 7) `div` 8) * h) (choose (0, 255 :: Int))
  let hex b = "0x" ++ [hd (b `div` 16), hd (b `mod` 16)]
      hd d = "0123456789abcdef" !! d
  pure $ unlines
    [ "#define i_width " ++ show w
    , "#define i_height " ++ show h
    , "static unsigned char i_bits[] = {"
    , "  " ++ intercalate ", " (map hex bytes) ++ " };"
    ]

-- an XPM of w by h in up to four colours, one maybe None (a shape mask)
gXpm :: Gen String
gXpm = do
  w <- choose (1, 24 :: Int)
  h <- choose (1, 24 :: Int)
  k <- choose (1, 4 :: Int)
  cols <- vectorOf k (frequency [(3, gHex), (1, pure "None"), (1, elements ["red", "blue", "white"])])
  let keys = take k ".+@#"
  rows <- vectorOf h (vectorOf w (elements keys))
  pure $ unlines $
    [ "/* XPM */"
    , "static char * i_xpm[] = {"
    , show (unwords [show w, show h, show k, "1"]) ++ ","
    ]
    ++ [show ([c] ++ " c " ++ col) ++ "," | (c, col) <- zip keys cols]
    ++ [show r ++ (if i == h then "};" else ",") | (i, r) <- zip [1 ..] rows]
  where
    gHex = ('#' :) <$> vectorOf 6 (elements "0123456789abcdef")

-- the case's icon files (name, text) and the names its markup may use
gIcons :: Gen [(String, String)]
gIcons = do
  n <- choose (0, 3 :: Int)
  fmap concat $ mapM (\i -> frequency
    [ (3, (\t -> [("i" ++ show i ++ ".xbm", t)]) <$> gXbm)
    , (3, (\t -> [("i" ++ show i ++ ".xpm", t)]) <$> gXpm)
    , (1, pure [("i" ++ show i ++ ".bad", "not a bitmap\n")])
    ]) [1 .. n]
