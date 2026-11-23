-- Data.Char's isAlphaNum as ranges of code points, from GHC 9.10.3's own
-- tables, written as a Bend module
import Data.Char
ranges :: (Char -> Bool) -> [(Int,Int)]
ranges p = go [c | c <- [minBound..maxBound], p c]
  where go [] = []
        go (c:cs) = let n = length (takeWhile id (zipWith (\a b -> ord b == ord a + 1) (c:cs) cs))
                    in (ord c, ord c + n) : go (drop n cs)
main :: IO ()
main = do
  putStrLn "# Written by tests/gen/Tables.hs: Data.Char's isAlphaNum (GHC 9.10.3's"
  putStrLn "# Unicode tables) as ranges of code points, lowest first. Not by hand."
  putStrLn "import Base"
  putStrLn ""
  putStrLn "type Range is Data:"
  putStrLn "  Range{lo: U32, hi: U32}"
  putStrLn ""
  putStrLn "# the non-ASCII ones (Hs.bend answers ASCII itself)"
  putStrLn "def alnum() -> List<&2, Range>:"
  let rs = [r | r@(_, hi) <- ranges isAlphaNum, hi >= 128]
      line (lo, hi) = "    Range{" ++ show (max lo 128) ++ ", " ++ show hi ++ "}"
  putStrLn "  ["
  putStrLn (concat (zipWith (\i r -> (if i > 0 then ",\n" else "") ++ line r) [0 :: Int ..] rs))
  putStrLn "  ]"
