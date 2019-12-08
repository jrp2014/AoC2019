module Day08 where

import           Data.List.Split                ( chunksOf )
import           Data.List                      ( minimumBy
                                                , transpose
                                                )
import           Data.Ord                       ( comparing )


mkLayers :: [a] -> Int -> Int -> [[a]]
mkLayers txt width height = chunksOf (width * height) txt

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

fewestZeros :: [[Char]] -> [Char]
fewestZeros = minimumBy (comparing $ count '0')

-- find top visible poxel
renderPixel :: [Char] -> Char
renderPixel ('2' : rest) = renderPixel rest
renderPixel (c   : _   ) = c

render :: [Char] -> String
render ('0' : cs) = '.' : render cs -- black
render ('1' : cs) = '*' : render cs -- white
render ('2' : cs) = ' ' : render cs -- transparent
render []         = []

main :: IO ()
main = do
  rawInput <- readFile "input.txt"
  let input  = init rawInput -- drop final newline
  let layers = mkLayers input 25 6
  let layer  = fewestZeros layers
  putStr "Part 1: "
  print $ count '1' layer * count '2' layer
  putStrLn "Part 2: "
  let renderedPixels = renderPixel <$> transpose layers
  let mkLines        = chunksOf 25 renderedPixels
  putStrLn $ unlines (render <$> mkLines)
