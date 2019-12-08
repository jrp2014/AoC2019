module Day08 where

import           Data.List.Split                ( chunksOf )
import           Data.List                      ( minimumBy
                                                , transpose
                                                )
import           Data.Ord                       ( comparing )


mkLayers :: [a] -> Int -> Int -> [[a]]
mkLayers txt width height = chunksOf (width * height) txt

count :: Eq a => a -> [a] -> Int
count x = foldr (\y -> (+) (if x == y then 1 else 0)) 0

fewestZeros :: [[Char]] -> [Char]
fewestZeros = minimumBy (comparing $ count '0')

-- find top visible pixel
renderPixel :: [Char] -> Char
renderPixel = head . dropWhile (== '2')

render :: [Char] -> [Char]
render = map render'
 where
  render' c = case c of
    '0' -> '.'
    '1' -> '*'
    '2' -> ' '

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
