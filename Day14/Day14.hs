module Day14 where

import           Data.List                      ( foldl' )
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Test.Hspec
import           Text.Parsec
import           Text.Parsec.String



data Reagent = Reagent { _quantity :: Int, _chemical :: String } deriving (Ord, Eq, Show)
data Reaction = Reaction {_lhs :: S.Set Reagent, _rhs :: Reagent} deriving (Eq, Show)

type Reactions = M.Map String Reaction
type Requirement = M.Map String Int


parseReactions :: String -> Reactions
parseReactions s = case parse recipeP "" s of
  Left  err       -> error $ show err
  Right reactions -> reactions

recipeP :: Parser Reactions
recipeP = do
  rs <- reactionP `sepEndBy1` endOfLine
  return $ mkReactions rs

reactionP :: Parser Reaction
reactionP = do
  ings <- ingredientP `sepBy` (char ',')
  spaces
  _   <- string "=>"
  rhs <- ingredientP
  return $ Reaction (S.fromList ings) rhs

ingredientP :: Parser Reagent
ingredientP = do
  q <- quantityP
  r <- chemicalP
  return $ Reagent q r

chemicalP :: Parser String
chemicalP = do
  spaces
  c  <- many1 upper
  return c

quantityP :: Parser Int
quantityP = do
  spaces
  digits <- many1 digit
  return $ read digits

mkReactions :: [Reaction] -> Reactions
mkReactions = foldl' addReaction M.empty
 where
  addReaction base reaction =
    M.insert (_chemical $ _rhs reaction) reaction base


--

produce :: Reactions -> Requirement -> Requirement
produce reactions required | M.null needToProduce = required
                           | otherwise            = produce reactions required''
 where
  needToProduce = M.filter (> 0) $ nonOre required
  (chem, qty)   = M.findMin needToProduce
  reaction      = reactions M.! chem
  productQty    = _quantity $ _rhs reaction
  applications  = max 1 (qty `div` productQty)
  qty'          = qty - (applications * productQty)
  required'     = M.insert chem qty' required
  required'' = S.foldl (addRequirement applications) required' (_lhs reaction)

nonOre :: Requirement -> Requirement
nonOre = M.filterWithKey (\c _ -> c /= "ORE")

addRequirement :: Int -> Requirement -> Reagent -> Requirement
addRequirement n requirements reagent = M.insert chem qty' requirements
 where
  chem = _chemical reagent
  qty  = M.findWithDefault 0 chem requirements
  qty' = qty + (n * _quantity reagent)

oreForFuel :: Reactions -> Int -> Int
oreForFuel reactions fuel = required M.! "ORE"
 where
  required0 = M.singleton "FUEL" fuel
  required  = produce reactions required0

solvePt1 :: String -> Int
solvePt1 input = oreForFuel (parseReactions input)  1

main :: IO()
main = do
  input <- readFile "input.txt"
  putStr "Part 1: "
  print $ solvePt1 input


test :: IO ()
test = hspec $ do
  describe "Part 1" $ do
    it "Example 1" $ do
      solvePt1 eg1 `shouldBe` 31
    it "Example 2" $ do
      solvePt1 eg2 `shouldBe` 165
    it "Example 3" $ do
      solvePt1 eg3 `shouldBe` 13312
    it "Example 4" $ do
      solvePt1 eg4 `shouldBe` 180697
    it "Example 5" $ do
      solvePt1 eg5 `shouldBe` 2210736


eg1 :: String
eg1 =
  "10 ORE => 10 A\n\
  \1 ORE => 1 B\n\
  \7 A, 1 B => 1 C\n\
  \7 A, 1 C => 1 D\n\
  \7 A, 1 D => 1 E\n\
  \7 A, 1 E => 1 FUEL"

eg2 :: String
eg2 =
  "9 ORE => 2 A\n\
  \8 ORE => 3 B\n\
  \7 ORE => 5 C\n\
  \3 A, 4 B => 1 AB\n\
  \5 B, 7 C => 1 BC\n\
  \4 C, 1 A => 1 CA\n\
  \2 AB, 3 BC, 4 CA => 1 FUEL"

eg3 :: String
eg3 =
  "157 ORE => 5 NZVS\n\
  \165 ORE => 6 DCFZ\n\
  \44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n\
  \12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n\
  \179 ORE => 7 PSHF\n\
  \177 ORE => 5 HKGWZ\n\
  \7 DCFZ, 7 PSHF => 2 XJWVT\n\
  \165 ORE => 2 GPVTF\n\
  \3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"

eg4 :: String
eg4 =
  "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n\
  \17 NVRVD, 3 JNWZP => 8 VPVL\n\
  \53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n\
  \22 VJHF, 37 MNCFX => 5 FWMGM\n\
  \139 ORE => 4 NVRVD\n\
  \144 ORE => 7 JNWZP\n\
  \5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n\
  \5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n\
  \145 ORE => 6 MNCFX\n\
  \1 NVRVD => 8 CXFTF\n\
  \1 VJHF, 6 MNCFX => 4 RFSQX\n\
  \176 ORE => 6 VJHF"

eg5 :: String
eg5 =
  "171 ORE => 8 CNZTR\n\
  \7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n\
  \114 ORE => 4 BHXH\n\
  \14 VRPVC => 6 BMBT\n\
  \6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n\
  \6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n\
  \15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n\
  \13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n\
  \5 BMBT => 4 WPTQ\n\
  \189 ORE => 9 KTJDG\n\
  \1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n\
  \12 VRPVC, 27 CNZTR => 2 XDBXC\n\
  \15 KTJDG, 12 BHXH => 5 XCVML\n\
  \3 BHXH, 2 VRPVC => 7 MZWV\n\
  \121 ORE => 7 VRPVC\n\
  \7 XCVML => 6 RJRHP\n\
  \5 BHXH, 4 VRPVC => 5 LTCX"
 
