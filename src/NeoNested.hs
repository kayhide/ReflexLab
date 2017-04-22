module NeoNested where

import Data.List (unfoldr)
import Data.Tree (Tree, unfoldTree)
import System.Random

data Thing = Universe | Supercluster | Galaxy

instance Show Thing where
  show Universe = "宇宙"
  show Supercluster = "超銀河団"
  show Galaxy = "銀河"

generateTree :: Int -> Tree String
generateTree seed = unfoldTree generate (Universe, mkStdGen seed)

generate :: (Thing, StdGen) -> (String, [(Thing, StdGen)])
generate (thing, gen) = (show thing, nexts)
  where things = bearableThings thing
        n = length things
        nexts = zip children gens
        (gen':gen'':gens) = unfoldr (Just . split) gen
        count = childrenCount thing gen'
        children = fmap (things !!) $ take count $ randomRs (0, n - 1) gen''

bearableThings :: Thing -> [Thing]
bearableThings Universe = [Supercluster]
bearableThings Supercluster = [Galaxy]
bearableThings Galaxy = [Universe]

childrenCount :: Thing -> StdGen -> Int
childrenCount _ gen = fst $ randomR (3, 8) gen
