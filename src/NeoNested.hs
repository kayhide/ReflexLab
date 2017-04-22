{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module NeoNested where

import Data.List (unfoldr)
import Data.Tree (Tree, unfoldTree)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import System.Random

import NameDB (german_all)

generateName :: StdGen -> String
generateName gen = names ! i
  where names = Vector.fromList german_all
        (i, _) = randomR (0, (length names) - 1) gen


data Thing = Universe | Supercluster | Galaxy
data NamedThing = NamedThing Thing String

instance Show Thing where
  show Universe = "宇宙"
  show Supercluster = "超銀河団"
  show Galaxy = "銀河"

instance Show NamedThing where
  show (NamedThing Universe _) = show Universe
  show (NamedThing thing name) = name ++ show thing

generateTree :: Int -> Tree String
generateTree seed = unfoldTree generate (Universe, mkStdGen seed)

generate :: (Thing, StdGen) -> (String, [(Thing, StdGen)])
generate (thing, gen) = (show (NamedThing thing name), nexts)
  where things = bearableThings thing
        n = length things
        nexts = zip children gens
        (gen':gen'':gen''':gens) = unfoldr (Just . split) gen
        count = childrenCount thing gen'
        children = fmap (things !!) $ take count $ randomRs (0, n - 1) gen''
        name = generateName gen'''

bearableThings :: Thing -> [Thing]
bearableThings Universe = [Supercluster]
bearableThings Supercluster = [Galaxy]
bearableThings Galaxy = [Universe]

childrenCount :: Thing -> StdGen -> Int
childrenCount _ gen = fst $ randomR (3, 8) gen
