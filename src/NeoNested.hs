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

splits :: StdGen -> [StdGen]
splits = unfoldr (Just . split)


data Thing = Universe
           | Supercluster
           | Galaxy
           | Nebula
           | SolarSystem
           | Blackhole
           | Whitehole
           | Star
           | Planet

data NamedThing = NamedThing Thing String

type Bearability = (Thing, (Int, Int))

instance Show Thing where
  show Universe = "宇宙"
  show Supercluster = "超銀河団"
  show Galaxy = "銀河"
  show Nebula = "星雲"
  show SolarSystem = "星系"
  show Blackhole = "ブラックホール"
  show Whitehole = "ホワイトホール"
  show Star = "太陽"
  show Planet = "星"

instance Show NamedThing where
  show (NamedThing thing@Universe _) = show thing
  show (NamedThing thing@Blackhole name) = name ++ "・" ++ show thing
  show (NamedThing thing@Whitehole name) = name ++ "・" ++ show thing
  show (NamedThing thing name) = name ++ show thing

generateUniverse :: Int -> Tree String
generateUniverse seed = unfoldTree generate (Universe, mkStdGen seed)

generate :: (Thing, StdGen) -> (String, [(Thing, StdGen)])
generate (thing, gen) = (show (NamedThing thing name), nexts)
  where (gen':gen'':gens) = splits gen
        name = generateName gen'
        children = do
          ((thing', range), g) <- zip (bearabilitiesOf thing) $ splits gen''
          let n = fst $ randomR range g
          replicate n thing'
        nexts = zip children gens

bearabilitiesOf :: Thing -> [Bearability]
bearabilitiesOf Universe = [(Supercluster, (4, 8))]
bearabilitiesOf Supercluster = [(Galaxy, (4, 5))]
bearabilitiesOf Galaxy = [(Nebula, (1, 3)), (SolarSystem, (2, 3)), (Blackhole, (0, 1))]
bearabilitiesOf Nebula = [(SolarSystem, (1, 2))]
bearabilitiesOf SolarSystem = [(Star, (1, 2)), (Planet, (5, 8))]
bearabilitiesOf Blackhole = [(Whitehole, (1, 1))]
bearabilitiesOf Whitehole = [(Universe, (1, 1))]
bearabilitiesOf Star = []
bearabilitiesOf Planet = []
