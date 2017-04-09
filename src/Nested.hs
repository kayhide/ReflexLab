{-# LANGUAGE ExistentialQuantification #-}

module Nested where

import System.Random

data AllExistence = forall a. (Existence a) => E a

class Existence a where
    seed   :: a -> Int
    parent :: a -> AllExistence
    childs :: a -> [AllExistence]
    childs x = []
    name   :: a -> String



data Universe = Universe Int AllExistence

instance Existence Universe where
    seed   (Universe s _) = s
    parent (Universe _ p) = p
    childs (Universe seed parent) =
        let (n_childs, gen) = randomR (7, 22) $ mkStdGen seed
            seedInts = take n_childs $ randoms gen
         in map (\s -> E $ Supercluster s $ E $ Universe seed parent ) seedInts
    name   (Universe _ _) = "宇宙"



data Supercluster = Supercluster Int AllExistence

instance Existence Supercluster where
    seed   (Supercluster s _) = s
    parent (Supercluster _ p) = p
    childs (Supercluster seed parent) = []
    name   (Supercluster _ _) = "超銀河団"


--data Galaxy = Galaxy Int AllExistence