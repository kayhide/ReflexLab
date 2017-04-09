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
        let this = Universe seed parent
            (n_childs, gen) = randomR (7, 22) $ mkStdGen seed
         in snd $ foldl
                (\(g, es) _ ->
                    let (s, ng) = random g
                     in (ng, es ++ [E $ Supercluster s $ E this]))
                (gen, [])
                (take n_childs [0..])
    name   (Universe _ _) = "宇宙"



data Supercluster = Supercluster Int AllExistence

instance Existence Supercluster where
    seed   (Supercluster s _) = s
    parent (Supercluster _ p) = p
    childs (Supercluster seed parent) = []
    name   (Supercluster _ _) = "超銀河団"


--data Galaxy = Galaxy Int AllExistence