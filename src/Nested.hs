module Nested (Existence(..), childs) where

import System.Random

-- Universe seed parentUniverse
-- seed           : random seed of the Existence
-- parentUniverse : what universe the Existence belongs to
data Existence =
      Universe     Int Existence
    | Supercluster Int Existence
    | Galaxy       Int Existence
    | SolarSystem  Int Existence
    | Nebula       Int Existence
    | Star         Int Existence
    | Planet       Int Existence
    | UpQuark      Int Existence
    | Blackhole    Int Existence
    | Whitehole    Int Existence

instance Show Existence where
    show (Universe     _ _) = "宇宙"
    show (Supercluster _ _) = "超銀河団"
    show (Galaxy       _ _) = "銀河"
    show (SolarSystem  _ _) = "星系"
    show (Nebula       _ _) = "星雲"
    show (Star         _ _) = "恒星"
    show (Planet       _ _) = "惑星"
    show (UpQuark      _ _) = "アップクオーク"
    show (Blackhole    _ _) = "ブラックホール"
    show (Whitehole    _ _) = "ホワイトホール"

-- random number generator with seed s (alias of mkStdGen)
gen :: Int -> StdGen
gen = mkStdGen

-- random Int value with seed s
randInts :: Int -> [Int]
randInts s = randoms $ gen s

-- x ~ y random value with seed s
dn :: Int -> Int -> Int -> [Int]
dn x y s = randomRs (x, y) $ gen s

-- get childs of the existence
childs :: Existence -> [Existence]
childs ext =
    case ext of
        Universe seed pu ->
            let n_childs = ((dn 7 22 seed) !! 0)
                seedInts = take n_childs $ tail $ randInts seed
             in map (\s -> Supercluster s ext) seedInts
        Supercluster seed pu ->
            let n_childs = ((dn 3 14 seed) !! 0)
                seedInts = take n_childs $ tail $ randInts seed
             in map (\s -> Galaxy s pu) seedInts
        Galaxy seed pu ->
            let n_childs = ((dn 8 19 seed) !! 0)
                d100Ints = take n_childs $ tail $ dn 1 100 seed
                seedInts = take n_childs $ drop n_childs $ tail $ randInts seed
             in map (\(d, s) -> case d of
                    d | d <= 20   -> Blackhole s pu
                    d | d <= 80   -> SolarSystem s pu
                    d | otherwise -> Nebula s pu) $ zip d100Ints seedInts
        SolarSystem seed pu ->
            let n_stars     = ((dn 1 3 seed) !! 0)
                n_planets   = ((dn 0 8 seed) !! 1)
                seedstars   = take n_stars   $ drop 2 $ randInts seed
                seedplanets = take n_planets $ drop (2 + n_stars) $ randInts seed
                stars       = map (\s -> Star s pu) seedstars
                planets     = map (\s -> Planet s pu) seedplanets
             in stars ++ planets
        Nebula seed pu -> []
        Star seed pu -> []
        Planet seed pu -> []
        UpQuark seed pu -> []
        Blackhole seed pu -> [Whitehole ((randInts seed) !! 0) pu]
        Whitehole seed pu -> [pu]