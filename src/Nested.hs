module Nested (Existence(..), childs) where

import NameDB (german_all)
import System.Random

data GalaxyType =
      Elliptical
    | Lenticular
    | Spirals
    | BarredSpirals

-- choice GalaxyType at random by the Galaxy's seed value
getGalaxyType :: Existence -> GalaxyType
getGalaxyType (Galaxy seed _) = case (dn 0 3 seed) !! 0 of
    0 -> Elliptical
    1 -> Lenticular
    2 -> Spirals
    3 -> BarredSpirals

getContinentName :: Existence -> String
getContinentName (Continent seed _) = (generateName seed) ++ "大陸"

generateName :: Int -> String
generateName seed = german_all !! ((dn 0 ((length german_all) - 1) seed) !! 0)

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
    | Continent    Int Existence
    | Ocean        Int Existence
    | Sky          Int Existence
    | Core         Int Existence
    | Moon         Int Existence
    | Rock         Int Existence
    | Soil         Int Existence
    | Water        Int Existence
    | Hydrogen     Int Existence
    | Oxygen       Int Existence
    | Helium       Int Existence
    | Proton       Int Existence
    | Neutron      Int Existence
    | Electron     Int Existence
    | UpQuark      Int Existence
    | DownQuark    Int Existence
    | Blackhole    Int Existence
    | Whitehole    Int Existence

instance Show Existence where
    show (Universe     _ _) = "宇宙"
    show (Supercluster _ _) = "超銀河団"
    show (Galaxy       s e) = case getGalaxyType (Galaxy s e) of
        Elliptical    -> "楕円型銀河"
        Lenticular    -> "レンズ型銀河"
        Spirals       -> "渦巻型銀河"
        BarredSpirals -> "棒渦巻型銀河"
    show (SolarSystem  _ _) = "星系"
    show (Nebula       _ _) = "星雲"
    show (Star         _ _) = "恒星"
    show (Planet       _ _) = "惑星"
    show (Continent    s e) = (getContinentName (Continent s e))
    show (Ocean        _ _) = "海"
    show (Sky          _ _) = "空"
    show (Core         _ _) = "核"
    show (Moon         _ _) = "月"
    show (Rock         _ _) = "岩石"
    show (Soil         _ _) = "土"
    show (Water        _ _) = "水"
    show (Hydrogen     _ _) = "水素"
    show (Oxygen       _ _) = "酸素"
    show (Helium       _ _) = "ヘリウム"
    show (Proton       _ _) = "陽子"
    show (Neutron      _ _) = "中性子"
    show (Electron     _ _) = "電子"
    show (UpQuark      _ _) = "アップクオーク"
    show (DownQuark    _ _) = "ダウンクオーク"
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
            let n_planets   = (dn 2 8 seed)   !! 0
                seedstar    = (randInts seed) !! 1
                seedplanets = take n_planets $ drop 2 $ randInts seed
             in Star seedstar pu : map (\s -> Planet s pu) seedplanets
        Nebula seed pu -> []
        Star seed pu -> [Hydrogen ((randInts seed) !! 0) pu, Helium ((randInts seed) !! 1) pu]
        Planet seed pu ->
            let n_moons = (dn 0 3 seed) !! 0
                n_conts = (dn 1 6 seed) !! 1
                n_ocean = (dn 1 9 seed) !! 2
                s_moons = take n_moons $ drop 3 $ randInts seed
                s_conts = take n_conts $ drop (3 + n_moons) $ randInts seed
                s_ocean = take n_ocean $ drop (3 + n_moons + n_conts) $ randInts seed
                s_sky   = (drop (3 + n_moons + n_conts + n_ocean) $ randInts seed) !! 0
                s_core  = (drop (4 + n_moons + n_conts + n_ocean) $ randInts seed) !! 0
             in Sky s_sky pu : Core s_core pu : map (\s -> Moon s pu) s_moons ++ map (\s -> Continent s pu) s_conts ++ map (\s -> Ocean s pu) s_ocean
        Continent seed pu -> []
        Ocean     seed pu -> []
        Sky       seed pu -> []
        Core      seed pu -> []
        Moon      seed pu -> 
            let n_rocks = (dn 0 11 seed) !! 0
                n_water = (dn 0 4 seed) !! 1
                n_soils = (dn 0 4 seed) !! 2
                n_oxygs = (dn 0 8 seed) !! 3
                s_rocks = take n_rocks $ drop 4 $ randInts seed
                s_water = take n_water $ drop (4 + n_rocks) $ randInts seed
                s_soils = take n_soils $ drop (4 + n_rocks + n_water) $ randInts seed
                s_oxygs = take n_oxygs $ drop (4 + n_rocks + n_water + n_soils) $ randInts seed
             in map (\s -> Rock s pu) s_rocks ++ map (\s -> Water s pu) s_water ++ map (\s -> Soil s pu) s_soils ++ map (\s -> Oxygen s pu) s_oxygs
        Rock      seed pu -> []
        Soil      seed pu -> []
        Water     seed pu -> [Hydrogen  ((randInts seed) !! 0) pu, Oxygen    ((randInts seed) !! 1) pu]
        Hydrogen  seed pu -> [Proton    ((randInts seed) !! 0) pu, Electron  ((randInts seed) !! 1) pu]
        Oxygen    seed pu -> [Proton    ((randInts seed) !! 0) pu, Neutron   ((randInts seed) !! 1) pu, Electron  ((randInts seed) !! 2) pu]
        Helium    seed pu -> [Proton    ((randInts seed) !! 0) pu, Neutron   ((randInts seed) !! 1) pu, Electron  ((randInts seed) !! 2) pu]
        Proton    seed pu -> [UpQuark   ((randInts seed) !! 0) pu, UpQuark   ((randInts seed) !! 1) pu, DownQuark ((randInts seed) !! 2) pu]
        Neutron   seed pu -> [DownQuark ((randInts seed) !! 0) pu, DownQuark ((randInts seed) !! 1) pu, UpQuark   ((randInts seed) !! 2) pu]
        Electron  seed pu -> [Universe  ((randInts seed) !! 0) pu]
        UpQuark   seed pu -> [Universe  ((randInts seed) !! 0) pu]
        DownQuark seed pu -> [Universe  ((randInts seed) !! 0) pu]
        Blackhole seed pu -> [Whitehole ((randInts seed) !! 0) pu]
        Whitehole seed pu -> [pu]