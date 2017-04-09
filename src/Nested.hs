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

randomRs' :: (Random a, RandomGen g) => (a, a) -> Int -> g -> ([a], g)
randomRs' range len gen = foldl
                (\(arr, g) _ ->
                    let (rnd, ng) = randomR range g
                     in (arr ++ [rnd], ng))
                ([], gen)
                (take len [0..])

randoms' :: (Random a, RandomGen g) => Int -> g -> ([a], g)
randoms' len gen = foldl
                (\(arr, g) _ ->
                    let (rnd, ng) = random g
                     in (arr ++ [rnd], ng))
                ([], gen)
                (take len [0..])



data Universe = Universe Int AllExistence

instance Existence Universe where
    seed   (Universe s _) = s
    parent (Universe _ p) = p
    childs (Universe seed parent) =
        let this = Universe seed parent
            (n_childs, gen) = randomR (7, 22) $ mkStdGen seed
            (seedInts, _  ) = randoms' n_childs gen
         in map ( \s -> E $ Supercluster s (E this) ) seedInts
    name   (Universe _ _) = "宇宙"



data Supercluster = Supercluster Int AllExistence

instance Existence Supercluster where
    seed   (Supercluster s _) = s
    parent (Supercluster _ p) = p
    childs (Supercluster seed parent) =
        let this = Supercluster seed parent
            (n_childs, gen1) = randomR (3, 14) $ mkStdGen seed
            (seedInts, gen2) = randoms' n_childs gen1
            (gTypes  , _   ) = randoms' n_childs gen2
         in map ( \(s, gt) -> E $ Galaxy s (E this) gt ) $ zip seedInts gTypes
    name   (Supercluster _ _) = "超銀河団"



data Galaxy = Galaxy Int AllExistence GalaxyType

instance Existence Galaxy where
    seed   (Galaxy s _ _) = s
    parent (Galaxy _ p _) = p
    childs (Galaxy seed parent gt) =
        let this = Galaxy seed parent gt
            (n_childs, gen1) = randomR (8, 19) $ mkStdGen seed
            (d100Ints, gen2) = randomRs' (0, 100) n_childs gen1
            (seedInts, _   ) = randoms' n_childs gen2
         in map ( \(d, s) -> case d of
            d | d <= (20 :: Int) -> E $ Blackhole   s (E this)
            d | d <= (80 :: Int) -> E $ SolarSystem s (E this)
            d | otherwise        -> E $ Nebula      s (E this) ) $ zip d100Ints seedInts
    name   (Galaxy _ _ Elliptical   ) = "楕円型銀河"
    name   (Galaxy _ _ Lenticular   ) = "レンズ型銀河"
    name   (Galaxy _ _ Spirals      ) = "渦巻型銀河"
    name   (Galaxy _ _ BarredSpirals) = "棒渦巻型銀河"

data GalaxyType =
      Elliptical
    | Lenticular
    | Spirals
    | BarredSpirals deriving (Enum, Bounded)

instance Random GalaxyType where
    randomR range gen =
        let t = randomR (fromEnum $ fst $ range, fromEnum $ snd $ range) gen
         in (toEnum $ fst t, snd t)
    random gen = 
        let t = randomR (fromEnum (minBound :: GalaxyType), fromEnum (maxBound :: GalaxyType)) gen
         in (toEnum $ fst t, snd t)



data SolarSystem = SolarSystem Int AllExistence

instance Existence SolarSystem where
    seed   (SolarSystem s _) = s
    parent (SolarSystem _ p) = p
    childs (SolarSystem seed parent) = []
    name   (SolarSystem _ _) = "星系"



data Nebula = Nebula Int AllExistence

instance Existence Nebula where
    seed   (Nebula s _) = s
    parent (Nebula _ p) = p
    childs (Nebula seed parent) = []
    name   (Nebula _ _) = "星雲"

data Water = Water Int AllExistence

instance Existence Water where
    seed   (Water s _) = s
    parent (Water _ p) = p
    childs (Water seed parent) =
        let this = Water seed parent
         in []
    name   (Water _ _) = "水"



data Hydrogen = Hydrogen Int AllExistence

instance Existence Hydrogen where
    seed   (Hydrogen s _) = s
    parent (Hydrogen _ p) = p
    childs (Hydrogen seed parent) =
        let this = Hydrogen seed parent
         in []
    name   (Hydrogen _ _) = "水素"



data Oxygen = Oxygen Int AllExistence

instance Existence Oxygen where
    seed   (Oxygen s _) = s
    parent (Oxygen _ p) = p
    childs (Oxygen seed parent) =
        let this = Oxygen seed parent
         in []
    name   (Oxygen _ _) = "酸素"



data Helium = Helium Int AllExistence

instance Existence Helium where
    seed   (Helium s _) = s
    parent (Helium _ p) = p
    childs (Helium seed parent) =
        let this = Helium seed parent
         in []
    name   (Helium _ _) = "ヘリウム"



data Proton = Proton Int AllExistence

instance Existence Proton where
    seed   (Proton s _) = s
    parent (Proton _ p) = p
    childs (Proton seed parent) =
        let this = Proton seed parent
         in []
    name   (Proton _ _) = "陽子"



data Neutron = Neutron Int AllExistence

instance Existence Neutron where
    seed   (Neutron s _) = s
    parent (Neutron _ p) = p
    childs (Neutron seed parent) =
        let this = Neutron seed parent
         in []
    name   (Neutron _ _) = "中性子"



data Electron = Electron Int AllExistence

instance Existence Electron where
    seed   (Electron s _) = s
    parent (Electron _ p) = p
    childs (Electron seed parent) =
        let this = Electron seed parent
         in []
    name   (Electron _ _) = "電子"


data UpQuark = UpQuark Int AllExistence

instance Existence UpQuark where
    seed   (UpQuark s _) = s
    parent (UpQuark _ p) = p
    childs (UpQuark seed parent) =
        let this = UpQuark seed parent
         in []
    name   (UpQuark _ _) = "アップクオーク"



data DownQuark = DownQuark Int AllExistence

instance Existence DownQuark where
    seed   (DownQuark s _) = s
    parent (DownQuark _ p) = p
    childs (DownQuark seed parent) =
        let this = DownQuark seed parent
         in []
    name   (DownQuark _ _) = "ダウンクオーク"



data Blackhole = Blackhole Int AllExistence

instance Existence Blackhole where
    seed   (Blackhole s _) = s
    parent (Blackhole _ p) = p
    childs (Blackhole seed parent) = 
        let this = Blackhole seed parent
         in [E $ Whitehole (fst $ random $ mkStdGen seed) (E this) ]
    name   (Blackhole _ _) = "ブラックホール"



data Whitehole = Whitehole Int AllExistence

instance Existence Whitehole where
    seed   (Whitehole s _) = s
    parent (Whitehole _ p) = p
    childs (Whitehole seed parent) =
        let this = Whitehole seed parent
         in [getParentUniverse $ E this]
    name   (Whitehole _ _) = "ホワイトホール"

getParentUniverse :: AllExistence -> AllExistence
getParentUniverse (E a) = (\(E b) -> case name b of
    "宇宙" -> E b -- TODO: あとで直す Unsafeつかわないとむりかも
    otherwise -> getParentUniverse $ E b) (parent a)