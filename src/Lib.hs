{-# LANGUAGE RecursiveDo #-}

module Lib
    ( startApp ) where

import qualified Data.Map as M
import qualified Data.Tree as T
import Reflex.Dom
import Data.Maybe (isNothing, fromJust)
import Safe (atMay)
import System.Random (getStdRandom, random, randoms, randomRs, mkStdGen)

getRandomInt :: IO Int
getRandomInt = getStdRandom $ random

makeStyle :: MonadWidget t m => m ()
makeStyle = el "style" $ text "\
\body {\
\    color: #333;\
\}\
\#selectModule, #treeModule {\
\    margin: 1em;\
\    padding: 1em;\
\    border: 1px #324 solid;\
\    border-radius: 4px;\
\    display: inline-block;\
\    vertical-align: top;\
\}\
\#selectModule {\
\    border: 1px #324 solid;\
\}\
\#treeModule {\
\    border: 1px #9817b2 solid;\
\}\
\.selectList {\
\    user-select: none;\
\    cursor: default;\
\    font-size: 12px;\
\}\
\.selectList .item {\
\    padding: 0px;\
\    margin: 12px;\
\}\
\.button {\
\    width: 10px;\
\    height:10px;\
\    background: white;\
\    border: 1px gray solid;\
\    display: inline-block;\
\    margin-right: 0.5em;\
\}\
\.selectList .button.enabled {\
\    background: black;\
\}\
\.selectList .item:hover .button, .tree .label:hover .button {\
\    padding-right: 4px;\
\}\
\.tree .button.enabled {\
\    background: black;\
\}\
\.tree .childs {\
\    padding-left: 1em;\
\}\
\.tree .childs.none {\
\    display: none;\
\}"

subForestAtMay :: T.Tree a -> Int -> Maybe ( T.Tree a )
subForestAtMay t i = atMay (T.subForest t) i

treeAtMay :: T.Tree a -> [Int] -> Maybe ( T.Tree a )
treeAtMay t i = foldl (\x y -> if isNothing x then Nothing else subForestAtMay (fromJust x) y) (Just t) i

subForestAt :: T.Tree a -> Int -> T.Tree a
subForestAt t i = (T.subForest t) !! i

treeAt :: T.Tree a -> [Int] -> T.Tree a
treeAt t i = foldl (\x y -> subForestAt x y) t i

elId :: MonadWidget t m => String -> String -> m a -> m a
elId tagName id func = elAttr tagName (M.fromList [("id", id)]) func

elClass' :: MonadWidget t m => String -> String -> m a -> m (El t, a)
elClass' tagName className func = elAttr' tagName (M.fromList [("class", className)]) func

div_attrsDyn :: MonadWidget t m => Dynamic t (M.Map String String) -> m (El t, Event t ())
div_attrsDyn attrsDyn = do
    (elm, _) <- elDynAttr' "div" attrsDyn $ text ""
    return (elm, domEvent Click elm)

generate_button_style :: Bool -> M.Map String String
generate_button_style bool = M.fromList [("class", (if bool == True then "button enabled" else "button"))]

generate_button :: MonadWidget t m => Dynamic t Bool -> m ( El t, Event t () )
generate_button boolDyn = do
    attrsDyn <- forDyn boolDyn $ \b -> generate_button_style b
    div_attrsDyn attrsDyn

btn :: MonadWidget t m => m (Dynamic t Bool)
btn = mdo
    bool <- toggle False events
    (btn_elem, events) <- generate_button bool
    return bool

selectList :: MonadWidget t m => [String] -> m ( Dynamic t Int )
selectList arr = do
    index <- elClass "div" "selectList" $ mdo
        index <- foldDyn (\e p -> e) (-1) $ leftmost events_arr
        events_arr <- mapM (\x -> do
            (elm, _) <- elClass' "div" "item" $ do
                bool <- forDyn index $ \i -> x == i
                generate_button bool
                text $ arr !! x
            return $ tag (constant x) $ domEvent Click elm) $ take (length arr) [0..]
        return index
    return index

selectList_value :: MonadWidget t m => [String] -> m ( Dynamic t (Maybe String) )
selectList_value arr = do
    index <- selectList arr
    value <- forDyn index $ \i -> atMay arr i
    return value

drawTree :: MonadWidget t m => T.Tree String -> m ()
drawTree tree = elClass "div" "tree" $ drawTreeMain tree []

-- # of clicks                     : 0 1 2 3 4 5 6 7 8 9 ...
-- whether the button is enabled   : F T F T F T F T F T ...
-- whether child items exists      : F T T T T T T T T T ...
-- whether child items are visible : F T F T F T F T F T ...
drawTreeMain :: MonadWidget t m => T.Tree String -> [Int] -> m ()
drawTreeMain tree pos = mdo
    let this        = treeAt tree pos
        this_label  = T.rootLabel this
        this_forest = T.subForest this

    (elm, _) <- elClass' "div" "label" $ do
        generate_button bool_button
        text this_label

    bool_button <- toggle False $ domEvent Click elm                 -- whether the button is enabled / whether child items are visible
    bool_childs <- foldDyn (\x y -> True) False $ domEvent Click elm -- whether child items exists

    let child_empty_list = M.fromList $ map (\k -> (k, ())) $ take (length this_forest) [0..]
    childs <- forDyn bool_childs $ \b -> if b then child_empty_list else M.fromList []

    attrDyn <- forDyn bool_button $ \b -> M.fromList [if b then ("class", "childs") else ("class", "childs none")]
    elDynAttr "div" attrDyn $ do
        listWithKey childs $ \k v ->
            let child        = treeAt tree $ pos ++ [k]
                child_label  = T.rootLabel child
                child_forest = T.subForest child
             in if child_forest == [] then elClass "div" "label" $ text child_label else drawTreeMain tree $ pos ++ [k]

    return ()

br :: MonadWidget t m => m ()
br = el "br" $ do
    return ()








selectModule :: MonadWidget t m => m ()
selectModule = elId "div" "selectModule" $ do
    elAttr "h1" (M.fromList [("style", "margin-top: 0;")]) $ text "select module #1"

    item <- selectList_value ["aaaaa", "bbbbb", "aiueo", "kakikukeko"]

    txt1 <- forDyn item $ \i -> do
        if isNothing i then
            "=== NOT SELECTED ==="
        else
            "current: " ++ (fromJust i)

    item_change <- foldDyn (:) [] (updated item)

    txt2 <- forDyn item_change $ \x ->
        if atMay x 0 == atMay x 1 then "" else "=== VALUE CHANGED ==="

    dynText txt1

    br

    dynText txt2

    return ()

data ExistenceType =
      Universe
    | Supercluster
    | Galaxy
    | SolarSystem
    | Nebula
    | Star
    | Planet
    | UpQuark
    | Blackhole
    | Whitehole
instance Show ExistenceType where
    show x = case x of
        Universe     -> "宇宙"
        Supercluster -> "超銀河団"
        Galaxy       -> "銀河"
        SolarSystem  -> "星系"
        Nebula       -> "星雲"
        Star         -> "恒星"
        Planet       -> "惑星"
        UpQuark      -> "アップクオーク"
        Blackhole    -> "ブラックホール"
        Whitehole    -> "ホワイトホール"

-- Existence et seed parentUniverse
-- et             : ExistenceType of the Existence
-- seed           : random seed of the Existence
-- parentUniverse : what universe the Existence belongs to
data Existence = Existence ExistenceType Int Existence

eType :: Existence -> ExistenceType
eType (Existence et _ _) = et

eSeed :: Existence -> Int
eSeed (Existence _ seed _) = seed

eParentUniverse :: Existence -> Existence
eParentUniverse (Existence _ _ e) = e

-- et     : ExistenceType
-- seed   : random seed
eChilds :: Existence -> [Existence]
eChilds (Existence et seed parentUniverse) =
    let this     = Existence et seed parentUniverse
        gen      = mkStdGen seed                     -- random number generator
        randInts = randoms gen :: [Int]              -- random Int value
        dn x y   = randomRs (x :: Int, y :: Int) gen -- x ~ y random value
        makeExistence et seed = Existence et seed parentUniverse
     in case et of
        Universe ->
            let n_childs = ((dn 7 22) !! 0)
                seedInts = take n_childs $ tail randInts
             in map (\s -> Existence Supercluster s this) seedInts
        Supercluster ->
            let n_childs = ((dn 3 14) !! 0)
                seedInts = take n_childs $ tail randInts
             in map (\s -> makeExistence Galaxy s) seedInts
        Galaxy ->
            let n_childs = ((dn 8 19) !! 0)
                d100Ints = take n_childs $ tail $ dn 1 100
                seedInts = take n_childs $ drop n_childs $ tail randInts
             in map (\(d, s) -> case d of
                    d | d <= 20   -> makeExistence Blackhole s
                    d | d <= 80   -> makeExistence SolarSystem s
                    d | otherwise -> makeExistence Nebula s) $ zip d100Ints seedInts
        SolarSystem ->
            let n_stars     = ((dn 1 3) !! 0)
                n_planets   = ((dn 0 8) !! 1)
                seedstars   = take n_stars   $ drop 2 $ randInts
                seedplanets = take n_planets $ drop (2 + n_stars) $ randInts
                stars       = map (\s -> makeExistence Star s) seedstars
                planets     = map (\s -> makeExistence Planet s) seedplanets
             in stars ++ planets
        Nebula -> []
        Star -> []
        Planet -> []
        UpQuark -> []
        Blackhole -> [makeExistence Whitehole (randInts !! 0)]
        Whitehole -> [parentUniverse]

instance Show Existence where
    show (Existence et _ _) = show et

treeModule :: MonadWidget t m => Int -> m ()
treeModule seed = elId "div" "treeModule" $ do

    elAttr "h1" (M.fromList [("style", "margin-top: 0;")]) $ text "tree module"

    let init_tree = Existence Universe seed init_tree

    drawTree $ T.unfoldTree (\x -> (show x, eChilds x)) init_tree

    return ()

startApp :: IO ()
startApp = do
    seed <- getRandomInt
    mainWidget $ do
        makeStyle
        selectModule
        -- treeModule seed
        treeModule 666
        return ()
