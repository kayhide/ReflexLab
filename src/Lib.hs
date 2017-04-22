module Lib
    ( startApp ) where

import qualified LibReflex (makeStyle)
import LibReflex hiding (makeStyle)
import qualified Nested as N

import qualified Data.Map as M
import qualified Data.Tree as T
import Reflex.Dom
import Data.Maybe (isNothing, fromJust)
import Safe (atMay)
import System.Random (getStdRandom, random)

import qualified NeoNested

makeStyle :: MonadWidget t m => m ()
makeStyle = el "style" $ text "\
\body {\
\    color: #333;\
\}\
\#selectModule, #selectModuleTogglable, #treeModule {\
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
\}"

selectModule :: MonadWidget t m => m ()
selectModule = elId "div" "selectModule" $ do
    elAttr "h1" (M.fromList [("style", "margin-top: 0;")]) $ text "select module #1"

    item <- selectList_value ["aaaaa", "bbbbb", "aiueo", "kakikukeko"]

    txt1 <- forDyn item $ \i -> do
        show i

    item_change <- foldDyn (:) [] (updated item)

    txt2 <- forDyn item_change $ \x ->
        if atMay x 0 == atMay x 1 then "" else "=== VALUE CHANGED ==="

    dynText txt1

    br

    dynText txt2

    return ()

selectModuleTogglable :: MonadWidget t m => m ()
selectModuleTogglable = elId "div" "selectModuleTogglable" $ do
    elAttr "h1" (M.fromList [("style", "margin-top: 0;")]) $ text "select module #2"

    item <- selectListTogglable_value ["aaaaa", "bbbbb", "aiueo", "kakikukeko"]

    txt <- forDyn item $ \i -> do
        show i

    dynText txt

    return ()

treeModule :: MonadWidget t m => Int -> m ()
treeModule seed = elId "div" "treeModule" $ do

    elAttr "h1" (M.fromList [("style", "margin-top: 0;")]) $ text "tree module"

    let init_ext = N.Universe seed (N.E init_ext)

    -- drawTree $ T.unfoldTree (\(N.E x) -> (N.name x, N.childs x)) $ N.E init_ext
    drawTree $ NeoNested.generateUniverse seed

    return ()

startApp :: IO ()
startApp = do
    seed <- (getStdRandom $ random :: IO Int)
    mainWidget $ do
        LibReflex.makeStyle
        makeStyle
        selectModule
        selectModuleTogglable
        treeModule seed
        return ()
