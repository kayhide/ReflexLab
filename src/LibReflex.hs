{-# LANGUAGE RecursiveDo #-}

module LibReflex where

import qualified Data.Map as M
import qualified Data.Tree as T
import LibTree (treeAt)
import Reflex.Dom
import Data.Maybe (isNothing, fromJust)
import Safe (atMay)

makeStyle :: MonadWidget t m => m ()
makeStyle = el "style" $ text "\
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

generateSelectList :: MonadWidget t m => ([Event t Int] -> m (Dynamic t Int)) -> [String] -> m ( Dynamic t Int )
generateSelectList events_arr_func arr =
    return =<< elClass "div" "selectList" $ mdo
        index <- events_arr_func events_arr
        events_arr <- mapM (\x -> do
            (elm, _) <- elClass' "div" "item" $ do
                generate_button =<< (forDyn index $ \i -> x == i)
                text $ arr !! x
            return $ tag (constant x) $ domEvent Click elm) $ take (length arr) [0..]
        return index

generateSelectList_value :: MonadWidget t m => ([Event t Int] -> m (Dynamic t Int)) -> [String] -> m ( Dynamic t (Maybe String) )
generateSelectList_value events_arr_func arr = do
    index <- generateSelectList events_arr_func arr
    value <- forDyn index $ \i -> atMay arr i
    return value

selectList :: MonadWidget t m => [String] -> m ( Dynamic t Int )
selectList       = generateSelectList       (\x -> foldDyn (\e p -> e) (-1) $ leftmost x)

selectList_value :: MonadWidget t m => [String] -> m ( Dynamic t (Maybe String) )
selectList_value = generateSelectList_value (\x -> foldDyn (\e p -> e) (-1) $ leftmost x)

selectListTogglable :: MonadWidget t m => [String] -> m ( Dynamic t Int )
selectListTogglable       = generateSelectList       (\x -> foldDyn (\e p -> if e == p then -1 else e) (-1) $ leftmost x)

selectListTogglable_value :: MonadWidget t m => [String] -> m ( Dynamic t (Maybe String) )
selectListTogglable_value = generateSelectList_value (\x -> foldDyn (\e p -> if e == p then -1 else e) (-1) $ leftmost x)

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