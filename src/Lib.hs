{-# LANGUAGE RecursiveDo #-}

module Lib
    ( startApp ) where

import qualified Data.Map as M
import Reflex.Dom
import Data.Maybe (isNothing, fromJust)
import Safe (atMay)

makeStyle :: MonadWidget t m => m ()
makeStyle = el "style" $ text "\
\body {\
\    color: #333;\
\}\
\#selectModule {\
\    padding: 1em;\
\    border: 1px #324 solid;\
\    border-radius: 4px;\
\    display: inline-block;\
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
\.selectList .item:hover .button {\
\    padding-right: 4px;\
\}\
\.button {\
\    width: 10px;\
\    height:10px;\
\    background: white;\
\    border: 1px gray solid;\
\    display: inline-block;\
\    margin-right: 0.5em;\
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
generate_button_style bool = M.fromList [
    ("style", (if bool == True then "background: black;" else "")),
    ("class", "button")]

btn :: MonadWidget t m => m (Dynamic t Bool)
btn = mdo
    bool <- toggle False events
    attrsDyn <- forDyn bool $ \b -> generate_button_style b
    (btn_elem, events) <- div_attrsDyn attrsDyn
    return bool

selectList :: MonadWidget t m => [String] -> m ( Dynamic t Int )
selectList arr = do
    index <- elClass "div" "selectList" $ mdo
        index <- foldDyn (\e p -> e) (-1) $ leftmost events_arr
        events_arr <- mapM (\x -> do
            (elm, _) <- elClass' "div" "item" $ do
                attrsDyn <- forDyn index $ \i -> generate_button_style $ x == i
                div_attrsDyn attrsDyn
                text $ arr !! x
            return $ tag (constant x) $ domEvent Click elm) $ take (length arr) [0..]
        return index
    return index

selectList_value :: MonadWidget t m => [String] -> m ( Dynamic t (Maybe String) )
selectList_value arr = do
    index <- selectList arr
    value <- forDyn index $ \i -> atMay arr i
    return value

br :: MonadWidget t m => m ()
br = el "br" $ do
    return ()








selectModule :: MonadWidget t m => m ()
selectModule = elId "div" "selectModule" $ do
    elAttr "h1" (M.fromList [("style", "margin-top: 0;")]) $ text "select module"
    
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

startApp :: IO ()
startApp = mainWidget $ do
    makeStyle
    selectModule
    return ()