{-# LANGUAGE LambdaCase #-}
module Data.VDOM.Internal.Interpreter(
  interpretActions) where

import           Control.Lens
import           Data.JSString            (JSString)
import qualified Data.VDOM.Internal.FFI   as FFI
import           Data.VDOM.Internal.Hash  (Hashed (..))
import           Data.VDOM.Internal.Types (ClientNamespace (..), ElementId (..),
                                           InsertWhere (..), Instruction (..))
import qualified Data.VDOM.Internal.Types as T
import           GHCJS.Types              (JSVal)

-- | Execute a sequence of actions that manipulate the DOM.
interpretActions :: [Instruction ElementId] -> IO ()
interpretActions = go Nothing where
  go _ []       = return ()
  go _ ((CreateElement p (ElementId i) (ClientNamespace (Hashed _ ns)) tp):xs) = do
    ne <- js_createElement ns tp
    _ <- js_setId ne i
    _ <- case p of
      InsertBefore (ElementId i')    -> do
        relTarget <- FFI.getElementById i'
        js_insertBefore relTarget ne
      InsertAsChildOf Nothing -> do
        relTarget <- js_getDocumentBody
        js_appendChild relTarget ne
      InsertAsChildOf (Just (ElementId i')) -> do
        relTarget <- FFI.getElementById i'
        js_appendChild relTarget ne
      InsertAfter (ElementId i')     -> do
        relTarget <- FFI.getElementById i'
        js_insertAfter relTarget ne
    go (Just (i, ne)) xs
  go Nothing (x:xs) = do
    let i = x ^. to T.elementId . to mkElementId
    target <- FFI.getElementById i
    go (Just (i, target)) (x:xs)
  go (Just (i, target)) (x:xs) =
    if (i /= x ^. to T.elementId . to mkElementId)
    then go Nothing (x:xs)
    else do
      _ <- case x of
        SetAttribute i p v  ->  js_setAttribute target p v
        SetCSSText i t      ->  js_setCSSText target t
        DeleteElement i     ->  js_deleteElement target
        SetTextContent i t  ->  js_setTextContent target t
        RemoveAttribute _ k -> js_removeAttribute target k
      go (Just (i, target)) xs

foreign import javascript unsafe "$1['id'] = $2;"
  js_setId :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.setAttribute($2, $3);"
  js_setAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript unsafe "document.createElementNS($1, $2)"
  js_createElement :: JSString -> JSString -> IO JSVal

foreign import javascript unsafe "$1['appendChild']($2)"
  js_appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$2.parentNode.insertBefore($1, $2)"
  js_insertBefore :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$2.parentNode.insertBefore($1, $2.nextSibling)"
  js_insertAfter :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "function(){var e=$1;if(e!=null && e.parentElement!=null)e.parentElement.removeChild(e)}()"
  js_deleteElement :: JSVal -> IO ()

foreign import javascript unsafe "function(){$1.style.cssText = $2;}()"
  js_setCSSText :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$r = document.body;"
  js_getDocumentBody :: IO JSVal

foreign import javascript unsafe "$1['textContent']=$2"
  js_setTextContent :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.removeAttribute($2);"
  js_removeAttribute :: JSVal -> JSString -> IO ()
