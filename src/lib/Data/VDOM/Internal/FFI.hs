{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.VDOM.Internal.FFI(
  -- * Worker messages
  postMessage,
  postMessage',
  setOnMessage,
  setOnMessage',
  -- * Events
  ClickData(..),
  setOnClick,
  -- * Misc.
  getElementById,
  getDocumentBody,
  consoleLog
) where

import           Control.Monad            ((<=<))
import           Data.JSString            (JSString)
import           Data.VDOM.Internal.Types (ClickData (..))
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal            (FromJSVal (..), ToJSVal (..),
                                           toJSVal_aeson)
import           GHCJS.Prim               (JSVal)
import qualified JavaScript.Web.Worker    as Worker

-- | Post a message to the main thread
postMessage :: ToJSVal a => a -> IO ()
postMessage v = js_postMessage =<< toJSVal v

-- | Post a message to a worker
postMessage' :: ToJSVal a => Worker.Worker -> a -> IO ()
postMessage'  w e = do
  m' <- toJSVal e
  Worker.postMessage m' w

-- | Set the `onmessage` handler of this worker to a callback
setOnMessage :: FromJSVal m => (m -> IO ()) -> IO ()
setOnMessage callback = do
  let cb' = callback <=< fromJSValUnchecked
  cb <- asyncCallback1 cb'
  js_selfSetOnMessage cb

-- | Set the `onmessage` handler of a worker object to a callback
setOnMessage' :: FromJSVal m => Worker.Worker -> (m -> IO ()) -> IO ()
setOnMessage' w callback = do
  let cb' = callback  <=< fromJSValUnchecked
  cb <- asyncCallback1 cb'
  js_setOnMessage w cb

setOnClick :: JSVal -> (ClickData -> IO ()) -> IO ()
setOnClick w callback = do
  cb <- asyncCallback1 (callback <=< fromJSValUnchecked)
  js_setOnClick w cb

getElementById :: JSString -> IO JSVal
getElementById = js_getElementById

getDocumentBody :: IO JSVal
getDocumentBody = js_getDocumentBody

consoleLog :: JSVal -> IO ()
consoleLog = js_consoleLog

-- TODO: Use GHCJS.DOM.DedicatedWorkerGlobalScope.postMessage for this (can't figure out how to construct a value of DedicatedWorkerGlobalScope)
foreign import javascript unsafe "postMessage($1);" js_postMessage :: JSVal -> IO ()

foreign import javascript unsafe "self.onmessage = function (e) { $1(e.data); };" js_selfSetOnMessage :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onmessage = function (e) { $2(e.data); };" js_setOnMessage :: Worker.Worker -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onclick = function (e) { $2(e); };" js_setOnClick :: JSVal -> Callback (JSVal -> IO()) -> IO ()

foreign import javascript unsafe "$r = document.getElementById($1);"
  js_getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "$r = document.body;"
  js_getDocumentBody :: IO JSVal

foreign import javascript unsafe "console.log($1);"
  js_consoleLog :: JSVal -> IO ()
