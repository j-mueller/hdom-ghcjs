{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.VDOM.Client(
  -- * Conveniences
  runApp,
  runApp',
  VDomOptions(..),
  defaultVDomOptions,
  -- * Pipes
  W.spawn,
  processInstructions,
  -- * Types
  Event(..),
  Instruction,
  interpretActions,
  -- ** Mailboxes
  W.Buffer,
  Input,
  Output,
  W.unbounded,
  W.bounded,
  W.latest,
  W.newest,
  W.createInput,
  W.createOutput
  ) where

import           Control.Concurrent                 (forkIO, threadDelay)
import           Control.Monad                      (mapM_)
import           Control.Monad.IO.Class
import           Data.JSString                      (JSString)
import           Data.VDOM.Internal.Types           (setState)
import           GHC.Generics                       (Generic)
import           GHCJS.Marshal                      (ToJSVal)
import qualified JavaScript.Web.Worker              as Worker

import qualified Data.VDOM.Internal.FFI             as FFI
import           Data.VDOM.Internal.Interpreter     (interpretActions)
import           Data.VDOM.Internal.Pipes.DOM       (onClick)
import           Data.VDOM.Internal.Pipes.WebWorker (Input, Output)
import qualified Data.VDOM.Internal.Pipes.WebWorker as W
import           Data.VDOM.Internal.Types           (ElementId, Event,
                                                     Instruction,
                                                     clientInitialised,
                                                     clientInitialised')
import           Pipes
import qualified Pipes.Prelude                      as P

runApp :: ToJSVal a => a -> IO ()
runApp a = runApp' opts where
  opts = defaultVDomOptions { initialState = Just a }

runApp' :: ToJSVal a => VDomOptions a -> IO ()
runApp' VDomOptions{..} = do
  (workerRequests, workerResponses) <- W.spawn workerScriptLocation W.unbounded
  _ <- threadDelay initialWait -- sleep to ensure the worker is ready to receive messages
  clicks <- FFI.getDocumentBody >>= onClick
  _ <- forkIO
    $ runEffect
    $ (W.fromInput clicks) >-> (W.toOutput workerRequests)
  let initialMsg = maybe clientInitialised clientInitialised' initialState
  _ <- runEffect $ (yield initialMsg) >-> (W.toOutput workerRequests)
  runEffect $ (W.fromInput workerResponses) >-> processInstructions

data VDomOptions a = VDomOptions {
  initialState         :: Maybe a,
  workerScriptLocation :: JSString,
  initialWait          :: Int
  } deriving (Eq, Ord, Show, Generic)

defaultVDomOptions = VDomOptions {
  initialState = Nothing,
  workerScriptLocation = "/render-worker.js",
  initialWait = 3000000
  }

processInstructions :: Consumer [Instruction ElementId] IO ()
processInstructions = loop >> processInstructions where
  loop = await >>= liftIO . interpretActions
