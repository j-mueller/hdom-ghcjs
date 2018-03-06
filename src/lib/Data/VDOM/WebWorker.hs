{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
-- Undec. instances is needed for
-- instance MonadSupply i m => MonadSupply i (StateT s m)
{-# LANGUAGE UndecidableInstances  #-}
module Data.VDOM.WebWorker(
  -- * Conveniences
  runWorker,
  -- * Pipes
  W.initialise,
  process,
  -- * Types
  Event(..),
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

import           Control.Lens                       hiding (children)
import           Control.Monad.State.Strict         (MonadState (..),
                                                     StateT (..), evalStateT)
import           Control.Monad.Supply
import           Data.Map.Lazy                      (Map)
import qualified Data.Map.Lazy                      as Map
import           Data.Maybe                         (listToMaybe)
import           Data.String                        (IsString (..))
import           Data.VDOM.Internal.Diff            (assignIDs, diff)
import           Data.VDOM.Internal.Pipes.WebWorker (Input, Output)
import qualified Data.VDOM.Internal.Pipes.WebWorker as W
import           Data.VDOM.Internal.Types           (AppState, ClientDomNode,
                                                     ElementId, Event (..),
                                                     Handler, HasAppState (..),
                                                     Instruction, appState,
                                                     children, initialState,
                                                     onClick, style)
import qualified Data.VDOM.Internal.Types           as T
import           GHCJS.Marshal                      (FromJSVal)
import           Pipes

-- | Start the worker
runWorker ::
  (FromJSVal s)
  => s
  -> (s -> ClientDomNode (Handler s) ())
  -> (forall i. Event s i -> s -> s)
  -> IO ()
runWorker mdl vw hdl = do
  let st = initialState & model .~ Just mdl
  (responses, requests) <- W.initialise W.unbounded
  flip evalSupplyT T.elementIDs
    $ flip evalStateT st
    $ runEffect
    $ (W.fromInput requests) >-> (process vw hdl) >-> (W.toOutput responses)

process ::
   (MonadSupply ElementId m,
    Monad m,
    MonadIO m,
    MonadState (AppState s ElementId) m)
  => (s -> ClientDomNode (Handler s) ())
  -> (Event s i -> s -> s)
  -> Pipe (Event s ElementId) [Instruction ElementId] m ()
process vw hdl = go where
  go = (await >>= lift . (processEvent vw) >>= yield) >> go

instance MonadSupply i m => MonadSupply i (StateT s m) where
  supply = lift supply
  peek = lift peek
  exhausted = lift exhausted

-- |
render :: (
  Ord i,
  MonadSupply i m,
  MonadState t m,
  HasAppState t s i)
  => (s -> ClientDomNode (Handler s) ())
  -> m [Instruction i]
render vw = do
  old <- use dom
  mdl <- use model
  (newVDom, instructions) <- diff (T.InsertAsChildOf Nothing) old (vw <$> mdl)
  dom .= newVDom
  return instructions

processEvent :: (
  Ord i,
  MonadSupply i m,
  MonadState t m,
  HasAppState t s i)
  => (s -> ClientDomNode (Handler s) ())
  -> Event s i
  -> m [Instruction i]
processEvent v e = handleEvent v e >> render v

-- | Change the state when a new event has been received
handleEvent :: (
  Ord i,
  MonadState s m,
  MonadSupply i m,
  HasAppState s a i)
  => (a -> ClientDomNode (Handler a) ())
  -> Event a i
  -> m ()
handleEvent vw = \case
  SetState a -> model .= Just a
  OnClick ids -> do
    newState <- applyEventHandler <$> use model <*> use clickHandlers <*> pure ids
    maybe (return ()) (assign model . Just) newState
  ClientInitialised Nothing -> return ()
  -- Compute IDs for the DOM and assign handlers but do not produce any
  -- rendering instructions
  --
  -- TODO: We should skip the subsequent "render" phase
  ClientInitialised (Just st) -> do
    newVDom <- assignIDs $ vw st
    model .= Just st
    dom .= Just newVDom


-- | Given an old state, a map of event handlers, and a list of IDs
-- (representing the event targets from bottom to top), compute the new
-- state using the first handler that matches the target list
applyEventHandler :: Ord i => Maybe a -> Map i (a -> Maybe a) -> [i] -> Maybe a
applyEventHandler v mp = go where
  go xs = do
    x <- listToMaybe xs
    oldState <- v
    case Map.lookup x mp of
      Nothing  -> go (tail xs)
      Just hdl -> hdl oldState
