{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Data.VDOM.Internal.Diff where

import           Control.Applicative
import           Control.Lens             hiding (children, transform)
import           Control.Monad.State      (MonadState (..))
import           Control.Monad.Supply     (MonadSupply (..))
import           Data.JSString            (JSString, append, intercalate)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (catMaybes)
import           Data.Semigroup           hiding (diff)
import           Data.String              (IsString (..))
import           Data.VDOM.Internal.Hash  (Hashable (..), Hashed (..), hash,
                                           _Hashed)
import qualified Data.VDOM.Internal.Hash  as Hash
import           Data.VDOM.Internal.Types (ClientDomNode (..),
                                           ClientNamespace (..), Handler (..),
                                           HasAppState (..), InsertWhere (..),
                                           Instruction (..), attributes,
                                           children, htmlNS, mkClientDomNode,
                                           style)

diff ::
  (Ord i,
  MonadState t m,
  HasAppState t s i,
  MonadSupply i m)
  => InsertWhere i -- ^ ID of the parent node
  -> Maybe (ClientDomNode (Handler s) i)  -- ^ The original (old) dom, if it exists
  -> Maybe (ClientDomNode (Handler s) ()) -- ^ The target state
  -> m (Maybe (ClientDomNode (Handler s) i), [Instruction i]) -- ^ A list of instructions that will transform the old dom into the new dom, with IDs assigned where necessary
diff _ Nothing Nothing    =
  -- No changes need to be made
  return (Nothing, [])
diff p Nothing (Just new) = do
  -- The entire subtree is new
  new' <- assignIDs new
  nws <- createNew p new'
  return (Just new', nws)
diff _ (Just old) Nothing = do
  i <- deleteNode old
  return (Nothing, [i])
diff p (Just old) (Just new) =
  -- Check if there are any differences
    if (_hash new) == (_hash old)
    then return (Just old, [])
    else if (_nodeType old /= _nodeType new)
         then do
          (dn, is) <- replace p old new
          return (Just dn, is)
         else do
          (dn, is) <- transform old new
          return (Just dn, is)

assignIDs :: MonadSupply i m => ClientDomNode a () -> m (ClientDomNode a i)
assignIDs = traverse (const supply)

deleteNode :: (
  Ord i,
  MonadState t m,
  HasAppState t s i)
  => ClientDomNode (Handler s) i
  -> m (Instruction i)
deleteNode n = do
  _ <- clickHandlers . at (_nodeId n) .= Nothing
  return $ DeleteElement $ _nodeId n

-- | Replace a node by deleting it and creating the replacement from scratch
replace :: (
  Ord i,
  MonadState t m,
  HasAppState t s i,
  MonadSupply i m)
   => InsertWhere i
   -> ClientDomNode (Handler s) i
   -> ClientDomNode (Handler s) ()
   -> m (ClientDomNode (Handler s) i, [Instruction i])
replace p old new = do
  new' <- assignIDs new
  d <- deleteNode old
  news <- createNew p new'
  return (new', d : news)

-- | Create a node and all its children from scratch
createNew :: (
  Ord i,
  MonadState t m,
  HasAppState t s i)
  => InsertWhere i
  -> ClientDomNode (Handler s) i
  -> m ([Instruction i])
createNew prnt d@ClientDomNode{..} = do
  let parent = CreateElement prnt _nodeId _ClientNamespace _nodeType
      stl = SetCSSText _nodeId $ toCssText $ view _Hashed _style
      atts = fmap (uncurry $ SetAttribute _nodeId) $ Map.toList $ view _Hashed _attributes
  _ <- clickHandlers . at _nodeId .= _onClick
  ch <- either (return . return . return . SetTextContent _nodeId) (mapM (createNew (InsertAsChildOf $ Just _nodeId))) $ view _Hashed _children
  return (parent:stl:atts ++ concat ch)

-- | Generate instructions that transform the old node into the new node
transform ::  (
  Ord i,
  MonadState t m,
  HasAppState t s i,
  MonadSupply i m)
  => ClientDomNode (Handler s) i
  -> ClientDomNode (Handler s) ()
  -> m (ClientDomNode (Handler s) i, [Instruction i])
transform old new = result where
  i = _nodeId old
  newChildren = _children new
  result = do
    let (sti, std) = if (_style old) == (_style new)
                     then ([], id)
                     else ([SetCSSText i $ toCssText $ view _Hashed $ _style new], set style (view style new))
    let (ati, atd) = if (_attributes old) == (_attributes new)
                     then ([], id)
                     else (changeAttributes (old ^. attributes) (new ^. attributes) i, set attributes (view attributes new))
    (chd, chi) <-
      if (_hashedVal $ _children old) == (_hashedVal $ _children new)
      then return (id, [])
      else
        case (old ^. children, new ^. children) of
          (Left _, Left t) ->
            return (set children (Left t), [SetTextContent i t])
          (Right xs, Left t) ->
            return (set children (Left t), (SetTextContent i t) : fmap (DeleteElement . _nodeId) xs)
          (Left _, Right xs) -> do
            newChildren' <- mapM assignIDs xs
            nws <- concat <$> mapM (createNew (InsertAsChildOf $ Just i)) newChildren'
            return (set children (Right newChildren'), nws)
          (Right olds, Right news) -> do
            (newChildren, instructions) <- syncChildren (InsertAsChildOf $ Just i) olds news
            return (set children (Right newChildren), instructions)
    let f = chd . std . atd
    return (f old, sti ++ chi ++ ati)

changeAttributes :: Map JSString JSString -> Map JSString JSString -> i -> [Instruction i]
changeAttributes old new i = actions where
  actions = fmap snd $ Map.toList $ inner old new
  inner = Map.mergeWithKey join mapOld mapNew
  join k a b
    | a == b    = Nothing
    | otherwise = Just $ SetAttribute i k b
  mapOld = Map.mapWithKey $ \k _ -> RemoveAttribute i k
  mapNew = Map.mapWithKey $ \k v -> SetAttribute i k v

syncChildren :: (
  Ord i,
  MonadState t m,
  HasAppState t s i,
  MonadSupply i m) =>
     InsertWhere i
  -> [ClientDomNode (Handler s) i]
  -> [ClientDomNode (Handler s) ()]
  -> m ([ClientDomNode (Handler s) i], [Instruction i])
syncChildren p olds news = case (olds, news) of
  ([], [])   -> return ([], [])
  ([], news) -> do
    news' <- mapM assignIDs news
    nws <- concat <$> mapM (createNew p) news'
    return (news', nws)
  (olds, []) -> do
    is <- mapM deleteNode olds
    return ([], is)
  (o:os, n:ns) -> do
    -- TODO: Look-ahead to catch insertions/deletions
    (Just n', is) <- diff p (Just o) (Just n)
    (rNodes, rInstructions) <- syncChildren (InsertAfter $ _nodeId n') os ns
    return ( n' : rNodes, is ++ rInstructions)

toCssText :: Map JSString JSString -> JSString
toCssText = intercalate "; " . fmap (\t -> fst t `append` ": " `append` snd t) . Map.toList

