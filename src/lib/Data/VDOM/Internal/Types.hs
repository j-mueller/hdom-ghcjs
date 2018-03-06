{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module Data.VDOM.Internal.Types(
  -- * DOM
  ClientDomNode(..),
  attributes,
  children,
  style,
  onClick,
  mkClientDomNode,
  -- * Events
  Event(..),
  clientInitialised,
  clientInitialised',
  onClickEvent,
  setState,
  -- * DOM instructions
  Instruction(..),
  elementId,
  InsertWhere(..),
  -- * ClientNamespaces
  ClientNamespace(..),
  htmlNS,
  svgNS,
  -- Identifiers
  ElementId(..),
  simpleId,
  elementIDs,
  -- * Application state
  Handler,
  AppState(..),
  HasAppState(..),
  initialState,
  -- * Low-level events
  ClickData(..),
  fromClickData
) where

import           Control.Lens            hiding (children)
import           Data.DOM                (DomNode (..))
import           Data.JSString           (JSString)
import qualified Data.JSString           as JSString
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Semigroup
import           Data.String             (IsString (..))
import           Data.VDOM.Internal.Hash (Hashable (..), Hashed, hash, _Hashed)
import           GHC.Generics
import           GHCJS.Marshal           (FromJSVal (..), ToJSVal (..))
import           GHCJS.Prim              (JSVal, fromJSInt, fromJSString,
                                          getProp)

-- | ID of a DOM element, including a list of IDs of parent elements all the way
-- to the root
newtype ElementId = ElementId { mkElementId :: JSString }
  deriving (Generic, Show, Eq, Ord)

instance Hashable ElementId where
  getHash = getHash . mkElementId

instance IsString ElementId where
  fromString = ElementId . fromString

simpleId :: JSString -> ElementId
simpleId = ElementId

instance ToJSVal ElementId
instance FromJSVal ElementId

-- | An infinite supply of fresh [[ElementId]]s
elementIDs :: [ElementId]
elementIDs = (ElementId . fromString . (:) 'i' . show) <$> [1..]

newtype ClientNamespace = ClientNamespace (Hashed JSString)
  deriving (Generic, Eq, Show)

instance ToJSVal ClientNamespace
instance FromJSVal ClientNamespace

htmlNS :: ClientNamespace
htmlNS = ClientNamespace "http://www.w3.org/1999/xhtml"

svgNS :: ClientNamespace
svgNS = ClientNamespace "http://www.w3.org/2000/svg"

instance Hashable ClientNamespace where
  getHash (ClientNamespace n) = getHash n

instance IsString ClientNamespace where
  fromString = ClientNamespace . hash . fromString

data ClientDomNode a i = ClientDomNode {
  _hash            :: !Int,
  _nodeId          :: !i,
  _nodeType        :: !JSString,
  _ClientNamespace :: !ClientNamespace,
  _style           :: Hashed (Map JSString JSString), -- TODO: maybe use a JavaScript object directly via FFI (instead of Map)
  _attributes      :: Hashed (Map JSString JSString), -- TODO: maybe use a JavaScript object directly via FFI (instead of Map)
  _onClick         :: Maybe a,
  _children        :: Hashed (Either JSString [ClientDomNode a i])
  } deriving (Functor, Foldable, Traversable)

instance DomNode ClientDomNode where
  type StringType ClientDomNode = JSString
  type NamespaceType ClientDomNode = ClientNamespace
  node i ns st = mkClientDomNode i st ns mp mp Nothing (hash $ Right []) where
    mp = hash Map.empty
  children = lens g s where
    g = view _Hashed . _children
    s (ClientDomNode _ n t ns st at c _) ch = mkClientDomNode n t ns st at c (hash ch)
  style = lens g s where
    g = view _Hashed . _style
    s (ClientDomNode _ n t ns _ at c ch) st = mkClientDomNode n t ns (hash st) at c ch
  attributes = lens g s where
    g = view _Hashed . _attributes
    s (ClientDomNode _ n t ns st _ c ch) at = mkClientDomNode n t ns st (hash at) c ch
  onClick = lens g s where
    g = _onClick
    s (ClientDomNode h n t ns st at _ ch) hdl = ClientDomNode h n t ns st at hdl ch

type Handler a = (a -> Maybe a)

mkClientDomNode ::
  i
  -> JSString
  -> ClientNamespace
  -> Hashed (Map JSString JSString)
  -> Hashed (Map JSString JSString)
  -> Maybe a
  -> Hashed (Either JSString [ClientDomNode a i])
  -> ClientDomNode a i
mkClientDomNode i t n st at c ch = ClientDomNode h i t n st at c ch where
    h = getHash [getHash t, getHash n, getHash $ fmap (fmap (const ())) ch, getHash st, getHash at]

data AppState s i = AppState {
  _dom           :: Maybe (ClientDomNode (Handler s) i),
  _model         :: Maybe s,
  _clickHandlers :: Map i (Handler s) }

makeClassy ''AppState

initialState :: AppState s i
initialState = AppState Nothing Nothing Map.empty

children' :: Lens' (ClientDomNode a i) (Hashed (Either JSString [ClientDomNode a i]))
children' = lens g s where
  g =  _children
  s (ClientDomNode _ n t ns st at c _) ch = mkClientDomNode n t ns st at c ch

-- | The hash of a node excludes the ID
instance forall i a. Hashable (ClientDomNode a i) where
  getHash = _hash

data ClickData = ClickData {
  clickX         :: Int,
  clickY         :: Int,
  clickTargetIds :: [JSString] }
  deriving (Generic)

instance FromJSVal ClickData where
  fromJSVal i = fmap Just (ClickData <$> cx <*> cy <*> ct) where
    cx = fromJSInt <$> getProp i "x"
    cy = fromJSInt <$> getProp i "y"
    ct = getProp i "target" >>= js_getTargetIDs >>= fromJSValUnchecked

-- | Events that are sent from the client (UI) to the rendering thread
data Event a i =
    OnClick { _evtElementId :: [i] }
  | SetState a
  | ClientInitialised (Maybe a)
  deriving (Generic, Show, Functor, Foldable, Traversable)

instance (ToJSVal a, ToJSVal i) => ToJSVal (Event a i)
instance (FromJSVal a, FromJSVal i) => FromJSVal (Event a i)

-- | An event signalling that a DOM element has been clicked
onClickEvent :: [i] -> Event a i
onClickEvent = OnClick

fromClickData :: ClickData -> Event a ElementId
fromClickData = onClickEvent . fmap ElementId . clickTargetIds

-- | Set the state to a value, overriding the current state
setState :: a -> Event a i
setState = SetState

-- | An event signalling that the client has been initialised and is ready to
-- receive messages from the rendering thread
clientInitialised :: Event a i
clientInitialised = ClientInitialised Nothing

-- | An event signalling that the client has been initialised with a
-- pre-rendered view
clientInitialised' :: a -> Event a i
clientInitialised' = ClientInitialised . Just

-- | Where to insert an element
data InsertWhere k = InsertBefore k | InsertAsChildOf (Maybe k) | InsertAfter k
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance ToJSVal k => ToJSVal (InsertWhere k)
instance FromJSVal k => FromJSVal (InsertWhere k)

data Instruction k =
  SetAttribute {
    _setPropElementId :: k,
    _setPropProperty  :: JSString,
    _setPropValue     :: JSString }
  | CreateElement {
    _createElemParentId :: InsertWhere k,
    _createElemId       :: k,
    _createElementNS    :: ClientNamespace,
    _createElementTp    :: JSString }
  | DeleteElement {
    _deleteElemId :: k }
  | SetTextContent {
    _setTextContentId   :: k,
    _setTextContentText :: JSString }
  | SetCSSText {
    _setCSSTextElemId :: k,
    _setCSSTextText   :: JSString }
  | RemoveAttribute {
    _removeAttributeElemeId :: k,
    _removeAttributeKey     :: JSString }
  deriving (Generic, Show, Functor, Foldable, Traversable)

instance ToJSVal k => ToJSVal (Instruction k)
instance FromJSVal k => FromJSVal (Instruction k)

elementId :: Instruction k -> k
elementId = \case
  SetAttribute i _ _ -> i
  CreateElement _ i _ _ -> i
  DeleteElement i -> i
  SetTextContent i _ -> i
  SetCSSText i _ -> i
  RemoveAttribute i _ -> i

foreign import javascript unsafe "$r = getTargetIDs($1);"
  js_getTargetIDs :: JSVal -> IO JSVal
