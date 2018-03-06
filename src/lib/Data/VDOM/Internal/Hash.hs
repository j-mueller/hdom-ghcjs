{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.VDOM.Internal.Hash(
  Hashable(..),
  Hashed(..),
  _Hashed,
  hash) where

import           Control.Lens
import           Data.Foldable   (foldl', toList)
import           Data.JSString   (JSString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String     (IsString (..))
import           GHC.Generics
import           GHCJS.Marshal   (FromJSVal (..), ToJSVal (..))
import           GHCJS.Prim      (fromJSInt)
import           GHCJS.Types     (JSVal)

class Hashable a where
  getHash :: a -> Int

instance Hashable JSString where
  getHash = js_hashString

instance (Hashable a, Hashable b) => Hashable (a, b) where
  getHash (a, b) = js_hashWithSalt (getHash a) (getHash b)

instance Hashable () where
  getHash _ = 2166136261

instance (Hashable a, Hashable b) => Hashable (Either a b) where
  getHash (Left a)  = getHash (a, ())
  getHash (Right b) = getHash ((), b)

instance Hashable a => Hashable [a] where
  getHash = foldl' step 2166136261 where
    step b a = js_hashWithSalt (getHash a) b

instance Hashable Int where
  getHash =  js_hashWithSalt 2166136261

instance (Hashable k, Hashable v) => Hashable (Map k v) where
  getHash = getHash . Map.toList

hashNode :: (Foldable f, Hashable t) => t -> t -> f t -> Int
hashNode nt ns ch = js_hashWithSalt (getHash nt)
    $ js_hashWithSalt (getHash ns)
    $ getHash $ toList ch

-- | Pair of a value and a hash with some convenience instances.
--
-- NOTE: Functor, Foldable, Traversable don't update the hash, so
--       they should only be used if the changes effected by those instances
--       have no impact on the hash value.
--       This is true in particular for [[Data.HTML.VDOM.DomNode]]
data Hashed a = Hashed { _hashedVal :: !Int, value :: !a}
  deriving (Generic, Show, Functor, Foldable, Traversable)

_Hashed :: Hashable a => Iso' (Hashed a) a
_Hashed = iso value mk where
  mk s = Hashed (getHash s) s

hash :: Hashable a => a -> Hashed a
hash = review _Hashed

instance Eq (Hashed a) where
  l == r = _hashedVal l == _hashedVal r

instance (IsString t, Hashable t) => IsString (Hashed t) where
  fromString s = Hashed (getHash s') s' where
    s' = fromString s

instance ToJSVal a => ToJSVal (Hashed a)
instance FromJSVal a => FromJSVal (Hashed a)

instance Hashable (Hashed a) where
  getHash = _hashedVal

foreign import javascript unsafe "$r = hashString($1);" js_hashString :: JSString -> Int

foreign import javascript unsafe "$r = hashWithSalt($1, $2);" js_hashWithSalt :: Int -> Int -> Int
