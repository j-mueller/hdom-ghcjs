-- | Asynchronous communication between a web worker and the main browser
-- thread using pipes
-- .
-- Modelled after the [[pipes-concurreny]] package
--
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
module Data.VDOM.Internal.Pipes.WebWorker(
  -- * Inputs and Outputs
  Input(..),
  Output(..),
  -- * Pipe utilities
  fromInput,
  toOutput,
  -- * Actors
  spawn,
  initialise,
  Buffer,
  unbounded,
  bounded,
  latest,
  newest,
  -- * Mailboxes
  createInput,
  createOutput
  ) where



import           Control.Applicative                  (Alternative (empty, (<|>)),
                                                       Applicative (pure, (*>), (<*>)),
                                                       (<$>), (<*))
import           Control.Concurrent.STM               (STM, atomically,
                                                       mkWeakTVar, newTVarIO,
                                                       readTVar)
import           Control.Monad                        (MonadPlus (..), void,
                                                       when)
import           Data.Functor.Contravariant           (Contravariant (contramap))
import           Data.Functor.Contravariant.Divisible (Decidable (choose, lose), Divisible (conquer, divide))
import           Data.JSString                        (JSString)
import           Data.Monoid                          (Monoid (mappend, mempty))
import           Data.Void                            (absurd)
import           GHCJS.Marshal                        (FromJSVal (..),
                                                       ToJSVal (..))
import qualified JavaScript.Web.Worker                as Worker
import           Pipes                                (Consumer',
                                                       MonadIO (liftIO),
                                                       Producer', await, yield)

import qualified Control.Concurrent.STM               as S
import qualified Control.Exception
import qualified Data.VDOM.Internal.FFI               as FFI

newtype Input a = Input { recv :: S.STM a }
    deriving (Functor)

instance Applicative Input where
  pure r    = Input (pure r)
  mf <*> mx = Input (recv mf <*> recv mx)

instance Monad Input where
  return r = Input (return r)
  m >>= f  = Input $ do
      ma <- recv m
      recv (f ma)


{-| An exhaustible sink of values

    'send' returns 'False' if the sink is exhausted
-}
newtype Output a = Output { send :: a -> IO () }

instance Monoid (Output a) where
  mempty = Output $ \_ -> return ()
  mappend l r = Output $ \a -> send l a >> send r a

instance Contravariant Output where
  contramap f (Output a) = Output (a . f)

instance Divisible Output where
  conquer = Output $ \_ -> return ()
  divide f l r = Output $ \a -> case f a of
    (b, c) -> send l b >> send r c

instance Decidable Output where
  lose f = Output (absurd . f)
  choose f i1 i2 = Output $ \a -> case f a of
      Left b  -> send i1 b
      Right c -> send i2 c

{-| Convert an 'Output' to a 'Pipes.Consumer'
    'toOutput' terminates when the 'Output' is exhausted.
-}
toOutput :: (MonadIO m) => Output a -> Consumer' a m ()
toOutput output = loop
  where
    loop = do
        a     <- await
        _ <- liftIO $ send output a
        loop
{-# INLINABLE toOutput #-}

{-| Convert an 'Input' to a 'Pipes.Producer'
    'fromInput' terminates when the 'Input' is exhausted.
-}
fromInput :: (MonadIO m) => Input a -> Producer' a m ()
fromInput input = loop
  where
    loop = do
        ma <- liftIO $ S.atomically $ recv input
        yield ma
        loop
{-# INLINABLE fromInput #-}

-- | 'Buffer' specifies how to buffer messages stored within the mailbox
data Buffer a
    = Unbounded
    | Bounded Int
    | Single
    | Latest a
    | Newest Int
    | New

-- | Store an unbounded number of messages in a FIFO queue
unbounded :: Buffer a
unbounded = Unbounded

-- | Store a bounded number of messages, specified by the 'Int' argument
bounded :: Int -> Buffer a
bounded 1 = Single
bounded n = Bounded n

{-| Only store the 'Latest' message, beginning with an initial value
    'Latest' is never empty nor full.
-}
latest :: a -> Buffer a
latest = Latest

{-| Like @Bounded@, but 'send' never fails (the buffer is never full).
    Instead, old elements are discarded to make room for new elements
-}
newest :: Int -> Buffer a
newest 1 = New
newest n = Newest n

{-| Create [[Input]] and [[Output]] that consume from the onMessage event and
    produce to the postMessage handler of 'self' (This should be called from
    the worker thread)
-}
initialise :: (ToJSVal rq, FromJSVal rsp) => Buffer rsp -> IO (Output rq, Input rsp)
initialise bf = do
  let out = createOutput FFI.postMessage
  i <- createInput FFI.setOnMessage bf
  return (out, i)

{-| Create a new web worker from a source location

-}
spawn :: (ToJSVal rq, FromJSVal rsp) => JSString -> Buffer rsp -> IO (Output rq, Input rsp)
spawn source buffer = do
  w <- Worker.create source
  let out = createOutput (FFI.postMessage' w)
  i <- createInput (FFI.setOnMessage' w) buffer
  return (out, i)

createOutput :: (rq -> IO ()) -> Output rq
createOutput = Output

createInput :: ((rsp -> IO ()) -> IO ()) -> Buffer rsp -> IO (Input rsp)
createInput receive buffer = do
  read <- case buffer of
    Bounded n -> do
      q <- S.newTBQueueIO n
      _ <- receive (atomically . S.writeTBQueue q)
      return $ S.readTBQueue q
    Unbounded -> do
      q <- S.newTQueueIO
      _ <- receive (atomically . S.writeTQueue q)
      return $ S.readTQueue q
    Single -> do
      m <- S.newEmptyTMVarIO
      _ <- receive (atomically . S.putTMVar m)
      return $ S.takeTMVar m
    Latest a -> do
      t <- S.newTVarIO a
      _ <- receive (atomically . S.writeTVar t)
      return $ S.readTVar t
    New -> do
      m <- S.newEmptyTMVarIO
      _ <- receive (atomically . \x -> S.tryTakeTMVar m *> S.putTMVar m x)
      return $ S.takeTMVar m
    Newest n -> do
      q <- S.newTBQueueIO n
      let write x = S.writeTBQueue q x <|> (S.tryReadTBQueue q *> write x)
      _ <- receive (atomically . write)
      return $ S.readTBQueue q
  return $ Input read
