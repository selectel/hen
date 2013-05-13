{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module System.Xen.High.Internal
    ( XenT(..)
    , Xen
    , MonadXen(..)
    , runXenT
    ) where

import Control.Applicative (Applicative)
import Control.Exception (SomeException)
import Data.Monoid (Monoid)

import Control.Monad.Exception (MonadException, try, bracket)
import Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT, mapReaderT, ask)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.State (MonadState(..))
import Control.Monad.Writer (MonadWriter(..))

import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans (MonadIO, MonadTrans(lift))
import qualified Control.Monad.Trans.Cont as Cont
import qualified Control.Monad.Trans.Error as Error
import qualified Control.Monad.Trans.State.Lazy as LazyState
import qualified Control.Monad.Trans.State.Strict as StrictState
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS

import System.Xen.Types (XcHandle)
import qualified System.Xen.Mid as Mid

------------------------------------------------------------------------------
-- * The mtl style typeclass

class (Functor m, MonadIO m) => MonadXen m where
    -- | Helper function for creating high-level interface functions from mid-level.
    -- Generally high-level function is just @highLevel = withXenHandle midLevel@.
    withXenHandle :: (XcHandle -> m a) -> m a

instance MonadXen m => MonadXen (Cont.ContT r m) where
    withXenHandle = Cont.mapContT id . withXenHandle

instance (MonadXen m, Error.Error e) => MonadXen (Error.ErrorT e m) where
    withXenHandle = Error.mapErrorT id . withXenHandle

deriving instance MonadXen m => MonadXen (IdentityT m)

instance MonadXen m => MonadXen (LazyState.StateT s m) where
    withXenHandle = LazyState.mapStateT id . withXenHandle

instance MonadXen m => MonadXen (StrictState.StateT s m) where
    withXenHandle = StrictState.mapStateT id . withXenHandle

instance MonadXen m => MonadXen (ReaderT r m) where
    withXenHandle = mapReaderT id . withXenHandle

instance (MonadXen m, Monoid w) => MonadXen (LazyWriter.WriterT w m) where
    withXenHandle = LazyWriter.mapWriterT id . withXenHandle

instance (MonadXen m, Monoid w) => MonadXen (StrictWriter.WriterT w m) where
    withXenHandle = StrictWriter.mapWriterT id . withXenHandle

instance (MonadXen m, Monoid w) => MonadXen (LazyRWS.RWST r w s m) where
    withXenHandle = LazyRWS.mapRWST id . withXenHandle

instance (MonadXen m, Monoid w) => MonadXen (StrictRWS.RWST r w s m) where
    withXenHandle = StrictRWS.mapRWST id . withXenHandle

-- * The @transformers@-style monad transfomer
------------------------------------------------------------------------------

newtype XenT m a = XenT { unXenT :: ReaderT XcHandle m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadException)

type Xen = XenT IO

instance (Functor m, MonadIO m, MonadException m) => MonadXen (XenT m) where
    withXenHandle f = f =<< XenT ask

instance MonadState s m => MonadState s (XenT m) where
    get = lift get
    put = lift . put
#if MIN_VERSION_mtl(2,1,0)
    state = lift . state
#endif

instance MonadReader r m => MonadReader r (XenT m) where
    ask = lift ask
    local f = XenT . mapReaderT (local f) . unXenT

instance MonadWriter w m => MonadWriter w (XenT m) where
    tell = lift . tell
    listen = XenT . listen . unXenT
    pass = XenT . pass . unXenT

instance MonadRWS r w s m => MonadRWS r w s (XenT m)

-- | Open new connection to the hypervisor, run any @Xen@ action and close
-- connection if nessesary. This function can fail with @Either SomeException@ with
-- 'System.Xen.Errors.XcHandleOpenError' and any error of providing @Xen@ action.
runXenT :: (Functor m, MonadIO m, MonadException m) => XenT m a -> m (Either SomeException a)
runXenT (XenT f) = try $ withNewHandle $ runReaderT f
  where
    withNewHandle = bracket Mid.interfaceOpen Mid.interfaceClose
