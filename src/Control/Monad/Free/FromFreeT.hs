{-# LANGUAGE LambdaCase #-}

module Control.Monad.Free.FromFreeT where

import qualified Control.Monad.Trans.Free as FT
import Control.Monad.Free 

-- | "Compile" a FreeT expression into a static Free instruction list
fromFreeT :: (Monad m, Traversable f) => FT.FreeT f m a -> m (Free f a)
fromFreeT freeT = FT.runFreeT freeT >>= \case
    FT.Pure a -> return (Pure a)
    FT.Free f -> Free <$> mapM fromFreeT f