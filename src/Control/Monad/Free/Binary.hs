{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DeriveAnyClass #-}
module Control.Monad.Free.Binary where
import Data.Binary
import GHC.Generics
import Control.Monad.Free

-- Free doesn't provide Generic or Binary instances, but we can derive orphans for them using StandaloneDeriving.

deriving instance Generic (Free f a)
deriving instance (Binary (f (Free f a)), Binary a) => Binary (Free f a)