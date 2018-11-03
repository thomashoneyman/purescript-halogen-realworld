module AppM where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Type.Equality (class TypeEquals, from)

{- Our global environment will store read-only information available
   to any function with the right MonadAsk constraint.
-}

type Env = { }

{- Our application monad will be the base of a `ReaderT` transformer.
   We gain the functionality of `ReaderT` in addition to any other
   instances we write.

   We can derive all the instances we need to make this new type
   a monad and allow the use of `Effect` and `Aff`.
-}

newtype AppM a = AppM (ReaderT Env Aff a)

derive instance newtypeAppM :: Newtype (AppM a) _

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

{- This instance has to be defined manually, as we are using a
   type synonym rather than a newtype for our environment.
-}

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from
