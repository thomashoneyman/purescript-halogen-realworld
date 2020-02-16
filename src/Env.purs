-- | A global, read-only state containing information useful to most components in the application.
-- | To represent mutable state, you can use a mutable reference stored in the immutable record. To
-- | broadcast changes in mutable state (or to broadcast any other messages you'd like) to
-- | subscribed components, you can use a many-to-many bus.
-- |
-- | https://thomashoneyman.com/guides/real-world-halogen
module Conduit.Env where

import Prelude

import Conduit.Api.Request (BaseURL)
import Conduit.Data.Profile (Profile)
import Data.Maybe (Maybe)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)

-- | Let's start with some types necessary for the rest of the module.
-- |
-- | Our app monad will provide a globally-accessible, read-only environment with a few fields.
-- | First, we'll use a `LogLevel` flag to indicate whether we'd like to log everything (`Dev`) or
-- | only critical messages (`Prod`). Next, we'll maintain a configurable base URL. Our `UserEnv`
-- | will represent some mutable state holding the currently-logged-in user (if there is one).
-- |
-- | In the `AppM` module I demonstrate how to make this information available (without passing it
-- | as an argument) to any function running in `AppM`.
type Env =
  { logLevel :: LogLevel
  , baseUrl :: BaseURL
  , userEnv :: UserEnv
  }

data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

-- | This type desribes a sub-section of our user environment. Having a section within the larger
-- | record makes it easier to write constraints for components; you can, for example, require
-- | a monad supporting the environment { userEnv :: UserEnv | more } rather than having to list
-- | all the individual fields in `UserEnv`.
-- |
-- | Our user environment needs mutable state, as the currently-logged-in user will change while
-- | the application is running. How do you get mutable state from a read-only record?
-- |
-- | Use a mutable reference! The environment will be read-only, which means we cannot modify the
-- | reference, but we *can* modify the data it points to. Use this sparingly!
-- |
-- | It's not enough just to have the mutable state, however; we also need to be able to notify
-- | components when the value has changed, so that any local copies they have are always kept
-- | in sync. We'll do that with a bus. The bus will emit the new (maybe) profile any time it
-- | changes to all subscribed components.
type UserEnv =
  { currentUser :: Ref (Maybe Profile)
  , userBus :: BusRW (Maybe Profile)
  }
