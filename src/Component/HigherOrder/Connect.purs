-- | This higher-order component exists to wrap other components which
-- | need to connect to the user data in global application state and
-- | stay in sync with changes to that data.
module Component.HigherOrder.Connect where

import Prelude

import Conduit.Component.Utils (busEventSource)
import Conduit.Data.Profile (Profile)
import Conduit.Env (UserEnv)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Prim.Row as Row
import Record as Record

data Action input output
  = Initialize
  | HandleUserBus (Maybe Profile)
  | Receive input
  | Emit output

type WithCurrentUser r =
  ( currentUser :: Maybe Profile | r )

type ChildSlots query output =
  ( inner :: H.Slot query output Unit )

_inner = SProxy :: SProxy "inner"

-- | This component can re-use the query type and output type of its child
-- | component because it has no queries or outputs of its own. That makes
-- | it a transparent wrapper around the inner component.
component
  :: forall query input output m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Row.Lacks "currentUser" input
  => H.Component HH.HTML query { | WithCurrentUser input } output m
  -> H.Component HH.HTML query { | input } output m
component innerComponent =
  H.mkComponent
    -- here, we'll insert the current user into the wrapped component's input
    -- minus the current user
    { initialState: Record.insert (SProxy :: _ "currentUser") Nothing
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }
  where
  handleAction = case _ of
    -- On initialization we'll read the current value of the user in
    -- state; we'll also subscribe to any updates so we can always
    -- stay in sync.
    Initialize -> do
      { currentUser, userBus } <- asks _.userEnv
      _ <- H.subscribe (HandleUserBus <$> busEventSource userBus)
      mbProfile <- liftEffect $ Ref.read currentUser
      H.modify_ _ { currentUser = mbProfile }

    -- When the user in global state changes, this event will occur
    -- and we need to update our local state to stay in sync.
    HandleUserBus mbProfile ->
      H.modify_ _ { currentUser = mbProfile }

    Receive input -> do
      { currentUser } <- H.get
      H.put $ Record.insert (SProxy :: _ "currentUser") currentUser input

    Emit output ->
      H.raise output

  -- We'll simply defer all queries to the existing H.query function, sending
  -- to the correct slot.
  handleQuery :: forall a. query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = H.query _inner unit

  -- We'll simply render the inner component as-is, except with the augmented
  -- input containing the current user.
  render state =
    HH.slot _inner unit innerComponent state (Just <<< Emit)
