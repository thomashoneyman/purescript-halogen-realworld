-- | This higher-order component exists to wrap other components which
-- | need to connect to the user data in global application state and 
-- | stay in sync with changes to that data.
module Component.HOC.Connect where

import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH

data Action 
  = Initialize
  | HandleUserBus (Maybe Profile)

type State input = 
  { input :: input
  , currentUser :: Maybe Profile
  }

type ChildSlots query output =
  ( inner :: H.Slot query output Unit )

_inner = SProxy :: SProxy "inner"

-- | This component can re-use the query type and output type of its child
-- | component because it has no queries or outputs of its own. That makes
-- | it a transparent wrapper around the inner component.
component
  :: forall query output m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => H.Component HH.HTML query input output m
  -> H.Component HH.HTML query (State input) output m
component innerComponent = 
  H.mkComponent
    { initialState: 
        { input: _
        , currentUser: Nothing 
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
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

    Bubble output ->
      H.raise output

  -- Here, we pass the inner component its input along with the current
  -- user from the global state.
  render state = 
    HH.slot _inner unit innerComponent state (Just <<< Bubble)
