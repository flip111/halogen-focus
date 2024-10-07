module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DOM.HTML.Indexed.InputType as I
import Data.Array
import Data.Maybe
import Data.Foldable (traverse_)
import Web.HTML.HTMLElement as HTMLElement
import Effect.Class (class MonadEffect)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

focusRef :: H.RefLabel
focusRef = H.RefLabel "input"

type State =
  { inputs :: Array String
  , text :: String -- last text
  , focus :: Maybe Int
  }

initialState :: forall i. i -> State
initialState _ =
  { inputs: []
  , text: ""
  , focus: Nothing
  }

data Action
  = AddInput
  | InputChange Int String

component :: forall q i o m. MonadEffect m => H.Component q i o m
component = H.mkComponent
  { initialState: initialState
  , render: render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  AddInput -> do
    H.modify_ \st -> do
      let new_inputs = st.inputs <> [""]
      st {inputs = new_inputs, focus = if new_inputs == [] then Nothing else Just ((length new_inputs) - 1) }
    H.getHTMLElementRef focusRef >>= traverse_ \input -> H.liftEffect $ HTMLElement.focus input

  InputChange idx text -> do
    {inputs} <- H.get
    case modifyAt idx (\_ -> text) inputs of
      Nothing         -> pure unit
      Just new_inputs -> H.modify_ \st -> st {inputs = new_inputs, text = text}

render :: forall m. State -> H.ComponentHTML Action () m
render s = HH.div_ $
  [ HH.text s.text
  , HH.button [HE.onClick \_ -> AddInput] [HH.text "Add"]
  ] <> mapWithIndex (renderInput s.focus) s.inputs

renderInput :: forall m. Maybe Int -> Int -> String -> H.ComponentHTML Action () m
renderInput focus idx text = HH.input $
  [ HP.type_ I.InputText
  , HP.value text
  , HE.onValueInput \val -> InputChange idx val
  ] <>
  case focus of
    Nothing   -> []
    Just f_id -> if f_id == idx then [HP.ref focusRef] else []
