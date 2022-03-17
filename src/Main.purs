module Main where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class.Console (log)
import Elmish (Transition, Dispatch, ReactElement, fork, forkVoid, (<?|))
import Elmish.Boot (defaultMain) -- We'll need this in a moment
import Elmish.Foreign (readForeign) -- We'll need this in a moment
import Elmish.HTML.Styled as H  -- This is more convenient to import qualified
import Foreign (Foreign)

data Message
  = ButtonClicked
  | WordChanged String
  | TimeoutElapsed

type Val
  = String

type Gridx
  = Array Val

type State
  = { word :: String
    , grid :: Gridx
    , test :: String
    }

type Matrix a
  = Array (GridRow a)

type GridRow a
  = Array a

type Grid
  = Matrix Digit

type Digit
  = Int

b3 :: Grid
b3 =
  [ [ 8, 2, 7, 1, 5, 4, 3, 9, 6 ]
  , [ 9, 6, 5, 3, 2, 7, 1, 4, 0 ]
  , [ 3, 4, 1, 6, 8, 9, 7, 5, 2 ]
  , [ 5, 9, 3, 4, 6, 8, 2, 7, 1 ]
  , [ 4, 7, 2, 5, 1, 3, 6, 8, 9 ]
  , [ 6, 1, 8, 9, 7, 2, 4, 3, 5 ]
  , [ 7, 8, 6, 2, 3, 5, 9, 1, 4 ]
  , [ 1, 5, 4, 0, 9, 6, 8, 2, 3 ]
  , [ 2, 3, 9, 0, 4, 1, 5, 6, 0 ] -- 0 => 7
  ]

init :: Transition Message State
init =
  pure
    { word: "World"
    , grid: [ "a", "b", "c", "d" ]
    , test: "a"
    }

update :: State -> Message -> Transition Message State
update state ButtonClicked = do
  forkVoid $ log "Button clicked"
  fork do
    delay $ Milliseconds 1000.0
    pure TimeoutElapsed
  pure state { word = "Elmish" }

update state (WordChanged s) = pure state { word = s }

update state TimeoutElapsed = pure state { word = state.word <> " after a while" }

eventTargetValue :: Foreign -> Maybe String
eventTargetValue f =
  (readForeign f :: _ { target :: { value :: String } })
    <#> _.target.value

view :: State -> Dispatch Message -> ReactElement
view state dispatch =
  H.div "p-4"
    [ H.input_ "d-block"
        { type: "text"
        , value: state.word
        , onChange: dispatch <?| \f -> WordChanged <$> eventTargetValue f
        }
    , H.div ""
        [ H.text "Hello, "
        , H.strong "" state.word
        , H.text "! "
        ]
    , H.button_ "btn btn-primary mt-3" { onClick: dispatch ButtonClicked } "Click me!"
    , H.div "" $ state.grid <#> \x -> H.text x
    ]

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }
