module Main where

import Prelude
import Data.Array (mapWithIndex, modifyAt, updateAt)
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class.Console (log)
import Elmish (Transition, Dispatch, ReactElement, fork, forkVoid, (<?|))
import Elmish.Boot (defaultMain)
import Elmish.Foreign (readForeign)
import Elmish.HTML.Styled as H
import Elmish.React.DOM as R
import Foreign (Foreign)

data Message
  = ButtonClicked
  | WordChanged String
  | TimeoutElapsed
  | UpdateBoard Int Int Int

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

b3b :: Grid
b3b =
  [ [ 8, 2, 7, 1, 5, 4, 3, 9, 6 ]
  , [ 9, 0, 0, 3, 0, 7, 0, 0, 8 ]
  , [ 0, 0, 0, 6, 0, 9, 0, 5, 2 ]
  , [ 0, 9, 3, 4, 6, 8, 2, 0, 1 ]
  , [ 0, 0, 0, 0, 0, 0, 0, 0, 9 ]
  , [ 6, 0, 8, 9, 7, 0, 4, 3, 5 ]
  , [ 7, 0, 0, 2, 0, 0, 0, 1, 4 ]
  , [ 1, 5, 4, 7, 0, 6, 8, 2, 3 ]
  , [ 2, 3, 9, 8, 4, 1, 5, 6, 0 ]
  ]

type State
  = { word :: String
    , board :: Grid
    }

init :: Transition Message State
init =
  pure
    { word: "World"
    , board: b3
    }

update :: State -> Message -> Transition Message State
update state (UpdateBoard x y newValue) =
  pure
    state
      { board =
        state.board
          # modifyAt y (\row -> fromMaybe row (updateAt x newValue row))
          # fromMaybe state.board
      }

update state ButtonClicked = do
  forkVoid $ log "Button clicked"
  fork do
    delay $ Milliseconds 1000.0
    pure (UpdateBoard 0 0 9)
  -- pure TimeoutElapsed
  pure state { word = "Elmish" }

update state (WordChanged s) = pure state { word = s }

update state TimeoutElapsed = pure state { word = state.word <> " after a while" }

eventTargetValue :: Foreign -> Maybe String
eventTargetValue f =
  (readForeign f :: _ { target :: { value :: String } })
    <#> _.target.value

squares :: Dispatch Message -> Int -> Int -> Int -> ReactElement
squares dispatch y x s =
  H.div_ "square" { style: H.css { gridArea: (show (y + 1)) <> " / " <> (show (x + 1)) <> " / auto / auto" } }
    ( H.input_ ""
        { type: "text"
        , value: show s
        , onChange: dispatch <?| \f -> UpdateBoard x y <$> (eventTargetValue f >>= fromString)
        }
    )

squares2 :: Dispatch Message -> Int -> Array Int -> ReactElement
squares2 dispatch y ss = R.fragment $ mapWithIndex (squares dispatch y) ss

view :: State -> Dispatch Message -> ReactElement
view state dispatch =
  H.div "board-container"
    [ H.div ""
        [ H.text ":: Hello, "
        , H.strong "" state.word
        , H.text "! "
        ]
    , H.button_ "btn btn-primary mt-3" { onClick: dispatch ButtonClicked } "Click me!"
    , H.div "board" $ state.board # mapWithIndex (squares2 dispatch)
    ]

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }
