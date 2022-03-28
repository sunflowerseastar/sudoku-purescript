module Main where

import Prelude
import Boards (Grid, b1, boards)
import Sudoku (solve)
import Data.Array (head, length, mapWithIndex, modifyAt, updateAt, (!!))
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (replace)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Effect (Effect)
import Elmish (Transition, Dispatch, ReactElement, (<?|))
import Elmish.Boot (defaultMain)
import Elmish.Foreign (readForeign)
import Elmish.HTML.Styled as H
import Elmish.React.DOM as R
import Foreign (Foreign)

-- ---------
-- UI
-- ---------
type State
  = { board :: Grid
    , currentBoardIndex :: Int
    }

data DecOrInc
  = Dec
  | Inc

data Message
  = ClickSolve
  | UpdateBoard Int Int Int
  | PreviousOrNextBoard DecOrInc

initCurrentBoardIndex :: Int
initCurrentBoardIndex = 0

defaultBoard :: Grid
defaultBoard = b1

init :: Transition Message State
init =
  pure
    { currentBoardIndex: initCurrentBoardIndex
    , board: boards !! initCurrentBoardIndex # fromMaybe defaultBoard
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

update state (PreviousOrNextBoard decOrInc) =
  let
    newBoardIndex = case decOrInc of
      Dec -> mod (state.currentBoardIndex - 1) (length boards)
      Inc -> mod (state.currentBoardIndex + 1) (length boards)
  in
    pure
      state
        { currentBoardIndex = newBoardIndex
        , board = boards !! newBoardIndex # fromMaybe defaultBoard
        }

update state ClickSolve = do
  pure state { board = state.board # solve >>> head >>> fromMaybe state.board }

eventTargetValue :: Foreign -> Maybe String
eventTargetValue f =
  (readForeign f :: _ { target :: { value :: String } })
    <#> _.target.value

squares :: Dispatch Message -> Int -> Int -> Int -> ReactElement
squares dispatch y x s =
  -- TODO see if gridArea can be handled completely in css
  H.div_ "square" { style: H.css { gridArea: (show (y + 1)) <> " / " <> (show (x + 1)) <> " / auto / auto" } }
    ( H.input_ ""
        { type: "text"
        , value: if s == 0 then "" else show s
        , onChange:
            dispatch
              <?| \f ->
                  eventTargetValue f
                    <#> replace (Pattern (show s)) (Replacement "")
                    >>= fromString
                    <#> UpdateBoard x y
        }
    )

squares2 :: Dispatch Message -> Int -> Array Int -> ReactElement
squares2 dispatch y ss = R.fragment $ mapWithIndex (squares dispatch y) ss

view :: State -> Dispatch Message -> ReactElement
view state dispatch =
  H.div "main"
    [ H.div "board-container"
        [ H.div "above-board constrain-width"
            [ H.div "left"
                [ H.a_ "arrow-left" { onClick: dispatch (PreviousOrNextBoard Dec) } "◀"
                , H.a_ "arrow-right" { onClick: dispatch (PreviousOrNextBoard Inc) } "▶"
                , H.span "em" ("board " <> (state.currentBoardIndex + 1 # show) <> " of " <> (boards # length >>> show))
                ]
            ]
        , H.div "board constrain-width"
            [ H.div "board-inner" $ state.board
                # mapWithIndex (squares2 dispatch)
            , H.div "board-horizontal-lines" " "
            , H.div "board-vertical-lines" " "
            ]
        ]
    , H.div "button-container"
        [ H.div "button-indicator"
            [ H.button_ "" { onClick: dispatch ClickSolve } "solve" ]
        ]
    ]

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }
