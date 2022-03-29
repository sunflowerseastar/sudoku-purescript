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
-- import Effect.Class.Console (log)
import Elmish (Transition, Dispatch, ReactElement, fork, (<?|))
-- import Elmish (Transition, Dispatch, ReactElement, forkMaybe, forkVoid, fork, (<?|))
import Elmish.Boot (defaultMain)
import Elmish.Foreign (readForeign)
import Elmish.HTML.Styled as H
import Elmish.React.DOM as R
import Foreign (Foreign)

-- ---------
-- UI
-- ---------
-- TODO add UI states
type State
  = { currentBoardIndex :: Int
    , board :: Grid
    , solutions :: Array Grid
    , currentSolutionIndex :: Int
    , isBoardPristine :: Boolean
    , isSolving :: Boolean
    , isSuccess :: Boolean
    , hasInitiallyLoaded :: Boolean
    }

data DecOrInc
  = Dec
  | Inc

data Message
  = ClickSolve
  | UpdateBoard Int Int Int
  | PreviousOrNextBoard DecOrInc
  | PreviousOrNextSolution DecOrInc
  | ClearUI
  | UpdateIsSolving Boolean
  | SolveSuccess (Array Grid)
  | SolveFailure
  | Noop

initCurrentBoardIndex :: Int
initCurrentBoardIndex = 0

defaultBoard :: Grid
defaultBoard = b1

init :: Transition Message State
init =
  pure
    { currentBoardIndex: initCurrentBoardIndex
    , board: boards !! initCurrentBoardIndex # fromMaybe defaultBoard
    , solutions: []
    , currentSolutionIndex: 0
    , isBoardPristine: true
    , isSolving: false
    , isSuccess: false
    , hasInitiallyLoaded: false
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

update state (PreviousOrNextSolution decOrInc) =
  let
    newSolutionIndex = case decOrInc of
      Dec -> mod (state.currentSolutionIndex - 1) (length state.solutions)
      Inc -> mod (state.currentSolutionIndex + 1) (length state.solutions)
  in
    pure
      state
        { currentSolutionIndex = newSolutionIndex
        , board = state.solutions !! newSolutionIndex # fromMaybe defaultBoard
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
        , board = boards !! newBoardIndex # fromMaybe state.board
        }

update state ClearUI = do
  pure
    state
      { isBoardPristine = true
      , isSolving = false
      , solutions = []
      , currentSolutionIndex = 0
      , isSuccess = false
      }

update state (UpdateIsSolving b) = do
  pure state { isSolving = b }

update state (SolveSuccess newSolutions) = do
  pure
    state
      { board = newSolutions # head >>> fromMaybe state.board
      , isBoardPristine = true
      , isSolving = false
      , solutions = newSolutions
      , currentSolutionIndex = 0
      , isSuccess = true
      }

update state SolveFailure = do
  pure
    state
      { isSolving = false
      , solutions = []
      , currentSolutionIndex = 0
      }

update state Noop = do
  pure state

update state ClickSolve =
  let
    newSolutions = solve state.board
  in
    do
      fork do
        pure (UpdateIsSolving true)
      fork do
        if (length newSolutions > 0) then
          pure (SolveSuccess (solve state.board))
        else
          pure SolveFailure
      pure state

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
        , H.div "below-board constrain-width"
            [ H.div_ "" { className: "left " <> (if state.isSuccess then "" else "is-hidden") } case length state.solutions of
                0 -> [ H.span "em" "no solutions" ]
                1 -> [ H.span "em" "1 solution" ]
                _ ->
                  [ H.a_ "arrow-left" { onClick: dispatch (PreviousOrNextSolution Dec) } "◀"
                  , H.a_ "arrow-right" { onClick: dispatch (PreviousOrNextSolution Inc) } "▶"
                  , H.span "em" ("solution " <> (state.currentSolutionIndex # (\x -> x + 1) >>> show) <> " of " <> (state.solutions # length >>> show))
                  ]
            ]
        ]
    , H.div "button-container"
        [ H.div_ ""
            { className:
                "button-indicator "
                  <> (if state.isSuccess then "is-success" else "")
                  <> (if state.isSolving then "is-solving" else "")
            }
            [ H.button_ "" { onClick: dispatch ClickSolve } "solve" ]
        ]
    ]

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }
