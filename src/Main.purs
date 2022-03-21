module Main where

import Prelude
import Data.Array (all, concat, cons, drop, filter, length, mapWithIndex, modifyAt, take, uncons, updateAt, zipWith, (:), (..))
import Data.Int (fromString)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Effect (Effect)
import Effect.Class.Console (log)
import Elmish (Transition, Dispatch, ReactElement, forkVoid, (<?|))
import Elmish.Boot (defaultMain)
import Elmish.Foreign (readForeign)
import Elmish.HTML.Styled as H
import Elmish.React.DOM as R
import Foreign (Foreign)

-- ---------
-- solver
-- ---------

type Matrix a
  = Array (GridRow a)

type GridRow a
  = Array a

type Grid
  = Matrix Digit

type Digit
  = Int

b2 :: Grid
b2 =
  [ [ 1, 2, 4, 3 ]
  , [ 3, 4, 2, 1 ]
  , [ 2, 1, 3, 4 ]
  , [ 4, 0, 1, 2 ]
  ]

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

b5 :: Grid
b5 = [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]

boxsize :: Int
boxsize = 2

-- boxsize = 3
digits :: Array Digit
digits = 1 .. (boxsize * boxsize)

blank :: Digit -> Boolean
blank = (==) 0

choices :: Grid -> Matrix (Array Digit)
choices = map (map choice)

-- choice d = if blank d then digits else [d]
choice :: Int -> Array Int
choice d = if blank d then digits else [ d ]

bx :: Array (Array Int)
bx = [ [ 1, 2 ], [ 3, 4 ] ]

bx2 :: Array (Array Int)
bx2 = [ [ 1, 0 ], [ 3, 4 ] ]

cp :: forall a. Array (Array a) -> Array (Array a)
cp matrix = case uncons matrix of
  Just { head: xs, tail: xss } -> do
    x <- xs
    ys <- cp xss
    pure (cons x ys)
  Nothing -> [ [] ]

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  pure [ i, j ]

expand :: Matrix (Array Digit) -> Array Grid
expand = cp <<< map cp

completions :: Grid -> Array Grid
completions = expand <<< choices

nodups :: forall a. Eq a => Array a -> Boolean
nodups arr = case uncons arr of
  Just { head: x, tail: xs } -> all (_ /= x) xs
  Nothing -> true

rows :: forall a. Matrix a -> Matrix a
rows = identity

cols :: forall a. Matrix a -> Matrix a
cols arr = case uncons arr of
  Just { head: xs, tail } -> case length tail > 0 of
    true -> zipWith (:) xs (cols tail)
    false -> do
      x <- xs
      pure [ x ]
  Nothing -> [ [] ]

group :: forall a. Array a -> Array (Array a)
group [] = []

group xs = take boxsize xs : group (drop boxsize xs)

ungroup :: forall a. Array (Array a) -> Array a
ungroup = concat

boxs :: forall a. Matrix a -> Matrix a
boxs = map ungroup <<< ungroup <<< map cols <<< group <<< map group

valid :: Grid -> Boolean
valid g =
  all nodups (rows g)
    && all nodups (cols g)
    && all nodups (boxs g)

solve :: Grid -> Array Grid
solve = filter valid <<< expand <<< choices

-- ---------
-- UI
-- ---------

type State
  = { board :: Grid }

data Message
  = ButtonClicked
  | UpdateBoard Int Int Int

init :: Transition Message State
init = pure { board: b3b }

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
  forkVoid $ log ("Button clicked :: " <> (show $ solve state.board))
  pure state

eventTargetValue :: Foreign -> Maybe String
eventTargetValue f =
  (readForeign f :: _ { target :: { value :: String } })
    <#> _.target.value

squares :: Dispatch Message -> Int -> Int -> Int -> ReactElement
squares dispatch y x s =
  H.div_ "square" { style: H.css { gridArea: (show (y + 1)) <> " / " <> (show (x + 1)) <> " / auto / auto" } }
    ( H.input_ ""
        { type: "text"
        , value: if s == 0 then "" else show s
        , onChange: dispatch <?| \f -> UpdateBoard x y <$> (eventTargetValue f >>= fromString)
        }
    )

squares2 :: Dispatch Message -> Int -> Array Int -> ReactElement
squares2 dispatch y ss = R.fragment $ mapWithIndex (squares dispatch y) ss

view :: State -> Dispatch Message -> ReactElement
view state dispatch =
  H.div "main"
    [ H.div "board-container"
        [ H.div "board constrain-width" $ state.board # mapWithIndex (squares2 dispatch)
        ]
    , H.div "button-container"
        [ H.div "button-indicator"
            [ H.button_ "" { onClick: dispatch ButtonClicked } "solve" ]
        ]
    ]

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }
