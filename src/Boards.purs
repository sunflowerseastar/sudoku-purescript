module Boards where

type Matrix a
  = Array (GridRow a)

type GridRow a
  = Array a

type Grid
  = Matrix Digit

type Digit
  = Int

b1 :: Grid
b1 =
  [ [ 0, 9, 0, 0, 0, 0, 0, 4, 0 ]
  , [ 0, 6, 4, 2, 0, 5, 7, 8, 0 ]
  , [ 0, 0, 8, 0, 4, 0, 9, 0, 0 ]
  , [ 4, 8, 3, 0, 6, 0, 2, 5, 1 ]
  , [ 9, 0, 0, 0, 0, 0, 0, 0, 7 ]
  , [ 2, 5, 7, 0, 8, 0, 4, 6, 9 ]
  , [ 0, 0, 9, 0, 1, 0, 5, 0, 0 ]
  , [ 0, 3, 1, 8, 0, 2, 6, 9, 0 ]
  , [ 0, 4, 0, 0, 0, 0, 0, 1, 0 ]
  ]

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

b3solution :: Grid
b3solution =
  [ [ 8, 2, 7, 1, 5, 4, 3, 9, 6 ]
  , [ 9, 6, 5, 3, 2, 7, 1, 4, 8 ]
  , [ 3, 4, 1, 6, 8, 9, 7, 5, 2 ]
  , [ 5, 9, 3, 4, 6, 8, 2, 7, 1 ]
  , [ 4, 7, 2, 5, 1, 3, 6, 8, 9 ]
  , [ 6, 1, 8, 9, 7, 2, 4, 3, 5 ]
  , [ 7, 8, 6, 2, 3, 5, 9, 1, 4 ]
  , [ 1, 5, 4, 7, 9, 6, 8, 2, 3 ]
  , [ 2, 3, 9, 8, 4, 1, 5, 6, 7 ]
  ]

b3a :: Grid
b3a =
  [ [ 8, 2, 7, 1, 5, 4, 3, 9, 6 ]
  , [ 9, 6, 5, 3, 2, 7, 1, 4, 8 ]
  , [ 3, 4, 1, 6, 8, 9, 7, 5, 2 ]
  , [ 5, 9, 3, 4, 6, 8, 2, 7, 1 ]
  , [ 4, 7, 2, 5, 1, 3, 6, 8, 9 ]
  , [ 6, 1, 8, 9, 7, 2, 4, 3, 5 ]
  , [ 7, 8, 6, 2, 3, 5, 9, 1, 4 ]
  , [ 1, 5, 4, 7, 9, 6, 8, 2, 3 ]
  , [ 2, 3, 9, 8, 4, 1, 5, 6, 0 ] -- 0 => 7
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

b3xNoSolutions :: Grid
b3xNoSolutions =
  [ [ 8, 2, 7, 1, 5, 4, 3, 9, 6 ]
  , [ 9, 6, 5, 3, 2, 7, 1, 4, 8 ]
  , [ 3, 4, 1, 6, 8, 9, 7, 5, 2 ]
  , [ 5, 9, 3, 4, 6, 8, 2, 7, 1 ]
  , [ 4, 7, 2, 5, 1, 3, 6, 8, 9 ]
  , [ 6, 1, 8, 9, 7, 2, 4, 3, 5 ]
  , [ 7, 8, 6, 2, 3, 5, 9, 1, 4 ]
  , [ 1, 5, 4, 7, 9, 6, 8, 2, 3 ]
  , [ 2, 3, 9, 8, 4, 1, 5, 6, 6 ]
  ]

b3x2NoSolutions :: Grid
b3x2NoSolutions =
  [ [ 8, 2, 7, 1, 5, 4, 3, 9, 6 ]
  , [ 9, 6, 5, 3, 2, 7, 1, 4, 8 ]
  , [ 3, 4, 1, 6, 8, 9, 7, 5, 2 ]
  , [ 5, 9, 3, 4, 6, 8, 2, 7, 1 ]
  , [ 4, 7, 2, 5, 1, 3, 6, 8, 9 ]
  , [ 6, 1, 8, 9, 7, 2, 4, 3, 5 ]
  , [ 7, 8, 6, 2, 3, 5, 9, 1, 4 ]
  , [ 1, 5, 4, 7, 9, 6, 8, 2, 3 ]
  , [ 2, 3, 9, 8, 4, 1, 0, 6, 6 ]
  ] -- 0, "6, 6"

-- X "6 6"
b5 :: Grid
b5 = [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]
