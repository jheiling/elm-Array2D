module Array2D where
{-| Module for working with 2-dimensional jagged arrays

# Creating
@docs repeat, initialize

# Size
@docs length1, length

# Get and Set
@docs get, getWithDefault, set

# Higher order functions
@docs foldl

# Conversion
@docs fromArrayWithDefault, fromListWithDefault

-}

import Array
import Array (Array)
import Maybe

type alias Array2D a = Array (Array a)

{-| Create a 2-dimensional array with the given dimensions, filled with a default element.

    repeat 2 3 0 == Array.fromList [Array.fromList [0,0,0],Array.fromList [0,0,0]]
-}
repeat : Int -> Int -> a -> Array2D a
repeat n m = Array.repeat m >> Array.repeat n

{-| Create a 2-dimensional array with the given dimensions, with the element at index i j initialized with (f i j).

    initialize 2 3 (,) == Array.fromList [Array.fromList [(0,0),(0,1),(0,2)],Array.fromList [(1,0),(1,1),(1,2)]]
-}
initialize : Int -> Int -> (Int -> Int -> a) -> Array2D a
initialize n m f = Array.initialize n (f >> Array.initialize m)

{-| Return the number of elements in the outer array.

    length1 (repeat 2 3 0) == 2
-}
length1 : Array2D a -> Int
length1 = Array.length

{-| Return the number of elements in the array.

    length (repeat 2 3 0) == 6
-}
length : Array2D a -> Int
length = Array.foldl (Array.length >> (+)) 0

{-| Return Just the element at the index or Nothing if the index is out of range.

    get 1 1 (initialize 2 3 (,)) == Just (1,1)
    get 3 1 (initialize 2 3 (,)) == Nothing
-}
get : Int -> Int -> Array2D a -> Maybe a
get i j xs =
  case Array.get i xs of
    Just ys -> Array.get j ys
    Nothing -> Nothing

{-| Return the element at the index or a default value if the index is out of range.

    getWithDefault 1 1 (-1,-1) (initialize 2 3 (,)) == (1,1)
    getWithDefault 3 1 (-1,-1) (initialize 2 3 (,)) == (-1,-1)
-}
getWithDefault : Int -> Int -> a -> Array2D a -> a
getWithDefault i j def = get i j >> Maybe.withDefault def

{-| Set the element at the index and return an updated array. If the index is out of range, the array is unaltered.

    set 1 1 5 (repeat 2 3 0) == Array.fromList [Array.fromList [0,0,0],Array.fromList [0,5,0]]
    set 3 1 5 (repeat 2 3 0) == Array.fromList [Array.fromList [0,0,0],Array.fromList [0,0,0]]
-}
set : Int -> Int -> a -> Array2D a -> Array2D a
set i j x xs =
  case Array.get i xs of
    Just ys -> Array.set i (Array.set j x ys) xs
    Nothing -> xs

{-| Reduce the inner arrays from the left, then reduce the resulting array from the left.

    foldl (::) [] (initialize 2 3 (,)) == [(1,2),(1,1),(1,0),(0,2),(0,1),(0,0)]
-}
foldl : (a -> b -> b) -> b -> Array2D a -> b
foldl f = Array.foldl (flip (Array.foldl f))

{- doesn't work, probably a bug in Array.append: https://github.com/elm-lang/core/issues/205
{-|
-}
toArray : Array2D a -> Array a
toArray = Array.foldl (flip Array.append) Array.empty

{-|
-}
toList : Array2D a -> List a
toList = toArray >> Array.toList
-}

{-| Create a 2-dimensional array with the given dimensions, initialized with the elements of the given array.
If the array is too short the rest is initialized with a default element.
The order in which the elements are initialized is (0, 0), (0, 1) .. (0, m) .. (n, 0), (n, 1) .. (n, m).

    fromArrayWithDefault 2 3 -1 (Array.fromList [0,1,2,3]) == Array.fromList [Array.fromList [0,1,2],Array.fromList [3,-1,-1]]
-}
fromArrayWithDefault : Int -> Int -> a -> Array a -> Array2D a
fromArrayWithDefault n m def xs = initialize n m (\i j -> Maybe.withDefault def (Array.get (i * m + j) xs))

{-| Create a 2-dimensional array with the given dimensions, initialized with the elements of the given list.
If the list is too short the rest is initialized with a default element.
The order in which the elements are initialized is (0, 0), (0, 1) .. (0, m) .. (n, 0), (n, 1) .. (n, m).

    fromListWithDefault 2 3 -1 [0,1,2,3] == Array.fromList [Array.fromList [0,1,2],Array.fromList [3,-1,-1]]
-}
fromListWithDefault : Int -> Int -> a -> List a -> Array2D a
fromListWithDefault n m def = Array.fromList >> fromArrayWithDefault n m def
