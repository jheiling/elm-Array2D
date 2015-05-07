module Array2D where
{-| Module for working with 2-dimensional jagged arrays

# Definition
@docs Array2D

# Creating
@docs repeat, initialize

# Size
@docs size

# Get and Set
@docs get, getUnsafe, getWithDefault, set

# Higher order functions
@docs map, indexedMap, foldl, foldr

# Conversion
@docs fromListWithDefault, fromArrayWithDefault, toList, toArray
-}

import Array
import Array exposing (Array)
import Debug
import List
import Maybe


{-| Alias for Array (Array a)
-}
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


{-| Return the number of elements in the array.

    length (repeat 2 3 0) == 6
-}
size : Array2D a -> Int
size = Array.foldl (Array.length >> (+)) 0


{-| Return Just the element at the index or Nothing if the index is out of range.

    get 1 1 (initialize 2 3 (,)) == Just (1,1)
    get 3 1 (initialize 2 3 (,)) == Nothing
-}
get : Int -> Int -> Array2D a -> Maybe a
get i j xs =
  case Array.get i xs of
    Just ys -> Array.get j ys
    Nothing -> Nothing


{-| Return the element at the index or crash if the index is out of range.
-}
getUnsafe : Int -> Int -> Array2D a -> a
getUnsafe i j xs =
  case get i j xs of
    Just x -> x
    Nothing -> Debug.crash ("Index " ++ toString i ++ " " ++ toString j ++ " is out of range.")


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


{-| Apply a function on every element in the array.

    map ((*) 2) (initialize 2 3 (\i j -> i * 2 + j)) == Array.fromList [Array.fromList [0,2,4],Array.fromList [4,6,8]]
-}
map : (a -> b) -> Array2D a -> Array2D b
map f = Array.map (Array.map f)


{-| Apply a function on every element in the array with its index as first and second argument.

    indexedMap (,,) (initialize 2 3 (,)) == Array.fromList [Array.fromList [(0,0,(0,0)),(0,1,(0,1)),(0,2,(0,2))],Array.fromList [(1,0,(1,0)),(1,1,(1,1)),(1,2,(1,2))]]
-}
indexedMap : (Int -> Int -> a -> b) -> Array2D a -> Array2D b
indexedMap f = Array.indexedMap (f >> Array.indexedMap)


{-| Reduce the inner arrays from the left, then reduce the resulting array from the left.

    foldl (::) [] (initialize 2 3 (,)) == [(1,2),(1,1),(1,0),(0,2),(0,1),(0,0)]
-}
foldl : (a -> b -> b) -> b -> Array2D a -> b
foldl f = Array.foldl (flip (Array.foldl f))


{-| Reduce the inner arrays from the right, then reduce the resulting array from the right.

    foldr (::) [] (initialize 2 3 (,)) == [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
-}
foldr : (a -> b -> b) -> b -> Array2D a -> b
foldr f = Array.foldr (flip (Array.foldr f))


{-| Create a 2-dimensional array with the given dimensions, initialized with the elements of the given list.
If the list is too short the rest is initialized with a default element.
The order in which the elements are initialized is (0,0),(0,1) .. (0,m) .. (n,0),(n,1) .. (n,m).

    fromListWithDefault 2 3 -1 [0,1,2,3] == Array.fromList [Array.fromList [0,1,2],Array.fromList [3,-1,-1]]
-}
fromListWithDefault : Int -> Int -> a -> List a -> Array2D a
fromListWithDefault n m def = Array.fromList >> fromArrayWithDefault n m def


{-| Create a 2-dimensional array with the given dimensions, initialized with the elements of the given array.
If the array is too short the rest is initialized with a default element.
The order in which the elements are initialized is (0,0),(0,1) .. (0,m) .. (n,0),(n,1) .. (n,m).

    fromArrayWithDefault 2 3 -1 (Array.fromList [0,1,2,3]) == Array.fromList [Array.fromList [0,1,2],Array.fromList [3,-1,-1]]
-}
fromArrayWithDefault : Int -> Int -> a -> Array a -> Array2D a
fromArrayWithDefault n m def xs = initialize n m (\i j -> Maybe.withDefault def (Array.get (i * m + j) xs))


{-| Create a list from the given array.
The order of elements is (0,0),(0,1) .. (0,m) .. (n,0),(n,1) .. (n,m).

    toList (initialize 2 3 (,)) == [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
-}
toList : Array2D a -> List a
toList = Array.toList >> List.map Array.toList >> List.concat


{-| Create an array from the given 2-dimensional array.
The order of elements is (0,0),(0,1) .. (0,m) .. (n,0),(n,1) .. (n,m).

    toArray (initialize 2 3 (,)) == Array.fromList [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
-}
toArray : Array2D a -> Array a
toArray = toList >> Array.fromList
{- causes a bug: https://github.com/elm-lang/core/issues/205
toArray = Array.foldl (flip Array.append) Array.empty -}
