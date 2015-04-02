module Array2D where

import Array
import Array (Array)

type alias Array2D a = Array (Array a)

repeat : Int -> Int -> a -> Array2D a
repeat l1 l2 = Array.repeat l2 >> Array.repeat l1

initialize : Int -> Int -> (Int -> Int -> a) -> Array2D a
initialize l1 l2 f = Array.initialize l1 (\i1 -> Array.initialize l2 (f i1))

length1 : Array2D a -> Int
length1 = Array.length

length : Array2D a -> Int
length = Array.foldl (\a c -> c + Array.length a) 0

get : Int -> Int -> Array2D a -> Maybe a
get i1 i2 xs =
  case Array.get i1 xs of
    Just ys -> Array.get i2 ys
    Nothing -> Nothing

getOr : Int -> Int -> a -> Array2D a -> a
getOr i1 i2 def xs =
  case get i1 i2 xs of
    Just x -> x
    Nothing -> def

set : Int -> Int -> a -> Array2D a -> Array2D a
set i1 i2 x xs =
  case Array.get i1 xs of
    Just ys -> Array.set i1 (Array.set i2 x ys) xs
    Nothing -> xs

foldl : (a -> b -> b) -> b -> Array2D a -> b
foldl f = Array.foldl (flip (Array.foldl f))

{- doesn't work, probably a bug in Array.append: https://github.com/elm-lang/core/issues/205
toArray : Array2D a -> Array a
toArray = Array.foldl (flip Array.append) Array.empty

toList : Array2D a -> List a
toList = toArray >> Array.toList
-}

fromArray : Int -> Int -> a -> Array a -> Array2D a
fromArray l1 l2 def xs =
  initialize l1 l2 (\i1 i2 ->
    case Array.get (i1 * l2 + i2) xs of
      Just a -> a
      Nothing -> def)

fromList : Int -> Int -> a -> List a -> Array2D a
fromList l1 l2 def = Array.fromList >> fromArray l1 l2 def
