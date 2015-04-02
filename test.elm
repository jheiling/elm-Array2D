import Array2D (..)
import Array
import List
import Graphics.Element (..)
import Text (..)

main : Element
main =
  [ ("repeat",(repeat 2 3 0 == Array.fromList [Array.fromList [0,0,0],Array.fromList [0,0,0]]))
  , ("initialize",(initialize 2 3 (,) == Array.fromList [Array.fromList [(0,0),(0,1),(0,2)],Array.fromList [(1,0),(1,1),(1,2)]]))
  , ("length1",(length1 (repeat 2 3 0) == 2))
  , ("length",(length (repeat 2 3 0) == 6))
  , ("get 1",(get 1 1 (initialize 2 3 (,)) == Just (1,1)))
  , ("get 2",(get 3 1 (initialize 2 3 (,)) == Nothing))
  , ("getWithDefault 1",(getWithDefault 1 1 (-1,-1) (initialize 2 3 (,)) == (1,1)))
  , ("getWithDefault 1",(getWithDefault 3 1 (-1,-1) (initialize 2 3 (,)) == (-1,-1)))
  , ("set 1",(set 1 1 5 (repeat 2 3 0) == Array.fromList [Array.fromList [0,0,0],Array.fromList [0,5,0]]))
  , ("set 2",(set 3 1 5 (repeat 2 3 0) == Array.fromList [Array.fromList [0,0,0],Array.fromList [0,0,0]]))
  , ("foldl",(foldl (::) [] (initialize 2 3 (,)) == [(1,2),(1,1),(1,0),(0,2),(0,1),(0,0)]))
  , ("fromArrayWithDefault",(fromArrayWithDefault 2 3 -1 (Array.fromList [0,1,2,3]) == Array.fromList [Array.fromList [0,1,2],Array.fromList [3,-1,-1]]))
  , ("fromListWithDefault",(fromListWithDefault 2 3 -1 [0,1,2,3] == Array.fromList [Array.fromList [0,1,2],Array.fromList [3,-1,-1]]))
  ]
  |> List.map (\(t,r) -> flow right [t ++ ": " |> fromString |> leftAligned,asText r])
  |> flow down
