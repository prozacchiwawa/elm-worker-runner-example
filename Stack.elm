module Stack exposing (..)

import Html exposing (..)
import Html.App exposing (program)
import String
import Lazy.List as LL exposing (LazyList)

type alias Stack a = List a

push : a -> Stack a -> Stack a
push tok stack = 
 (tok :: stack)

pop : Stack a -> (Maybe a, Stack a)
pop stack = 
 case stack of
   hd :: tl -> (Just hd, tl)
   _ -> (Nothing, [])

stackIterator : Stack a -> LazyList a
stackIterator stack =
 LL.iterate (\(mhd, tl) -> pop tl) (pop stack)
  |> LL.map fst
  |> LL.takeWhile (\a -> a /= Nothing)
  |> LL.flatMap 
    (\a -> 
      case a of
        Just x -> LL.singleton x
        Nothing -> LL.empty)
