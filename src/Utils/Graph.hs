module Utils.Graph
    ( Graph
    , fromList
    , roots
    , removeRoot
    )
    where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe



-- GRAPH REPRESENTATION


data Graph key value =
  Graph
    { _nodes :: Map.Map key [key]
    , _blockedBy :: Map.Map key (value, Int)
    }


type Node key value =
    (key, value, [key])


(!) :: (Ord k) => Map.Map k v -> k -> v
(!) dict key =
  case Map.lookup key dict of
    Just value ->
        value

    Nothing ->
        error "problem demanding node from Graph"



-- BUILD A GRAPH


fromList :: (Ord k) => [Node k v] -> Graph k v
fromList nodes =
  List.foldl' insertNode (Graph Map.empty Map.empty) nodes


insertNode :: (Ord k) => Graph k v -> Node k v -> Graph k v
insertNode (Graph oldNodes oldBlockedBy) (key, value, dependencies) =
  Graph
    (List.foldl' (\nodes dep -> Map.insertWith (++) dep [key] nodes) oldNodes dependencies)
    (Map.insertWith noDups key (value, length dependencies) oldBlockedBy)


noDups :: a -> a -> a
noDups _ _ =
  error "duplicate node keys in Graph.fromList"



-- GATHER ROOTS
--
-- best if this is only called once per graph


roots :: Graph k v -> [(k,v)]
roots (Graph _ blockedBy) =
  let
    keepRoots (key, (value, count)) =
      if count == 0 then Just (key,value) else Nothing
  in
    Maybe.mapMaybe keepRoots (Map.toList blockedBy)



-- REMOVE ROOTS FROM A GRAPH

removeRoot :: (Ord k) => k -> Graph k v -> (Graph k v, [(k,v)])
removeRoot key (Graph oldNodes oldBlockedBy) =
  let
    blockedBy =
      Map.update checkedRemoval key oldBlockedBy

    freedNodes =
      oldNodes ! key

    (newBlockedBy, unblocked) =
      List.foldl' updateBlockedCounts (blockedBy, []) freedNodes
  in
    ( Graph (Map.delete key oldNodes) newBlockedBy
    , unblocked
    )


checkedRemoval :: (v, Int) -> Maybe (v, Int)
checkedRemoval (_, count) =
  if count == 0 then
      Nothing

  else
      error "cannot take arbitrary nodes out of the graph, only roots"


updateBlockedCounts :: (Ord k) => (Map.Map k (v, Int), [(k,v)]) -> k -> (Map.Map k (v, Int), [(k,v)])
updateBlockedCounts (blockedBy, unblocked) key =
  let
    (value, count) =
      blockedBy ! key
  in
    if count == 1 then
        ( Map.delete key blockedBy
        , (key, value) : unblocked
        )

    else
        ( Map.insert key (value, count - 1) blockedBy
        , unblocked
        )

