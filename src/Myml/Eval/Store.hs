module Myml.Eval.Store
  ( Store(..)
  , WithMark(..)
  , clearStoreMark
  , setMark
  , unsetMark
  , isMarkedTrue
  , isMarkedFalse
  , removeMark
  , markStore
  , sweepStore
  , markSweepClear
  , emptyStore
  , allocate
  , allocate'
  , assign
  , lookupStore
  , Locations(..)
  )
where

import           Myml.Syntax
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

type Location = Integer

data Store a = Store
  { storeData    :: Map.Map Location a
  , storeMinFree :: Location
  }
  deriving (Eq, Show)

data WithMark a = WithMark Bool a
  deriving (Eq, Show)

isMarkedTrue :: WithMark a -> Bool
isMarkedTrue (WithMark m _) = m

isMarkedFalse :: WithMark a -> Bool
isMarkedFalse = not . isMarkedTrue

removeMark :: WithMark a -> a
removeMark (WithMark _ x) = x

setMark :: WithMark a -> WithMark a
setMark (WithMark _ x) = WithMark True x

unsetMark :: WithMark a -> WithMark a
unsetMark (WithMark _ x) = WithMark False x

clearStoreMark :: Store (WithMark a) -> Store (WithMark a)
clearStoreMark (Store d m) = Store (Map.map unsetMark d) m

markStore :: Locations a => [a] -> Store (WithMark a) -> Store (WithMark a)
markStore items store@(Store sData _) = if Map.null new
  then store
  else markStore' new store
 where
  ls  = foldl (\a b -> a `Set.union` locations b) Set.empty items
  new = Map.filter isMarkedFalse (sData `Map.restrictKeys` ls)

markStore'
  :: Locations a
  => Map.Map Location (WithMark a)
  -> Store (WithMark a)
  -> Store (WithMark a)
markStore' new (Store sData sMinFree) = markStore
  (map removeMark (Map.elems new))
  (Store newData sMinFree)
 where
  newData =
    Map.mapWithKey (\k x -> if k `Map.member` new then setMark x else x) sData

sweepStore :: Store (WithMark a) -> Store (WithMark a)
sweepStore (Store sData sMinFree) = Store hold sMinFree
  where (hold, _) = Map.partition isMarkedTrue sData

markSweepClear :: Locations a => [a] -> Store (WithMark a) -> Store (WithMark a)
markSweepClear items s = clearStoreMark $ sweepStore $ markStore items s

emptyStore :: Store (WithMark a)
emptyStore = Store Map.empty 0

allocate :: Store (WithMark a) -> a -> (Location, Store (WithMark a))
allocate (Store sData sMinFree) item = (sMinFree, Store newData newMinFree)
 where
  newData    = Map.insert sMinFree (WithMark False item) sData
  newMinFree = sMinFree + 1

allocate' :: Store (WithMark a) -> a -> Store (WithMark a)
allocate' s item = s' where (_, s') = allocate s item

assign :: Store (WithMark a) -> Location -> a -> Store (WithMark a)
assign (Store sData sMinFree) l item = if l `Map.member` sData
  then Store (Map.insert l (WithMark False item) sData) sMinFree
  else error "assign to unallocated location"

lookupStore :: Location -> Store (WithMark a) -> Maybe a
lookupStore l (Store sData _) = removeMark <$> Map.lookup l sData

class Locations a where
    locations :: a -> Set.Set Location

instance Locations Term where
  locations (TmAbs _  t   ) = locations t
  locations (TmApp t1 t2  ) = locations t1 `Set.union` locations t2
  locations (TmLet _ t1 t2) = locations t1 `Set.union` locations t2
  locations (TmLoc l      ) = Set.singleton l
  locations _               = Set.empty
