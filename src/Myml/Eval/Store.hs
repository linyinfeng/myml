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
  , Locations(..)
  )
where

import           Myml.Syntax
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

type Location = Integer

data Store a = Store {
    storeData :: Map.Map Location a,
    storeMinFree :: Location
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

infixl 6 `allocate`
allocate :: Store (WithMark a) -> a -> Store (WithMark a)
allocate (Store sData sMinFree) item = Store newData newMinFree
 where
  newData    = Map.insert sMinFree (WithMark False item) sData
  newMinFree = sMinFree + 1

class Locations a where
    locations :: a -> Set.Set Location

instance Locations Term where
  locations (TmAbs _  t   ) = locations t
  locations (TmApp t1 t2  ) = locations t1 `Set.union` locations t2
  locations (TmVar _      ) = Set.empty
  locations (TmLet _ t1 t2) = locations t1 `Set.union` locations t2
  locations (TmRcd m) =
    Map.foldl (\a b -> a `Set.union` locations b) Set.empty m
  locations (TmRcdExtend t1 _ t2) = locations t1 `Set.union` locations t2
  locations (TmRcdAccess t _    ) = locations t
  locations (TmMatch m) =
    Map.foldl (\a b -> a `Set.union` locations b) Set.empty m
  locations (TmMatchExtend t _ c) = locations t `Set.union` locations c
  locations (TmVariant _ t      ) = locations t
  locations (TmRef   t          ) = locations t
  locations (TmDeref t          ) = locations t
  locations (TmAssign t1 t2     ) = locations t1 `Set.union` locations t2
  locations (TmLoc l            ) = Set.singleton l
  locations TmUnit                = Set.empty
  locations TmTrue                = Set.empty
  locations TmFalse               = Set.empty
  locations (TmIf t1 t2 t3)       = Set.unions (map locations [t1, t2, t3])
  locations TmZero                = Set.empty
  locations (TmSucc t)            = locations t

instance Locations TermCase where
  locations (TmCase _ t) = locations t
