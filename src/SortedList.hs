{-# LANGUAGE TypeFamilies #-}

module SortedList (SortedList) where

import Data.List (sort)
import GHC.Exts (IsList (..))

newtype SortedList a = SortedList {unSortedList :: [a]}
  deriving (Foldable)

instance (Ord a) => IsList (SortedList a) where
  type Item (SortedList a) = a
  fromList = SortedList . sort
  toList = unSortedList
