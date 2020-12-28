{-# LANGUAGE AllowAmbiguousTypes, DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
module TypeLevel.Elem where

type family Elem a as where
  Elem a '[]       = 'False
  Elem a (a ': as) = 'True
  Elem a (b ': as) = Elem a as

unit :: Elem a as ~ 'True => ()
unit = ()
