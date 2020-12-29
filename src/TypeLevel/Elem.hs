{-# LANGUAGE AllowAmbiguousTypes, DataKinds, TypeFamilies#-}
module TypeLevel.Elem where

import GHC.TypeLits

type family Elem (a :: Nat) (as :: [Nat]) :: Bool

unit :: Elem a as ~ 'True => ()
unit = ()
