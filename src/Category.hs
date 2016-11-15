{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Category
  (
  -- * Objects and Morphisms
    Hom (..)
  , Obj (..)
  -- * Semicategories
  , SemicatHom (..)
  , Semicat (..)
  -- * Categories
  , CatHom (..)
  , Cat (..)
  -- * Hask
  , All (..)
  , Hask
  -- * Constrained subcategories of Hask
  , SubHask
  -- * Opposite category
  , Flip (..)
  , Op (..)
  ) where

import GHC.Exts (Constraint)
import Data.Constraint

class All a
instance All a

--------------------------------------------------------------------------------

type family Hom k :: * -> * -> *
type family Obj k :: * -> Constraint

--------------------------------------------------------------------------------

class SemicatHom obj hom where
  (.>) ::
    (obj a, obj b, obj c) =>
    hom a b -> hom b c -> hom a c
  (<.) ::
    (obj a, obj b, obj c) =>
    hom b c -> hom a b -> hom a c

class SemicatHom obj hom => CatHom obj hom where
  _id :: obj a => hom a a

--------------------------------------------------------------------------------

class SemicatHom (Obj k) (Hom k) => Semicat k
instance SemicatHom (Obj k) (Hom k) => Semicat k

class CatHom (Obj k) (Hom k) => Cat k
instance CatHom (Obj k) (Hom k) => Cat k

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Hask
type instance Hom Hask = (->)
type instance Obj Hask = All

instance SemicatHom obj (->) where
  (.>) = flip (.)
  (<.) = (.)

instance CatHom obj (->) where
  _id = id

--------------------------------------------------------------------------------

data SubHask (k :: * -> Constraint)
type instance Hom (SubHask k) = (->)
type instance Obj (SubHask k) = k

--------------------------------------------------------------------------------

data Flip q a b = Flip { unFlip :: q b a }
data Op q
type instance Hom (Op q) = Flip (Hom q)
type instance Obj (Op q) = Obj q

instance SemicatHom obj hom => SemicatHom obj (Flip hom) where
  f .> g = let comp = (.>) @obj in
    Flip (unFlip g `comp` unFlip f)
  f <. g = let comp = (<.) @obj in
    Flip (unFlip g `comp` unFlip f)

instance CatHom obj hom => CatHom obj (Flip hom) where
  _id = Flip (_id @obj)

--------------------------------------------------------------------------------
