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
{-# LANGUAGE InstanceSigs #-}

module Subcategory
  (
  -- * Subsemicategories and subcategories
    Subsemicat (..)
  , Subcat (..)
  ) where

import GHC.Exts (Constraint)
import Data.Constraint
import Category
import Functor

class (Semicat k1, Semicat k2) => Subsemicat k1 k2 where
  subcat :: Obj k1 a :- Obj k2 a
  inj :: Hom k1 a b -> Hom k2 a b

class (Cat k1, Cat k2, Subsemicat k1 k2) => Subcat k1 k2
instance (Cat k1, Cat k2, Subsemicat k1 k2) => Subcat k1 k2

instance Semicat k => Subsemicat k k where
  subcat = refl
  inj = id

instance Subsemicat (SubHask k) Hask where
  subcat = Sub Dict
  inj = id

{-
instance (Subsemicat k1' k1, Semifunc k1 k2 f) =>
  Semifunc k1' k2 f where
  _fmap ::
    forall a b.
    (Obj k1' a, Obj k1' b, Obj k2 (f a), Obj k2 (f b)) =>
    Hom k1' a b -> Hom k2 (f a) (f b)
  _fmap = (inj @k1' @k1) .> _fmap'
    where
      _fmap' = (_fmap @k1 @ k2) \\ (subcat @k1' @k1 @a) *** (subcat @k1' @k1 @b)
-}
