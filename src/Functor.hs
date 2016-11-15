{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Functor
  (
   -- * Semifunctors
    Semifunc (..)
   -- * Functors
  , Func (..)
  ) where

import Category

class (Semicat k1, Semicat k2) => Semifunc k1 k2 f where
  _fmap ::
    (Obj k1 a, Obj k1 b, Obj k2 (f a), Obj k2 (f b)) =>
    Hom k1 a b -> Hom k2 (f a) (f b)

class (Cat k1, Cat k2, Semifunc k1 k2 f) => Func k1 k2 f

--------------------------------------------------------------------------------

instance Functor f => Semifunc Hask Hask f where
  _fmap = fmap

instance Functor f => Semifunc (SubHask k) (SubHask k) f where
  _fmap = fmap

instance Functor f => Func Hask Hask f
instance Functor f => Func (SubHask k) (SubHask k) f

--------------------------------------------------------------------------------
