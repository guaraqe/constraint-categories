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

module Category.Monoidal where

import GHC.Exts (Constraint)
import Data.Constraint

--------------------------------------------------------------------------------

class PremonoidalObj obj prod where
  premonoidalObj :: (obj a, obj b) :- obj (prod a b)

class MonoidalObj obj id where
  monoidalObj :: Dict (obj id)
