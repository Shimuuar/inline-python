{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module Py.Extra where

import Data.Proxy
import Language.Haskell.TH.Quote
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import GHC.OverloadedLabels


import Python.Inline
import Python.Inline.Eval
import Python.Inline.Literal
import Python.Inline.QQ



-- | Newtype that evaluates action before converting it to python value.
--
--   For example @pure 12 :: Py Int@ will be converted into 0-ary
--   function which will return 12. @EvalPy (pure 12) :: EvalPy Int@ will
--   first evaluate action and then convert it python 12.
newtype EvalPy a = EvalPy (Py a)

instance ToPy a => ToPy (EvalPy a) where
  basicToPy (EvalPy py) = basicToPy =<< py


----------------------------------------------------------------
-- Python Monad
----------------------------------------------------------------

class Monad m => MonadPy m where
  liftPy :: Py a -> m a
  usingGlobalLocal
    :: (forall global local. (Namespace global, Namespace local) => global -> local -> m a)
    -> m a

instance MonadPy Py where
  liftPy = id
  usingGlobalLocal f = f Main Temp


lpymain :: QuasiQuoter
lpymain = QuasiQuoter
  { quoteExp  = \txt -> [| usingGlobalLocal (\g _ -> liftPy $ exec g g $(quoteExp pycode txt)) |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

lpy :: QuasiQuoter
lpy = QuasiQuoter
  { quoteExp  = \txt -> [| usingGlobalLocal (\g l -> liftPy $ exec g l $(quoteExp pycode txt)) |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }


----------------------------------------------------------------
-- Keywords dict
----------------------------------------------------------------

-- | String labeled unit type which could be constructed using
--   overloaded labels notation
data Keyword (var :: Symbol) = Keyword
  deriving stock (Show, Eq)

instance v ~ v' => IsLabel v (Keyword v') where
  fromLabel = Keyword

-- | Set of keyword parameters
newtype Kwd = Kwd (PyObject -> Py ())

-- | Key-value pair
data KeyVal k v = k := v

-- | Type class for building keyword arguments
class ToKwd a where
  toKwd :: a -> Kwd


instance (key ~ Keyword key', KnownSymbol key', ToPy v) => ToKwd (KeyVal key v) where
  toKwd (_ := a) = Kwd $ \dct -> [py_| dct_hs[key_hs] = a_hs |]
    where key = symbolVal (Proxy @key')

instance ToKwd () where
  toKwd _ = mempty
instance (ToKwd a, ToKwd b) => ToKwd (a, b) where
  toKwd (a, b) = toKwd a <> toKwd b
instance (ToKwd a, ToKwd b, ToKwd c) => ToKwd (a, b, c) where
  toKwd (x, y, z) = toKwd x <> toKwd y <> toKwd z
instance (ToKwd a, ToKwd b, ToKwd c, ToKwd d) => ToKwd (a, b, c, d) where
  toKwd (w, x, y, z) = toKwd w <> toKwd x <> toKwd y <> toKwd z
instance (ToKwd a, ToKwd b, ToKwd c, ToKwd d, ToKwd e) => ToKwd (a, b, c, d, e) where
  toKwd (v, w, x, y, z) = toKwd v <> toKwd w <> toKwd x <> toKwd y <> toKwd z
instance (ToKwd a, ToKwd b, ToKwd c, ToKwd d, ToKwd e, ToKwd f) => ToKwd (a, b, c, d, e, f) where
  toKwd (u, v, w, x, y, z) = toKwd u <> toKwd v <> toKwd w <> toKwd x <> toKwd y <> toKwd z


instance Semigroup Kwd where
  Kwd f <> Kwd g = Kwd $ \p -> f p >> g p

instance Monoid Kwd where
  mempty = Kwd $ \_ -> pure ()

instance ToPy Kwd where
  basicToPy (Kwd fun) = do
    dct <- [pye| {} |]
    fun dct
    basicToPy dct
