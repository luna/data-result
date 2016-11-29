module Data.Result where

import Prologue hiding (Assert)

import Control.Monad.Poly
import Control.Applicative.Poly
import Data.Maybe               (fromJust)

-----------------------------
-- === Result wrappers === --
-----------------------------

data Ok      a = Ok a  deriving (Show, Functor, Foldable, Traversable)
data Error e a = Error deriving (Show)


-- Utils

fromOk :: Ok a -> a
fromOk (Ok a) = a

class    MaybeResult m         where maybeResult :: m a -> Maybe a
instance MaybeResult Ok        where maybeResult = Just ∘ unwrap'
instance MaybeResult (Error e) where maybeResult = const Nothing

unsafeFromResult :: MaybeResult m => m a -> a
unsafeFromResult = fromJust ∘ maybeResult


class    IfOk m         where ifOk :: m a -> (a -> b) -> b -> b
instance IfOk Ok        where ifOk (Ok a) f _ = f a
instance IfOk (Error e) where ifOk _ _ e      = e

-- Basic instances

instance Applicative Ok where pure = Ok
                              Ok f <*> Ok a = Ok $ f a

instance Monad       Ok where return = Ok
                              Ok a >>= f = f a


instance Functor     (Error e) where fmap f e = Error
instance Applicative (Error e) where pure _   = Error
                                     _ <*> _  = Error

instance Monad       (Error e) where return _ = Error
                                     _ >>= f  = Error


-- Wrappers
--instance      Coated    Ok
type instance Unlayered (Ok a) = a
instance      Layered   (Ok a)
instance      Rewrapped (Ok a) (Ok a')
instance      Wrapped   (Ok a) where
	type      Unwrapped (Ok a) = a
	_Wrapped' = iso (\(Ok a) -> a) Ok

-- Poly instances

type instance PolyBind Ok        Ok         = Ok
type instance PolyBind (Error e) a          = Error e
type instance PolyBind Ok        (Error e)  = Error e

instance {-# OVERLAPPABLE #-} PolyApplicative (Error e) a        where _ <<*>> _ = Error
instance {-# OVERLAPPABLE #-} PolyApplicative (Error e)(Error e) where _ <<*>> _ = Error
instance {-# OVERLAPPABLE #-} PolyApplicative Ok       (Error e) where _ <<*>> _ = Error

instance PolyMonad (Error e) a         where _    >>>= _ = Error
instance PolyMonad Ok        (Error e) where Ok a >>>= f = f a


-------------------------------
-- === Result constrains === --
-------------------------------

class CompilationError a

class                          Check a (ok :: Bool)
instance                       Check a 'True
instance CompilationError a => Check a 'False

type family Asserted e check where Asserted e 'True  = Ok
                                   Asserted e 'False = Error e

class    Assert e (ok :: Bool) where assert :: Proxy e -> Proxy ok -> a -> Asserted e ok a
instance Assert e 'True        where assert _ _   = Ok
instance Assert e 'False       where assert _ _ _ = Error
