module FixedPointData where

import Control.Monad
import Data.Monoid

data FixedPointData a = Fixed a | Data a

getData :: FixedPointData a -> a
getData (Fixed a) = a
getData (Data a) = a

instance Show a => Show (FixedPointData a) where
    show (Fixed a) = "Fixed " ++ show a
    show (Data a) = show a

instance Monoid a => Monoid (FixedPointData a) where
    mempty = Data mempty
    mappend _ (Fixed a) = Fixed a
    mappend (Fixed a) (Data b) = Fixed $ a <> b
    mappend (Data a) (Data b) = Data $ a <> b

instance Functor FixedPointData where
    fmap f (Fixed x) = Fixed $ f x
    fmap f (Data x) = Data $ f x

instance Monad FixedPointData where
    return x = Data x
    m >>= g = case m of
                Fixed x -> g x
                Data x -> g x
