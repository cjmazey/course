{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))
  deriving Show

-- Implement a Functor instance for Compose
instance (Functor f,Functor g) => Functor (Compose f g) where
  (<$>) :: (a -> b) -> Compose f g a -> Compose f g b
  h <$> (Compose x) =
    Compose $
    ((<$>) . (<$>)) h x

-- Implement the (<*>) function for an Apply instance for Compose
instance (Apply f,Apply g) => Apply (Compose f g) where
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose h) <*> (Compose x) = Compose $ (<*>) <$> h <*> x

-- Implement the pure function for an Applicative instance for Compose
instance (Applicative f,Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x =
    Compose $
    (pure . pure) x

-- Implement the (=<<) function for a Bind instance for Compose
instance (Bind f, Bind g) => Bind (Compose f g) where
  (=<<) :: (a -> Compose f g b) -> Compose f g a -> Compose f g b
  k =<< (Compose m) =
    error "todo"
