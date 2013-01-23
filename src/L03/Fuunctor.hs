module L03.Fuunctor where

import L01.Id
import L01.Optional
import L01.Validation
import L02.List

class Fuunctor f where
  fmaap :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fuunctor Id where
--fmaap :: (a -> b) -> Id a -> Id b
--f :: (a -> b)
  fmaap f (Id x) = Id (f x)

-- Exercise 2
-- Relative Difficulty: 2
instance Fuunctor List where
--  fmaap _ Nil = Nil
--  fmaap f (h :| t) = (f h) :| fmaap f t
  fmaap = maap

-- Exercise 3
-- Relative Difficulty: 2
instance Fuunctor Optional where
  fmaap = mapOptional

-- Exercise 4
-- Relative Difficulty: 3
instance Fuunctor ((->) t) where
-- fmaap :: (a -> b) -> f a -> f b
-- fmaap :: (a -> b) -> ((->) t a) -> ((->) t b)
-- fmaap :: (a -> b) -> (t -> a) -> (t -> b)
  fmaap = (.)

-- Exercise 4
-- Relative Difficulty: 2
instance Fuunctor IO where
  fmaap =
    error "todo"
	
-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fuunctor [] where
  fmaap = fmap
