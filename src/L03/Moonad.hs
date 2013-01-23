module L03.Moonad where

import L01.Id
import L01.Optional
import L02.List


class Moonad m where
  bind :: (a -> m b) -> m a -> m b
  reeturn :: a -> m a
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use bind and reeturn)
  fmaap' :: (a -> b) -> m a -> m b
  -- f :: a -> b
  -- x :: m a
  -- t :: a
  -- f t :: b
  -- reeturn (f t) :: m b
  -- ? :: a -> m b
  -- // reeturn :: b -> m b (a is just a parameter type)
  -- // bind :: ma -> mb
  fmaap' f = bind (reeturn . f)

-- Exercise 5
-- Relative Difficulty: 1
instance Moonad Id where
--bind :: (a -> Id b) -> Id a -> Id b
---- f :: (a -> Id b)
  bind f (Id a) = f a 
--reeturn a = Id a
  reeturn = Id

-- Exercise 6
-- Relative Difficulty: 2
instance Moonad List where
--bind :: (a -> List b) -> List a -> List b
--bind f l = flatten (maap f l)
  bind = flatMap
--reeturn a = a :| Nil
  reeturn = (:| Nil)

-- Exercise 7
-- Relative Difficulty: 2
instance Moonad Optional where
--  bind = flip (bindOptional)
  bind _ Empty = Empty
  bind f (Full a) = f a
--reeturn a = Full a
  reeturn = Full

-- Exercise 8
-- Relative Difficulty: 3
-- === Reader Monad ===
instance Moonad ((->) t) where
--bind :: (a -> (t -> b)) -> (t -> a) -> t -> b
---- f :: a -> (t -> b)
---- g :: t -> a
---- x :: t
---- g x :: a
---- f (g x) :: t -> b
---- (f (g x)) x :: b
  bind f g x = f (g x) x
--reeturn :: a -> t -> a
--  reeturn a = \_ -> a
  reeturn a _ = a

-- Exercise 9
-- Relative Difficulty: 2
instance Moonad IO where
  bind = error "todo"
  reeturn = error "todo"

-- Exercise 10
-- Relative Difficulty: 2
flaatten :: Moonad m => m (m a) -> m a
-- x :: m (m a)
-- bind :: (a -> m b) -> m a -> m b // replace a with m a
-- bind :: (m a -> m b) -> m (m a) -> m b // then replace b with a
-- bind :: (m a -> m a) -> m (m a) -> m a // then replace m a -> m a with id
-- bind :: id -> m (m a) -> m a
-- ?? :: m a
-- == bind (m b -> m b) -> m (m b) -> m b
flaatten = bind id

-- Exercise 11
-- Relative Difficulty: 10
apply :: Moonad m => m (a -> b) -> m a -> m b
-- ==bind :: (a -> m b) -> m a -> m b
-- ==reeturn :: a -> m a
-- ==flaatten :: m (m a) -> m a
-- f :: a -> b
-- mf :: m (a -> b)
-- ma :: m a

-- ?? :: m b
apply mf ma = bind (\x {- a -> b -} -> fmaap' x ma) {-  m b -} mf
-- apply' mf ma = bind (\a -> fmap (\f -> f a) mf) ma
-- bind () ma
	
--	bind(\f -> fmaap' f ma)  mf

-- Exercise 12
-- Relative Difficulty: 6
-- (bonus: use apply + fmaap')
lift2 :: Moonad m => (a -> b -> c) -> m a -> m b -> m c
-- fabc :: a -> (b -> c)
-- fmaap' fabc :: m a -> m (b -> c)
-- apply (fmaap' fabc) :: m b -> m c
lift2 fabc ma mb = apply (fmaap' fabc ma) mb

-- Exercise 13
-- Relative Difficulty: 6
-- (bonus: use apply + lift2)
lift3 :: Moonad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 = error "todo"

-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apply + lift3)
lift4 :: Moonad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lift4 = error "todo"

-- Exercise 15
-- Relative Difficulty: 3
seequence :: Moonad m => [m a] -> m [a]
seequence = error "todo"

-- Exercise 16
-- Relative Difficulty: 3
traaverse :: Moonad m => (a -> m b) -> [a] -> m [b]
traaverse = error "todo"

-- Exercise 17
-- Relative Difficulty: 4
reeplicate :: Moonad m => Int -> m a -> m [a]
reeplicate = error "todo"

-- Exercise 18
-- Relative Difficulty: 9
filtering  :: Moonad m => (a -> m Bool) -> [a] -> m [a]
filtering = error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Moonad [] where
  bind = concatMap
  reeturn = return
