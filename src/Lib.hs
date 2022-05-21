{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib (someFunc) where

--(MonadReader, ask, local)

import Control.Applicative
--import Control.Lens.TH (makeClassy)
import Control.Monad (join)
--import Control.Monad.Free (Free)
import Control.Monad.Reader
import qualified Data.Foldable as Fold
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (filter, replicate)
import Data.Text (Text)
import Data.Vector (fromList, length, (!))
import Lens.Micro (Lens, Lens', lens, over, set, _2)
import Lens.Micro.Extras (view)
import System.Random (Random, StdGen (..), mkStdGen, random, randomR, split)
import Prelude hiding (length)

newtype Prg = Prg
  { _stdGen :: StdGen
  }

defaultPrg = Prg $ mkStdGen 314156

class HasStdGen x where
  stdGen :: Lens' x StdGen

instance HasStdGen Prg where
  stdGen = lens _stdGen (\s x -> s {_stdGen = x})

instance HasStdGen StdGen where
  stdGen = id

class HasPrg x where
  prg :: Lens' x Prg

instance HasPrg Prg where
  prg = id

{- class HasIntSeed x where

  intSeed :: Lens' x Int

instance HasIntSeed StdGen where
  intSeed = lens unStdGen _ -}

class (MonadReader r m, HasPrg r) => Fake r m --where
--generate :: (MonadIO io) => m a -> io a
--generate :: (MonadIO io) => Prg -> m (io a) -> io a

instance (MonadReader r m, HasPrg r) => Fake r m

--instance (HasPrg r) => Fake r (Reader)
type VanillaReaderT r m a = (HasStdGen r) => ReaderT r m a

newtype FakerT r m a = FakerT
  {_unFake :: r -> m a}

unFake :: Lens (FakerT r m s) (FakerT r m a) (ReaderT r m s) (ReaderT r m a)
unFake = lens (ReaderT . _unFake) $ \f r -> f {_unFake = unReader r}
  where
    unReader (ReaderT x) = x

runFaker :: Prg -> FakerT Prg Identity a -> a
runFaker p (FakerT f) = runIdentity $ f p

instance (Functor m) => Functor (FakerT r m) where
  fmap f (FakerT g) = FakerT $ fmap f . g

instance (Monad m, HasPrg r) => Monad (FakerT r m) where
  return = FakerT . const . return
  m >>= k = joinFaker $ fmap k $ m

{-
m >>= k = FakerT $ \r -> do
  a <- _unFake m r
  _unFake (k a) r-}

instance (Monad m, HasPrg r) => Applicative (FakerT r m) where
  pure = FakerT . const . pure
  f <*> a = f >>= (\xf -> a >>= (\xa -> return (xf xa)))

instance (Monad m, HasPrg r) => MonadReader r (FakerT r m) where
  --local r = over unFake (local r)
  reader = FakerT . fmap return
  ask = FakerT $ return . id

-- | a private fn just used to implement Monad
joinFaker :: (Monad m, HasPrg r) => FakerT r m (FakerT r m a) -> FakerT r m a
joinFaker (FakerT f) = FakerT g
  where
    g r = join $ fmap (\f -> f prgA) $ (fmap (fmap _unFake) f) prgB
      where
        (genA, genB) = split $ view (prg . stdGen) r
        (prgA, prgB) = (set (prg . stdGen) genA r, set (prg . stdGen) genB r)

--prg' :: Prg <- asks (view prg)
--let (genA, genB) = split $ view stdGen prg'
--newReader <- ReaderT $ fmap _unFake . f
--local (set (prg . stdGen) genA) $ local (set (prg . stdGen) genB) $ newReader

-- | Pick an `a`, any `a`.
pickAny :: (Random a, Fake r m) => m a
pickAny = do
  gen <- asks (view $ prg . stdGen)
  return $ fst $ random gen

-- | Pick an item, usually a number, between the upper and lower bound (inclusive)
pickBetween :: (Random a, Fake r m) => (a, a) -> m a
pickBetween range = do
  gen <- asks (view $ prg . stdGen)
  return $ fst $ randomR range gen

-- | Generates a list of `n` fakes
listOf :: (Fake r m) => Int -> m a -> m [a]
listOf = replicateM

-- | Picks an elements from the collections
pickElement :: (Fake r m, Foldable t) => t a -> m a
pickElement xs =
  if len == 0
    then error "Cannot pickElement from empty set"
    else fmap (items !) $ pickBetween (0, len - 1)
  where
    items = fromList $ Fold.toList xs
    len = length items

-- | Like `pickElement`, only more general. Each element of the given collection is actually a fake generator
pickFake :: (Fake r m, Foldable t) => t (m a) -> m a
pickFake = join . pickElement

-- | Like `pickElement`, only you can change the weights. Space taken is equal to the sum of the weights.
-- * @handedness = pickWeightedElement [(11, "left handed"), (89, "right handed")]@
pickWeightedElement :: (Fake r m, Foldable t, Functor t) => t (Int, a) -> m a
pickWeightedElement = pickWeightedFake . fmap (over _2 pure)

-- | Like `pickWeightedElement`, only more general. See docs for `pickFake`
pickWeightedFake :: (Fake r m, Foldable t) => t (Int, m a) -> m a
pickWeightedFake = pickFake . join . fmap (\(i, x) -> replicate i x) . Fold.toList

probability :: (Fake r m) => Float -> m Bool
probability p = fmap (< p) $ pickAny

--growWhile :: (Fake r m, Functor f) => (a -> m Bool) -> f a -> m (Free f a)
--growWhile pred things = undefined

someFunc :: IO ()
someFunc = do
  let oneToTen :: FakerT Prg Identity Int = pickBetween (1, 10)
  let person :: FakerT Prg Identity Text = pickElement ["Jane", "Jack", "Jerry", "Jessica"]
  let people :: FakerT Prg Identity [Text] = listOf 10 person
  let seventyFivePercent = fmap (length . fromList . filter id) $ listOf 10000 $ probability 0.75
  print $ runFaker defaultPrg seventyFivePercent
  print $ runFaker defaultPrg people
