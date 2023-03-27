{-# LANGUAGE LambdaCase #-}

module Auxiliary (module Auxiliary) where

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (MonadError)
import Data.Either.Combinators (maybeToRight)
import TypeChecker.TypeCheckerIr (Type (TFun))
import Prelude hiding ((>>), (>>=))

(>>) a b = a ++ " " ++ b
(>>=) a f = f a

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

maybeToRightM :: MonadError l m => l -> Maybe r -> m r
maybeToRightM err = liftEither . maybeToRight err

mapAccumM :: Monad m => (s -> a -> m (s, b)) -> s -> [a] -> m (s, [b])
mapAccumM f = go
  where
    go acc = \case
        [] -> pure (acc, [])
        x : xs -> do
            (acc', x') <- f acc x
            (acc'', xs') <- go acc' xs
            pure (acc'', x' : xs')
