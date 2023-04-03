{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Auxiliary (module Auxiliary) where

import           Control.Applicative       (Applicative (liftA2))
import           Control.Monad.Error.Class (liftEither)
import           Control.Monad.Except      (MonadError)
import           Data.Either.Combinators   (maybeToRight)
import           Data.List                 (foldl')
import           Grammar.Abs
import           Prelude                   hiding ((>>), (>>=))

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

onM :: Monad m => (b -> b -> c) -> (a -> m b) -> a -> a -> m c
onM f g x y = liftA2 f (g x) (g y)

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 =
    foldl'
        ( \(as, bs, cs, ds) (a, b, c, d) ->
            (as ++ [a], bs ++ [b], cs ++ [c], ds ++ [d])
        )
        ([], [], [], [])

litType :: Lit -> Type
litType (LInt _)  = int
litType (LChar _) = char

int = TLit "Int"
char = TLit "Char"

tupSequence :: Monad m => (m a, b) -> m (a, b)
tupSequence (ma, b) = (,b) <$> ma

fst_ :: (a, b, c) -> a
snd_ :: (a, b, c) -> b
trd_ :: (a, b, c) -> c
snd_ (_, a, _) = a
fst_ (a, _, _) = a
trd_ (_, _, a) = a

partitionDefs :: [Def] -> ([Data], [Sig], [Bind])
partitionDefs defs = (datas, sigs, binds)
  where
    datas = [ d | DData d <- defs ]
    sigs  = [ s | DSig s  <- defs ]
    binds = [ b | DBind b <- defs ]
