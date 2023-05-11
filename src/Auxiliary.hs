{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Auxiliary (module Auxiliary) where

import           Control.Monad.Error.Class (liftEither)
import           Control.Monad.Except      (MonadError, liftM2)
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

onMM :: Monad m => (b -> b -> m c) -> (a -> m b) -> a -> a -> m c
onMM f g x y = liftMM2 f (g x) (g y)

onM :: Monad m => (b -> b -> c) -> (a -> m b) -> a -> a -> m c
onM f g x y = liftM2 f (g x) (g y)

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 =
    foldl'
        ( \(as, bs, cs, ds) (a, b, c, d) ->
            (as ++ [a], bs ++ [b], cs ++ [c], ds ++ [d])
        )
        ([], [], [], [])

liftMM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftMM2 f m1 m2 = do
    x1 <- m1
    x2 <- m2
    f x1 x2

typeof :: Lit -> Type
typeof (LInt _)  = int
typeof (LChar _) = char
typeof (LString _) = string

string = TLit "String"
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
