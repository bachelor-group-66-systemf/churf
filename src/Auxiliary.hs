
module Auxiliary (module Auxiliary) where

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]
