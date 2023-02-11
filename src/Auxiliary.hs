
module Auxiliary (module Auxiliary) where
import           Control.Monad.Error.Class (liftEither)
import           Control.Monad.Except      (MonadError)
import           Data.Either.Combinators   (maybeToRight)

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

maybeToRightM :: MonadError l m => l -> Maybe r -> m r
maybeToRightM err = liftEither . maybeToRight err
