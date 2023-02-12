{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
module Interpreter where

import           Auxiliary               (maybeToRightM)
import           Control.Applicative     (Applicative)
import           Control.Monad.Except    (Except, MonadError (throwError),
                                          liftEither)
import           Control.Monad.State     (MonadState, StateT, evalStateT)
import           Data.Either.Combinators (maybeToRight)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe              (maybe)
import           Grammar.Abs
import           Grammar.ErrM            (Err)
import           Grammar.Print           (printTree)

interpret :: Program -> Err Integer
interpret (Program scs) = do
  main <- findMain scs
  eval (initCxt scs) main >>=
    \case
      VClosure {} -> throwError "main evaluated to a function"
      VInt i      -> pure i


initCxt :: [Bind] -> Cxt
initCxt scs =
  Cxt { env = mempty
      , sig = foldr insert mempty $ map expandLambdas scs
      }
  where insert (Bind name _ rhs) = Map.insert name rhs

expandLambdas :: Bind -> Bind
expandLambdas (Bind name parms rhs) = Bind name [] $ foldr EAbs rhs parms


findMain :: [Bind] -> Err Exp
findMain []       = throwError "No main!"
findMain (sc:scs) = case sc of
                      Bind "main" _ rhs -> pure rhs
                      _                 -> findMain scs

data Val = VInt Integer
         | VClosure Env Ident Exp
           deriving (Show, Eq)

type Env = Map Ident Val
type Sig = Map Ident Exp

data Cxt = Cxt
  { env :: Map Ident Val
  , sig :: Map Ident Exp
  } deriving (Show, Eq)

eval :: Cxt -> Exp -> Err Val
eval cxt = \case

  --  ------------ x ∈ γ
  --  γ ⊢ x ⇓ γ(x)

  EId x     -> do
    case Map.lookup x cxt.env of
      Just e -> pure e
      Nothing ->
        case Map.lookup x cxt.sig of
          Just e  -> eval (emptyEnv cxt) e
          Nothing -> throwError ("Unbound variable: " ++ printTree x)

  --  ---------
  --  γ ⊢ i ⇓ i

  EInt i    -> pure $ VInt i

  --  γ     ⊢ e  ⇓ let δ in λx. f
  --  γ     ⊢ e₁ ⇓ v
  --  δ,x=v ⊢ f  ⇓ v₁
  --  ------------------------------
  --  γ ⊢ e e₁ ⇓ v₁

  EApp e e1 ->
    eval cxt e >>= \case
      VInt _ -> throwError "Not a function"
      VClosure delta x f -> do
        v <- eval cxt e1
        let cxt' = putEnv (Map.insert x v delta) cxt
        eval cxt' f


  --
  --  -----------------------------
  --  γ ⊢ λx. f ⇓ let γ in λx. f

  EAbs par e  -> pure $ VClosure cxt.env par e


  --  γ ⊢ e  ⇓ v
  --  γ ⊢ e₁ ⇓ v₁
  --  ------------------
  --  γ ⊢ e e₁ ⇓ v + v₁

  EAdd e e1 -> do
    v <- eval cxt e
    v1 <- eval cxt e1
    case (v, v1) of
      (VInt i, VInt i1) -> pure $ VInt (i + i1)
      _                 -> throwError "Can't add a function"

  ELet _ _ -> throwError "ELet pattern match should never occur!"


emptyEnv :: Cxt -> Cxt
emptyEnv cxt = cxt { env = mempty }

putEnv :: Env -> Cxt -> Cxt
putEnv env cxt = cxt { env = env }
