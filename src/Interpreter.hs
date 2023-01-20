{-# LANGUAGE LambdaCase #-}
module Interpreter where

import           Control.Applicative     (Applicative)
import           Control.Monad.Except    (Except, MonadError (throwError),
                                          liftEither)
import           Data.Either.Combinators (maybeToRight)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Grammar.Abs
import           Grammar.Print           (printTree)

interpret :: Program -> Except String Integer
interpret (Program e) =
  eval mempty e >>= \case
    VClosure {} -> throwError "main evaluated to a function"
    VInt i      -> pure i


data Val = VInt Integer
         | VClosure Cxt Ident Exp

type Cxt = Map Ident Val

eval :: Cxt -> Exp -> Except String Val
eval cxt = \case


  --  ------------ x ∈ γ
  --  γ ⊢ x ⇓ γ(x)

  EId x     ->
    maybeToRightM
      ("Unbound variable:" ++ printTree x)
      $ Map.lookup x cxt

  --  ---------
  --  γ ⊢ i ⇓ i

  EInt i    -> pure $ VInt i

  --  γ     ⊢ e  ⇓ let δ in λx → f
  --  γ     ⊢ e₁ ⇓ v
  --  δ,x=v ⊢ f  ⇓ v₁
  --  ------------------------------
  --  γ ⊢ e e₁ ⇓ v₁

  EApp e e1 ->
    eval cxt e >>= \case
      VInt _ -> throwError "Not a function"
      VClosure delta x f -> do
        v <- eval cxt e1
        eval (Map.insert x v delta) f

  --
  --  -----------------------------
  --  γ ⊢ λx → f ⇓ let γ in λx → f

  EAbs x e  -> pure $ VClosure cxt x e


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



maybeToRightM :: MonadError l m => l -> Maybe r -> m r
maybeToRightM err = liftEither . maybeToRight err

