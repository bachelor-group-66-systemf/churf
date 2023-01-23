{-# LANGUAGE LambdaCase #-}

module Interpreter where

import           Control.Monad.Except    (Except, MonadError (throwError))

import           Grammar.Abs

interpret :: Program -> Except String Integer
interpret (Program _) = throwError "Can not interpret program yet"
