module Monomorphizer.Monomorphizer (monomorphize) where

import Monomorphizer.MonomorphizerIr
import TypeChecker.TypeCheckerIr qualified as T

monomorphize :: T.Program -> Program
monomorphize (T.Program ds) = Program $ monoDefs ds

monoDefs :: [T.Def] -> [Def]
monoDefs = map monoDef

monoDef :: T.Def -> Def
monoDef (T.DBind bind) = DBind $ monoBind bind
monoDef (T.DData d) = DData d

monoBind :: T.Bind -> Bind
monoBind (T.Bind name args e) = Bind name args e
