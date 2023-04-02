module Codegen.Auxillary where

import Codegen.LlvmIr (LLVMType (..), LLVMValue (..))
import Control.Monad (foldM_)
import Monomorphizer.MonomorphizerIr as MIR (ExpT, Type (..))
import TypeChecker.TypeCheckerIr qualified as TIR

type2LlvmType :: MIR.Type -> LLVMType
type2LlvmType (MIR.TLit id@(TIR.Ident name)) = case name of
    "Int" -> I64
    "Char" -> I8
    _ -> CustomType id
type2LlvmType (MIR.TFun t xs) = do
    let (t', xs') = function2LLVMType xs [type2LlvmType t]
    Function t' xs'
  where
    function2LLVMType :: Type -> [LLVMType] -> (LLVMType, [LLVMType])
    function2LLVMType (TFun t xs) s = function2LLVMType xs (type2LlvmType t : s)
    function2LLVMType x s = (type2LlvmType x, s)

getType :: ExpT -> LLVMType
getType (_, t) = type2LlvmType t

extractTypeName :: MIR.Type -> TIR.Ident
extractTypeName (MIR.TLit id) = id
extractTypeName (MIR.TFun t xs) =
    let (TIR.Ident i) = extractTypeName t
        (TIR.Ident is) = extractTypeName xs
     in TIR.Ident $ i <> "_$_" <> is

valueGetType :: LLVMValue -> LLVMType
valueGetType (VInteger _) = I64
valueGetType (VChar _) = I8
valueGetType (VIdent _ t) = t
valueGetType (VConstant s) = Array (fromIntegral $ length s) I8
valueGetType (VFunction _ _ t) = t

typeByteSize :: LLVMType -> Integer
typeByteSize I1 = 1
typeByteSize I8 = 1
typeByteSize I32 = 4
typeByteSize I64 = 8
typeByteSize Ptr = 8
typeByteSize (Ref _) = 8
typeByteSize (Function _ _) = 8
typeByteSize (Array n t) = n * typeByteSize t
typeByteSize (CustomType _) = 8

enumerateOneM_ :: Monad m => (Integer -> a -> m b) -> [a] -> m ()
enumerateOneM_ f = foldM_ (\i a -> f i a >> pure (i + 1)) 1