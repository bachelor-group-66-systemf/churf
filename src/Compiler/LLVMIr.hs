{-# LANGUAGE LambdaCase #-}
module Compiler.LLVMIr where
import           Data.List   (intercalate)
import           Grammar.Abs (Ident (Ident))

-- | A datatype which represents some basic LLVM types
data LLVMType = I1 | I8 | I32 | I64 | Ptr
            | Ref LLVMType | Array Integer LLVMType | CustomType Ident

instance Show LLVMType where
    show :: LLVMType -> String
    show t = case t of
        I1                    -> "i1"
        I8                    -> "i8"
        I32                   -> "i32"
        I64                   -> "i64"
        Ptr                   -> "ptr"
        Ref ty                -> show ty <> "*"
        Array n ty            -> concat ["[", show n, " x ", show ty, "]"]
        CustomType (Ident ty) -> ty

-- | Represents a LLVM "value", as in an integer, a register variable,
--   or a string contstant
data Value = VInteger Integer | VIdent Ident | VConstant String
instance Show Value where
    show :: Value -> String
    show v = case v of
        VInteger i       -> show i
        VIdent (Ident i) -> "%" <> i
        VConstant s      -> "c" <> show s

type Params = [(LLVMType, Ident)]
type Args = [(LLVMType, Value)]

-- | A datatype which represents different instructions in LLVM
data LLVMIr = Define LLVMType Ident Params
            | DefineEnd
            | Declare LLVMType Ident Params
            | SetVariable Ident
            | Variable Ident
            | Add LLVMType Value Value
            | Sub LLVMType Value Value
            | Div LLVMType Value Value
            | Mul LLVMType Value Value
            | Srem LLVMType Value Value
            | Call LLVMType Ident Args
            | Alloca LLVMType
            | Store LLVMType Ident LLVMType Ident
            | Bitcast LLVMType Ident LLVMType
            | Ret LLVMType Value
            | Comment String
            | UnsafeRaw String -- This should generally be avoided, and proper
                               -- instructions should be used in its place
    deriving (Show)

-- | Converts a LLVM inststruction to a String, allowing for printing etc.
llvmIrToString :: LLVMIr -> String
llvmIrToString = \case
    (Define t (Ident i) params)           -> concat ["define ", show t, " @", i, "("
                                                    , intercalate ", " (fmap (\(x,Ident y) -> unwords [show x, "%"<>y]) params)
                                                    ,") {\n"]
    DefineEnd                             -> "}\n"
    (Declare _t (Ident _i) _params)       -> undefined
    (SetVariable (Ident i))               -> concat ["%", i, " = "]
    (Add t v1 v2)                         -> concat ["add ", show t, " "
                                                    , show v1, ", ", show v2
                                                    , "\n"]
    (Sub t v1 v2)                         -> concat ["sub ", show t, " "
                                                    , show v1, ", "
                                                    , show v2, "\n"]
    (Div t v1 v2)                         -> concat ["sdiv ", show t, " "
                                                    , show v1, ", "
                                                    , show v2, "\n"]
    (Mul t v1 v2)                         -> concat ["mul ", show t, " "
                                                    , show v1, ", "
                                                    , show v2, "\n"]
    (Srem t v1 v2)                        -> concat ["srem ", show t, " "
                                                    , show v1, ", "
                                                    , show v2, "\n"]
    (Call t (Ident i) arg)                -> concat ["call ", show t, " @", i, "("
                                                   , intercalate ", " $ Prelude.map (\(x,y) -> show x <> " " <> show y) arg
                                                   , ")\n"]
    (Alloca t)                            -> unwords ["alloca", show t, "\n"]
    (Store t1 (Ident id1) t2 (Ident id2)) -> concat ["store ", show t1, " %"
                                                    , id1, ", ", show t2, " %"
                                                    , id2, "\n"]
    (Bitcast t1 (Ident i) t2)             -> concat ["bitcast ", show t1, " %"
                                                    , i, " to ", show t2, "\n"]
    (Ret t v)                             -> concat ["ret ", show t
                                                    , " ", show v, "\n"]
    (UnsafeRaw s)                         -> s
    (Comment s)                           -> "; " <> s <> "\n"
    (Variable (Ident id))                 -> "%" <> id

