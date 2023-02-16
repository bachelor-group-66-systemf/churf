{-# LANGUAGE LambdaCase #-}

module LlvmIr (LLVMType (..), LLVMIr (..), llvmIrToString, LLVMValue (..), LLVMComp (..)) where

import           Data.List     (intercalate)
import           TypeCheckerIr


-- | A datatype which represents some basic LLVM types
data LLVMType
    = I1
    | I8
    | I32
    | I64
    | Ptr
    | Ref LLVMType
    | Array Integer LLVMType
    | CustomType Ident

instance Show LLVMType where
    show :: LLVMType -> String
    show = \case
        I1                    -> "i1"
        I8                    -> "i8"
        I32                   -> "i32"
        I64                   -> "i64"
        Ptr                   -> "ptr"
        Ref ty                -> show ty <> "*"
        Array n ty            -> concat ["[", show n, " x ", show ty, "]"]
        CustomType (Ident ty) -> ty

data LLVMComp
    = LLEq
    | LLNe
    | LLUgt
    | LLUge
    | LLUlt
    | LLUle
    | LLSgt
    | LLSge
    | LLSlt
    | LLSle
instance Show LLVMComp where
    show :: LLVMComp -> String
    show = \case
        LLEq  -> "eq"
        LLNe  -> "ne"
        LLUgt -> "ugt"
        LLUge -> "uge"
        LLUlt -> "ult"
        LLUle -> "ule"
        LLSgt -> "sgt"
        LLSge -> "sge"
        LLSlt -> "slt"
        LLSle -> "sle"

{- | Represents a LLVM "value", as in an integer, a register variable,
  or a string contstant
-}
data LLVMValue = VInteger Integer | VIdent Id | VConstant String

instance Show LLVMValue where
    show :: LLVMValue -> String
    show v = case v of
        VInteger i    -> show i
        VIdent (n, _) -> "%" <> fromIdent n
        VConstant s   -> "c" <> show s

type Params = [(Ident, LLVMType)]
type Args = [(LLVMType, LLVMValue)]

-- | A datatype which represents different instructions in LLVM
data LLVMIr
    = Define LLVMType Ident Params
    | DefineEnd
    | Declare LLVMType Ident Params
    | SetVariable Ident LLVMIr
    | Variable Ident
    | Add LLVMType LLVMValue LLVMValue
    | Sub LLVMType LLVMValue LLVMValue
    | Div LLVMType LLVMValue LLVMValue
    | Mul LLVMType LLVMValue LLVMValue
    | Srem LLVMType LLVMValue LLVMValue
    | Icmp LLVMComp LLVMType LLVMValue LLVMValue
    | Br Ident
    | BrCond LLVMValue Ident Ident
    | Label Ident
    | Call LLVMType Ident Args
    | Alloca LLVMType
    | Store LLVMType Ident LLVMType Ident
    | Bitcast LLVMType Ident LLVMType
    | Ret LLVMType LLVMValue
    | Comment String
    | UnsafeRaw String -- This should generally be avoided, and proper
    -- instructions should be used in its place
    deriving (Show)

-- | Converts a list of LLVMIr instructions to a string
llvmIrToString :: [LLVMIr] -> String
llvmIrToString = go 0
  where
    go :: Int -> [LLVMIr] -> String
    go _ [] = mempty
    go i (x : xs) = do
        let (i', n) = case x of
                Define{}  -> (i + 1, 0)
                DefineEnd -> (i - 1, 0)
                _         -> (i, i)
        insToString n x <> go i' xs

{- | Converts a LLVM inststruction to a String, allowing for printing etc.
  The integer represents the indentation
-}
{- FOURMOLU_DISABLE -}
    insToString :: Int -> LLVMIr -> String
    insToString i l =
        replicate i '\t' <> case l of
            (Define t (Ident i) params) ->
                concat
                    [ "define ", show t, " @", i
                    , "(", intercalate ", " (map (\(Ident y, x) -> unwords [show x, "%" <> y]) params)
                    , ") {\n"
                    ]
            DefineEnd -> "}\n"
            (Declare _t (Ident _i) _params) -> undefined
            (SetVariable (Ident i) ir) -> concat ["%", i, " = ", insToString 0 ir]
            (Add t v1 v2) ->
                concat
                    [ "add ", show t, " ", show v1
                    , ", ", show v2, "\n"
                    ]
            (Sub t v1 v2) ->
                concat
                    [ "sub ", show t, " ", show v1, ", "
                    , show v2, "\n"
                    ]
            (Div t v1 v2) ->
                concat
                    [ "sdiv ", show t, " ", show v1, ", "
                    , show v2, "\n"
                    ]
            (Mul t v1 v2) ->
                concat
                    [ "mul ", show t, " ", show v1
                    , ", ", show v2, "\n"
                    ]
            (Srem t v1 v2) ->
                concat
                    [ "srem ", show t, " ", show v1, ", "
                    , show v2, "\n"
                    ]
            (Call t (Ident i) arg) ->
                concat
                    [ "call ", show t, " @", i, "("
                    , intercalate ", " $ Prelude.map (\(x, y) -> show x <> " " <> show y) arg
                    , ")\n"
                    ]
            (Alloca t) -> unwords ["alloca", show t, "\n"]
            (Store t1 (Ident id1) t2 (Ident id2)) ->
                concat
                    [ "store ", show t1, " %", id1
                    , ", ", show t2 , " %", id2, "\n"
                    ]
            (Bitcast t1 (Ident i) t2) ->
                concat
                    [ "bitcast ", show t1, " %"
                    , i, " to ", show t2, "\n"
                    ]
            (Icmp comp t v1 v2) ->
                concat
                    [ "icmp ", show comp, " ", show t
                    , " ", show v1, ", ", show v2, "\n"
                    ]
            (Ret t v) ->
                concat
                    [ "ret ", show t, " "
                    , show v, "\n"
                    ]
            (UnsafeRaw s) -> s
            (Label (Ident s)) -> "\nlabel_" <> s <> ":\n"
            (Br (Ident s)) -> "br label %label_" <> s <> "\n"
            (BrCond val (Ident s1) (Ident s2)) ->
                concat
                    [ "br i1 ", show val, ", ", "label %"
                    , "label_", s1, ", ", "label %", "label_", s2, "\n"
                    ]
            (Comment s) -> "; " <> s <> "\n"
            (Variable (Ident id)) -> "%" <> id
{- FOURMOLU_ENABLE -}

fromIdent :: Ident -> String
fromIdent (Ident s) = s
