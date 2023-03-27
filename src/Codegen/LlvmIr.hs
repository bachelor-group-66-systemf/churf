{-# LANGUAGE LambdaCase #-}

module Codegen.LlvmIr (
    LLVMType (..),
    LLVMIr (..),
    llvmIrToString,
    LLVMValue (..),
    LLVMComp (..),
    Visibility (..),
    CallingConvention (..),
    ToIr (..),
) where

import Data.List (intercalate)
import TypeChecker.TypeCheckerIr (Ident (..))

data CallingConvention = TailCC | FastCC | CCC | ColdCC deriving (Show)
instance ToIr CallingConvention where
    toIr :: CallingConvention -> String
    toIr TailCC = "tailcc"
    toIr FastCC = "fastcc"
    toIr CCC = "ccc"
    toIr ColdCC = "coldcc"

-- | A datatype which represents some basic LLVM types
data LLVMType
    = I1
    | I8
    | I32
    | I64
    | Ptr
    | Ref LLVMType
    | Function LLVMType [LLVMType]
    | Array Integer LLVMType
    | CustomType Ident
    deriving (Show)

class ToIr a where
    toIr :: a -> String

instance ToIr LLVMType where
    toIr :: LLVMType -> String
    toIr = \case
        I1 -> "i1"
        I8 -> "i8"
        I32 -> "i32"
        I64 -> "i64"
        Ptr -> "ptr"
        Ref ty -> toIr ty <> "*"
        Function t xs -> toIr t <> " (" <> intercalate ", " (map toIr xs) <> ")*"
        Array n ty -> concat ["[", show n, " x ", toIr ty, "]"]
        CustomType (Ident ty) -> "%" <> ty

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
    deriving (Show)
instance ToIr LLVMComp where
    toIr :: LLVMComp -> String
    toIr = \case
        LLEq -> "eq"
        LLNe -> "ne"
        LLUgt -> "ugt"
        LLUge -> "uge"
        LLUlt -> "ult"
        LLUle -> "ule"
        LLSgt -> "sgt"
        LLSge -> "sge"
        LLSlt -> "slt"
        LLSle -> "sle"

data Visibility = Local | Global deriving (Show)
instance ToIr Visibility where
    toIr :: Visibility -> String
    toIr Local = "%"
    toIr Global = "@"

{- | Represents a LLVM "value", as in an integer, a register variable,
or a string contstant
-}
data LLVMValue
    = VInteger Integer
    | VChar Char
    | VIdent Ident LLVMType
    | VConstant String
    | VFunction Ident Visibility LLVMType
    deriving (Show)

instance ToIr LLVMValue where
    toIr :: LLVMValue -> String
    toIr v = case v of
        VInteger i -> show i
        VChar i -> show i
        VIdent (Ident n) _ -> "%" <> n
        VFunction (Ident n) vis _ -> toIr vis <> n
        VConstant s -> "c" <> show s

type Params = [(Ident, LLVMType)]
type Args = [(LLVMType, LLVMValue)]

-- | A datatype which represents different instructions in LLVM
data LLVMIr
    = Type Ident [LLVMType]
    | Define CallingConvention LLVMType Ident Params
    | DefineEnd
    | Declare LLVMType Ident Params
    | SetVariable Ident LLVMIr
    | Variable Ident
    | -- extractvalue <aggregate type> <val>, <idx>{, <idx>}*
      ExtractValue LLVMType LLVMValue Integer
    | GetElementPtr LLVMType LLVMType LLVMValue LLVMType LLVMValue LLVMType LLVMValue
    | GetElementPtrInbounds LLVMType LLVMType LLVMValue LLVMType LLVMValue LLVMType LLVMValue
    | Add LLVMType LLVMValue LLVMValue
    | Sub LLVMType LLVMValue LLVMValue
    | Div LLVMType LLVMValue LLVMValue
    | Mul LLVMType LLVMValue LLVMValue
    | Srem LLVMType LLVMValue LLVMValue
    | Icmp LLVMComp LLVMType LLVMValue LLVMValue
    | Br Ident
    | BrCond LLVMValue Ident Ident
    | Label Ident
    | Call CallingConvention LLVMType Visibility Ident Args
    | Alloca LLVMType
    | Store LLVMType LLVMValue LLVMType Ident
    | Load LLVMType LLVMType Ident
    | Bitcast LLVMType LLVMValue LLVMType
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
                Define{} -> (i + 1, 0)
                DefineEnd -> (i - 1, 0)
                _ -> (i, i)
        insToString n x <> go i' xs

-- \| Converts a LLVM inststruction to a String, allowing for printing etc.
--      The integer represents the indentation
--
{- FOURMOLU_DISABLE -}
    insToString :: Int -> LLVMIr -> String
    insToString i l =
        replicate i '\t' <> case l of
            (GetElementPtr t1 t2 p t3 v1 t4 v2) -> do
                -- getelementptr inbounds %Foo, %Foo* %x, i32 0, i32 0
                concat
                    [ "getelementptr ", toIr t1, ", " , toIr t2
                    , " ", toIr p, ", ", toIr t3, " ", toIr v1
                    , ", ", toIr t4, " ", toIr v2, "\n"
                    ]
            (ExtractValue t1 v i) -> do
                concat
                    [ "extractvalue ", toIr t1, " "
                    , toIr v, ", ", show i, "\n"
                    ]
            (GetElementPtrInbounds t1 t2 p t3 v1 t4 v2) -> do
                -- getelementptr inbounds %Foo, %Foo* %x, i32 0, i32 0
                concat
                    [ "getelementptr inbounds ", toIr t1, ", " , toIr t2
                    , " ", toIr p, ", ", toIr t3, " ", toIr v1,
                    ", ", toIr t4, " ", toIr v2, "\n" ]
            (Type (Ident n) types) ->
                concat
                    [ "%", n, " = type { "
                    , intercalate ", " (map toIr types)
                    , " }\n"
                    ]
            (Define c t (Ident i) params) ->
                concat
                    [ "define ", toIr c, " ", toIr t, " @", i
                    , "(", intercalate ", " (map (\(Ident y, x) -> unwords [toIr x, "%" <> y]) params)
                    , ") {\n"
                    ]
            DefineEnd -> "}\n"
            (Declare _t (Ident _i) _params) -> undefined
            (SetVariable (Ident i) ir) -> concat ["%", i, " = ", insToString 0 ir]
            (Add t v1 v2) ->
                concat
                    [ "add ", toIr t, " ", toIr v1
                    , ", ", toIr v2, "\n"
                    ]
            (Sub t v1 v2) ->
                concat
                    [ "sub ", toIr t, " ", toIr v1, ", "
                    , toIr v2, "\n"
                    ]
            (Div t v1 v2) ->
                concat
                    [ "sdiv ", toIr t, " ", toIr v1, ", "
                    , toIr v2, "\n"
                    ]
            (Mul t v1 v2) ->
                concat
                    [ "mul ", toIr t, " ", toIr v1
                    , ", ", toIr v2, "\n"
                    ]
            (Srem t v1 v2) ->
                concat
                    [ "srem ", toIr t, " ", toIr v1, ", "
                    , toIr v2, "\n"
                    ]
            (Call c t vis (Ident i) arg) ->
                concat
                    [ "call ", toIr c, " ",  toIr t, " ", toIr vis, i, "("
                    , intercalate ", " $ Prelude.map (\(x, y) -> toIr x <> " " <> toIr y) arg
                    , ")\n"
                    ]
            (Alloca t) -> unwords ["alloca", toIr t, "\n"]
            (Store t1 val t2 (Ident id2)) ->
                concat
                    [ "store ", toIr t1, " ", toIr val
                    , ", ", toIr t2 , " %", id2, "\n"
                    ]
            (Load t1 t2 (Ident addr)) ->
                concat
                    [ "load ", toIr t1, ", "
                    , toIr t2, " %", addr, "\n"
                    ]
            (Bitcast t1 v t2) ->
                concat
                    [ "bitcast ", toIr t1, " "
                    , toIr v, " to ", toIr t2, "\n"
                    ]
            (Icmp comp t v1 v2) ->
                concat
                    [ "icmp ", toIr comp, " ", toIr t
                    , " ", toIr v1, ", ", toIr v2, "\n"
                    ]
            (Ret t v) ->
                concat
                    [ "ret ", toIr t, " "
                    , toIr v, "\n"
                    ]
            (UnsafeRaw s) -> s
            (Label (Ident s)) -> "\n" <> lblPfx <> s <> ":\n"
            (Br (Ident s)) -> "br label %" <> lblPfx <> s <> "\n"
            (BrCond val (Ident s1) (Ident s2)) ->
                concat
                    [ "br i1 ", toIr val, ", ", "label %"
                    , lblPfx, s1, ", ", "label %", lblPfx, s2, "\n"
                    ]
            (Comment s) -> "; " <> s <> "\n"
            (Variable (Ident id)) -> "%" <> id
{- FOURMOLU_ENABLE -}

lblPfx :: String
lblPfx = "lbl_"
