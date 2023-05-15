# Thesis

The branch `thesis` contain the state of the project when the thesis report was submitted. (2023-05-15)

# Build
First generate the parser using [BNFC](https://bnfc.digitalgrammars.com/),
this is done using the command `bnfc -o src -d Grammar.cf`

Churf can then be built using `cabal install`

Using the tool [make](https://www.gnu.org/software/make/) the entire thing can be built by running `make`
or using [just](https://github.com/casey/just), `just build`

# Dependencies
If you have Nix installed, simply run `nix-shell --pure shell.nix` to get into an environment
with the right versions of packages. Then run `make` and the compiler should build.

# Compiling a program

Using the Hindley-Milner type checker: `./language -t hm example.crf`

Using the bidirectional type checker: `./language -t bi example.crf`

The program to compile has to have the file extension `.crf`
# Syntax and quirks

See Grammar.pdf for the full syntax.

The syntactic requirements differ a bit using the different type checkers.
The bidirectional type checker require explicit `forall` everywhere a type
forall quantified type variable is declared. In the Hindley-Milner type checker
all type variables are assumed to be forall quantified.

Currently for the code generator and monomorphizer to work correctly it is
expected that the function `main` exist with either explicitly given type `Int`
or inferrable.

Single line comments are written using `--`
Multi line comments are written using `{-` and `-}`

Braches and semicolons are optional.

## Program

A program is a list of defs separated by semicolons, which in turn is either a bind, a signature, or a data types
`Program ::= [Def]`

```hs
data Test () where 
    Test : Test ()
test : Int
test = 0
```

## Bind

A bind is a name followed by a white space separated list of arguments, then an equal sign followed by an expression.
Both name and arguments have to start with lower case letters

`Bind ::= LIdent [LIdent] "=" Exp`

```hs
example x y = x + y
```

## Signature
A signature is a name followed by a colon and then the type
The name has to start with a lowe case letter

`Sig ::= LIdent ":" Type`

```hs
const : a -> b -> a
```

## Data type
A data type is declared as follows

`Data ::= "data" Type "where" "{" [Inj] "}"`

The words in quotes are necessary keywords
The type can be any type for parsing, but only `TData` will type check.

The list of Inj is separated by white space. Using new lines is recommended for ones own sanity.

```hs
data Maybe (a) where
    Nothing : Maybe (a)
    Just    : a -> Maybe (a)
```
The parens are necessary for every data type to make the grammar unambiguous.
Thus in `data Bool () where ...` the parens *do* *not* represent Unit

### Inj
An inj is a constructor for the data type

It is declared like a signature, except the name has to start with a lower case letter.
The return type of the constructor also has match the type of the data type to type check.

`Inj ::= UIdent ":" Type`

## Type

A type can be either a type literal, type variable, function type, explicit forall quantified type or a type representing a data type
A type literal have to start with an upper case letter, type variables have to start with a lower case letter,
data types have to start with an upper case letter, a function type is two types separated by an arrow (arrows right associative),
and foralls take one type variable followed by a type.

`TLit ::= UIdent`

`TVar ::= LIdent`

`TData ::= UIdent "(" [Type] ")"`

`TFun ::= Type "->" Type`

`TAll ::= "forall" LIdent "." Type`

```hs
exampleLit : Int
exampleVar : a
exampleData : Maybe (a)
exampleFun : Int -> a
exampleAll : forall a. forall b. a -> b
```

## Expressions

There are a couple different expressions, probably best explained by their rules

Type annotated expression

`EAnn  ::= "(" Exp ":" Type ")"`

Variable

`EVar  ::= LIdent`
```hs
x
```

Constructor

`EInj  ::= UIdent`
```hs
Just
```

Literal

`ELit  ::= Lit`
```hs
0
```

Function application

`EApp  ::= Exp2 Exp3`
```hs
f 0
```

Addition

`EAdd  ::= Exp1 "+" Exp2`
```hs
3 + 5
```

Let expression

`ELet  ::= "let" Bind "in" Exp `
```hs
let f x = x in f 0 
```

Abstraction, known as lambda or closure

`EAbs  ::= "\\" LIdent "." Exp`
```hs
\x. x
```

Case expression consist of a list semicolon separated list of Branches

`ECase ::= "case" Exp "of" "{" [Branch] "}"`

```hs
case xs of
    Cons x xs => 1
    Nil => 0
```

### Branch
A branch is a pattern followed by the fat arrow and then an expression

`Branch ::= Pattern "=>" Exp`

### Pattern
A pattern can be either a variable, literal, a wildcard represented by `_`, an enum constructor (constructor with zero arguments)
, or a constructor followed by a recursive list of patterns.

Variable match

`PVar   ::= LIdent`

The x in the following example
```hs
x => 0
```
Literal match

`PLit   ::= Lit`

The 1 in the following example
```hs
1 => 0
```
A wildcard match

`PCatch ::= "_"`

The underscore in the following example
```hs
_ => 0
```
A constructor without arguments

`PEnum  ::= UIdent`

The Nothing in the following example
```hs
Nothing => 0
```
The recursive match on a constructor

`PInj   ::= UIdent [Pattern1]`

The outer Just represents the UIdent and the rest is the recursive match
```hs
Just (Just 0) => 1
```

For simplicity sake a user does not need to consider these last two cases as different in parsing.
We allow arbitrarily deep pattern matching.

## Literal
We currently allow two different literals: Integer and Char
