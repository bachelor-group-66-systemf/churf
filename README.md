# Thesis

The branch [thesis](https://github.com/bachelor-group-66-systemf/churf/tree/thesis) contain the state of the project when the thesis report was submitted (2023-05-15).

# Build

Using [make](https://www.gnu.org/software/make/) the entire thing can be built by running `make`

# Compiling a program

Using the Hindley-Milner type checker: `./churf -t hm <FILENAME>`

Using the bidirectional type checker: `./churf -t bi <FILENAME>`

Running `./churf` will display a help message for the different available flags

# Syntax

Single line comments are written using `--`
Multi line comments are written using `{-` and `-}`

The syntax of Churf can be read in [Grammar.pdf](https://github.com/bachelor-group-66-systemf/churf/blob/main/Grammar.pdf)

Here is an example program in Churf

```hs
main = case odd (sum 123) of
    True => printStr "odd!"
    False => printStr "even!"

sum = \x. case x of
    0 => 0
    n => n + (sum (n - 1))

odd x = case x of
    0 => False
    n => even (n - 1)

even x = case x of
    0 => True
    n => odd (n - 1)
```
