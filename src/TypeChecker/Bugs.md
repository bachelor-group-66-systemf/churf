# Bugs

## Using uninstantiated type variables

Program below should not type check

```hs
data Test (a) where {
    Test : b -> Test (a)
                    };
```

## Duplicate definitions of functions

Program below should not type check

```hs
id x = x ;
id x = x ;
```

## What?

Program below should not type check

```hs
main : a -> b ;
main x = x;
```
