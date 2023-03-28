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
## Pattern match on functions

Program below should not type check

```hs
main = case \x. x of {
    _ => 0;
};
```

# Inference should not depend on order

This one is really tough, strangely
Spent many hours on this so far

```hs
main = id 0 ;
id x = x;
```
