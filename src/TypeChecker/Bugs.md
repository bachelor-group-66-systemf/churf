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

## Bugged error message
```hs
data Maybe () where {
    Nothing : Maybe
    Just : Int -> Maybe
    };

fmap : (Int -> Int) -> Maybe -> Maybe ;
fmap f ma = case ma of {
    Nothing => Nothing ;
    Just a => Just (f a) ;
};

pure : Int -> Maybe ;
pure x = Just x ;

ap mf ma = case mf of {
    Just f => case ma of {
        Nothing => Nothing;
        Just a => Just (f a);
    };
    Nothing => Nothing;
};
    
return = pure;

bind ma f = case ma of {
    Nothing => Nothing ;
    Just a => f a ;
};
```
```
TYPECHECKER ERROR
Inferred type '("c" -> "Int") -> "Maybe" -> "Maybe" does not match specified type '("Int" -> "Int") -> "Maybe" -> "Maybe"'
```