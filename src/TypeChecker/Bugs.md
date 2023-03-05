## Bugs

### Polymorphic type variables are global?

This doesn't work (occurs check failed, can't unify `(a -> a) = a`
```hs
data Maybe ('a) where {
    Nothing : Maybe ('a)
    Just : 'a -> Maybe ('a)
};

id : 'a -> 'a ;
id x = x ;

main : Maybe ('a -> 'a) ; 
main = Just id;
```

But this does
```hs
data Maybe ('a) where {
    Nothing : Maybe ('a)
    Just : 'a -> Maybe ('a)
};

id : 'b -> 'b ;
id x = x ;

main : Maybe ('a -> 'a) ; 
main = Just id;
```
UPDATE: Might have found a fix. Need to be tested.

### The function f is not carried into the case-expression

Code example that does not type check
```hs
fmap : ('a -> 'b) -> Maybe ('a) -> Maybe ('b) ;
fmap f x =
    case x of {
        Just x => Just (f x) ;
        Nothing => Nothing
    }
```
