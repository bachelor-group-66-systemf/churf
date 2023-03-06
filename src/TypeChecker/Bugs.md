## Bugs

None known at this moment

## Fixed bugs

* 1

```hs
fmap : ('a -> 'b) -> Maybe ('a) -> Maybe ('b) ;
fmap f x =
    case x of {
        Just x => Just (f x) ;
        Nothing => Nothing
    }
```

* 2

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
