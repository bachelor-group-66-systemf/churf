## Bugs

None known at this moment

main\_bug should not typecheck

```hs
apply : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c ;
apply f x = \y. f x y ;

id : 'a -> 'a ;
id x = x ;

add : _Int -> _Int -> _Int ;
add x y = x + y ;

main_bug : _Int -> _Int -> _Int ;
main_bug= (apply id) add ;

idadd : _Int -> _Int -> _Int ;
idadd = id add ;
```

main\_bug should typecheck

```hs
apply : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c ;
apply f x = \y. f x y ;

id : 'a -> 'a ;
id x = x ;

add : _Int -> _Int -> _Int ;
add x y = x + y ;

main_bug : _Int -> _Int -> _Int ;
main_bug = apply (id add) ;

idadd : _Int -> _Int -> _Int ;
idadd = id add ;
```

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
