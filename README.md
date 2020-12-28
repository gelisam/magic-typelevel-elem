# Magic Typelevel Elem

A variant of

```haskell
type family Elem a as where
  Elem a '[]       = 'False
  Elem a (a ': as) = 'True
  Elem a (b ': as) = Elem a as
```

implemented as a "magic" type family. That is, we do not give an implementation:

```haskell
type family Elem a as
```

And then we write a typechecker plugin which rewrites occurrences of `Elem a as`.
