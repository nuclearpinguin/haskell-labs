# Functional Programming at WUT

1. Notes from lectures 
2. Codes from labs and more

User: password
Pass: user


# Lecture 1 21.02.19

### Rules:
- project 20pt (circa about 10th labs)
- 2 x scored labs (10pt each, in first 5 weeks?)
- Theory test on last lecture (20pt)


### Theory scope:
- lambda-calculus - one of the simplest programming languages
- monads and its use in FP
- lazy evalutaion

Fucntional programming languages, some informal talk:
- declarative - you focus more on what to do rather than how to to
- no side-effects - sometimes functions use env or global variables or things like that what makes the code non-deterministic
- lazy-eval - means that we start with evaluation of outer-most expressions. In other words if you don't have to use something then do not evaluate that.
- T: Lazy-eval evaluates in no more steps than eager evaluation.
- Expression has a *normal form* if it's fully evaluable.
- Weak head normal form (WHNM) = most-outer constructor is evaluable 


Because we have [CCC](https://ncatlab.org/nlab/show/cartesian+closed+category):
```
A->B->C = A -> (B->C) ~ A -> (1 -> C^B) ~ A -> C^B ~ A x B -> C
```

If `f` is a function then we may define `g = f 2`. This allows use to define simple data structures.

Bang patterns
```haskell
{-# LANGUAGE BangPatterns #-}
...
!xs = xs -- the ! require evaluation of the value
...
```

Some base:
```haskell
map func []
foldl func def_value [] --reduce
foldl (+) 0 [1,2,3,4]
```

# Lecture 2 28.02.19

