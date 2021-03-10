# Lecture 9
### Lecture plan
* Monads recap + quiz
* Demo of the `Parser` monad
* Evaluation strategies: *call-by-value* vs *call-by-name*
* Termination and non-termination
* Lazy evaluation
* Forcing strictness

### Evaluation sequences:
**Question:** What are the possible ways to evaluate `(\x -> x * x) (2 + 2)`?

**Answer:** Here are two options:
```haskell
(\x -> x * x) (2 + 2)
--> (\x -> x * x) 4
--> 4 * 4
--> 16

(\x -> x * x) (2 + 2)
--> (2 + 2) * (2 + 2)
--> 4 * (2 + 2)
--> 4 * 4
--> 16
```

**Question:** What are the possible ways to evaluate `(\x -> 42) (2+2)`?

**Answer:** Here are two options:
```haskell
-- first evaluate the argument
(\x -> 42) (2 + 2)
--> (\x -> 42) 4
--> 42

-- immediately calculate the function
(\x -> 42) (2 + 2)
--> 42
```

**Question:**
Take: 
```haskell
f :: Bool -> Bool
f b = b || f b
```
What are the possible ways to evaluate `f True`?

**Answer:** Here are two options:
```haskell
f True
True || f True
True || (True || f True)
...
--or:
f True
--> True || f True
--> True
```

### Choosing between evaluation sequences
There are many ways to evaluate an expression. Questions:
* Do they all lead to the same result?
* Is this different for different programming languages?
* Do they all terminate after a finite number of steps?
* Which ones uses the least number of steps?
* Which one has the smallest intermediate terms?

## Evaluation strategies
### Reducible expressions
A reducible expression, or **redex**, is a function application that can be 'reduced'.

**Example:** `(\x -> (\y -> x * y)) (1+2) (3+4)` has three redexes. Possible reductions:
```haskell
(\x -> \y -> x * y) 3 (3+4)

(\x -> \y -> x * y) (1+2) 7

(\y -> (1+2) * y) (3+4)
```
**Note:** reducing does not necessarily mean making the expression smaller!

### Evaluation strategies
An evaluation strategy gives a general way to pick a redex to reduce next:
* innermost reduction: pick a redex that contains no other redex
* outermost reduction: pick a redex  that is contained in another redex

**Note**: There are many other evaluation strategies 'in between'.

### Example of innermost reduction
```haskell
(\x -> \y -> x * y) (1+2) (3+4)
--> (\x -> \y -> x * y) 3 (3+4)
--> (\y -> 3 * y) (3+4)
--> (\y -> 3 * y) 7
--> 3 * 7
--> 21
```

### Example of outermost reduction
```haskell
(\x -> \y -> x * y) (1+2) (3+4)
--> (\y -> (1+2) * y) (3+4)
--> (1+2) * (3+4)
--> 3 * (3+4)
--> 3 * 7
--> 21
```

### No reduction under lambdas

Haskell treats a lambda expression such as `\x -> 1+2` as a black box: it is not allowed to
‘look inside’, only to apply it. Hence selecting redexes inside a lambda is not allowed:

```haskell
(\x -> 1 + 2) 0
--> 1 + 2
--> 0
```

### Call-by-name and call-by-value
Innermost reduction that does not evaluate under lambdas is called call-by-value:
```haskell
(\x -> 1 + 2) (3 + 4)
--> (\x -> 1 + 2) 7
--> 1 + 2
--> 0
```

Outermost reduction that does not evaluate under lambdas is called call-by-name:
```haskell
(\x -> 1 + 2) --> (no reduction)
```

## Lazy evaluation
### Non-terminating programs
Some programs will go into an infinite loop with any evaluation strategy:
```haskell
inf :: Integer
inf = 1 + inf

inf
--> 1 + inf
--> 1 + 1 + inf
--> 1 + 1 + 1 + inf
--> ...
```

Other programs will go into an infinite loop with call by value, but not with call by name:
```haskell
-- with call-by-value
fst (0, inf)
--> fst (0, 1 + inf)
--> fst (0, 1 + 1 + inf)
--> ...
-- with call-by-name
fst (0, inf)
--> 0
```

### Counting evaluation steps:
For functions that don't always use their arguments, call-by-value will do useless work:
```haskell
(\x -> 42) (2+3)
--> (\x -> 42) 5 --> 42
```

For functions that use their arguments more than once, call by name will do useless work:
```haskell
(\x -> x * x) (2+3)
--> (2+3) * (2+3)
--> 5 * (2+3) --> 5 * 5 --> 25
```

Can we get the best of both worlds? Yes!

### Lazy evaluation
Lazy evaluation (aka call-by-need) is a variant of call-by-name that avoids double evaluation. Each function argument is turned into a thunk:
* the first time the argument is used, the thunk is evaluated and the result is stored in the thunk
* the next time the value stored in the thunk is used

### Lazy evaluation in a nutshell
Under lazy evaluation, programs are evaluated at most once and only as far as needed.

### Example of lazy evaluation:
The current thunks are shown between []:
```haskell
(\x -> x * x) (1 + 2)
--> x * x [ x := 1 + 2 ]
--> 3 * x [ x := 3 ]
--> 3 * 3 [ x := 3 ]
--> 9 [ x := 3 ]
```

### Advantages of lazy evaluation
* it always terminates if possible
* it takes the smallest number of steps of all strategies
* it allows us to work with infinite data structures
* it results in code that is modular and easy to refactor

### Pitfalls of lazy evaluation
* creation and management of thunks has some runtime overhead
* it can lead to a drastic increase in memory usage for some programs
* it becomes much harder to predict the order of evaluation

### Lazy evaluation in other languages
Haskell is a lazy language: all evaluation is lazy by default.

Most other languages are eager (aka strict), but still have some form of lazy evaluation:
* Lazy *and*/*or* - `True && b` evaluates to `True` without evaluating `b` in most languages
* Iterators (e.g. Java) can produce values on demand
* Generator functions (e.g. Python) can use `yield` to lazily return values
* `lazy val` in Scala declares a value that is computed lazily

## Forcing evaluation
### Performance drawbacks of lazy evaluation
The number of steps is not the only thing that matters for performance - the size of intermediate steps is also important:
* For big expressions that evaluate to a small value, call-by-value is better: `foldl (+) 0 [1..1000000000]`
* For small expressions that evaluate to a large data structure, call-by-need is better: `replicate 1000000 "spam"`.

### Forcing evaluation
Haskell provides a built-in function `seq`:
```haskell
seq :: a -> b -> b
```
The expression `seq u v` will evaluate `u` to *head normal form* before returning `v`.
```haskell
(1+2) `seq` 5 --> 3 `seq` 5 --> 5
replicate 5 'c' `seq` 42
--> 'c':(replicate 4 'c') `seq` 42
--> 42
```

### Strict application
Using `seq`, we can define **strict application**:
```haskell
($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x
```
"Please evaluate `x` before applying `f`!"

```haskell
(\y -> y*y) $! (1+2)
--> x `seq` (\y -> y*y) x [ x := 1+2 ]
--> x `seq` (\y -> y*y) x [ x := 3 ]
--> (\y -> y*y) x [ x := 3 ]
--> x*x [ x := 3 ]
--> 9
```

### Forcing evaluation of multiple arguments
```haskell
-- force evaluation of first arg:
(f $! x) y
-- force evaluation of second arg:
(f x) $! y
-- force evaluation of both args:
(f $! x) $! y
```

### The problem with `foldl`
The function `foldl` has bad memory usage:
```haskell
foldl (+) 0 [1..100]
--> foldl (+) (0+1) [2..100]
--> foldl (+) ((0+1)+2) [3..100]
--> foldl (+) (((0+1)+2)+3) [4..100]
--> ...
```
To reduce the size of intermediate expressions, we need to be strict in the second argument, but lazy in the third.

### A strict version of `foldl`
The Prelude defines `foldl'` as follows:
```haskell
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' (#) v [] = v
foldl' (#) v (x:xs) =
((foldl' (#)) $! (v # x)) xs
```

We can evaluate `foldl' (+) 0 [1..1000000000]` without running out of memory.



