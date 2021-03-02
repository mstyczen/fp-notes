# Lecture 7
### Lecture plan
* Working with Haskell's IO
* The `Applicative` type class

## IO
### Recap: Purity
A function is called pure if its behavior is fully described by its inputs and outputs.
* no mutable state
* no input or output (text, file system, network, ...)
* No random or non-deterministic choices
All Haskell functions (of type `a->b`) are pure functions.

**Question:** How to write Haskell programs that interact with the world?
**Answer:** Use the builtin Haskell type `IO a`!

### What is `IO`?
`IO a` is the type of programs that interact wit h the world and return a value of type a.

**Examples:**
```haskell
putStrLn "Hello" :: IO ()
getLine :: IO String 
```

Unlike builtin types such as lists or tuples, we cannot give a definition of `IO` ourselves: it is built into Haskell.

### Performing IO actions
An expression of type `IO a` is called an action.

Actions can be passed around and returned like any Haskell type, but they are not performed except in specific cases:
* `main :: IO ()` is performed when the whole program is executed
* GHCi will also perform any action it is given
* Other actions are only performed when called by another actions

### Separating the pure from the impure
Haskell programs are separated into a pure part (that does not use `IO`) and an impure (or effectful) part that does use `IO`.

We can convert a pure value of type `a` to an impure one of type `IO a`, but not the other way. Once a value is in `IO`, it is stuck there~

**Advice**: Most haskell program should only use `IO` in a small part of the program (~10%).

### `do` notation
Haskell has a special syntax for writing `IO` programs, known as `do` notation:
```haskell
main :: IO ()
main = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn("Hello, " ++ name ++ "!")
```
### Anatomy of a `do` block
* Each `vi <- ai` is a generator with an action `ai :: IO bi`.
* The result `vi` of each generator can be used as a pure value of type `bi` in the rest of the do block.
* The final line must be an `IO` action of type `IO a` (not a generator).
```haskell
f :: IO a
f = do
    v1 <- a1
    ...
    vn <- an
    g v1 ... vn
```

### The function `return`
The function `return :: a -> IO a` turns a pure value into an `IO` action. It is often used at the end of a do block:
```haskell
f :: a -> IO (b,c)
f x = do
    y <- g x
    z <- h x
    return (y,z)
```

**Warning:** The `return` function is unrelated to `return` in imperative languages. A `return` in the middle of a `do` block does not terminate the function:
```haskell
main = do
putStr "Spam"
return ()
putStr " and eggs"
return ()
-- Output: Spam and eggs
```

### Conditional actions with `when`
`when :: Bool -> IO () -> IO()` executes an action only when the condition is `True`:
```haskell
main = do
    putStrLn "Pick a password:"
    pwd <- getLine
    when (length pwd < 6) $
        putStrLn "Warning: too short"
```

### Running a list of actions with `sequence`
`sequence :: [IO a] -> IO [a]` runs a list of `IO` actions and collect the results.

```haskell
main = do
    putStrLn "Pick three colors"
    colors <- sequence
        [getLine, getLine, getLine]
    putStrLn "The sorted colors are:"
    sequence
        (map putStrLn (sort colors))
```

### Mapping actions with `mapM` and `forM`
The function `mapM` combines `map` and `sequence`:
```haskell
mapM :: (a -> IO b) -> [a] -> IO [b]
mapM f xs = sequence (map f xs)
```

The function `forM` is `mapM` with its arguments flip:
```haskell
forM :: [a] -> (a -> IO b) -> IO [b]
forM = flip mapM
```

### Example: generating multiplication table:
```haskell
pad :: Int -> String -> String
    pad n s
    | length s < n =
        let k = n - length s
        in replicate k ' ' ++ s
    | otherwise = s

main = do
    forM [1..10] $ \k -> do
        forM [1..10] $ \l ->
            putStr (pad 4 (show (k*l)))
        putStrLn ""
    return () 
```

### `IO` primitives: terminal I/O:
```haskell
putChar :: Char -> IO ()
putStr :: String -> IO ()
putStrLn :: String -> IO ()
print :: Show a => a -> IO ()
getChar :: IO Char
getLine :: IO String
readLn :: Read a => IO a
```

### `IO` primitives: reding and writing files:
```haskell
data IOMode = ReadMode
    | WriteMode
    | ReadWriteMode
    | AppendMode

openFile :: FilePath -> IOMode -> IO Handle
hPutStrLn :: Handle -> String -> IO ()
hGetLine :: Handle -> IO String
hIsEOF :: Handle -> IO Bool
hClose :: Handle -> IO ()
```

### Other things you can do with `IO`:
* generate random numbers
* read and write mutable variables (`IORef`)
* throw and catch IO exceptions
* write graphics on the display
* listen to keyboard and mouse events
* communicate over the network
* start other processes
* ...

Basically: anything that's not a pure function

### The hidden backdoor: `unsafePerformIO`
The module `System.IO.Unsafe` provides a function `unsafePerformIO :: IO a -> a`  that allows executing an `IO` action inside a pure function.

**Warning (!!)**: Due to laziness, predicting if and when the action will be executed is very hard!

Only use `unsafePerformIO` for debugging or better understanding of laziness, but avoid at all costs in real programs.

### Tracing Haskell functions
A common use of unsafe IO is the `trace` function:
```haskell
trace :: String -> a -> a
trace string expr =
    unsafePerformIO $ do
        traceIO string
        return expr
```

By adding `trace` to your functions, you can get a better understanding of how Haskell functions are evaluated at runtime.

## Functors
### Recap: type classes
A type class defines a family of types that share a common interface, for example `Show`, `Eq`, `Ord`, `Num`, `Semigroup`, `Monoid`.

### The `Functor` type class
The function `map :: (a->b) -> [a] -> [b]` applies a function every element in a list.

`Functor` is a family of type constructors that have a `map`-like function, called `fmap`:
```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```
We often think of a functor as a container storing elements of some type `a`.

### Example of functors
```haskell
instance Functor [] where
    fmap f xs = map f xs

instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node l r) =
    Node (fmap f l) (fmap f r) 
```

### `IO` is a functor
TODO
`fmap f act` applies the pure function `f` to the result of the `IO a` action `act`, producing a new `IO b` action:
```haskell
    instance Functor IO where
    -- fmap :: (a -> b) -> IO a -> IO b
    fmap f act = do
        x <- act
        return (f x)
```
So we can view `act :: IO a` as a 'container' holding possible output values of type 'a'.

### `Either a` is a functor
```haskell
instance Functor (Either a)
    -- fmap :: (b -> c)
    -- -> Either a b
    -- -> Either a c
    fmap f (Left x) = Left x
    fmap f (Right y) = Right (f y)
```

### `(->) a` is a functor
**Reminder:** `(->) a b` is the same as `a -> b`.
```haskell
instance Functor ((->) a)
    -- fmap :: (b -> c)
    -- -> (a -> b)
    -- -> (a -> c)
    fmap f g = f . g

```

## The functor laws
`fmap f` applies `f` to each value stored in the container, but should leave the structure of the container unchanged. This is expressed formally by the functor laws:
* `fmap id = id`
* `fmap (g . h) = fmap g . fmap h`

### A bogus instance of `Functor`
```haskell
instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node l r) = Node (fmap f r) (fmap f l)
```
This does not satisfy the law `fmap id = id`:
```haskell
fmap id (Node (Leaf 1) (Leaf 2))
= Node (Leaf 2) (Leaf 1)
= id (Node (Leaf 1) (Leaf 2))
```

**Question:** Can you think of a type constructor that can not be made into an instance of `Functor`?

**Answer:** Here is an example:
```haskell
newtype Endo a = Endo (a -> a)
instance Functor Endo where
    fmap f (Endo g) = Endo ???
```
More generally, if the type parameter `a` occurs to the left of a function arrow, the type cannot be made into a `Functor`.

## Applicative functors
`Applicative` is a subclass of `Functor` that adds two new operations `pure` and `<*>`
(pronounced as 'ap' or 'zap').

### The function `pure`
The function `pure :: a -> f a` takes a value of type a and creates a container filled with one or more copies of this value:
* For `Maybe : pure x = Just x`
* For `lists: pure x = [x]`
* For `Tree : pure x = Leaf x`
* For `IO : pure x = return x`
* For `Either a : pure x = Right x`
* For `(->) a : pure x = const x`

### The function `(<*>)`
The function `(<*>) :: f (a - > b) -> f a -> f b` combines two containers by applying functions in the first to values in the second one.

**Example:** for the `Maybe` functor we have:
```haskell
Just f <*> Just x = Just (f x)
Nothing <*> _ = Nothing
_ <*> Nothing = Nothing
```

### Combining containers
We can combine two applicative containers by
chaining `pure` and `(<*>)`:
```haskell
zipA :: Applicative f => f a -> f b -> f (a,b)
zipA xs ys = pure (,) <*> xs <*> ys
```

**Examples:** 
```haskell
zipA (Just 1) (Just 2) = Just (1,2)
zipA (Just 1) Nothing = Nothing
```

### Applicative instance for lists:
For lists, the function `(<*>)` iterates over all
possible combinations of functions and values:
```haskell
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
```

**Example:**
```haskell
zipA [1,2] ['a','b'] = [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```
Remark: This is not the only way to make lists into an applicative functor (see Weblab).

### Applicative instance for `IO`
```haskell
instance Applicative IO where
    pure x = return x
    mf <*> mx = do
        f <- mf
        x <- mx
        return (f x)
```

## Applicative laws
### The four laws of `Applicative`
```haskell
pure id <*> x = x
pure (f x) = pure f <*> pure x
mf <*> pure y = pure (\g -> g y) <*> mf
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
```

## Effectful programming with applicative functors
### Containers vs effects
Until now, we have viewed (applicative) functors as containers holding values of some type `a`. Another way to view them is as an effect that may occur when computing the result `a`.

**Examples:**
* `Maybe` captures the effect of possible failure (`Nothing`)
* `IO` captures the effect of interacting with the outside world

### Non-deterministic programming with lists
We can view `[a]` as a non-deterministic computation of type `a`:
```haskell
> pure (+1) <*> [1,2,3]
[2,3,4]

> pure (*) <*> [1,2] <*> [3,4]
[3,4,6,8]

> [(+),(-),(*)] <*> [0,10] <*> [1,2,3]
[1,2,3,11,12,13,-1,-2,-3,9,8,7,0,0,0,10,20,30]
```

### Generic operation for effectful programming
```haskell
-- same function as for IO
when :: Applicative f => Bool -> f () -> f ()

-- generalization of sequence :: [IO a] -> IO [a]
sequenceA :: Applicative f => [f a] -> f [a]

-- generalization of mapM :: (a -> IO b) -> [a] -> IO [b]
traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
```