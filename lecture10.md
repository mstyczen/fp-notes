# Lecture 10
### Lecture plan
* laziness recap
* infinite lists
* modular programming with infinite data
* case study: doubly-linked lists
* case study: tree labeling
* case study: lazy IO
* JQ project FAQ

### Lazy evaluation in a nutshell
Under lazy evaluation programs are evaluated at most once and only as far as needed.

### Advantages of lazy evaluation
* it always terminates if possible
* it takes the smallest number of steps of all strategies
* it allows us to work with infinite data structures
* it results in code that is modular and easy to refactor

### Pitfalls of lazy evaluation
* creation and management of thunks has some runtime overhead
* it can lead to a drastic increase in memory usage for some programs
* it becomes much harder to predict the order of evaluation

### Lazy vs strict evaluation
Lazy evaluation is often more efficient when working with small expressions that produce big data structures.

Strict evaluation is often more efficient when working with big expressions that produce small data structures.

**Example:** `foldl' (+) 0 [1..1000]`
* Second argument becomes big expression but computes to small `Int` - strict is better
* Third argument is small expression but computes to long `[Int]` - lazy is better

## Infinite data structures
### An infinite list
```haskell
ones :: [Int]
ones = 1 : ones

-- ones --> 1 : ones
        --> 1 : (1 : ones)
        --> ...

-- List is infinite, but with lazy evaluation we can still evaluate the head function:
-- head ones --> head (1 : ones) --> 1
```

### Infinite data structures
An infinite dat a structure is an expression that would contain an infinite number of constructors if it is fully evaluated.

**Intuition**: An infinite list is a stream of data that produces as much elements as required
by its context.

### Syntactic sugar for infinite lists
`[m..]` denotes the list of all integers starting from m:
```haskell
> [1..]
[1,2,3,4,5,6,7,{Interrupted}
> zip [1..] "hallo"
[(1,'h'),(2,'a'),(3,'l'),(4,'l'),(5,'o')]
```

In fact, `[m..]` is syntactic sugar for `enumFrom m`:
```haskell
enumFrom :: (Enum a) => a -> [a]
```

### Functions for constructing infinite lists:
```haskell
repeat :: a -> [a]
repeat x = xs
    where xs = x : xs

cycle :: [a] -> [a]
cycle xs = xs'
    where xs' = xs ++ xs'

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
```

### Different ways to create infinite lists
**Question:** is there a difference between these functions?
```haskell
repeat x = xs
    where xs = x : xs

repeat' x = x : repeat' x
```

**Answer:** The functional behavior is the same, but memory usage is very different:
* `xs = x : xs` creates a cyclic list: a single thunk that refers to itself
* `repeat x = x : repeat xs` creates an infinite list: one thunk for each requested element

### Filtering infinite lists
**Warning**: Filtering infinte list will loop forever, even if the result is finite:
```haskell
> filter (<5) [1..]
[1,2,3,4,<loop>
```
Instead, use `takeWhile` to get an initial fragment of an infinite list:
```haskell
> takeWhile (<5) [1..]
[1,2,3,4]
```

### Merging infinite lists:
**Exercise**: Compute the infinite ascending list of all multiples of 2 and 3, but not (2 or 3 themselves)

```haskell
multOf2 = map (*2) [2..]
multOf3 = map (*3) [3..]

merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | x == y = x : merge xs ys
    | otherwise = y : merge (x:xs) ys

multOf2Or3 = merge multOf2 multOf3
```
Alternatively, you could use `filter` for this step.

**Exercise**: Compute the infinite ascending list of all multiples of all numbers starting from 2.

```haskell
multiples = [map (*n) [n..] | n <- [2..]]

merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | x == y = x : merge xs ys
    | otherwise = y : merge (x:xs) ys
xmerge(x:xs) ys = x : merge xs ys

mergeAll (xs:xss) = xmerge xs (mergeAll xss)
composites = mergeAll multiples
```

### Other infinite data structures
Any (recursive) datatype in Haskell can have infinite structure.
TODO

## Modular programming
### Separating data and control
With infinite data structure we can define what we want to compute (the data) independently of how it will be used (the control flow).

We can get the data we need for each situation by applying the right function to the infinite list: `take`, `!!`, `takeWhile`, `dropWhile`, ...

### Example: prime numbers
```haskell
sieve (x:xs) =
    let xs' = [y | y <- xs, y `mod` x /= 0]
    in x : sieve xs'

primes = sieve [2..]
```
```
> take 10 primes
[2,3,5,7,11,13,17,19,23,29]
> primes !! 10000
104743
> head (dropWhile (<2021) primes)
2027
```

### Another way to calculate primes
```haskell
(\\) :: Ord a => [a] -> [a] -> [a]
[] \\ _ = []
xs \\ [] = xs
(x:xs) \\ (y:ys)
    | x < y = x : (xs \\ (y:ys))
    | x == y = xs \\ ys
    | otherwise = (x:xs) \\ ys
primesV2 :: [Integer]
primesV2 = [2..] \\ composites
```
Unfortunately, this is slower than what we proposed before...

### Calculating primes without double work:
We can avoid a lot of work by only considering multiples of prime numbers in the calculation of `composites`:
```haskell
primesV3 = [2..] \\ composites
    where
        composites = mergeAll primeMultiples
        primeMultiples = [ map (p*) [p..] | p <- primesV3 ]
```
But this does not terminate!

### Starting the prime engine
To get the recursion started, we need to specify that 2 is the first prime number:
```haskell
primesV3 = 2 : ([3..] \\ composites)
    where
        composites = mergeAll primeMultiples
        primeMultiples = [ map (p*) [p..] | p <- primesV3 ]
```

## Case study: doubly-linked lists
### Cyclic data structures:
In most programming languages, data structures are often cyclic: they contain references for traversing the structures in any direction.

In contrast, Haskell data types are acyclic: we can only go from the root of a tree to its children, but not vice versa.

Cyclic structures typically require `null`-pointers to initialize, but in Haskell we can achieve the same thing by using laziness.

### Example: a doubly-linked list
Our goal is to create a book with 3 pages, where we can go forward an backward:
```haskell
> book
"Page 1"
> next book
"Page 2"
> prev (next book)
"Page 1"
> next (next book)
"Page 3"
```

### Doubly linked lists
```haskell
data DLL a = Node { elem :: a, mPrev :: Maybe (DLL a), mNext :: Maybe (DLL a) }
prev :: DLL a -> DLL a
prev n = case mPrev n of
    Just x -> x
    Nothing -> error "no previous item!"

next :: DLL a -> DLL a
next n = case mNext n of
    Just x -> x
    Nothing -> error "no next item!"

instance Show a => Show (DLL a) where
    show (Node x _ _) = show x

book = p1
    where p1 = Node "Page 1" Nothing (Just p2)
        p2 = Node "Page 2" (Just p1) (Just p3)
        p3 = Node "Page 3" (Just p2) Nothing
```

### Converting a doubly-linked list to a list
```haskell
listFromDLL :: DLL a -> [a]
listFromDLL (Node x _ mnext) = case mnext of
    Nothing -> [x]
    (Just xs) -> x : listFromDLL xs
```

### Converting a list to a doubly-linked list
```haskell
listToDLL' :: [a] -> Maybe (DLL a) -> DLL a
listToDLL' [] prev = error "empty list"
listToDLL' [x] prev = Node x prev Nothing
listToDLL' (x:xs) prev =
    let this = Node x prev (Just next)
        next = listToDLL' xs (Just this)
    in this

listToDLL :: [a] -> DLL a
listToDLL xs = listToDLL' xs Nothing
```

## Case study: tree labeling
### The tree labeling problem
Datatype of labeled trees reminder:
```haskell
data Tree a = Leaf | Node (Tree a) a (Tree a)
```
**Assignment**: Given a tree and an infinite list of labels `xs :: [Int]`, define a function `label :: [Int] -> Tree a -> Tree (Int, a)` that labels the tree with `xs`, using each label at most once.

**Question**: Does this definition work for infinite trees? If not, how we change it so it does?

## Case study: Lazy IO
### Recap: basic IO actions
```haskell
putChar :: Char -> IO ()
putStr :: String -> IO ()
putStrLn :: String -> IO ()
print :: Show a => a -> IO ()
getChar :: IO Char
getLine :: IO String
readLn :: Read a => IO a
```

### The `getContents` function
The function `getContents :: IO String` reads everything from standard input until it encounters an end-of-file character. `getContents` produces its output lazily, so we can already use the input before it is complete.

### Example: REPEAT IN ALL CAPS
```haskell
import Data.Char (toUpper)
main = do
    input <- getContents
    putStr (map toUpper import)

> stack runghc AllCaps.hs
hey ho
HEY HO
lets go
LETS GO
```

### Thunks for `IO`  actions:
In the program:
```haskell
main = do
    input <- getContents
    putStr (map toUpper import)
```
the variable `input` is not an actual string, but a thunk (or promise) that will read (or wait for) input when it is forced.

### Example: checking palindromes
```haskell
checkPalindrome s
    | s == reverse s = "palindrome!"
    | otherwise = "not a palindrome..."
main = do
    input <- getContents
    putStr  $ unlines
            $ map checkPalindrome
            $ lines input
```

### The `interact` function
The patter used in the last example is so common, that the prelude has a function for it:
```haskell
interact :: ([String] -> [String]) -> IO ()
```
With it, we can rewrite the palindrome checker as follows:
```haskell
checkPalindrome s
    | s == reverse s = "palindrome!"
    | otherwise = "not a palindrome..."
main = interact (map checkPalindrome)
```