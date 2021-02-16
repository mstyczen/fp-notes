# Lecture 3 #
### Lecture topics: ###
* pattern matching and recursion
* higher-order functions
* `if ... then ... else`, `let ... in`, `case ... of`, lambdas, sections
* sample exam questions

### Restrictions of Haskell ###
* No mutable variables
* No loops
* No `try`/`catch` blocks
* No side effects
* No objects

How do we even write programs in such a language?

## Pattern matching ##
### Anatomy of a Haskell function ###
```
doubleSmallNumber :: Int -> Int
doubleSmallNumber x = 
    if isSmall x then 2*x else x
    where isSmall x = x < 10
```
* each definition has (optional) **type signature**: `f :: ...`
* definition consists of one or more **clauses**: `f x = ...`
* Auxiliary definitions can appear in a `where` block

### Reminder: the layout rule ###
**Layout rule**: within a block, all definitions must start at the exact same column.
Blocks start with keywords `where`, `let`, `case`, and `do`.

### Pattern matching - introduction ###
We can define functions by case analysis using **pattern matching**:
```
not :: Bool -> Bool
not True = False
not False = True
```
Pattern matching is one of the most powerful and useful features of Haskell.
 ### Matching on numbers and characters ###
 We can match on **numbers**:
 ```
 int2string :: Int -> String
 int2string 1 = "one"
 int2string 2 = "two"
 int2string n = "many"
 ```
 We can also match on **characters**:
 ```
 hasDot :: Char -> Bool
 hasDot 'i' = True
 hasDot 'j' = True
 hasDot chr = False
 ```
 

 ### Matching on multiple arguments ###
 ```
 xor :: Nool -> Bool -> Bool
 xor True False = True
 xor False True = True
 xor _ _ = False
 ```
 You can match on several arguments at once. A **wildcard** `_` matches anything.

 ### Order of classes ###
 ```
 f True _ = 1
 f _ True = 2
 f _ _ = 3
 ```
 vs
 ```
 f _ True = 2
 f True _ = 1
 f _ _ = 3
 ```
 Does `f True True` evaluate to `1` or `2`?
 In the first case, it will evaluate to `1`, in the second case to `2`. It is an example of the order influencing the evaluation -  the first matching clause will be executed.

### Exercise - definition of `(&&)`###
Redefine the library function `(&&) :: Bool -> Bool -> Bool`:
```
True && True = True
_ && _ = False
```
alternatively:
```
True && b = b
False && _ = False
```
**Question**: is there any difference in practice?
The latter version is **lazy** - it will not look at the value of the second argument.

### Pattern matching on tuples ###
An example of defining a function of vector addition over the 2D space.
```
addVectors :: (Int, Int) -> (Int, Int) -> (Int, Int)
addVectors (x,y) (z,w) = (x+z, y+w)
```
An example of `fst`, `snd`, `trd` definitions for 3-tuples.
```
fst3 (x,y,z) = x
snd3 (x,y,z) = y
trd (x,y,z) = z
```

### Pattern matching on lists ###
```
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty (x:xs) = False
```
* Any list is either `[]` or `x:xs`.
* `[1,2,3]` is syntactic sugar for `1:2:3:[]`
* The type of `(:)` is `a-> [a] -> [a]`.  

### Incomplete matches ###
```
head :: [a] -> a
head (x:xs) = x
```
What happens when there's no clause?
```
> head []
*** Exception: Non-exhaustive patterns in function head
```
You can enable `=fwarn-incomplete-patterns`, to warn you for incomplete functions.
```
-- At top of the file
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
```

### Using guards ###
We can use **guards** to restrict when a clause is applied:
```
signum :: Int -> Int
signum x | x < 0 = -1
         | x == 0 = 0
         | otherwise = 1
```
* Guards `| b` are expressions of type `Bool`
* `otherwise` is simply defined as `True`

### Mixing guards and pattern matching ###
```
capitalize :: String -> String
capitalize (c : cs)
    | isLower c = (toUpper c) : cs
capitalize cs = cs
```

## Recursion ##
### Example: factorial ###
```
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)
```
**Question:** what happens if `n < 0`?
It will apply the last case, and will result in infinite loop. You could avoid this but adding a guard in the last case, checking if `n > 0`.

### Why use recursion? ###
* Often it is the most natural way to write functional programs
* Recursion + list comprehensions completely remove the need for traditional loops
* We can prove properties of recursive function by induction

### Recursion on lists ###
Recursion is not limited to numbers - we can also recurse over structured data such as lists:
Examples:
* a `product` function calculating the product of all elements of a list
```
product :: Num a => [a] -> a 
product [] = `
product (x:xs) = x * product xs
```
* a `zip` function, that zips two lists together.
```
zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : (zip xs ys)
zip _ _ = []
```
### Writing recursive functions ###
1. Write down the type
2. Enumearte the cases
3. Define base case
4. Define recursive cases
5. Generalize/simplify/join cases

### Example: Insertion sort:
Insertion sort definition using recursion and guards.
```
isort :: [Int] -> [int]
isort [] = []
isort (x:xs) = insert x (isort xs)
    where
        insert :: Int -> [Int] -> [Int]
        insert x [] = [x]
        insert x (y:ys)
            | x <= y = x:y:ys
            | otherwise y:(insert x ys)
```

## Higher-order functions ##
### The DRY principle of programming ###
**DRY** stands for **Don't Repeat Yourself**.

"Every piece of knowledge must have a single, unambiguous, authoritative representation with a system."

From *The pragmatic programmer* by Hunt & Thomas.
**Higher-order functions** are the ultimate expression of DRY, as they allow you to abstract over programming patterns.

### Higher order functions - introduction ###
A higher order function is a function that either takes a funciton as an argument or returns a function as a result. The latter can also be called a **curried function**.

Example - a function `twice`, that takes a function and an argument applies the function twice.
```
twice :: (a -> a) -> a -> a
twice f x = f f x
```
Example output:
```
> twice (*2) 3
12
> twice reverse [1,2,3]
[1,2,3]
```
### Higher order function: `map` ###
```
> :t map
map :: (a -> b) -> [a] -> [b]
> map (+1) [1,3,5,7]
[2,4,6,8]
```
`map f xs` corresponds to the list comprehensions [f x | x <- xs].

### Higher order function: `filter` ###
```
> :t filter
filter :: (a -> Bool) -> [a] -> [a]
> filter even [1..8]
[2,4,6,8]
```
`filter p xs` corresponds to the list comprehensions [x | x <- xs, p x]

### Three ways to write a program ###
1. Using list comprehension
```
result = [f x | x <- xs, p x]
```
2. Using pattern matching + recursion
```
result = aux xs
    where
        aux [] = []
        aux (x:xs) | p x = f x : aux xs
                   | otherwise = aux xs
```
3. Using higher-order functions
```
result = map f (filter p xs)
```

### Higher-order functions: `all` and `any` ###
```
> :t all
all :: Foldable t => (a -> Bool) -> t a -> Bool
> :t +d all
all :: (a -> Bool) -> [a] -> Bool
> :t +d any
any :: (a -> Bool) -> [a] -> Bool
> import Data.Char (isSpace)
> any isSpace "Hello, world!"
True
```
Note: `:t + d` returns a specific type.

### Higher-order functions: takeWhile and dropWhile ###
`takeWhile` takes all the elements that satisfy the predicate until the first non-matching element is found. `dropWhile` works similarly, but it drops the elements until it finds the first non-matching element.
```
> :t takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
> :t dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
> dropWhile isSpace " Hello, world!"
"Hello, world!"
```

### Live programming problems ###
Define:
1. `applys` - apply a sequence of functions to an array
```
applys :: [a->b] -> [a] -> [b]`
applys fs xs = map app (zip fs xs)
    where
        app (f,x) = f x
``` 
alternatively:
```
apply (f:fs) (x:xs) = f x : (applys fs xs)
apply _ _ = []
```

2. `flip` - takes a function that takes two arguments, and flip the order of the arguments
```
flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \a b -> f b a
``` 

3. `first`
```
first :: (a -> b) -> (a,b) -> (b,c)
first f (x,y) = (f x, y)
```

4. `second`
```
second :: (a -> b) -> (a,b) -> (b,c)
second f (x,y) = (x, f y)
```

5. `(***)`
```
(***) : (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f *** g) (x,y) = (f x, g y)
```

## An assortment of Haskell syntax ##
### Conditional expressions ###
Conditionals can be expressed with `if ... then ... else ...`
* `else` branch is required
* `if ... then ... else ...` can be nested
```
abs :: Int -> Int
abs x = if x < 0 then -x else x
signum :: Int -> Int
signum x =
    if x < 0 then -1 else
        if x == 0 then 0 else 1
```

### Let bindings ###
We can give a name to a subexpression using a **let binding**. Example:
```
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea
```
### `let` vs `where` ###
Two major differences between `let` and `where`.
* `let` defines variables before they are used, `where` defines variables after they are used
* `let ... in ...` can appear anywhere in an expression, `where` is always atttached to a clause.

### Lambda expressions ###
We can define functions anonymously using **lambda expressions** (syntax: `\x -> ...`):
```
add x y = x + y
add` x = \y -> x + y
add`` = \x -> (\y -> x + y)
add``` = \x y -> x + y
```
Constant function:
```
const :: b -> a -> b
const x = \_ -> x
```
Produces a function that ignores its argument. Example use case - replacing all the values in a list, you can mape them with a const function.

### Using `map`/`filter` with lambdas ###
Example: you can replace this:
```
> let f x = x*2+1 in map f [1..5]
[3,5,7,9,11]
```
with this:
```
> map (\x -> x*2+1) [1..5]
[3,5,7,9,11]
```

### Operator sections ###
An operator sections is an operator that has been partially applied. Examples:
```
> :t (+1)
(+1) :: Num a => a -> a
> map (+1) [1..5]
[2,3,4,5,6]
```
and
```
> :t (>5)
(>5) :: (Ord a, Num a) => a -> a
> filter (>5) [1..10]
[6,7,8,9,10]
```

### Case expressions ###
We can pattern match on a value using `case ... of ...`.
Example:
```
describeList :: [a] -> String
describeList xs =
    "The list is " ++
    case xs of [] -> "empty."
        [x] -> "a singleton list."
        xs -> "a longer list."
```

## Sample exam questions ##
### Sample question #1 ###
Define the following functions in three different styles: using a list comprehension, using pattern matching and recursion, and using higher order functions:
* `allCaps :: String -> String`
* `intersect :: Eq a => [a] -> [a] -> [a]`
* `allPairs :: [a] -> [b] -> [(a,b)]`

Solution for the `allCaps` function:
```
import Data.Char (isLower, toUpper)

allCaps xs = [toUpper x | x <- xs]

allCaps' [] = []
allCaps' (x:xs) = toUpper x : (allCaps' xs)

allCaps'' xs = map toUpper xs
```
The last one can be simplified to `allCaps'' xs = map toUpper`, then returning a partially applied map.

### Sample question #2 ###
Someone who is just learning Haskell tells you the following statement:

"The `foldr` function encampsulates all possible recursive functions on lists".

Do you agree or not? Explain your answer and illustrate with at least two examples or counterexamples that do not appear in chapter 7 of the book.

### Sample question #3 ###
Implement a function: `giveChange :: Int -> [Int] -> [Int]`, such that `giveChange total coins` returns the shortest sublist `coins'` of `coins` such that `sum coins' = total`. You may assume that both `total` and all `coints` are greater than `0`, and that there is at least one possible solution.

Solution hint: one possible way to solve this is to generate all the possible solutions and then sort by their length. The first element will be the shortest list.