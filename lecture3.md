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

