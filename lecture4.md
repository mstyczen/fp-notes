# Lecture 4 #
### Lecture plan ###
* More higher-order functions: `foldr`, `foldl`, `foldl'`
* Function composition `(.)`
* Property-based testing with **QuickCheck**

## The higher order function `foldr` ##
### A common pattern of recursion ###
```haskell
sum [] = 0
sum (x:xs) = x + sum xs

product [] = 1
product (x:xs) = x * product xs

or [] = False
or (b:bs) = b || or bs

and [] = True
and (b:bs) = b && and bs

```
**Don't repeat yourself!** - we can define a common function to handle all those similar recursive patterns.

Many recursive functions on lists follow the following pattern:
```haskell
f [] = v
f (x:xs) = x # f xs
```
`#` here stands for some combining function.

The higher-order function `foldr` encapsulates this pattern. Instead of the above, we can just write:
```haskell
f = foldr (#) v
```

### Examples of using `foldr` ###
```haskell
sum = foldr (+) 0
product = foldr (*) 1
or = foldr (||) False
and = foldr (&&) True
```

### What does `foldr` do? ###
**Intuition**: `foldr (#) v` replaces each occurrence of (:) by (#) and the final [] by v:
```haskell
foldr (+) 0 [x1, x2, x3] 
= foldr (+) 0 (x1 : (x2 : (x3 : [])))
= x1 + (x2 + (x3 + 0)) 
```
Note that parentheses are associated to the **right**, hence the **r** in `foldr`.

### Recursive definition of foldr ###
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr (#) v [] = v
foldr (#) v (x:xs) = x # (foldr (#) v xs)
```

### Folds in other languages ###
Folding functions exist in many other languages:
* Haskell : `foldr (+) 0 seq`
* Scala:   `seq.fold(0) ((a,b) => a + b)`
* Python: `reduce (lambda a,b: a+b, seq, 0)`
* ...

### More suspicious patterns ###
**Example 1 - `length`**:
```haskell
length [] = 0
length (x:xs) = 1 + length xs
```

We can rewrite this definition:
```haskell
length [] = 0
length (x:xs) = (\_ n -> 1 + n) x (length xs)
```

And we can notice it is equivalent to the following:
```haskell
length = foldr (\_ n -> 1+n) 0
```
And:
```haskell
length = foldr (const (1+)) 0
```
Notice we omit the `xs` parameter and we just return partially applied `foldr` function.

**Example 2 - `++`:**
```haskell
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
```
Can be rewritten to:
```haskell
[] ++ ys = ys
(x:xs) ++ ys = (:) x (xs ++ ys)
```
And to:
```haskell
xs ++ ys = foldr (:) ys xs
```

**Example 3 - `map`:**
```haskell
map f [] = []
map f (x:xs) = f x : map f xs
```
Can be rewritten to:
```haskell
map f [] = []
map f (x:xs) = (\x ys -> f x : ys) x (map f xs)
```
And then to:
```haskell
map f = foldr (\x ys -> f x : ys) []
```

**Example 4 - `reverse`**:
```haskell
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
```
Can be rewritten to:
```haskell
reverse [] = []
reverse (x:xs) = (\x xs -> xs ++ [x]) x (reverse xs)
```
And then to:
```haskell
reverse = foldr (\x xs -> xs ++ [x]) []
```

**Example 5 - `filter`**:
```haskell
filter p [] = []
filter p (x:xs) =
    if p x
    then x : filter p xs
    else filter p xs
```
Can be rewritten to
```haskell
filter p = foldr (\x xs' -> if p x then x:xs` else xs`) []
```

### Extra benefit of `foldr`: optimizations ###
Some advanced compiler optimizations are easier on programs with `foldr`:
* `foldr` fusion:
```haskell
foldr f v (map g xs)
==>
foldr (\x y -> f (g x) y) xs
```
The first version traverses the list twice, the second expression only goes over the list once.
* The *banana split* rule:
```haskell
(sum xs, length xs)
==>
foldr (\n (x,y) -> (n+x, 1+y)) (0,0)
```
If you have a tuple, where both functions are defined in terms of `foldr`, they can be merged into one `foldr`.

### From the book: Binary string transmitter ###
**Goal:** simulate transmission of a string of characters encoded as a list of binary numbers.
```haskell
bin2int :: [Int] -> Int
bin2int = foldr (\x y -> x + 2*y)
int2bin :: Int -> [Int]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)
```
See section 7.6 of the book for the full example.
## The higher-order functions `foldl` and `foldl'` ###
`foldl` is a version of `foldr` that associates to the **left**:
```haskell
foldl (+) 0 [x1,x2,x3]
==>
((0 + x1) + x2) + x3
```

The difference is significant for **non-associative operations** such as `(-)`:
```haskell
foldr (-) (0) [1,2,3] = 1 - (2 - (3 - 0)) = 2
foldl (-) (0) [1,2,3] = ((0 - 1) - 2) - 3 = -6
```

### Example of using `foldl`: ###
More efficient version of `reverse`:
```haskell
    reverse-acc :: [a] -> [a] -> [a]
    reverse-acc acc [] = acc
    reverse-acc acc (x:xs) = reverse0acc (x:acc) xs
    reverse xs = reverse-acc [] xs
```
Can be rewritten with `foldl`:
```haskell
reverse = foldl (\xs x -> x : xs) []
```

### Recursive definition of `foldl` ###
```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl (#) v [] = v
foldl (#) v (x:xs) = foldl (#) (v # x) xs
```
**Challenge: define `foldl` in terms of `foldr`.**

### The problem with `foldl` ###
Warning: the `foldl` function is notorious for causing performance problems, in particular (temporary) memory leaks.

The reason is that it is **too lazy**:

`foldl (#) v [x1..xn]` constructs the full expression `((v # x1) ...) # xn` before evaluating the first call to `(#)`.

The prelude provides a **strict version** `foldl'` with the same type, that is almost always more efficient.

## Function composition ##
### Definition and examples ###
```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)
```

Examples:
```haskell
odd = not . even
twice f = f . f
sumsqeven = sum . map (^2) . filter even
```

### Building pipelines with `(.)` ###
We can use (.) to compose functions without naming their arguments:
```haskell
processData = foldr combine init . map process . filter isValid
    where
        isValid = ...
        process = ...
        combine = ...
        init = ...
```

### The three laws of function composition ###
```haskell
id . f = f
f . id = f
f . (g .  h) = (f . g) . h 
```

## Property-based testing with QuickCheck ##
### Unit testing ###
Writing unit tests is **important**, but also:
* Boring, because you need a lot of unit tests
* Difficult, because it is very easy to miss cases

What if we could generate test cases automatically?

### Property-based testing ###
Instead of writing individual test cases, we can write down properties of our programs and generate test cases from those.

Example properties:
```haskell
abs (x-y) == abs (y - x)
reverse (reverse xs) == xs
isSorted (isort xs)
```

### Random testing of properties ###
To test a property, we can simply generate many inputs randomly and check if the property holds for all of them. 

Randomly testing the property `abs (x - y) == abs (y - x)`:
```haskell
abs (0.0 - 2.7) == abs (2.7 - 0.0)
abs ((-0.7) - (-0.9)) == abs ((-0.9) - (-0.7))
abs (6.5 - (-4.0)) == abs ((-4.0) - 6.5)
abs (2.0 - 7.7) == abs (7.7 - 2.0)
abs ((-19.0) - 2.8) == abs (2.8 - (-19.0))
...
```

### Advantages of property-based testing ###
* you spend less time writing tests: a single property replaces many tests
* you get better coverage: test lots of combination you'd never try by hand
* you spend less time on diagnosing errors: failing tests can be minimized - *shrinking* process allows us to produce a small counterexample

### The QuickCheck library for Haskell ###
QuickCheck is a Haskell library for writing property-based tests.

It was introduced in 1999 by Koen Claessen and John Hughes. 

It has been ported to many other languages: C, C++, Java, JavaScript, Python, Scala, ...

### Installing QuickCheck ###
In a Stack project, add the following to the list of dependencies in `package.yaml`:
```
- QuickCheck >= 2.14
```
Then, at the top of your Haskell file, import QuickCheck:
```haskell
import Test.QuickCheck
```
### Basic usage of QuickCheck ###
 ```haskell
import Test.QuickCheck

import Test.QuickCheck
dist :: Int -> Int
dist x y = abs (x - y)

prop_dist_self :: Int -> Bool
prop_dist_self x = dist x x == 0

prop_dist_sym :: Int -> Int -> Bool
prop_dist_sym x y = dist x y == dist y x

prop_dist_pos :: Int -> Int -> Bool
prop_dist_pos x y = dist x y > 0
 ```

### Running QuickCheck from GHCi ###
```
> quickCheck prop_dist_self
+++ OK, passed 100 tests.
> quickCheck prop_dist_sym
+++ OK, passed 100 tests.
> quickCheck prop_dist_pos
*** Failed! Falsified (after 1 test):
0
0
```

### Running QuickCheck tests in batch ###
Haskell code in `Distance.hs`:
```haskell
main = do
    quickCheck prop_dist_self
    quickCheck prop_dist_sym
    quickCheck prop_dist_pos
```
Console:
```
> stack runghc Distance.hs
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
*** Failed! Falsified (after 1 test):
0
0
```

### Anatomy of a QuickCheck test ###
```haskell
prop_qsort_isSorted :: [Int] -> Bool
prop_qsort_isSorted xs = isSorted(qsort xs)
```
* Name starts with `prop_` (convention, but required on Weblab)
* Type `[Int]` of argument must implement `Arbitrary` typeclass
* Return type must be `Bool` or `Property` (see later)

## Shrinking counterexamples ##
### Finding minimal counterexamples ###
If QuickCheck finds a counterexample, it will apply shrinking to find a counterexample that is as small as possible.

Consider the following false property, claiming that all lists are sorted:
```haskell
prop_all_sorted :: [Int] -> Bool
prop_all_sorted xs = isSorted xs
```
Running `quickCheck prop_all_sorted` will always return either `[1,0]` or `[0,-1]`

### Shrinking inputs ###
QuickCheck determines how to shrink a counterexample based on its type:
* `Int` : try number closer to 0
* `Bool` : try `False` instead of `True`
* `(a,b)` : shrink one of the components
* `[a]` : either shrink one value in the list, or delete a random element

## Discovering properties ##
### Roundtrip properties ###
A roundtrip property arises from applying two or more functions that together result in the original input. For example, reversing a list twice, or inserting an element to a list and removing it immediately.

```haskell
prop_reverse xs = reverse (reverse xs) == xs
prop_ins_del x xs = del x (ins x xs) == xs
```
General form: `f (g ( ... (h x))) == x`.

### Equivalent implementations ###
Two functions are observationally equal if they produce the same output for all inputs.
```haskell
prop_isort_qsort :: [Int] -> Bool
prop_isort_qsort xs = isort xs == qsort xs
```
General form: `f x == g x`.

### Side note: testing polymorphic properties ###
QuickCheck will instantiate all polymorphic types with `()`, the empty tuple, which is usually not what we want:
```haskell
prop_isort_qsort_bad :: (Ord a) => [a] -> Bool
prop_isort_qsort_bad xs = isort xs == qsort xs
-- ^ will test if isort [(),...,()] == qsort [(),...,()]
```

### Algebraic properties ###
An algebraic law is any property of an algebraic structure:
* Associativity: `x # (y # z) == (x # y) # z`
* Commutativity: `x # y == y # x`
* Neutrality: `z # x == x` or `x # z == x`
* Cancellation: `neg x # x == z` or `x # neg x == z`
* Distributivity: `x @ (y # z) = (x @ y) # (x @ z)`
* Idempotence: `f (f x) == f x`

## Testing with different distributions ##
### Properties with a limited domain ###
```haskell
-- replicate n x produces the list
-- [x,x,...,x] (with n copies of x)
prop_replicate n x i = replicate n x !! i == x
```
```
> quickCheck prop_replicate
*** Failed! Exception:
'Prelude.!!: index too large'
```
Generating random integers and using them as indexes for a list will result in an error.

### Solution 1: silencing invalid tests ###

```haskell
prop_replicate n x i = i < 0 || i >= n || replicate n x !! i == x
```
```
> quickCheck prop_replicate
+++ OK, passed 100 tests.
```
Problem: This gives a false sense of security, as the index is out of bounds in almost all tests.

### Solution 2: adding preconditions ###
```haskell
prop_replicate n x i = (i >= 0 && i < n) ==> replicate n x !! i == x
```

```
> quickCheck prop_replicate
+++ OK, passed 100 tests;
695 discarded.
```

`... ==> ...` is a conditional property: test cases that do not satisfy the condition are discarded.

Note: if the conditions you set are too harsh, it may not be possible for QuickCheck to generate test cases randomly, as vast majority of the generated samples will be rejected.

### Solution 3: using a custom generator ###
```haskell
prop_replicate n x = forAll (chooseInt (0,n-1)) (\i -> replicate n x !! i == x)
```
```
> quickCheck prop_replicate
+++ OK, passed 100 tests.
```
No tests discarded anymore.

`chooseInt (0,n-1)` is an example of a generator: an object that can be used to generate random values of type `Int`.


### The `Gen` type ###
For any type `a`, the type `Gen a` represent a random generator for values of type a.

Ways to  construct a generator:
```haskell
arbitrary :: Arbitrary a => Gen a
chooseInt :: (Int, Int) -> Gen Int
elements :: [a] -> Gen a
listOf :: Gen a -> Gen [a]
vectorOf :: Int -> Gen a -> Gen [a]
shuffle :: [a] -> Gen [a]
```
More generators in `Test.QuickCheck`.

### The `Property` type ###
The type `Property` is a generalization of `Bool`: it includes properties with built-in randomness and shrinking.

Operations on `Property`:
```haskell
(==>) :: Property -> Property -> Property
forAll :: Gen a -> (a -> Property)-> Property
(===) :: Eq a => a -> a -> Property
(.&&.) :: Property -> Property -> Property
(.||.) :: Property -> Property -> Property
```

