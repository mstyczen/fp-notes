# Lecture 2 #
## Haskell types ##
### What is a type ###
A **type** is a name for a collection of values.

Example: `Bool` has two values `True` and `False`.
Haskell syntax for typing: `True :: Bool`
You can write type annotations (almost) anywhere in your Haskell program.

### Type inference ###
In Haskell type annotations are optional - the compiler will try to infer the type if it is not given. In `GHCi` we can ask the type of an expression with `:t`, e.g.
```haskell
> :t True
Bool
```

When a function is applied to an argument of the wron type, you get a `type error`.
For example, try `1 + True`.

### Basic Haskell types ###
* `Bool` - `True` and `False`
* `Int` - 64 bit integere
* `Integer` - arbitrary size integer
* `Float`, `Double` - floating point numbers
* `Char` - characters, `'a', 'b'`
* `String` - strings, `"Hello, world!"`

### List type [a] ###
Example: `[1,2,3] :: [Int]`.
In general, `[t]` is the type of lists `[x1,x2,...,xn]` with elements of type `t`.

### Strings as lists ###
Strings are lists of characters - `type String = [Char]`. We can use the two types interchangably, for example: `['a','b'] == "abc"` evaluates to `True`.

Note: the module `Data.Text` has a more efficient representation of strings.

### Ranges of values ###
You can construct a range from `i` to `j` by calling `[i..j]`. For example:
```haskell
> [1..5] 
[1,2,3,4,5]
```
and:
```haskell
> ['a'..'d']
"abcd"
```

### Tuple types ###
Example: ```(42, True) :: (Int, Bool)```. In general, types in a tuple can be different, while types in a list have to be consisted. Like lists, tuples can be nested.

### Lists vs tuples ###
List had arbitrary length, but consistent types. Tuples may have varying types, but the size has to be fixed.

### Function type `a->b` ###
Examples:
```haskell
not :: Bool -> Bool
isDigit :: Char -> Bool
```
In general, `a->b` is the type of pure functions from `a` to `b`.

### Functions with several arguments ###
Two ways:
```haskell
add1 :: (Int, Int) -> Int
add2 :: Int -> (Int -> Int)
```
`add1` takes as argument a tuple `x,y` and returns and `Int`. `add2` takes as argument an `Int` and returns a function of type `Int -> Int` that waits for the second argument.

### Working with curried functions ###
A function that returns another function is called a curried function.

The function arrow `->` associates to the right:
```haskell
a -> b -> c = a -> (b -> c)
```
Function application associates to the left:
```haskell
f x y = (f x) y
```

###  Polymorphic functions ###

Many functions can be given several types, for example the `length` function has a type `length :: [a] -> Int`, and it would work for any array type. Unlike generics in Java, we do not need to give the type, since Haskell can infer it automatically.

### Examples of polymorphic functions ###

Identity function:
```haskell
id :: a -> a 
```
First and second element of tuples:

```haskell
fst :: (a,b) -> a
snd :: (a,b) -> b
```

Curry and uncurry:
```haskell
curry :: ((a,b)->c) -> (a->b->c)
uncurry :: (a->b->c) -> ((a,b) -> c)
```

For lists:
```haskell
head :: [a] -> a
tail :: [a] -> [a]
(!!) :: [a] -> a
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]
zip :: [a] -> [b] -> [(a,b)]
unzip :: [(a,b)] -> ([a], [b])
```

### `error` and `undefined` ###
Functions like `head` call `error` for an empty list.

```haskell
undefined :: a
error :: String -> a
```

These functions throw a run-time error when evaluated, making your code partial - opposite of total, a function that is only defined for some of its input.

**Advice**:
* Only use `undefined` during development.
* Only use `error` for unreachable cases.

### Type classes ###
Question: what should be the type of `double x = x + x`?
If you set it to `double :: Int - > Int`, it is too restrictive, as it works for `Float` too! You can utilize `Num` type class:
```haskell
double :: Num a => a -> a
```

Num is an example of a **type class**: a collection of types that support a common interface, containing `(+), (-), (*), negate, abs, fromInteger`. Compare with interfaces in Java.

### The `Eq` class ###
Comparable types, implementing the equality/inequality operators.
```haskell
(==) :: Eq a => a -> a -> Bool
(/=) :: Eq a => a -> a -> Bool
```

### The `Ord` class ###
Ordered type class, implementing comparison and `min`/`max` operators. An example use case would be the `sort` function - if we want to sort a variable, it has to be ordered.

### The `Show` class ###
Printing / providing string representation. 
Contains a single function `show`.

### The `Read` class ###
Tries to construct an object from string. Contains a single function, `read`.
For example:
```haskell
> read "False" :: Bool
False
```
**Note**: you have to provide a type for reading, else it will not be able to determine what type it should parse the value to, for example `"False"` could be parsed to `String` or `Bool`.

### The `Integral` class ###
Extension of `Num` class (if a type implements `Integral` it has to implement `Num` first), contains two functions:
`div` and `mod`.

### Infix for normal functions ###
You can use any function as an infix operator by surrounding it with \` (backtick).
For example:
```haskell
> 5 `mod` 2
1
```

### The `Fractional` class ###
Implements `/` - floating point division and 
`recip` - calcualating inverse.

## List comprehensions ##
We can construct new lists using a **list comprehension**:
```haskell
> [x * x | x <- [1..5]]
[1,4,9,16,25]
```
The part `x <- [1..5]` is called a **generator**.
Compare with **set comprehensions** from mathematics: {x^2 | x \in [1,5]}

### Using multiple generators ###
We can use more than one generator:
```haskell
> [x * y | x <-[1,2,3], y<-[10,100]]
[10, 100, 20, 200, 30, 300]
```
**Note**: the order of generators matter:

Later generators can depend on previous ones:
```haskell
> [10*x + y | x <- [1..3], y <- [x..3]]
[11,12,13,22,23,33]
```

### Filtering lists ###
We can select only elements that satisfy a boolean predicate:
```haskell
> [x | x <- [1..10], even x]
[2,4,6,8,10]
```
The predicate `even x` is called a **guard**. A guard must be of type `Bool`.

### List comprehensions as a general control structure ###
Haskell does not have built-in control, such as `for` or `while`. Instead, we use lists to define iterative algorithms.
 Example:
 ```haskell
 result = sum [i*i | i <- [1..10]]
 ```
 instead of:
 ```haskell
 int result = 0;
 int i = 0;
 while (i<=10) {
     result += i*i;
     i+=1;
 }
 ```
