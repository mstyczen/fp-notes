# Lecture 6
### Lecture plan
* Data types FAQ
* Recap: working with type classes
* Defining your own instances
* Defining your own type classes
* More builtin classes: `Semigroup` and `Monoid`

## Data types FAQ
### Drawing elements of datatypes:
You can visualize elements of datatypes by drawing a tree structure:

For example, for type `Nat`, we have the following elements:
```haskell
data Nat = Zero | Suc Nat
```
```
0: Zero
1: Suc -> Zero
2: Suc -> Suc -> Zero
...
```
And for `List`:
```haskell
data List a = Nil | Cons a (List a)
```
```
[]: Nil
[1]: Cons 1 -> Nil
[0,2]: Cons 0 -> Cons 2 -> Nil
...
```
Finally, for `Tree`:
```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)
```
```
Leaf 3

      Node
     /    \
Leaf 3    Leaf 6
...
```

### Counting functions:
What are the 9 elements of `Bool -> Answer`?
```haskell
\b -> if b then Yes else Yes
\b -> if b then Yes else No
\b -> if b then Yes else Unknown
\b -> if b then No else Yes
\b -> if b then No else No
\b -> if b then No else Unknown
\b -> if b then Unknown else Yes
\b -> if b then Unknown else No
\b -> if b then Unknown else Unknown
```
Any other function will be equivalent to one of the above, even if the syntax is different.

### Answer to the brainteaser question from previous lecture
**Question:** Can you construct an element of the
following type?
```haskell
data B a = C (B a -> a)
```
**Answer**: here’s one example:
```haskell
C (\x -> case x of C f -> f x)
```

## Recap: working with type classes
### Example: Generic `lookup` using Eq
```haskell
type Assoc k a = [(k,a)]
lookup :: Eq k => k -> Assoc k a -> Maybe a
lookup k [] = Nothing
lookup k ((l,x):xs)
    | k == l = Just x
    | otherwise = lookup k xs
```

### What is a type class?
A type class is a **family** of types that implement a common interface.
**Example:** `Eq` is the family of types that implement `(==)` and `(/=)`.
A type that belongs to this family is called an **instance** of the type class.

### Definition of the EQ class
```haskell
class EQ a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```
First line declares a new type class `Eq` with one
parameter `a`. Body of class has one or more function
declarations. Each function optionally has a default
implementation.


### Declaring new instances of `Eq`
```haskell
data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
```
* First line declares `TrafficLight` to be an instance of `Eq`
* Body gives implementation of class functions
* Skipped functions use default implementation 

### Using our type class instance
```haskell
> Red == Red
True

> Red == Green
False

> let speeds = [(Green , 100), (Yellow, 50 ), (Red , 0 )]
> lookup Yellow speeds
Just 50
```

### Minimal complete definitions
An instance of `Eq X` has to either define `(==) :: X -> X -> Bool` or `(/=) :: X -> X -> Bool `, or both.

In Haskell docs: “Minimal complete definition: `(==) | (/=)`”.

**Question.** What happens if we define neither?

**Answer:** Infinite loop.

**Question.** What if Eq didn’t have default
implementations?

**Answer:** You would have to implement both.

### Another example: the `Show` class
```haskell
class Show a where
    show :: a -> String
    showList :: [a] -> ShowS
    showList = ...
    showsPrec :: Int -> a -> showS
    showsPrec = ...

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
```

### Automatically deriving classes
Instead of writing instances by hand, Haskell can automatically derive instances of built-in type classes such as `Eq` and `Show`.
```haskell
data TrafficLight = Red | Yellow | Green
    deriving (Eq, Show)

> Red /= Red
False

> show Green
"Green" 
```

## Instances of parametrized datatypes
### Defining `Eq` instance for `Maybe`?
1. First attempt:
```haskell
instance Eq Maybe where ...
```
```
Expecting one more argument to
‘Maybe’ Expected a type, but
‘Maybe’ has kind ‘* -> *’
```
2. Second attempt:
```haskell
instance Eq (Maybe a) where
Nothing == Nothing = True
Just x == Just y = x == y
_ == _ = False
```
```
No instance for (Eq a) arising from
a use of ‘==’
```

3. Third attempt:
```haskell
instance Eq a => Eq (Maybe a) where
Nothing == Nothing = True
Just x == Just y = x == y
_ == _ = False
```

```
> Just 5 == Just 7
False
```
It works now!

### `Eq` and `Show` instances of `Tree`
```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Eq a => Eq (Tree a) where
    Leaf x == Leaf y = x == y
    Node u v == Node w x = u == w && v == x
    _ == _ = False

instance Show a => Show (Tree a) where
    show (Leaf x) = "Leaf " ++ show x
    show (Node u v) = "Node " ++ showP u ++ showP v
        where showP x = "(" ++ show x ++ ")"
```

### `Eq` and `Show` instances for `Tree`
```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Eq)
```
`deriving` also works for parametrized datatypes!

## Working with subclasses
### Subclass example
Some type classes are a subclass of another class: each instance must also be an instance of the base class.
**Example:** `Ord` is a subclass of `Eq`:
```haskell
class (Eq a) => Ord a where
    (<) :: a -> a -> a
    -- ...
```

### Why use subclasses?
Two reasons to declare a class as a subclass of another:
* Shorter type signatures: we can write ```Ord a => ...``` instead of ```(Eq a, Ord a) => ...``` 
* We can use functions from the base class in default implementations:
```haskell
x <= y = x < y || x == y
```

### More examples of subclasses
```haskell
class (Num a, Ord a) => Real a
    where ...
class (Real a, Enum a) => Integral a
    where ...
class (Num a) => Fractional a
    where ...
class (Fractional a) => Floating a
    where ...
```

## Defining your own classes:
### Example: Truthy and Falsy values in Haskell
In Python and other languages, values of many types are considered `truthy` or `falsy`.
```python
>>> if 5: print("hey whoah")
hey whoah
```

### Instances of `Booly`
```haskell
instance Booly Bool where
    bool x = x

instance Booly Int where
    bool x = x /= 0

instance Booly Double where
    bool x = x /= 0.0

instance Booly (Maybe a) where
    bool Nothing = False
    bool (Just x) = True

instance Booly [a] where
    bool [] = False
    bool (_:_) = True
```

### An `if/then/else` for `Booly` values
```haskell
iffy :: Booly a => a -> b -> b -> b
iffy b x y = if bool b then x else y
```

```
> iffy [] "yes" "no"
"no"

> iffy False "yes" "no"
"no"

> iffy (Just False) "yes" "no"
"yes"
```

### Another example: `Reversible`:
```haskell
class Reversible a where
    rev :: a -> a

instance Reversible [a] where
    rev xs = reverse xs

instance Reversible (Tree a) where
    rev (Leaf x) = Leaf x
    rev (Node l r) = Node (rev r) (rev l)
```

## Type class laws
### Laws of `Eq`:
Reflexivity: x == x = True
1. Symmetry: `(x == y) = (y == x)`
2. Transitivity: If `(x == y && y == z) = True` then `x == z = True`
3. Substitutivity: If `x == y = True` then `f x == f y = True`
4. Negation: `x /= y = not (x == y)`

### Type class laws:
Most Haskell type classes have one or more laws that instances should satisfy.
These laws are not checked by the compiler, but they form a contract between Haskell
programmers.

So avoid defining unlawful instances!

### `Eq` instances for function types
**Question**: can we give lawful instances for the following types? Are these instances useful?

```haskell
instance Eq (Bool -> Bool) where ...
instance Eq (Bool -> Integer) where ...
instance Eq (Integer -> Bool) where ...
instance (Eq a, Eq b) => Eq (a -> b) where ...
```
For the first two:
```haskell
(f True == g True) && (f False == g False)
```

For the remaining two, you cannot write an instance that would terminate.

### Laws for our own classes
**Question:** can you identify any laws for `Booly` or `Reversible`?

**Answer:**
* For `Booly`: no immediately visible laws
* For `Reversible`: `rev (rev) = x` 

## The `Semigroup` and `Monoid` classes
### A bit of mathematics:
A semigroup is a set A with an operator `<>` that is associative:
`(x <> y) <> z = x <> (y <> z)`

A monoid is a semigroup with a neutral element ∅: ∅ <> x = x = x <> ∅

**Question**: What Haskell types can you think of that are monoids?

### The `Semigroup` and `Monoid` instances:
```haskell
class Semigroup a where
    (<>) :: a -> a -> a

class Semigroup a => Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
    mappend = (<>)
    mconcat = foldr mappend mempty
```

### The `Semigroup` and `Monoid` laws:
`Semigroup` law:
* `(x <> y) <> z = x <> (y <> z)`

`Monoid` laws :
* ```mempty `mappend` x = x```
* ```x `mappend` mempty = x```
* ``` (x `mappend` y) `mappend` z = `mappend` (y `mappend` z)```

### `[a]` is a monoid:
```haskell
instance Semigroup [a] where
    (<>) = (++)
instance Monoid [a] where
    mempty = []
```

### Numeric instances of `Monoid`
**Question**: What operation should we use for `(<>)` for numeric types such as `Int` or `Double`? `(+)` or `(-)`?
**Answer:** Both are valid choices!

### Numeric instances of `Monoid`:
Haskell does not allow the same type to be an instance of a typeclass in two different ways:
```haskell
instance Semigroup Int where
(<>) = (+)
instance Semigroup Int where
(<>) = (*)
```
```
Duplicate instance declarations:
instance Semigroup Int
instance Semigroup Int
```

### Global coherence
The Haskell compiler guarantees global coherence of type class instances: The result of
a program at run time should not depend on which instances were chosen at compile time.
In practice, the compiler will forbid all overlapping instances.

**Example**: it is not allowed to have both instance `MyClass (a,Int)` and instance `MyClass (Bool,b)`, since they overlap at `MyClass (Bool,Int)`.

### The wrapper types `Sum` and `Product`:
`Sum` and `Product` are wrapper types to help type class resolution:
```haskell
newtype Sum a = Sum { getSum :: a }
newtype Product a = Product a { getProduct :: a }

instance Num a => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

instance Num a => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)
```
### Working with monoids:
```haskell
> Sum 3 <> Sum 9
Sum 12

> Product 3 <> Product 9
Product 27

> Sum 3 <> mempty
Sum 3

> Product 3 <> mempty
Product 3

> mconcat (map Product [3,4,2])
Product 24
```

### The wrappers `Any` and `All`
There are two ways to make `Bool` into a monoid: either using `(&&)` or using `(||)`.
```haskell
newtype Any = Any { getAny :: Bool }
newtype All = All { getAll :: Bool }

instance Semigroup Any where 
    Any x <> Any y = Any (x || y)

instance Monoid Any where
    mempty = Any False

instance Semigroup All where
    All x <> All y = All (x && y)

instance Monoid Any where
    mempty = All True
```

```haskell

### `Maybe a` is a monoid if `a` is a monoid
```haskell
instance Semigroup a => Semigroup (Maybe a) where
    mx <> Nothing = mx
    Nothing <> my = my
    Just x <> Just y = Just (x <> y)

instance Monoid (Maybe a) where
    mempty = Nothing
```

### Working with monoids, another example:
```haskell
> Nothing <> Nothing
Nothing

> Just [1] <> Nothing
Just [1]

> Nothing <> Just [2]
Just [2]

> Just [1] <> Just [2]
Just [1]

> mconcat [Just [1], mempty, Just [2], Just [3], mempty]
Just [1,2,3]
```

### A monoid of trees
**Question:** can we make `Tree a` into an instance of `Monoid`?

### The `IO` monoid:
`IO a` is the type of computations that can perform I/O:
* `putStrLn :: String -> IO ()`
* `writeFile :: FilePath -> String -> IO ()`
* `appendFile :: FilePath -> String -> IO ()`
The Haskell Prelude defines the following instance: `instance Monoid a => Monoid (IO a)`

So we can write IO programs that do several things in sequence:
```
main = mconcat [ writeFile "log.txt" "Starting...\n"
, putStrLn "Hello"
, putStrLn "World!"
, appendFile "log.txt" "All done!\n" ]
```
