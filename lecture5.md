# Lecture 5
### Lecture plan
* Quiz about last week
* Defining new Haskell types
* Useful builtin types: `Maybe` and `Either`
* Recursive datatypes and folding functions
* The `containers` library: `Map` and `Set`
* A few more sample exam questions

## Defining type aliases
### Type aliases
A **type alias** gives a new name to an existing type:
```haskell
type String = [Char]
type Coordinate = (Int, Int)
```

They can be used to convey **meaning**, but are treated transparently by the compiler.

### More examples of type aliases:
```haskell
--- Two parametrized types
type Pair a = (a, a)
type Assoc k v = [(k,v)]

--- An alias for a function type
type Transformation = Coordinate -> Coordinate
```

**Warning**: type aliases cannot be recursive:
```haskell
type Tree = (Int, [Tree])
```
Results in:
```
Cycle in type synonym declarations: type Tree = (Int, [Tree])
```

## Defining algebraic datatypes (ADTs)
### A simple algebraic datatype
```haskell
data Answer = Yes | No | DontKnow
    deriving (Show)

answers :: [Answer]
answers = [Yes, No, DontKnow]

--- example of a function definition with the newly defined type
flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip DontKnow = DontKnow
```

**Note**: `data` is the keyword for declaring ADTs.

### The `Bool` type
**Question**: How to define `Bool`?

**Answer**:
```haskell
data Bool = True | False 
```

### The `Ordering` type
The Prelude defines the following:
```haskell
data Ordering = LT | EQ | GT
compare :: Ord a => a -> a -> Ordering
```

`compare` returns `LT`, `EQ` or `GT` depending on whether the first argument is smaller, equal or greater than the second.

### Constructors arguments
```haskell
data Shape = Circle Double | Rect Double Double

square :: Double -> Shape
square x = Rect x x

area :: Shape -> Double
are (Circle r) = pi * r * r
area (Rect l h) = l * h
```

### Constructors as functions
Each constructor defines a function into the datatype:
```haskell
> :t Circle
Circle :: Double -> Shape

>:t Rect
Rect :: Double -> Double -> Shape

> map Circle [1.0, 2.0, 3.0]
[Circle 1.0, Circle 2.0, Circle 3.0]
```

### Record syntax
Haskell provides an alternative **record syntax** to define constructors with arguments:
```haskell
data Shape 
    = Circle {radius :: Double}
    | Rectangle {width :: Double,
                height :: Double}
```

This is syntactic sugar for previous definition but also defines functions `radius`, `width`, and `height`.

Each field also defines a function from the datatype:
```haskell
radius :: Shape -> Double
radius (Circle r) = r
```
**Warning**: Fields such as `radius` and `width` are partial functions: they raise a runtime error when applied to the wrong constructor.

We can also use record syntax when applying or matching on a constructor:
```haskell
square :: Double -> Shape
square x = Rect {width = x, height = x}

getWidth :: Shape -> Double
getWidth (Circle {radius = r}) = 2 * r
getWidth (Rect {width w}) = w
```

### Functional style vs OO style

```haskell
data Shape = Circle Double | Rect Double Double

square :: Double -> Shape
square x = Rect x x

area :: Shape -> Double
are (Circle r) = pi * r * r
area (Rect l h) = l * h
```

```java
abstract class Shape {
    abstract double area();
}

class Circle extends Shape {
    double r;
    Circle (double radius) {r = radius; }
    double area() {return Math.PI * r * r}
}

class Rectangle extends Shape {
    double w;
    double h;
    Rectangle(double width, double height) {
        w = width; h = height;
    }

    Rectangle(double side) {
        w = side; h = side;
    }

    double area() {return w * h}
}
```

What differences do you see?
* In Haskell the functions are top level elements, in Java the fields are the most important
* In Java it is easier to add more types of Shapes, in Haskell it is easier to add new functions 

### The expression problem
In an **object oriented** language, it is easy to add new cases to a type, but hard to add new functions.

In a **functional** language it is easy to add new functions to a type, but hard to add new cases.

This tradeoff is known as the **expression problem** (John Reynolds, 1975).

## `newtype` definitions
### `newtype` declarations
A `newtype` declaration is a specialized kind of `data` declaration with exactly one constructor taking exactly one argument:
```haskell
newtype EuroPrice = EuroCents Integer
newtype DollarPrice = DollarCents Integer

dollarToEur :: DollarPrice -> EuroPrice
dollarToEuro (DollarCents x) = 
    EuroCents (round (0.83 * fromInteger x))
```

### `newtype` vs `type` vs `data`
Differences of `newtype` compared to `type`:
* cannot accidentally mix up two types
* need to wrap/unwrap elements by hand

Differences of `newtype` compared to `data`:
* Only one constructor with one argument
* More efficient representation
* No recursive types (see later)

## Parametrized datatypes
### The Haskell type `Maybe`
The type `Maybe a` represents an optional value of type `a`:
```haskell
data Maybe a = Nothing | Just a
```
`Maybe` is often used to represent functions that can fail:
```haskell
safeDiv :: Int -> Int -> Maybe Int
safeDiv x y
 | y == 0 = Nothing
 | otherwise = Just (x `div` y)
```

### A safer `head` function
```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x
```

### Non-empty lists
The type `NonEmpty a` represents lists with at least one element:
```haskell
data NonEmpty a = a :| [a]

toList :: NonEmpty a -> [a]
toList (x :| xs) = x : xs
```

### Another safe `head` function
```haskell
safeHead' :: NonEmpty a -> a
safeHead' (x :| xs) = x
```
**Question**: which version is better in what situation?

### The `Either` type
The `Either` type represent a disjoint union of `a` and `b`: each element is either `Left x` for `x :: a` or `Right y` for `y :: b`.
```haskell
data Either a b - Left a | Right b
```

### A poor man's exceptions
Convention: `Right` is often used to represent a successful operation, while `Left` is often used to represent an error:
```haskell
throwError :: err -> Either err a
throwError = Left

catchError :: Either err a
    -> (err -> Either err a)
    -> Either err a
catchError (Left err) handle = handle err
catchError (Right x) handle = Right x
```

Example: 
```haskell
get :: Int -> [a] -> Either String a
get i xs
    | i < 0 = Left "Negative index!"
    | i >= length xs = Left "Index too large!"
    | otherwise = Right (xs !! i)

getTwo :: (Int,Int) -> [a] -> Either String (a,a)
getTwo (i, j) xs = case (get i xs) of
    Left err1 -> Left err1
    Right x -> case (get j xs) of
        Left err2 -> Left err2
        Right y -> Right (x,y)
```

### Counting the elements of a type:
How many elements are in the following types:
* `Either Bool Answer`: 2 + 3 = 5
* `(Bool, Bool, Answer)`:  2 * 2 * 3 = 12, as there are two values of `Bool` and three of `Answer`
* `Maybe (Bool, Bool)`: 1 (`Nothing`) + 4 (`Bool` 2-tuples) = 5
* `Bool -> Answer`: 3^2 = 9
* `Answer -> Either Bool Bool` 4^3

### What's algebraic about ADTs?
An algebraic datatype is a type that is formed from other types using sums and products:
* The product of `a` and `b` is the tuple type `(a,b)`
* The sum of a and b is the disjoint union type `Either a b`

Each constructor of an ADT is the product of the types of its arguments, and the ADT itself is
the sum of the constructor types.


## Recursive datatypes
### Example: unary natural numbers
We can define a type `Nat` represents natural numbers (inefficiently) as `Zero`, `Suc Zero`, `Suc (Suc Zero)`, ...:
```haskell
data Nat = Zero | Suc Nat
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n | n > 0 = Suc (int2nat (n-1))
```
**Exercise:** Define
```haskell
maximum :: Nat -> Nat -> Nat
```

### Defining lists
**Question:** how would you define the list type `[a]` as a datatype?
```haskell
data List a = Nil | Cons a (List a)
```
### A brain teaser
**Question**: can you construct an element of the following type?
```haskell
data B a = C (B a -> a)
```

### Example: Binary trees
```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)

occurs :: Eq a => a -> Tree a 0 -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l r) = occurs x l || occurs x r

flatten :: Tree a -> List a
flatten (Leaf x) = [x]
flatten (Node l r) = flatten l ++ flatten r
```

### Folding binary trees
For any recursive datatype we can define a folding function (not just for lists!)
```haskell
foldt :: (a -> b) -> (b -> b -> b) ->Tree a -> b
foldt w f (Leaf x) = w x
foldt w f (Node l r) = f (foldt l) (foldt r)

occurs x = foldt (\y -> x == y) (||)
flatten = foldt (\x -> [x]) (++)
```
`foldt w f` replaces each `Leaf` by `w` and each `Node` by `f`.

## The `containers` library
### The `containers` library introduction
The `containers` library provides several purely functional data structures, such as maps (= dictionaries) and sets.

### `Set` type
`Set a` is a type representing unordered sets of type `a`:
```haskell
empty :: Set a
singleton :: a -> Set a
fromList :: Ord a => [a] -> Set a
insert :: Ord a => a -> Set a -> Set a
delete :: Ord a => a -> Set a -> Set a
member :: Ord a => a -> Set a -> Bool
size :: Set a -> Int
union :: Ord a => Set a -> Set a -> Set a
difference :: Ord a => Set a -> Set a -> Set a
intersection :: Ord a => Set a -> Set a -> Set a
```

### `Map` type
```haskell
empty :: Map k a
singleton :: k -> a -> Map k a
insert :: Ord k => k -> a -> Map k a -> Map k a
delete :: Ord k => k -> Map k a -> Map k a
lookup :: Ord k => k -> Map k a -> Maybe a
size :: Map k a -> Int
union :: Ord k => Map k a -> Map k a -> Map k a
difference :: Ord k => Map k a -> Map k a -> Map k a
intersection :: Ord k => Map k a -> Map k a -> Map k a
```

### Importing names qualified
Many names in the `containers` library are conflicting, so it is recommended to import its modules in a qualified way:
```haskell
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

mySet :: Set Int
mySet = Set.fromList [1,2,3]

myMap :: Map Int String
myMap = Map.singleton 42 "The answer"
```