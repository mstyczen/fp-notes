# Lecture 12
### Lecture plan
* What is a dependent type?
* Dependent function types
* The `Vector` and `Fin` types
* Defining types by pattern matching
* The dependent pair type

### Exercise
**Question**: Is it possible to implement a function of type `{A : Set} → List A → Nat → A` in Agda?

**Answer**: No, because there is no way to construct an element of empty type.

### Interactive programming with Agda
* `Ctrl+c Ctrl+l` Load the file
* `Ctrl+c Ctrl+d` Deduce type of an expression
* `Ctrl+c Ctrl+n` Normalise an expression
* `Ctrl+c Ctrl+,` Get information about the hole
* `Ctrl+c Ctrl+c` Case split on a variable
* `Ctrl+c Ctrl+space` Give a solution for the hole

These commands will become more and more useful, so start using them now!

## What's a dependent type?
### Cooking with dependent types
```haskell
data Food : Set where
    pizza : Food
    cake : Food
    bread : Food

data Flavour : Set where
    cheesy : Flavour
    chocolatey : Flavour

okFlavour : Food → Flavour → Bool
okFlavour pizza cheesy = true
okFlavour cake chocolatey = true
okFlavour bread _ = true
okFlavour _ _ = false

amountOfCheese : Food → Nat
amountOfCheese pizza = 100
amountOfCheese cake = {!!}
amountOfCheese bread = 20
```
How can we statically rule out cases that do not make sense?

We can make the type `Food` more precise by **indexing** it over `Flavour`:
```haskell
data Food : Flavour → Set where
    pizza : Food cheesy
    cake : Food chocolatey
    bread : (f : Flavour) → Food f
```

`Food` is now a dependent type over `Flavour`.

We can now rule out invalid inputs by using more precise type `Food cheesy`:
```haskell
amountOfCheese : Food cheesy → Nat
amountOfCheese pizza = 100
amountOfCheese (bread cheesy) = 20
```
The coverage of Agda knows that `cake chocolatey` and `bread chocolatey` are not valid inputs!

### Dependent type theory (1972)
A **dependent type** is a family of types, depending on a term of a **base type** (*Per Martin-Lof*).

**Example:** Instead of `Food`, we have `Food chocolatey` and `Food cheesy`.

### Dependent types vs. parametrized types
**Question**: What is the difference between a dependent type, such as `Food f` and a parametrized type such as `Maybe a`?

**Answer:** 
* the argument of a parametrized type in Haskell is a type, while the argument of a dependent type is an expression of some type (in this case Flavour).
* All types `Maybe a` have the same constructors (`Nothing` and `Just`) no matter what `a` is, while `Food f` has different constructors depending on `f`.

## The `Vec` type
### Vectors: lists that know their length
`Vec A n` is the type of vectors with exactly `n` arguments of type `A`.
```haskell
myVec1 : Vec Nat 4
myVec1 = 1 :: 2 :: 3 :: 4 :: []
myVec2 : Vec Nat 0
myVec2 = []
myVec3 : Vec (Bool → Bool) 2
myVec3 = not :: id :: []
```

### Type-level computation
During type-checking, Agda will evaluate expressions in types:
```haskell
myVec4 : Vec Nat (2 + 2)
myVec4 = 1 :: 2 :: 3 :: 4 :: []
```

Since Agda is a total language, any expression can appear inside a type.
A non-total language with dependent types would only allow a few 'safe' expressions.

### Checking the length of a vector
Constructing a vector of the wrong length in any way is a type error:
```haskell
myVec5 : Vec Nat 0
myVec5 = 1 :: 2 :: []
```
Results in:
```
suc _n_46 != zero of type Nat
when checking that the inferred
type of an application
Vec Nat (suc _n_46)
matches the expected type
Vec Nat 0
```

### Dependent function type
A **dependent function** type is a type of the form  `(x : A) → B x` where the type of the output depends on the value of the input.

**Example:**
```haskell
zeroes : (n : Nat) → Vec Nat n
zeroes zero = []
zeroes (suc n) = 0 :: zeroes n
```

E.g. `zeroes 3` has type `Vec Nat 3` and evaluates to `0 :: 0 :: 0 :: []`.

### Definition of the `Vec` type
`Vec A n` is a dependent type indexed over the base type `Nat`:
```haskell
data Vec (A : Set) : Nat → Set where
    [] : Vec A 0
    _::_ : {n : Nat} →
        A → Vec A n → Vec A (suc n)
```

This has two constructors `[]` and `_::_` like `List`, but the constructors specify the length in their types.

### Parameters vs. indices
The argument `(A : Set)` in the definition of `Vec` is a parameter, and has to be the same in the type of each constructor.

The argument of type `Nat` in the definition of `Vec` is an index, and must be determined
individually for each constructor.

### Concatenation of vectors
We can pattern match on `Vec` just like on `List`:
```haskell
mapVec : {A B : Set} {n : Nat} → (A → B) → Vec A n → Vec B n
mapVec f [] = []
mapVec f (x :: xs) = f x :: mapVec f xs
```
**Note:** The type of `mapVec` specifies that the output has the same length as the input.

### A safe `head` function
By making the input type of a function more precise, we can rule out certain cases statically (= during type checking):
```haskell
head : {A : Set}{n : Nat} → Vec A (suc n) → A
head (x :: xs) = x
```
Agda knows the case for `head []` is impossible! (just like `amountOfCheese cake`).

### A safe `tail` function
**Question**: what should be the type of `tail` on vectors with the following definition?
```haskell
tail (x :: xs) = xs
```
**Answer**: 
```haskell
tail : {A : Set} {n : Nat} → Vec A (suc n) → Vec A n
tail (x :: xs) = xs
```

## The `Fin` type
### A safe lookup
By combining `head` and `tail` we can can get the 1st, 2nd, 3rd,... element of a vector with at least that many elements.
How can we define a function `lookupVec` that gets the element at position `i` of a `Vec A n` where `i` < `n`?

**Note**: We want to get an element of `A`, not of `Maybe A`!

### The `Fin` type
We need a type of indices that are safe for a vector of length `n`, i.e. numbers between `0` and ``n − 1`. This is the type `Fin n` of finite numbers:

```haskell
zero3 one3 two3 : Fin 3
zero3 = zero
one3 = suc zero
two3 = suc (suc zero)
```

### An empty type
`Fin n` has `n` elements, so in particular `Fin 0` has zero elements: it is an empty type.
This means there are no valid indices for a vector of length 0. 

**Note:** Unlike in Haskell, we cannot even construct an expression of `Fin 0` using
undefined or an infinite loop.

### Definition of the `Fin` type
```haskell
data Fin : Nat → Set where
    zero : {n : Nat} → Fin (suc n)
    suc : {n : Nat} → Fin n → Fin (suc n)
```

### A safe lookup
```haskell
lookupVec : {A : Set} {n : Nat} → Vec A n → Fin n → A
lookupVec [] ()
lookupVec (x :: xs) zero = x
lookupVec (x :: xs) (suc i) = lookupVec xs i
```
If a datatype has no possible constructors (e.g. `Fin 0`) we can use the absurd pattern `()`.
An absurd clause (one with a `()` pattern) does not require a right-hand side, since the type
system guarantees it will never be called.

We now have a safe and total version of the Haskell `(!!)` function, without having to change the return type in any way.

## Dependent types as functions into `Set`
### Functions that return a type
Since `Set` is just another type, we can use pattern matching to define new types:
```haskell
pickType : Bool → Set
pickType true = Nat
pickType false = Bool → Bool
```
Now `pickType true` is an alias for `Nat`, and `pickType false` is an alias for `Bool`:
```haskell
test2 : pickType true
test2 = suc zero
```

### A dependent if/then/else
```haskell
IfT : (b : Bool) → Set → Set → Set
IfT true A B = A
IfT false A B = B

if_then_else_ : {A B : Set} → (b : Bool) → A → B → IfT b A B
if true then x else y = x
if false then x else y = y

test : Nat
test = if true then 42 else false
```

### No pattern matching on `Set`
It is not allowed to pattern match on arguments of type `Set`:
```haskell
-- Not valid code:
sneakyType : Set → Set
sneakyType Bool = Nat
sneakyType Nat = Bool
```
One reason for this is that Agda (like Haskell) erases all types during compilation

### The Σ type
The type `Σ A B1` is the type of dependent pairs `(x, y)` where the type of `y` can change depending on the value of `x`.

**Examples:**
```haskell
pair1 : Σ Nat (λ n → Vec Bool n)
pair1 = (2 , (true :: false :: []))

pair2 : Σ Nat (λ n → Vec Bool n)
pair2 = (0 , [])
```

### Definition of the dependent pair type:
The type `Σ` is defined as follows:

```haskell
data Σ (A : Set) (B : A → Set) : Set where
_,_ : (x : A) → B x → Σ A B
```

The second parameter `B` is a dependent type over the base type `A`.
Elements of type `Σ A B` are pairs `(x, y)` where `x : A` and `y : B x`.

### Normal pairs as degenerate dependent pairs
We can define regular pairs as a special case of the `Σ` type:
```haskell
_×’_ : (A B : Set) → Set
A ×’ B = Σ A (λ _ → B)
```
*“A normal pair is a dependent pair where the type of the second component doesn’t actually depend on the first."*

### Projecting out of a dependent pair
The projections from the `Σ` type are defined as follows:
```haskell
fstΣ : {A : Set}{B : A → Set} → Σ A B → A
fstΣ (x , y) = x

sndΣ : {A : Set}{B : A → Set} →(z : Σ A B) → B (fstΣ z)
sndΣ (x , y) = y
```

### Dependent types: summary
A **dependent type** is a type that depends on a value of some base type.

With dependent types, we can specify the allowed inputs of a function more precisely, ruling out invalid inputs at compile time.

**Examples of dependent types:**
* `Food f`, indexed over `f : Flavour`
* `Vec A n`, indexed over `n : Nat`
* `Fin n`, indexed over `n : Nat`
* `IfT b A B`, indexed over `b : Bool`