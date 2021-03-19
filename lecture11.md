# Lecture 11
### Lecture plan
* A brief overview of formal verification, dependent types, and Agda
* Syntax differences between Agda and Haskell
* Interactive programming in Agda
* Types as first-class value
* Total functional programming

### When testing is just not enough
**Question**: in what situations might testing not be enough to ensure software works correctly?

**Answer**: For example, when:
* failure is very costly (e.g. spacecraft, medical equipment, self-driving cars)
* the software is difficult to update (e.g. embedded software)
* it is security sensitive (e.g. banking, private chats)
* errors are hard to detect or not apparent until much later (e.g. compilers, concurrent systems)

### Formal verification
Formal verification is a collection of techniques for proving correctness of programs with respect to a certain formal specification. These techniques often rely on ideas from formal logic and mathematics to ensure a very high degree of trustworthiness.

### Forms of formal verification
* model checking systematically explores all possible executions of a program.
* deductive verification uses external or internal tools to analyse and prove correctness of a program
* lightweight formal methods try to automatically verify a specific class of properties (e.g. type safety or memory safety)

### Why dependent types?
Dependent types are a form of deductive verification that is embedded in the programming language.

Advantages:
* no different syntax to learn or tools to install
* tight integration between IDE and type system
* Express invariants of programs in their types
* Use same syntax for programming and proving

Formally verifying a program should not be more difficult than writing the program in the first place!

### The Agda language
Agda is a purely functional programming language, similar to Haskell. Unlike Haskell, it has full support for dependent types. It also supports interactive programming with help from the type checker.

### Building your own Agda

An important goal of Agda is to experiment with new language features.

As a consequence, many common language features are not built into Agda, but they can be defined. 

**Example:** We will define the type of boolean and the if/then/else operator.

While we could import these from the standard library, here we will instead build them ourselves from the ground up.

### A first Agda program
```haskell
data Greeting : Set where
    hello : Greeting

greet : Greeting
greet = hello
```

This program:
* Defines a datatype `Greeting` with one constructor `hello`.
* Defines a function `greet` of type `Greeting` that returns `hello`.

### Loading an Agda file:
You cam load an Agda file by pressing `ctrl+c` followed by `ctrl+d`. Once the file is loaded (and there are no errors), other commands become available:
* `ctrl+c ctrl+d` - infer the type of an expression
* `ctrl+c ctrl+n` - evaluate an expression

## Syntax of Agda vs Haskell
### Basic syntax differences
1. Typing uses a single colon: `b : Bool` instead of `b :: Bool`.
2. Naming has fewer restrictions: any name can start with small or capital letter, symbols can occur in names.
3. Whitespace is required more often: `1+1` is a valid function name, so you need to write `1 + 1` instead for addition.
4. Infix operators are indicated by underscores: `_+_` instead of `(+)`

### Unicode syntax:
Agda allows unicode characters in its syntax: 
* → can be used instead of ->
* λ can be used instead of \
* other symbols can also be used as names of functions, variables or types

### Entering unicode
Editors with Agda support will replace LaTeX-like syntax (e.g. `\to`) with unicode:

* → `\to`
* λ `\lambda`
* × `\times`
* Σ `\Sigma`
* \> `\top`
* ⊥ `\bot`
* ≡ `\equiv`

### Declaring new datatypes
To declare a datatype in Agda, we need to give
the full type of each constructor:
```haskell
data Bool : Set where
    true : Bool
    false : Bool
```
We also need to specify that `Bool` itself has type `Set` (see later).

### Defining functions by pattern matching
Just as in Haskell, we can define new functions by pattern matching:
```haskell
not : Bool → Bool
not true = false
not false = true
_xor_ : Bool → Bool → Bool
true xor true = false
false xor false = false
_ xor _ = true
```

### The type of natural numbers
```haskell
data Nat : Set where
    zero : Nat
    suc : Nat → Nat
one = suc zero
two = suc one
three = suc two
four = suc three
five = suc four
```

### Built-in support for numbers
Writing numbers with `zero` and `suc` is annoying and inefficient. We can enable Agda's support for machine integers as follows:
```
{-# BUILTIN NATURAL Nat #-}
```
Now we can work with numbers as usual:
```haskell
one' = 1
two' = 2
three' = 3
```

### Functions on natural numbers
```haskell
isEven : Nat → Bool
isEven zero = true
isEven (suc zero) = false
isEven (suc (suc x)) = isEven x

_+_ : Nat → Nat → Nat
zero + y = y
(suc x) + y = suc (x + y)
```

### Integers in Agda
**Question**: How would you define a type of integers in Agda?
**Answer:** Here is one possibility: 
```haskell
data Int : Set where
    pos : Nat → Int
    zero : Int
    neg : Nat → Int
```
Where `pos n` represents the number *1 + n* and `neg n` represents *-(1 + n)*.

## Interactive programming in Agda
### Holes in programs
A **hole** is a part of a program that is not yet complete. A hole can be created by writing `?` or `{!!}` and loading the file (`ctrl+c ctrl+l`).

New commands for files with holes:
* `ctrl+c ctrl+,` - give information about the hole
* `ctrl+c ctrl+c` - case split on a variable
* `ctrl+c ctrl+space` - give a solution for the hole

## Types as first-class values
### The type `Set`
In Agda, types such as `Nat` and `Bool -> Bool` are themselves expressions of type `Set`.

We can pass around and return values of type `Set`, just like values of any other type.

**Example:** Defining a type alias as a function:
```haskell
MyNat : Set
MyNat = Nat
myFour : MyNat
myFour = suc (suc (suc (suc zero)))
```

### Polymorphic functions in Agda
We can define polymorphic as functions that take an argument of type `Set`:
```haskell
id : (A : Set) → A → A
id A x = x
```

For example, we have `id Nat zero : Nat` and `id Bool true : Bool`. 

**Note:** this is a first example of a dependent function: the type of `id` depends on the value of its first argument.

### Hidden arguments
To avoid repeating the type at which we apply a polymorphic function, we can declare it as a hidden argument using curly braces:
```haskell
id : {A : Set} → A → A
id x = x
```
Now we have `id zero : Nat` and `id true : Bool`.

### If/then/else as a function
We can define if/then/else in Agda as follows:
```haskell
if_then_else_ : {A : Set} →
    Bool → A → A → A
if true then x else y = x
if false then x else y = y
```
This is an example of a mixfix operator.

**Example usage:**
```haskell
test : Nat → Nat
test x = if (x ≤ 9000) then 0 else 4
```

### Polymorphic datatypes
Just like we can define polymorphic functions, we can also define polymorphic datatypes by adding a parameter `(A : Set)`:
```haskell
data List (A : Set) : Set where
[] : List A
_::_ : A → List A → List A
```

**Note**: Agda does not have built-in support for list syntax. Instead we can write `1 :: (2 :: (3 :: []))`.

### A tuple type in Agda
Agda does not have a built-in type of tuples, but we can define the  product type `A × B`:
```haskell
data _×_ (A B : Set) : Set where
    _,_ : A → B → A × B

fst : {A B : Set} → A × B → A
fst (x , y) = x

snd : {A B : Set} → A × B → B
snd (x , y) = y
```

## Total functional programming
### Reminder: pure functional programming
Question. What can or cannot happen when we
call a pure function in Haskell?
* It returns a value - YES
* It modifies a global variable - NO
* It loops forever - YES
* It throws a runtime error - YES
* It does some IO - NO
* It makes a random choice - NO

### Total functional programming:
Agda is a total language:
* NO runtime errors
* NO incomplete pattern matches
* NO non-terminating functions
So functions are true functions in the mathematical sense: evaluating a function call always returns a result in finite time.

### Why should we care about totality?
Some reasons to write total programs:
* Better guarantees of correctness
* Spend less time debugging infinite loops
* Easier to refactor without introducing bugs
* Less need to document valid inputs
Totality is also crucial for working with dependent types and using Agda as a proof assistant (see coming lectures). 

### Coverage checking
Agda performs a coverage check to ensure all definitions by pattern matching are complete:
```haskell
pred : Nat → Nat
pred (suc x) = x
```
Results in:
```
Incomplete pattern matching for pred.
Missing cases: pred zero
```

### Termination checking
Agda performs a termination check to ensure all recursive definitions are terminating:
```haskell
inf : Nat → Nat
inf x = 1 + inf x
```
Results in:
```
Termination checking failed for the following functions: inf
Problematic calls: inf x
```

### To solve or not to solve the halting problem
**Question**: isn't it impossible to determine whether a function is terminating? Or does Agda solve the halting problem?

**Answer**: No, Agda only accepts functions that are obviously terminating, and rejects all other functions.

### Structural recursion
Agda only accepts functions that are structurally recursive: the argument of each recursive call must be a subterm of the argument on the left of the clause.

For example, this definition is rejected:
```haskell
f : Nat → Nat
f (suc (suc x)) = f zero
f (suc x) = f (suc (suc x))
f zero = zero
```
This obviously always evaluates to zero, but Agda cannot see it - based on the third line it rejects the definition.