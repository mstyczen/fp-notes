# Lecture 1 #
### Today: ###
* Introduction to the course
* What is functional programming?
* Installing Stack and GHC
* A taste of Haskell

### What is functional programming? ###
* A style of programming where programs are constructed by defining and applying functions
* A toolbox of techniques aimed at writing clear code at a high level of abstraction
* A family of programming languages that support and encourage these techniques

### Why study functional programming? ###
* Write code that is clear, concise and correct
* Refactor with confidence
* Learn new ways to think about programming
* Reason about program correctness
* Explore the future of programming languages

### Course plan: ###
**Week 1-5:** Functional programming in Haskell:
* Week 1 - basics, list comprehensions
* Week 2 - defining and testing functions
* Week 3 - algebraic datatypes and type classes
* Week 4 - applicatives and monads
* Week 5 - lazy evaluation, reasoning about programs

**Week 6-7:** Agda and dependent types:

* Week 6: Agda, Curry-Howard correspondence
* Week 7 indexed datatypes, equational reasoning

**Week 8:**: rehearsal

### Course book ###
Graham Hutton, *Programming in Haskell*, second edition.

### Course examination ###
Two required components:
* Final project in Haskell (50%)
* Online exam via Weblab (50%)

### Weblab exercises: ###
Weekly, ungraded, exercises on Weblab.

## What is functional programming? ##
### Imperative vs functional programming ###
In imperative programming, computation happens primarily by *updating values*:
```java
sum = 0;
for (int i = 1; i <= 10; i++) {
    sum = sum + i;
}
```
In functional programming, computation happens primarily by applying functions:
```haskell
sum [1..10]
```
### What is functional programming? ###
**Narrow definition:** functional programming is programming with functions as first-class data.

**Broader definition:** functional programming is a toolbox of techniques for programming with functions as the primary components.

### A timeline of functional programming ###
|date|event|
|-|-|
|1930 |Alonzo Church introduces lambda calculus|
|1950 |John McCarty develops Lisp, the first functional language|
|1960 |Peter Landin develops ISWIM, the first pure functional language|
|1970 |Robin Milner develops ML, a functional language with type inference and polymorphism|
|1985 |David Turner develops Miranda, a lazy functional language|
|1987 | An international committee starts development of Haskell|
|1990s| Phil Wadler and others develop monads and type classes|
|2003 |Publication of the Haskell report|
|2007 |Ulf Norell develops Agda 2|
|2010 |New Haskell 2010 standard|
|current|New features, new libraries, better tooling, use in industry, influence on other languages|

### The toolbox of functional programming ###
* Higher-order functions
* Lambda expressions
* Algebraic expressions
* Pattern matching
* Recursion
* Immutable data
* Pure functions
* Lazy evaluation
* Monads
* Equational reasoning
* ...

A "functional programming language" is any language that allows us to use these tools!

### Example of programming languages ###
**Pure functional languages**: Haskell, Elm, PureScript, Mercury, Agda, Coq, Idris, ...

**Functional languages**: Lisp, Scheme, ML, Clojure, Racket, Erlang, OCaml, F#, ...

**Other languages with functional features**: Python, JavaScript, Scala, Go, Java, C++11, ...

### Pure vs effectful languages ###
What can happen when we call a function?
* it can return a value
* it can modify a (global) variable
* it can do some IO
* it can throw an exception
* it can go into an infinite loop

In a pure language, like Haskell, a function can only return a value or loop forever.

### What is Haskell? ### 
Haskell is a statically typed, lazy, purely functional programming language.
**Static types**: All types are checked at compile time.

**Laziness**: Expressions are only evaluated when required.

**Purity**: Functions do not have side effects.

**Note**: Static typing /= explicit type annotations: Haskell can infer types automatically!

### Why Haskell? ###
Haskell is the most popular pure functional language.
* Functional concepts are much clearer than in ‘mixed’ languages
* There is no ‘escape hatch’ to avoid functional programming

It also has some unique features such as lazy evaluation and monads.

### What is Agda? ###
Agda is a dependently typed, total functional programming language and a proof assistant:

**Dependent types**: Types can refer to program expressions.

**Totality**: Functions must be defined and terminating for all inputs.

**Proof assistance**: Express properties of programs in their types, proofs are checked by the type checker.

### Why Agda? ###
Agda has one of the most cutting-edge type systems in existence.
* Make use of the full power of functional programming at the type level
* Explore a possible future of programming languages
It also supports interactive development of programs in collaboration with the type checker.

## The Glasgow Haskell Compiler ##
### GHC and Stack ###
**GHC** (the Glasgow Haskell Compiler) is the most popular and modern Haskell compiler.

**GHCi** is the interactive mode of GHC where you can type-check and evaluate Haskell
expressions.

**Stack** is a build tool for installing Haskell programs and libraries using GHC.

## A taste of Haskell ##
### Writing Haskell scripts ###
A Haskell script has extension `.hs` and consists of one or more definitions:
```haskell
-- file MyFunctions.hs
double x = x + x
quadruple x = double (double x)
password = "123456"
checkPassword x = x == password
```

### Naming rules ###
* Names of functions and variables start with a small letter (`not`, `length`, ...)
* Names of constructors start with a capital letter (`True`, `False`, ...)
* Names of concrete types start with a capital letter (`Bool`, `Int`, ...)
* Names of type variables start with a small letter (`a`, `t`, ...)

Reserved keywords (`if`, `let`, `data`, `type`, `module`, ...) are not allowed as names.

### Function application in Haskell ###
Haskell uses `f x` for function application.
|Mathematics|Haskell|
|-|-|
|f(x)|`f x`|
|f(x,y)|`f x y`|
|f(g(x))|`f (g x)`|
|f(x,g(y))|`f x (g y)`|
|f(x)g(y)|`f x * g y`|

Function application has highest priority in the parser and associates to the left.

### The layout rule ###
Unlike C or Java, Haskell is layout-sensitive: indentation of code matters!
Definitions at the same level of indentation belong to the same block:
```haskell
a = b + c
    where
        b = 1
        c = 2
d = a * 2
```

### Haskell comments ###
```haskell
-- This is a single-line comment
x = 42 -- It can appear in-line
-- Each line must start with --
{- This is a multi-line comment
It starts with {- and ends with -}
Comments can be {- nested -}
-}
```
### Hello world in Haskell ###
A Haskell program is a script with a `main` function:
```haskell
-- file HelloWorld.hs
main = putStrLn "Hello, world!"
```
We can compile `HelloWorld.hs` using Stack:
```
$ stack ghc HelloWorld.hs
[1 of 1] Compiling Main
Linking HelloWorld ...
$ ./HelloWorld
Hello, world! 
```

### Interpreting GHC type errors ###
```
> not 't'
<interactive>:1:5: error:
• Couldn't match expected type ‘Bool’ with
  actual type ‘Char’
• In the first argument of ‘not’, namely ‘'t'’
  In the expression: not 't'
  In an equation for ‘it’: it = not 't'
```

Haskell tells us that `not` takes an argument of type `Bool`, but was given an argument of type
`Char` instead.

```
> 5 + True
<interactive>:2:1: error:
• No instance for (Num Bool) arising from
  a use of ‘+’
• In the expression: 5 + True
  In an equation for ‘it’: it = 5 + True
```
Haskell tells us that `+` works on any type that implements the `Num` typeclass, but `Bool` (the type of `True`) does not.
When you get a type error, don’t ignore it but try to understand what GHC is telling you!
