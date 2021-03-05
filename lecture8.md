# Lecture 8
### Lecture plan
* The `Maybe` monad
* The `Monad` typeclass
* Other monads: state, exceptions, non-determinism, ...
* The Monad laws
* The `Parser` monad

### Looking for patterns
A type class capture a family of types that share some structure:
* `Ord`: types with a total ordering, like `<`
* `Monoid`: types with an `empty element` and a `concatenation` like `[]` and `(++)`
* `Functor`: types constructors where we can `map` a function over all values

Why look for these shared patterns?
* Avoid boilerplate code (DRY)
* Enforce abstraction barriers
* Typeclass laws provide a sanity check for correctness
* Deeper understanding of your class

**"Type classes are the design patterns of FP"**

## The Maybe monad
### Walk the line
```haskell
type Birds = Int
type Pole = (Birds,Birds)

checkBalance :: Pole -> Maybe Pole
checkBalance (x,y)
    | abs (x-y) < 4 = Just (x,y)
    | otherwise = Nothing

landL :: Birds -> Pole -> Maybe Pole
landL n (x,y) = checkBalance (x+n,y)

landR :: Birds -> Pole -> Maybe Pole
landR n (x,y) = checkBalance (x,y+n)

landingSequence :: Pole -> Maybe Pole
landingSequence pole0 = case landLeft 1 pole0 of
    Nothing -> Nothing
    Just pole1 -> case landRight 4 pole1 of
        Nothing -> Nothing
        Just pole2 -> case landLeft (-1) pole2 of
            Nothing -> Nothing
            Just pole3 -> case landRight (-2) pole3 of
                Nothing -> Nothing
                Just pole4 -> Just pole4
```

**Can we do better? Yes, we can!**

```haskell
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>= f = Nothing
Just x >>= f = f x

landingSequence :: Pole -> Maybe Pole
landingSequence pole0 = Just pole0
    >>= landLeft 1
    >>= landRight 4
    >>= landLeft (-1)
    >>= landRight (-2)
```

### The bind operator
The function `(>>=)` is called **bind**. The general type of bind is:
```haskell
(>>=) :: m a -> (a -> m b) -> m b
```
where the constructor `m` is a **monad**.

## The Monad class
### `Functor`, `Applicative`, and `Monad`
```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
```

### The sequencing operator `(>>)`
The **sequencing operator)** executes one action after another, ignoring the output of the first.
```haskell
(>>) :: Monad m => m a -> m b -> m b
mx >> my = mx >>= (\_ -> my)
```

## The State monad
### Example: Generating random numbers
**Solution:** write a pure function that takes a random seed as input.
```haskell
import System.Random

randomNumber :: StdGen -> (Int, StdGen)
randomNumber = random
```

**Exercise**: Roll 3 six-sided dice & add results.
```haskell
roll :: StdGen -> (Int, StdGen)
roll gen = let (x, newGen) = random gen in (x `mod` 6, newGen)

roll3 :: StdGen -> (Int, StdGen)
roll3 gen0 = 
    let (die1, gen1) = roll gen0
    (die2, gen2) = roll gen1
    (die3, gen3) = roll gen2
    in (die1+die2+die3, gen3)
```
**Can't we do better??**

### The `State` monad

```haskell
newtype State s a = State (s -> (a,s))

get :: State s s
get = State (\st -> (st,st))

put :: s -> State s ()
put st - State (\_ -> ((), st))
```

### Functor and applicative for state
```haskell
instance Functor State where
    fmap f (State h) =
        State (\oldSt ->
            let (x, newSt) = h oldSt
            in (f x, newSt))

instance Applicative State where
    pure x = State (\st -> (x,st))

    State g <*> State h =
        State (\oldSt ->
            let (f, newSt1) = g oldSt
                (x, newSt2) = h newSt1
            in (f x, newSt2)) 
```

### Binding the state
```haskell
runState :: State s a -> s -> (a,s)
runState (State h) = h

instance Monad State where
    return x = pure x
    
    State h >>= f =
        State (\oldSt ->
            let (x, newSt) = h oldSt
            in runState (f x) newSt)
```

### Rolling dice with `State`
```haskell
randomInt :: State StdGen Int
randomInt = State random
roll :: State StdGen Int
roll = randomInt >>= \x -> return (x `mod` 6)

roll3 :: State StdGen Int
    roll3 = roll >>= \die1 ->
        roll >>= \die2 ->
            roll >>= \die3 ->
                return (die1+die2+die3)
```

### `do` notation for monads
We can use `do` notation for any monad!
```haskell
roll3 :: State StdGen Int
roll3 = do
    die1 <- roll
    die2 <- roll
    die3 <- roll
    return (die1+die2+die3)
```

### Desugaring of `do` notation:
With `do` notation:
```haskell
do
    x <- f
    g x
    y <- h x
    return (p x y)
```
Without `do` notation:
```haskell
f >>= (\x ->
    g x >> (
        h x >>= (\y ->
            return (p x y)
        )
    )
)
```

## More monads
### The `Either` monad
We can see `Either` as a generalization of `Maybe`:
* `Right` takes the role of `Just`
* `Left err` is a more informative version of `Nothing`

```haskell
instance Monad (Either e) where
    return x = Right x
    Left err >>= f = Left err
    Right x >>= f = f x
```

### The list monad
We can see `[]` as another generalization of `Maybe`:
* `[x]` takes the role of `Just x`.
* `[]` takes the role of `Nothing`
* Functions can have multiple outputs

```haskell
instance Monad [] where
    return x = [x]
    xs >>= f = [y | x <- xs, y <- f x]
```

### Using the list monad
```haskell
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do
    x <- xs
    y <- ys
    return (xs, ys)
```
Compare this with list comprehensions:
```haskell
pairs xs ys = [(x,y) | x <- xs, y <- ys]
```
We've been using the list monad all this time, since week 1!

### The `IO` monad
For `IO`, we could define a `Monad` instance as follows:
```haskell
instance Monad IO where
    return x = return x
    act >>= f = do
        x <- act
        f x
```
But this is a circular definition!

Instead, the `Monad IO` instance is built into Haskell.

### Other monads
The reader monad gives access to an extra input of some type `r`:
```haskell
newtype Reader r a = Reader (r -> a)
```
The writer monad allows writing some output of type `w`:
```haskell
newtype Writer w a = Writer (w,a)
```
See exercises on Weblab!

### Monads in other languages
Several other languages use monads too:
* The `std::optional` library defines the `Maybe` monad in C++
* The `flatMap` function in scala is precisely `(>>=)` for the list monad
* Promises in JAvaScript (and other languages) form a monad, with `.then()` acting as `(>>=)`

### The Promise monad in JavaScript
`async/await` is equivalent to `do`-notation.
```js
async function getBalances() {
    const accounts = await web3.eth.accounts();
    const balances = await Promise.all(accounts.map(web3.eth.getBalance));
    return Ramda.zipObject(balances, accounts);
}
```

## Monad laws
### Monad law #1: Left identity
```haskell
return x >>= f = f x
```
**Intuition:** We can remove `return` statements in the middle of a `do` block.

### Monad law #2: Right identity
```haskell
mx >>= (\x -> return x) = mx
```
**Intuition:** we can eliminate `return` at the end of a `do` block.
```haskell
do
    ...
    x <- mx
    return x
-- is equivalent to:
do
    ...
    mx
```

### The associativity law
```haskell
(mx >>= f) >>= g 
-- is equivalent to:
mx >>= (\x -> (f x >>= g))
```
**Intuition:** We can 'flatten' nested `do`-blocks.
```haskell
do
    y <- do x <- mx
        f x
    g y
-- is equivalent to:
do
    x <- mx
    y <- f x
    g y
```

### Monoids vs monads
For any monad `m`, we get a monoid `Wrap m`:
```haskell
newtype Wrap m = Wrap (m ())

instance Monad m => Semigroup (Wrap m) where
    Wrap m1 <> Wrap m2 = Wrap (m1 >> m2)

instance Monad m => Monoid (Wrap m) where
    mempty = Wrap (return ())
```
The monoid laws for `Wrap m` follow from the monad laws for `m`!

## Monadic parsing:
At a basic level, a parser turns strings into objects of some type:
```haskell
type Parser a = String -> a
char :: Parser Char
char (x:[]) = x
char _ = error "Parse failed!"
```
Problems:
* No option for graceful failure
* Hard to compose parser

### A better parser
```haskell
type Parser = String -> [(a, String)]

item :: Parser Char
item (x:xs) = [(x:xs)]
item [] = []
```

* Parsing returns a list of possible parser
* Each parse comes with a 'remainder' of the string for further parsing

### A sick poem about parsers:

*A parser of things*

*is a function from strings*

*to lists of pairs*

*of things and strings!*

### A monadic parser:
We can now make use of `do` notation to write parsers:
```haskell
three :: Parser (Char, Char)
three = do
    c1 <- char
    c2 <- char
    c3 <- char
    return (c1,c3)
```

### Writing monadic parsers:
We can now make use of `do` notation to write parsers:
```haskell
word :: Parser String
word = do
    c <- char
    if (isSpace c) then
        return ""
    else do
        cs <- word
        return (c:cs)
```

### Picky parsers:
We can define a parsers `empty` that always fails:
```haskell
empty :: Parser a
empty = Parser []
```
This is useful to write parsers that only succeed when some property is satisfied:
```haskell
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- char if p x then return x else empty

digit :: Parser Char
digit = sat isDigit
```

### Choosing between parses
We can combine two parsers by using the second one if the first one fails:
```haskell
(<|>) :: Parser a -> Parser a -> Parser a
(Parser f <|> Parser g) = Parser
    (\inp -> case f of
    [] -> g inp
    result -> result)
-- Parsing an optional thing
maybeP :: Parser a -> Parser (Maybe a)
maybeP p = (pure Just <*> p)
<|> (pure Nothing)
```

### Parsing several things:
`some` repeats a parser one or more times.
`many` repeats a parser zero or more times.

```haskell
some many :: Parser a -> Parser [a]
many x = some x <|> pure []
some x = pure (:) <*> x <*> many x

nat :: Parser Int
nat = do xs <- some digit
return (read xs)
```