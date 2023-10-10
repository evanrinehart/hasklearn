## Assignment

Haskell doesn't have assignment. Equals sign is used in top level definitions and let-in expressions. E.g.

```haskell
-- version is a constant pair, type inferred
version = (13, 37)

-- nines evaluates to the nested list [[9,9,9],[9,9,9],[9,9,9]], explicit type signature
nines :: [[Int]]
nines =
    let x = [9,9,9] in [x,x,x]
```

Variable names must start with a lowercase letter.

`let` can contain multiple definitions.

```haskell
import Data.Char (ord, chr)

-- mixed is a triple of values (65, True, 'a')
mixed :: (Int, Bool, Char)
mixed =
    let n = ord 'A'
        b = n < 128
        c = chr (n + 32)
    in (n,b,c)
```

Variable bindings in Haskell are referentially transparent. In other words a variable can be
replaced with its definition without changing the meaning of the program.

An alternative syntax for local bindings is the `where` clause. The `where` clause attaches
to a definition or case alternative.

```haskell
-- where clause desugars to let
mixed2 = (n,b,c) where
  n = ord 'A'
  b = n < 128
  c = chr (n + 32)

-- subdefinitions can have their own nested where clause
mixed2 = (n,b,c) where
  n = x + y where
    x = 13
    y = 7
  b = n > 10
  c = chr n
```

`where` is syntactic sugar which gets translated to an equivalent `let` in the
simplified core language.

Within a file, a `let` or a `where` clause the order of definitions doesn't
matter.

## Arrays

`Data.Vector` in the `vector` package provides an efficient array type.
Vectors are appropriate for some problems but very often you will want a list
instead.

```haskell
-- zilch is an empty list
zilch = []

-- oneO is the singleton list consisting of one 'o'
oneO = 'o' : []

-- msg is a list constructed by consing 5 chars onto the empty list
msg = 'h' : 'e' : 'l' : 'l' : 'o' : []

-- alternate definition: using syntactic sugar for list of several items
msg = ['h','e','l','l','o']

-- alternate definition: using syntactic sugar specifically for list of Char
msg = "hello"
```

The `length` function computes the length of a list. This is a library function
defined in `Data.Foldable` but is automatically imported as part of the
standard Prelude.

```haskell
n = length msg -- n would evaluate to 5
```

List and tuple have special constructor syntax. But they are otherwise
ordinary algebraic data types. Detailed later.

## If

Haskell has no statements, much less if statements. But there is an if-then-else expression. E.g.

```haskell
mixed :: (Int, Bool, Char)
mixed =
  let n = if b then 13 else 37
      b = isLower c
      c = toUpper 'a'
  in (n,b,c)
```

if-then-else itself adds no additional expressive power to the language. It is
syntactic sugar for case analysis on a `Bool`.

Guard syntax allows more convenient definitions which would traditionally use a chain of
nested if-then-else.

```haskell
-- example is a constant equal to 'B'
example
  | 7 < 3       = 'A'
  | isUpper 'Z' = 'B'
  | otherwise   = 'C'

-- equivalent longer version using nested if-then-else
example =
    if 7 < 3
        then 'A'
        else if isUpper 'Z'
            then 'B'
            else
                if otherwise
                    then 'C'
                    else error "Non-exhaustive patterns in definition of example"
```

The variable `otherwise` is defined as `True` in the Prelude for use as a final catch-all.

## Functions

All the definitions above defined constant values. By adding parameters to the defining
equation you get a function definition.

```haskell
-- neighborhood is a function taking a number and constructing a triple based on it
neighborhood n = (n - 1, n, n + 1)

-- example defined by using a function, has value (6,7,8)
example = neighborhood 7
```

It's usually best to put an explicit type signature for top level definitions.
Type checker messages will greatly improve.

```haskell
neighborhood :: Int -> (Int,Int,Int)
neighborhood n = (n - 1, n, n + 1)
```

Functions in Haskell are first class values. I.e. they can be passed to and
from functions and stored in data structures.

Use a lambda expression to construct an anonymous function. This is actually
what function definitions desugar to when compiled into the simplified core
language.

```haskell
-- equivalent definition of neighborhood above
neighborhood :: Int -> (Int,Int,Int)
neighborhood = \n -> (n - 1, n, n + 1)
```

Multiparameter functions are syntactic sugar for currying[^2].

[^2]: Currying. A *curried function* of 2 arguments is a 1 argument function
returning a 1 argument function.

When reading the type signature of curried functions note the parentheses for
`->` leans to the right.

```haskell
-- circle is a function returning a function, or colloquially a 2-arg function
circle :: Int -> Int -> (Int,Int)
circle radius center = (center - radius, center + radius)

-- equivalent definition of circle
circle radius = \center -> (center - radius, center + radius)

-- fully desguared definition of circle
circle = \radius -> \center -> (center - radius, center + radius)

-- applying circle to 2 arguments works because parentheses for application leans left
circle1 :: Int -> (Int,Int)
circle1 center = circle 1 center -- same as: (circle 1) center

-- another way to define circle1, works naturally
circle1 = circle 1
```


## Loops

Haskell is purely functional[^1]. So many tasks which would use loops and side effects are
accomplished another way or wouldn't come up. One place where loops with effects are
unavoidable is when executing a sequence of I/O operations. In this case recursion or
library functions can be used to iterate in the desired way and execute the actions.

```haskell
-- a loop which does I/O and stops on a certain condition
main = do
    c <- getChar
    if c < 'A'
        then do
            putStrLn "char too low try again"
            main
        else do
            putStrLn "char high enough"

-- helper functions can be defined and used to handle patterns of recursion
main = do
    for ["Hello", "World", "!"] putStrLn
    return ()
```

`do` notation adds no additional expressive power to the language and has a
straightforward desugaring.

```haskell
-- main is a constant IO action defined with do notation
main = do
    c <- getChar
    if c < 'A'
        then do
            putStrLn "char too low try again"
            main
        else do
            putStrLn "char high enough"

-- equivalent action without do notation
main =
    getChar >>= \c ->
    if c < 'A'
        then
            putStrLn "char too low try again" >>
            main
        else
            putStrLn "char high enough"
```

The `>>` operator combines two IO actions sequentially. The IO action `act1 >>
act2` is equivalent to executing `act1` first then upon completion executing
`act2`.  The result of the first IO action is discarded.

The `>>=` operator allows the result of the first IO action to be used. The
second argument is a function to handle the first result and produce a followup
IO action.

An imperative-style program in do notation is essentially a bunch of actions
chained together with callback functions.

[^1]: A pure function has no side effects and only depends on its input.
Example: the `abs` function in C. Counterexample: the `gettimeofday` function
in C. In Haskell all functions are pure. This is the sense behind "purely
functional". The decision to make Haskell pure was made because it was felt
that lazy evaluation mixed with side-effects would be insane.

## Practical Example of Function That Takes Functions as Argument

Functions can take functions as arguments. Among many other things this allows us to factor out an
otherwise repetitive acquire-use-release pattern. In Control.Exception there is a library function
`bracket` which encapsulates this. Quoting the docs:

```haskell
bracket :: IO a          computation to run first ("acquire resource")
        -> (a -> IO b)   computation to run last ("release resource")
        -> (a -> IO c)   computation to run in-between
        -> IO c
```

`bracket` builds a non-trivial IO action from 3 arguments: one IO action and
two functions. The functions both take a resource acquired by executing the
first argument and produce IO actions to be used in the right sequence.

```haskell
import Control.Exception (bracket)

-- action which opens a file, reads the file, closes the file, and returns the first line
getFirstLine :: IO String
getFirstLine =
    bracket
        (openFile "filename" ReadMode)
        (hClose)
        (\fileHandle -> hGetLine fileHandle)

```

The `bracket` pattern not only reduces repetition but makes sure resources are
cleaned up if there was an exception raised between acquiring and releasing.

## Computations That Can Fail

Haskell has an exception system accessed through Control.Exception. Exceptions
can be thrown from any haskell code but only caught using an IO action. One use
for exceptions is I/O errors which could happen for many reasons, but are rare
enough you don't want to complicate the code dealing with them all immediately.
Another use for exceptions are computations that encountered a supposedly
impossible situation, indicating a bug.  E.g. we thought we ruled out division
by zero but it happened anyway, throwing an exception.

Sometimes we want to admit that a computation really can fail and explicitly
build that into the type signature. Take for example a parser. The point of the
parser is to verify the well-formedness of the input and return something more
useful. To indicate parsing failed we can return `Nothing` instead of something
useful.

```haskell
parseDigit :: Char -> Maybe Int
parseDigit c =
    let code = ord c in
    if 48 <= code && code <= 57
        then Just code
        else Nothing

-- parseDigit '7' evaluates to Just 7
-- parseDigit 'A' evaluates to Nothing
```

`Nothing` and `Just something` construct data of the `Maybe t` type, assuming
`something` has type `t`.  The `Maybe` result can be scrutized directly using
case analysis:

```haskell
-- this expression evaluates to 17
case parseDigit '7' of
    Just n  -> n + 10
    Nothing -> 0

-- same thing but using a helper function from Data.Maybe
maybe 0 (\n -> n + 10) (parseDigit '7')
```

The `case` expression is a fundamental primitive in Haskell for analyzing data.
Many high level syntax forms are translated to `case` expressions in the
simplified core language.

For example the if-then-else expression translates to a `case` on the data type
`Bool`[^3]:

[^3]: `True` and `False` are the two constructors for the data type `Bool`

```haskell
-- this
if x < 10 then "ok" else "not ok"

-- is syntactic sugar for
case x < 10 of
    True  -> "ok"
    False -> "not ok"
```



## Bottom (⊥)

In the semantics of Haskell, an expression ideally evaluates to a value of some
type. But because of recursive definitions and other phenomena, evaluation
sometimes doesn't result in anything at all.

```haskell
-- problematic recursive definition, what is the value of `x'
x = x + 1

-- unfortunate numeric computation, what is the value of `n'
n = 37 `div` 0

-- bad use of infinite data, what is the value of `end'
ones = 1 : ones -- i.e. 1 : 1 : 1 : 1 : ...
end = last ones
```

To neatly explain what programs containing the above troublesome components
compute, Haskell includes a bottom "value" (⊥) in the denotation (or
interpretation) of the data types.

Bottom represents the lack of value or a totally undefined value. It can result
from an infinite loop, an error caused by a bad computation, or any computation
that fails to produce a result. The above variables `x`, `n`, and `end`
evaluate to ⊥, semantically speaking. Attempting to actually evaluate a ⊥ could
throw an exception or freeze up the computer and is generally not a good idea.

Because Haskell uses lazy evaluation the presence of ⊥ in a program might not
be a big deal. In this example we use `undefined` to intentionally put a ⊥ in
the code.

```haskell
-- example evaluates to 13, despite a ⊥ lurking in the list
numbers = [7, undefined, 9, 13]
example = last numbers
```

This variable `example` evaluates to 13 because the `last` function walks down
to the end of list without evaluating the list contents. If it did, it would
crash. Nonetheless `numbers` itself has a well-defined semantic value:
[7, ⊥, 9, 13].


`undefined` can be used where the code is unfinished but you want to run the
code anyway. As long as `undefined` is not evaluated, no problem.

In provenly-impossible branches of the code, `undefined` would work but `error
"specific message"` would be more helpful when the impossible happens.

## FFI

Haskell has an FFI which allows interfacing with foreign code and data.

The nuts and bolts of the FFI mirror the C interface on the host system.

Usually foreign functions are given an IO-action type or function returning
IO action. But if the foreign function is pure it can be given a non-IO type.

## Data Types

Haskell allows the programmer to define new algebraic data types (ADTs)[^4].

[^4]: Not to be confused with abstract data types.

Examples of ADT definitions, including some types already seen:

```haskell
data Bool = False | True
--    ^       ^     ^
--    |       |_____|____ 2 nullary data constructors
--    |_ typename

data Maybe t = Nothing | Just t
--    ^    ^       ^      ^
--    |    |       |      |_ 1-ary data constructor expecting value of type t
--    |    |       |_ nullary data constructor
--    |    |_ type variable
--    |
--    |_ type constructor expecting a type e.g. Maybe Char is a type

data Pair a b = MkPair a b
--    ^   ^ ^     ^
--    |   | |     |___ one 2-ary data constructor expecting values of type a then b
--    |   |_|__ two type variables
--    |____ type constructor expecting 2 types, e.g. Pair Int Char is a type
```

The example definitions of `Bool` and `Maybe` are the actual definitions used by
the base library.

The `Pair` example defines a pair type equivalent to `(a,b)` but without any
syntactic sugar for the constructor. In actual ADT definitions, constructors
must begin with a capital letter or be an infix operator starting with `:`
(colon).

This is a sugarless definition of a type equivalent to list, demonstrating a
recursive data type.

```haskell
data List t = MkEmpty | t :< List t
--    ^   ^       ^       ^
--    |   |       |       |_ 2-ary data constructor expecting value of type t then a list of t
--    |   |       |_ nullary data constructor
--    |   |_ type variable
--    |
--    |_ type constructor expecting one type

-- equivalent pseudo-Haskell definition of the standard list data type, not real code
data [t] = [] | t : [t]

-- pseudo-Haskell definition of other standard types
data Int = INT_MIN | ... | -3 | -2 | -1 | 0 | 1 | 2 | 3 | ... | INT_MAX
data Char = '\0' | ... | 'A' | 'B' | 'C' | 'D' | ... | CHAR_MAX
data (a,b) = (a,b) -- type constructor looks the same as data constructor, is fine
data (a,b,c) = (a,b,c)
data (a,b,c,d) = (a,b,c,d)
...
```

Even though the pseudo-Haskell definitions above are not valid Haskell syntax,
pattern matching acts as though those definitions are real, shown below.

```haskell
-- test if the list empty
null :: [a] -> Bool
null list =
    case list of
        []  -> True            -- pattern [] matches only [], otherwise check next pattern
        _:_ -> False           -- pattern _ ignores and matches anything, even bottom

-- same function using multiple defining equations and pattern matching
null []    = True
null (_:_) = False

tailMaybe :: [a] -> Maybe [a]
tailMaybe []       = Nothing
tailMaybe (_:list) = Just list -- variable pattern matches anything and binds the value to variable

fst :: (a,b) -> a
fst (x,_) = x

snd :: (a,b) -> b
snd (_,y) = y

is7or11 :: Int -> Bool
is7or11 7  = True              -- pattern 7 only matches 7, otherwise check next equation
is7or11 11 = True              -- pattern 11 only matches 11, otherwise check next equation
is7or11 _  = False

```

Any algebraic data type can be deconstructed using case analysis on the constructors
and pattern matching of the constructor contents.

## Records

A record is an ADT defined with field names.

```haskell
data Employee = MkEmployee { name :: String, section :: Char, salary :: Int }
--     ^            ^         ^        ^
--     |            |         |        |_ field type
--     |            |         |_ field name
--     |            |__ one 3-ary constructor
--     |__ type name (nullary type constructor)

alice :: Employee
alice = MkEmployee { name = "Alice", section = 'K', salary = 50000 }
betty = MkEmployee { name = "Betty", section = 'T', salary = 210000 }
chuck = MkEmployee "Chunk" 'K' 50000 -- without fields, acts like constructor as usual
```

The field names automatically generate field selector functions for the type. Note
the field selectors could be defined manually using pattern matching.

```haskell
-- these are generated automatically unless NoFieldSelectors extension is enabled.
name :: Employee -> String
name MkEmployee { name = x } = x

section :: Employee -> Char
section MkEmployee { section = x } = x

-- etc
```

Records have special update syntax which constructs an updated version of the original.

```haskell
-- function returns new record based on emp with two fields modified
promote :: Employee -> Employee
promote emp = emp { section = 'T', salary = salary emp * 4 }
```

Update syntax adds no additional expressive power to the language. It desugars
into pattern matching and record reconstruction.


## Numeric Literals

Numeric literals in previous examples mostly had type `Int`. But numeric
literals also work where Double, Word8, Rational, and other numeric types are
expected. How?

Because of a special translation prior to type checking. 

```haskell
-- source code involving numeric literals
circle1 center = circle 1 center

-- in theory, is translated to the following (where, this time, 1 has type Integer)
circle1 center = circle (fromInteger 1) center

-- fromInteger's type as defined in the Num typeclass
fromInteger :: Num a => Integer -> a
```

In the case of `circle1` there is no explicit type information so it is
inferred to have the most general possible type.

```haskell
circle1 :: Num a => a -> (a, a)
```

This is an example of a polymorphic type. A user of `circle1` can substitute
any type for `a` as long as there's a `Num` instance for it. The choice
determines how `1` will be interpreted by determining an implementation of
`fromInteger`.

Performance note. The `fromInteger` translation above might not necessarily
make it into compiled code. When in doubt check Core output to see what GHC
does.

### Numeric Type Defaulting

Simple expressions containing numeric literals given at the interactive
interpreter can easily be ambiguous. I.e. the interpreter doesn't know what
Num type you wanted and normally wouldn't be able to start evaluation.  But

```haskell
ghci> 2 + 2 -- ambiguous number type
4           -- somehow computes anyway
```

There is special defaulting behavior for the `Num` class which picks a type
when it would otherwise be ambiguous. By default the default `Num` type is
`Integer`[^5]. The default can be configured per module, but in actual code
it's better to write explicit type signatures.

```haskell
ghci> 255 + 1 :: Word8  -- type annotation determines the number type instead of defaulting
0
```

[^5]: `Integer` is the standard arbitrary precision integer type.


## Numeric Conversions

Haskell has many numeric types and no implicit conversions. Instead the basic
way to convert between numeric types is to use a small number of standard
library helpers.

```haskell
-- converting from an integer-like type
fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger

-- converting from one real-like type to another
realToFrac :: (Real a, Fractional b) => a -> b
realToFrac = fromRational . toRational

-- converting from a real-like type to an integer-like type
floor    :: (RealFrac a, Integral b) => a -> b
ceiling  :: (RealFrac a, Integral b) => a -> b
truncate :: (RealFrac a, Integral b) => a -> b
round    :: (RealFrac a, Integral b) => a -> b
```

These generic conversions use methods in the standard numeric type classes.
More specialized conversions are available for special use cases.

## Some Numeric Types Available

Haskell allows the programmer to define new numeric types by implementing
`Num`.  But several numeric types are already included in the base library.

| Number Types | Imported From | Notes                         |
|--------------|---------------|-------------------------------|
| `Int`        | `Prelude`     | limited range machine integer |
| `Integer`    | `Prelude`     | arbitrary precision integer   |
| `Double, Float` | `Prelude` | double and single precision ieee754 floating point type |
| `Rational` | `Data.Rational` | rational number type |
| `Complex a` | `Data.Complex` | complex numbers with components of type `a` |
| `Int8, Int16, Int32, Int64` | `Data.Int` | sized integers |
| `Word8, Word16, Word32, Word64` | `Data.Word` | sized unsigned integers |
| `Word` | `Data.Word` | the unsigned version of Int |
| `Deci, Centi, Milli, Micro, Nano, Pico` | `Data.Fixed` | fixed point decimal |
| `Natural` | `Data.Natural` | non-negative Integer |
| `CInt,CUint,CDouble,CChar,CSize,...` | `Foreign.C.Types` | numeric types matching those in the platform's C environment, for FFI |
| `Scientific` | `scientific` package | scientific notation number type |

## Modules

Haskell code is organized into modules. One file per module. The module
name determines its location in the source directory tree.

```haskell
-- module exports 1 type, all 4 constructors, and 1 function without an export list
module Path.To.Colors where

data Color = Red | Green | Blue | Yellow

opponent :: Color -> Color
opponent Red    = Green
opponent Green  = Red
opponent Blue   = Yellow
opponent Yellow = Blue
```

Each module can include an export list of data types, constructors, functions,
type classes, and constants it would like to export.

```haskell
-- export list exports Color type, all constructors, and the one function
module Colors (
    Color(..),
    opponent
) where

...
```

```haskell
-- export list exports Color type, a function, but hides the constructors
module Colors (
    Color,
    opponent
) where

...
```

A common pattern is to not export the constructors, but export one or more
functions ("smart constructors") which verify a condition before returning
a constructed value.

The `main` IO action goes in the Main module. This action is executed to begin
running your program.

## End of Chapter 1

This chapter is incomplete.
