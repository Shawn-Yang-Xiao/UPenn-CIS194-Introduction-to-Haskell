# Haskell

with UPenn CIS 194 2013 && Upenn CIS 1940 2023
and with *Real World Haskell*

## Week 1: What is Haskell?

**Functional**
  - Functions are *first-class* 
  - The meaning of Haskell programs is centered around evaluating expressions rather than executing instructions.

**Pure**
  - No mutation.
  - Calling the same function with the same argument results in the same output.


**Lazy**

In Haskell, expressions are not evaluated until their results are needed. 
  - Enables a more compositional programming style.
  - Make it possible to work with infinite data structures.
  - Leads to complications when reasoning about time and space usage.

**Statically typed**

Every Haskell expression has a type.

**Declarations and variables**

In Haskell, `=` denotes "definition" instead of "assignment".

**Basic Types**

integers `Int`, floating-pointer number `Double`, booleans `Bool`, characters `Char`, and strings `String`.

**Constructing lists**

`[]` empty list.

`[Int]` denote a list of integers.
*cons* operator `(:)`.


## Week 2: Type Construct

- Enumeration types:
  - `data TypeName = ele1 ; | ele2 ; ... ; deriving (Show, Eq, ... )`
  - `data AlgDataType = Constr1 Type11 Type12; | Constr2 Type 21 ; ... ; deriving (Show, Eq , ...)`
  - `func :: type1 -> type2 ; func ele1 = ele2 ; func ele3 = ele4 ; ... ; func _ = rest`

- Pattern Matching
  - Finding out which constructor a value is built with.
  - `_` matches anything
  - `x@pat` match a value against the pattern `pat` and give the name `x` to the entire value being matched.
  - Patterns can be nested. You can match one pattern in another.


## Week 3: Recursion Patterns

- Map
  - `map :: (a -> b) -> [a] -> [b]`
- Filter
  - `filter :: (a -> Bool) -> [a] -> [b]`
- Fold
  - ```Haskell
    fold :: (a -> b -> b) -> b -> [a] -> b
    fold f z [] = z
    fold f z (x : xs) = f x (fold f z xs)
    ...
    foldr f z (x:xs) = x `f` foldr f z xs
    ...
    foldl f z (x:xs) = let z' = z `f` x in foldl f z' xs
    ...
    seq :: a -> b -> b  -- when applied to x and y will first reduce x then return y
    foldl' f z (x:xs) = let z' = z `f` x in seq z' $ foldl' f z' xs
    ```
  - fold is already provided in the standard Prelude, under the name of `foldr :: (a -> b -> b) -> a -> [a] -> b`
  -	There is also `foldl`, which folds from the left (use `foldl'` from Data.List is more efficient)

  - `foldr` needs to push all argument into the stack, so it may cause overflow, try `foldr (+) 0 [1..1000000]`.
  - `foldl` will also trigger stack overflow, as the `let` sequence will grow before calculation of the final element.
  - `foldl'` doesn't build a huge thunk and thus is efficient.


- Anonymous functions (lambda abstraction)
  - Here is an example
    ```Haskell
    greaterThan100 [1, 9, 349, 6, 907] = [349, 907]
    greaterThan100 :: [Integer] -> [Integer]
    greaterThan100 xs = filter (\x → x > 100) xs
    Or equivalent: greaterThan199 xs = filter (>100) xs
    ```
  - `\` is supposed to be as lambda, function `\x -> x > 100` outputs whether x is greater than 100
  - `(>100)` is an operator section: `?y` is equivalent to `\x -> x ? y`, and `y?` Is equivalent to `\x -> y ? x`
- Function composition `(f.g)`
  - useful in writing concise, elegant code
- Currying and partial application
  - `\x y z ->` is syntax for `\x -> (\y -> (\z -> ... ))`
- Wholemeal programming
  - ```Haskell
    foobar :: [Integer] -> Integer
    foobar [] = 0
    foobar (x:xs)
      | x > 3 = (7*x+2) + foobar xs
      | otherwise = foobar xs

    foobar' :: [Integer] -> Integer
    foobar' = sum . map (\x -> 7*x + 2) . filter (>3)
    ```
- Avoid the problem of doing too much at once and working at too low of a level.
- This defines foobar' as a "pipeline" of three functions. First filter, then apply to every element, eventually sum them up.



## Week 4: More Polymorphism and Type Classes

- Polymorphic data types
  - `Maybe`
    - ```Haskell
      data Maybe a
        = Nothing
        | Just a
      ```
    - a type constructor used to make a type proper.

- Total and partial functions
  - `head` is a partial function, and there are certain inputs for which `head` will crash. When give `head` an empty list, it crashes. 



- Parametricity
  - `a -> a -> a` is a promise that `a` function with this type will work no matter what type the caller chooses, otherwise specify the type
- Type classes
  - Num, Eq, Ord and Show are type classes, and we say that `(==)`, `(<)`, `(+)` are "type-class polymorphic".
  - `deriving (Eq, Ord, Show)` Tell GHC to automatically derive instances of the `Eq`, `Ord`, and `Show` type classes for our data type.
- Standard type classes
  - Ord: totally ordered, any two elements can be compared to see which is less than the other.
  - Num: numeric types, support things like addition, subtraction, and multiplication.
  - Show: defines the method Show, which is used to convert values into Strings
  - Read: the dual of Show
  - Integal: represents whole number types such as Int and Integer


## Week 5: Lazy Evaluation

- Strict evaluation
  - Opposite to lazy evaluation, function arguments are completely evaluated before passing them to the function.

- Lazy evaluation
  - Pattern matching drives evaluation
    - Expressions are only evaluated when pattern-matched
  - Short-circuiting operators
    - `(&&)` and `(||)` does not pattern-match on their second argument, but `&&!` will do.
  - Infinite data structures
    - Lazy evaluation also means that we can work with infinite data structures. 
    - ```Haskell
      repeat :: a -> [a]
      repeat x = x : repeat x

      take :: Int -> [a] -> [a]
      take n _ | n <= 0 = []
      take _ [] = []
      take n (x : xs) = x : take (n - 1) xs
      ```


- Consequences
  - Purity
  - To the recursion function, whether the list should be recursed before processed, or computed first before unwinding.
  - For example, foldl’ requires the second argument to be evaluated before it proceeds, so a large thunk never builds up (compared with function foldl).
- Infinite data structures
  - Lazy evaluation means that we can work with inifinite data structures. Defining an infinite data structure actually only creates a thunk, which we can think of as a “seed”, out of which the entire data structure can potentially grow.
- Dynamic programming
  - One must take great care to fill in entries of a dynamic programming table in the proper order, so that its dependencies have already been computed. If we get the order wrong, we gor bogus results.

## Week 6: Introduction to Type Classes

- Type classes
  - Some examples:
    ```Haskell
    (+) :: Num a => a -> a -> a
    (==) :: Eq a => a -> a -> Bool
    show :: Show a => a -> String
    ```
  - vs. Java interfaces
    - Type classes are more general than Java interfaces
      - Type classes are declared separately from the declaration of the corresponding types.
      - The types are more general and flexible.
    - Standard type classes
      - `Ord` is for types whose elements can be totally ordered, provides comparison operations like `(<)` and `(<=)`
      - `Num` is for "numeric" types, which support things like addition, subtraction and multiplication. 
      - `Show` defines the method `show`
      - `Read` is the dual of `Show`; `read` converts `String`s into values.


## Week 7: Functor and Foldable

- A brief digression on kinds
  - `Maybe :: * -> *`
- Functor
  - Make a type class so that we can write a higher-order function.
  - ```Haskell
    class Functor f where
      fmap :: (a -> b) -> f a -> f b
    ```
    or
    ```Haskell
    instance Functor Int where
      fmap = ...
    ```

- Foldable
  - To generalize `foldr` beyond lists, we can try the `Foldable` type class. 
    ```Haskell
    class Foldable t where
      foldr :: (a -> b -> b) -> b -> t a -> b
    ```
  - For example, we can write a `Foldable` instance for lists:
    ```Haskell
    instance Foldable [] where
      foldr :: (a -> b -> b) -> b -> [a] -> b
      foldr = -- same implementation as we saw previously
    ```


## Week 8: I/O

- The problem with purity
  - Functions may not have any external effects, including pringting on the screen. 
  - Functions may not depend on external stuff, i.e. read from the keyboard, filesystem, or network. 
- The IO type
  - Haskell compiler uses `main :: IO()` which is an I/O action with type IO()
  - Use `putStrLn :: String -> IO ()`, we can write
    ```Haskell
    main :: IO ()
    main = putStrLn "Hello, Haskell!"
    ```
  - `do` defines a sequence of actions
- Combining IO
  - `(>>) :: IO a -> IO b -> IO b` (and then) running two input computation in sequence
  - But `(>>)` (bind) is not applicable when we want the second output to be dependent on the first. So we introduce `(>>=) :: IO a -> (a -> IO b) -> IO b`.


## Week 9: Monads

- Monad
  - The `Monad` type class is defined as follows:
    ```Haskell
    class Monad m where
      return :: a -> m a

      (>>=) :: m a -> (a -> m b) -> m b

      (>>) :: m a -> m b -> m b
      m1 >> m2 = m1 >>= \_ -> m2
    ```
- Monad combinators
  - Using only `return` and `(>>=)`, we can build up a lot of nice general combinators for programming with monads.
  - `sequence` takes a list of monadic values and produces a single monadic value which collects the results. 
    - In the case of `Maybe` it means that the entire computation succeeds only if all individual ones do; in the case of `IO` it means to run all the computations in sequence; and so on.
    - 

## Week 10: Property-Based Testing

- generating random data
  - ```Haskell
    data Gen a = Gen (Rand -> a)
    
    instance Monad Gen where
      return :: a -> Gen a
      (>>=) :: Gen a -> (a -> Gen b) -> Gen b
    ```

## Week 11: Parsing in Haskell

^_^ Haskell makes writing parsers simple and fun!

- A Parser is a function
  ```Haskell
  type Parser :: String -> StructureObject
  ```
- A Parser doesn't need to consume all of its input
  ```Haskell
  type Parser = String -> (StructuredObject, String)
  ```
- A Parser should be polymorphic and be able to fail
  ```Haskell
  type Parser a = String -> Maybe (a, String)
  ```



## Chapter 6: Using Typeclasses

Typeclasses define a set of functions that can have different implementations depending on the type of data they are given. 

Here is a simple example:
```Haskell
class BasicEq a where
  isEqual :: a -> a -> Bool
```

In this example, the newly defined function is applicable to variables of the typeclass.

Then we give concrete classes "interface" of the typeclass by `instance`:
```Haskell
instance BasicEq Bool where
  isEqual True True = True
  isEqual False False = True
  isEqual _ _ = False
```

If there is some dependent relationships between functions in one typeclass, we can relate them to each other and avoid redundant job of defining both in instances.

```Haskell
class BasicEq a where
  isEqual :: a -> a -> Bool
  isEqual x y = not (isNotEqual x y)

  isNotEqual :: a -> a -> Bool
  isNotEqual x y = not (isEqual x y)
```


