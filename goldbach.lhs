I'm going to construct a lambda term `goldbach` that reduces to a normal form
if and only if there is a counterexample to the goldbach conjecture.

For this I am using Haskell which is a functional programming language. I am
writing my functions completely in the style of lambda calculus in a way that
should be understandable.
λx. M
is written as
\x -> M

Note that we are not using church numerals, instead Haskell uses some low-level
representation of integers which means that we are limited to some maximum
value, but for values below that maximum our term should be sound.

This is a literate haskell file, so if you have the haskell compiler (GHC) installed
you can run `ghci goldbach.lhs` and try and evaluate goldbach yourself!

To open things up, we need to import a few things and rename some functions, so
feel free to ignore this

> {-# LANGUAGE UnicodeSyntax #-}
> import Prelude hiding (any, all, and)
> import Control.Monad.Fix (fix)
> 
> -- Renaming things to match the notes
> y = fix
> and = (&&)
> type ℕ = Int

Now we are going to define an `all` function. `all` takes a 'predicate' function, p,
from ℕ -> Bool, this means it states whether a certain property holds for a natural n.

It also takes a starting value x, the term then evaluates, incrementally for all numbers
y >= x the result of p x.
It runs until it finds a condition where p x is False
so if this is never the case, it runs forever.

> all :: ℕ -> (ℕ -> Bool) -> Bool
> all = y all'

Note how all is defined in terms of n' which takes the recursive call as the first
argument, r.
The fixed point of all' (ie. all) is all' with the references to r swapped out
for a call to itself.
This is hard to explain simply, if you are interested you should look up an explanation,
otherwise feel free to move on.

> all' = \r x f ->
>   if not (f x)
>   then False
>   else r (x + 1) f

Similarly `any` takes a predicate p, and an initial value x, but also a maximum,
max. This function runs the predicate over all values y <- [x, max) and returns
true if any of p y are true, and false if all p y are false.

> any :: ℕ -> ℕ -> (ℕ -> Bool) -> Bool
> any = y any'
> 
> any' = \r x max f ->
>   if x >= max
>     then False
>     else (if f x
>       then True
>       else r (x + 1) max f)

`prime` is a naive function for testing if a given number is a prime, written
using the two previous functions.

If you are running ghci you can test this out to convince yourself it works.

> prime :: ℕ -> Bool
> prime = \n -> 
>   not
>     (any 2 n 
>       (\x -> any 2 n (\y -> x * y == n)))

The `check` function here comprises the main point of the conjecture.
Given three numbers x, y, z this is a predicate that returns true iff x and y
are prime and x + y = z.
 
> check :: ℕ -> ℕ -> ℕ -> Bool
> check = \x y n ->
>   and 
>     (and (prime x) (prime y))
>     (x + y == n)

Finally we define the goldbach term itself, note that this has type Bool, as that
is what we would get assuming it reduces to some value.

What this is doing is running over all numbers n halting only if the following predicate
is false. The predicate we pass in is false iff for some x, y <- [1, 2n] check fails.

> goldbach :: Bool
> goldbach =
>   all 1
>     (\n -> any 1 (2 * n)
>       (\y -> any 1 (2 * n)
>         (\x -> (check x y (2 * n)))))
