# Haskell as two heads

- pure functional style
- monads/arrow/functor style

For pure functional style, LYAH make an excellent work to learn it to you.

# First Head: Pure functional style

- declare function
- type a function
- strange but efficient syntax, the marvellous "space".
- create its own complex type
- move into you data structures


# Second Head: Monadic style

Haskell is really pure.

Definition of pure.

> No side effect!

For Haskell it will be a bit less:

> Every function evaluated twice with the same argument will render the exact same result.

There is no `getChar()` like in `C`. If you write getChar three times, it will always return the same value. Too bad.

Imagine a way to simulate `getChar`? Simple, to provide different value each time, simply add a parameter, and make this parameter change.

<code class="haskell">
getChar :: World -> (Char,World)

print :: String -> World -> ((),World)

main :: World -> ((),World)

main w0 =
    let (c, w1) = getChar     w0
        (d, w2) = getChar     w1
        ((),w3) = print (c:d:[]) w2
    in
        ((),w3)
</code>

Clearly this sucks. We are forced to write a bunch of ws.
One of the goal of these ws is to force the evaluation order.

Now we could start to make some notation from here:

<code class="haskell">
main = do
        c <- getChar
        d <- getChar
        print (c:d:[])
</code>

This would work. But in fact, there is a more interesting pattern.
To try to see it, we just have to look at types.

Explain `>>=`, `>>` and `return`.
Make it fast, make it hard.

Now usage:

<code class="haskell">
main = do
       x <- foo
       bar
       y <- baz x
       return y
</code>

Follow types:

- `foo` must be of type: `IO a`
- `bar` must be of type: `IO ()`
- `baz` must be of type: `a -> IO b`
- `y`   must be of type: `b`


You know what? 
_IO is a monad._

And what more? 
_There is a lot of monad everywhere._

You learned about the IO monad. 
Mostly this monad make you work with functions who use a real world hidden variable.
A monad not only come with `>>`, `>>=` and `return` but also a lot of functions of type `m a` and `(a -> m b)` to work within the monad.

The IO monad come with, getChar, getLine, print, putStrLn, putStr, etc...

This means, you have a `DSL`.
You can now simulate imperative programming and IO inside a completely pure language!

Now, using monads, we can make a lot of DSL for many different usages.

- [] monad to simulate non deterministic programming
- Maybe monad to simulate a return error
- Either monad to simulate a return error with error messages
- State monad to simulate local state (like our World state)
