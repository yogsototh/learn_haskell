# Haskell as two heads

- pure functional/statically typed style
- monads/arrow/functor style

# First Head: Pure functional style

- declare function
- type a function
- strange but efficient syntax, the marvellous "space".
- create its own complex type
- move into you data structures
- fmap? the start of the second Head

## The walls

### Functional programming is declarative not imperative.

Imperative programs read mostly like a story with an action:

> Make tea:
>     - open the tea pot
>     - put the water in kettle
>     - put fire on
>     - put kettle on fire
>     - put some tea bag inside the tea pot
>     - once the water boil in the kettle, put water inside the tea pot
>     - wait for 3 minutes
>     - remove the tea bag of the tea pot

Object imperative programs read like Imperative programs but with characters:

> Scotty
>   maketea

Remark: Scotty forgot to take a tea pot. A bug will ensue.

Functional programs reads like a huge description.

> My program is some "hot tea"
> a "hot tea" is a 
>       "tea pot" containing a "tea packet" and "hot water"
>       "a tea" packet is a "packet" inside the kitchen
>       ...

This is a very different style.
The execution of a functional program is just:

    give me my program. 
        -> I need to get some "hot tea"
            -> I need to get a "tea pot" and some "hot water"
                ...

Also there isn't really "actor". Therefore, you can easily say:
Hey, the two of you, make me my program.
And some will get the tea pot, while the other will get the hot water in parallel.

It is also possible in imperative programming, but, it is far easier in imperative programming that actor will percute.

Imperative programming:
It is like having one slave which will obey your order one after one. If you need another slave, you'll have to tell each slave a lot of detail to be sure they won't use the same resource at the same time.

Imperative style => Story. You are a chief who give order.
Functional style => Description. You are a demiurge who describe his world.

- Imperative style, easier to make war and destroy cathedrals.
- Functional style, easier to construct cathedrals.

### Forget everything

- No variable!
- No loop!
- A lot of recursion!
- Can be extremely terse and difficult to read.
- Close to mathematic notations.

If you think you can handle it.
Just stay a bit more up until Hardcore parts.

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
