<h3 id="io-trick-explained">IO trick explained</h3>

blogimage("magritte_pipe.jpg","Magritte, ceci n'est pas une pipe")

 > Here is a %tldr for this section.
 >
 > To separate pure and impure parts,
 > `main` is defined as a function
 > which modifies the state of the world
 >
 > ~~~
 > main :: World -> World
 > ~~~
 >
 > A function is guaranteed to have side effects only if it has this type.
 > But look at a typical main function:
 >
 > ~~~
 > main w0 =
 >     let (v1,w1) = action1 w0 in
 >     let (v2,w2) = action2 v1 w1 in
 >     let (v3,w3) = action3 v2 w2 in
 >     action4 v3 w3
 > ~~~
 >
 > We have a lot of temporary elements (here `w1`, `w2` and `w3`)
 > which must be passed on to the next action.
 >
 > We create a function `bind` or `(>>=)`.
 > With `bind` we don't need temporary names anymore.
 >
 > ~~~
 > main =
 >   action1 >>= action2 >>= action3 >>= action4
 > ~~~
 >
 > Bonus: Haskell has syntactical sugar for us:
 >
 > ~~~
 > main = do
 >   v1 <- action1
 >   v2 <- action2 v1
 >   v3 <- action3 v2
 >   action4 v3
 > ~~~


Why did we use this strange syntax, and what exactly is this `IO` type?
It looks a bit like magic.

For now let's just forget all about the pure parts of our program, and focus
on the impure parts:

<code class="haskell">
askUser :: IO [Integer]
askUser = do
  putStrLn "Enter a list of numbers (separated by commas):"
  input <- getLine
  let maybeList = getListFromString input in
      case maybeList of
          Just l  -> return l
          Nothing -> askUser

main :: IO ()
main = do
  list <- askUser
  print $ sum list
</code>

First remark; it looks like an imperative structure.
Haskell is powerful enough to make impure code look imperative.
For example, if you wish you could create a `while` in Haskell.
In fact, for dealing with `IO`, imperative style is generally more appropriate.

But you should had noticed the notation is a bit unusual.
Here is why, in detail.

In an impure language, the state of the world can be seen as a huge hidden global variable.
This hidden variable is accessible by all functions of your language.
For example, you can read and write a file in any function.
The fact that a file exists or not can be seen as different states of the world.

For Haskell this state is not hidden.
It is explicitly said `main` is a function that _potentially_ changes the state of the world.
Its type is then something like:

<code class="haskell">
main :: World -> World
</code>

Not all functions may have access to this variable.
Those which have access to this variable are impure.
Functions to which the world variable isn't provided are pure[^032001].

[^032001]: There are some _unsafe_ exceptions to this rule. But you shouldn't see such use on a real application except maybe for debugging purpose.

Haskell considers the state of the world as an input variable to `main`.
But the real type of main is closer to this one[^032002]:

[^032002]: For the curious the real type is `data IO a = IO {unIO :: State# RealWorld -> (# State# RealWorld, a #)}`. All the `#` as to do with optimisation and I swapped the fields in my example. But mostly, the idea is exactly the same.

<code class="haskell">
main :: World -> ((),World)
</code>

The `()` type is the null type.
Nothing to see here.

Now let's rewrite our main function with this in mind:

<code class="haskell">
main w0 =
    let (list,w1) = askUser w0 in
    let (x,w2) = print (sum list,w1) in
    x
</code>

First, we note that all functions which have side effects must have the type:

<code class="haskell">
World -> (a,World)
</code>

Where `a` is the type of the result.
For example, a `getChar` function should have the type `World -> (Char,World)`.

Another thing to note is the trick to fix the order of evaluation.
In Haskell, in order to evaluate `f a b`, you have many choices:

- first eval `a` then `b` then `f a b`
- first eval `b` then `a` then `f a b`.
- eval `a` and `b` in parallel then `f a b`

This is true, because we should work in a pure language.

Now, if you look at the main function, it is clear you must eval the first
line before the second one since, to evaluate the second line you have
to get a parameter given by the evaluation of the first line.

Such trick works nicely.
The compiler will at each step provide a pointer to a new real world id.
Under the hood, `print` will evaluate as:

- print something on the screen
- modify the id of the world
- evaluate as `((),new world id)`.

Now, if you look at the style of the main function, it is clearly awkward.
Let's try to do the same to the askUser function:

<code class="haskell">
askUser :: World -> ([Integer],World)
</code>

Before:

<code class="haskell">
askUser :: IO [Integer]
askUser = do
  putStrLn "Enter a list of numbers:"
  input <- getLine
  let maybeList = getListFromString input in
      case maybeList of
          Just l  -> return l
          Nothing -> askUser
</code>

After:

<code class="haskell">
askUser w0 =
    let (_,w1)     = putStrLn "Enter a list of numbers:" in
    let (input,w2) = getLine w1 in
    let (l,w3)     = case getListFromString input of
                      Just l   -> (l,w2)
                      Nothing  -> askUser w2
    in
        (l,w3)
</code>

This is similar, but awkward.
Look at all these temporary `w?` names.

The lesson, is, naive IO implementation in Pure functional languages is awkward!

Fortunately, there is a better way to handle this problem.
We see a pattern.
Each line is of the form:

<code class="haskell">
let (y,w') = action x w in
</code>

Even if for some line the first `x` argument isn't needed.
The output type is a couple, `(answer, newWorldValue)`.
Each function `f` must have a type similar to:

<code class="haskell">
f :: World -> (a,World)
</code>

Not only this, but we can also note that we always follow the same usage pattern:

<code class="haskell">
let (y,w1) = action1 w0 in
let (z,w2) = action2 w1 in
let (t,w3) = action3 w2 in
...
</code>

Each action can take from 0 to n parameters.
And in particular, each action can take a parameter from the result of a line above.

For example, we could also have:

<code class="haskell">
let (_,w1) = action1 x w0   in
let (z,w2) = action2 w1     in
let (_,w3) = action3 x z w2 in
...
</code>

And of course `actionN w :: (World) -> (a,World)`.

 > IMPORTANT, there are only two important patterns to consider:
 >
 > ~~~
 > let (x,w1) = action1 w0 in
 > let (y,w2) = action2 x w1 in
 > ~~~
 >
 > and
 >
 > ~~~
 > let (_,w1) = action1 w0 in
 > let (y,w2) = action2 w1 in
 > ~~~

leftblogimage("jocker_pencil_trick.jpg","Jocker pencil trick")

Now, we will do a magic trick.
We will make the temporary world symbol "disappear".
We will `bind` the two lines.
Let's define the `bind` function.
Its type is quite intimidating at first:

<code class="haskell">
bind :: (World -> (a,World))
        -> (a -> (World -> (b,World)))
        -> (World -> (b,World))
</code>

But remember that `(World -> (a,World))` is the type for an IO action.
Now let's rename it for clarity:

<code class="haskell">
type IO a = World -> (a, World)
</code>

Some example of functions:

<code class="haskell">
getLine :: IO String
print :: Show a => a -> IO ()
</code>

`getLine` is an IO action which takes a world as parameter and returns a couple `(String,World)`.
Which can be summarized as: `getLine` is of type `IO String`.
Which we also see as, an IO action which will return a String "embeded inside an IO".

The function `print` is also interesting.
It takes one argument which can be shown.
In fact it takes two arguments.
The first is the value to print and the other is the state of world.
It then returns a couple of type `((),World)`.
This means it changes the state of the world, but doesn't yield anymore data.

This type helps us simplify the type of `bind`:

<code class="haskell">
bind :: IO a
        -> (a -> IO b)
        -> IO b
</code>

It says that `bind` takes two IO actions as parameter and return another IO action.

Now, remember the _important_ patterns. The first was:

<code class="haskell">
let (x,w1) = action1 w0 in
let (y,w2) = action2 x w1 in
(y,w2)
</code>

Look at the types:

<code class="haskell">
action1  :: IO a
action2  :: a -> IO b
(y,w2)   :: IO b
</code>

Doesn't it seem familiar?

<code class="haskell">
(bind action1 action2) w0 =
    let (x, w1) = action1 w0
        (y, w2) = action2 x w1
    in  (y, w2)
</code>

The idea is to hide the World argument with this function. Let's go:
As an example imagine if we wanted to simulate:

<code class="haskell">
let (line1,w1) = getLine w0 in
let ((),w2) = print line1 in
((),w2)
</code>

Now, using the bind function:

<code class="haskell">
(res,w2) = (bind getLine (\l -> print l)) w0
</code>

As print is of type (World -> ((),World)), we know res = () (null type).
If you didn't see what was magic here, let's try with three lines this time.


<code class="haskell">
let (line1,w1) = getLine w0 in
let (line2,w2) = getLine w1 in
let ((),w3) = print (line1 ++ line2) in
((),w3)
</code>

Which is equivalent to:

<code class="haskell">
(res,w3) = bind getLine (\line1 ->
             bind getLine (\line2 ->
               print (line1 ++ line2)))
</code>

Didn't you notice something?
Yes, no temporary World variables are used anywhere!
This is _MA_. _GIC_.

We can use a better notation.
Let's use `(>>=)` instead of `bind`.
`(>>=)` is an infix function like
`(+)`; reminder `3 + 4 ⇔ (+) 3 4`

<code class="haskell">
(res,w3) = getLine >>=
           \line1 -> getLine >>=
           \line2 -> print (line1 ++ line2)
</code>

Ho Ho Ho! Happy Christmas Everyone!
Haskell has made syntactical sugar for us:

<code class="haskell">
do
  x <- action1
  y <- action2
  z <- action3
  ...
</code>

Is replaced by:

<code class="haskell">
action1 >>= \x ->
action2 >>= \y ->
action3 >>= \z ->
...
</code>

Note you can use `x` in `action2` and `x` and `y` in `action3`.

But what about the lines not using the `<-`?
Easy, another function `blindBind`:

<code class="haskell">
blindBind :: IO a -> IO b -> IO b
blindBind action1 action2 w0 =
    bind action (\_ -> action2) w0
</code>

I didn't simplify this definition for clarity purpose.
Of course we can use a better notation, we'll use the `(>>)` operator.

And

<code class="haskell">
do
    action1
    action2
    action3
</code>

Is transformed into

<code class="haskell">
action1 >>
action2 >>
action3
</code>

Also, another function is quite useful.

<code class="haskell">
putInIO :: a -> IO a
putInIO x = IO (\w -> (x,w))
</code>

This is the general way to put pure values inside the "IO context".
The general name for `putInIO` is `return`.
This is quite a bad name when you learn Haskell. `return` is very different from what you might be used to.
