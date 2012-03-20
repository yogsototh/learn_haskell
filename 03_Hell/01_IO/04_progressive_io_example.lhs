<h2 id="io-trick-explained">`IO` trick explained</h2>

Why did we used some strange syntax, and what exactly is this `IO` type.
It looks a bit like magic.

For now let's just forget about all the pure part of our program, and focus
on the impure part:


> askUser :: IO [Integer]
> askUser = do
>   putStrLn "Enter a list of numbers (separated by comma):"
>   input <- getLine
>   let maybeList = getListFromString input in
>       case maybeList of
>           Just l  -> return l
>           Nothing -> askUser
> 
> main :: IO ()
> main = do
>   list <- askUser
>   print $ sum list

First noticiable thing: 
the structure of these function is very similar to the one of an imperative language.
The fact is, Haskell is powerful enough to recreate function to help code look like in an imperative language.
For example, if you wish you could create a `while` in Haskell.
In fact, for dealing with `IO`, imperative style is generally more appropriate.

But, you also see there are some light differences.
The notation is a bit strange.

This is here that reside the beauty of how Haskell handle IOs.

Imagine you want to write a pure language.
But, a completely pure language will have few utility in real life.
Wihout effect, you couldn't print anything on a screen, read the user input, etc...

You can imagine, in standard impure language, there is a hidden global variable.
For example, you could write something in a file.
Somebody else could modify this file.
And you could later read the content of the file.

Each time something changed in the external world, it was like a global variable had changed its value. 
This global variable can be represented as a World state.

Now, to have a pure language with some utility you could simply state the execution of your program will be an evaluation of the main function with the following type.

~~~
main :: World -> World
~~~

Which means, main instead of having a global variable accessible by all functions of you program.
Main will be given as parameter an id representing the state of the World on which you can access.
And it will certainly make some changes to it.

In reality, the real type is closer to

~~~
main :: World -> ((),World)
~~~

The `()` type is the null type.
Nothing to see here.

Now let's write our main function:

~~~
main w0 =
    let (list,w1) = askUser w0 in
    let (x,w2) = print (sum list,w1) in
    x 
~~~

Also remember, the order of evaluation is generally not fixed in Haskell.
For example in general to evaluate `f a b`, you have many choices: 

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
Let's try to make the same to the askUser function:

~~~
askUser :: World -> ([Integer],World)
~~~

The type has changed as we will modify the "World" we simulate this by
returning a world value different than the input "World" value.
This way we remain "pure" in the language. 
You could write a completely pure implementation and it will works.
In the real world, the evaluation will have some side effect each time a function
return another value of the world input.

Before:

> askUser :: IO [Integer]
> askUser = do
>   putStrLn "Enter a list of numbers:"
>   input <- getLine
>   let maybeList = getListFromString input in
>       case maybeList of
>           Just l  -> return l
>           Nothing -> askUser

After:

> askUser w0 =
>     let (_,w1)     = putStrLn "Enter a list of numbers:"
>         (input,w2) = getLine w1
>         (l,w3)     = case getListFromString input of
>                       Just l   -> (l,w2)
>                       Nothing  -> askUser w2
>     in
>         (l,w3)

This is similar, but awkward. All these `let ... in`. Even if with Haskell
you could remove most, it's still awkard.

The lesson, is, naive IO implementation in Pure functional language is awkward!

Fortunately, some have found a better way to handle this problem.
We see a pattern.
Each line is of the form:

~~~
let (y,w') = action x w in
~~~

Even if for some line the first `x` argument isn't needed.
The output type is a couple, `(answer, newWorldValue)`.
Each function `f` must have a type of kind:

~~~
f :: World -> (a,World)
~~~

Not only this, but we can also remark we use them always 
with the following general pattern:

~~~
let (y,w1) = action1 x w0 in
let (z,w2) = action2 y w1 in
...
~~~

Now, we will make a magic trick. We will make the world variable "disappear".
We will `bind` the two lines. Let's define the `bind` function.

~~~
bind :: (World -> (a,World)) 
        -> (a -> (World -> (b,World))) 
        -> (World -> (b,World)) 
~~~

(World -> (a,World)) is the type for an IO action. Like getLine, printing something, etc... Now let's rename it for more clarity.

~~~
type IO a = World -> (a, World)
~~~

Some example of functions:

~~~
getLine :: IO String
print :: Show a => a -> IO ()
~~~

`getLine` is an IO action which take a world as parameter, then return a couple (String,World).
Which can be said as: `getLine` is of type IO String.
Which we also see as, an IO action which will return a String "embeded inside an IO".

The function `print` is also interresting.
It takes on argument which can be shown.
In fact it takes two arguments.
The first is the value to print and the other is the state of world.
It then return a couple of type `((),World)`. 
This means it changes the world state, but don't give anymore data.

We simplify the bind type:

~~~
bind :: IO a 
        -> (b -> IO b) 
        -> IO b
~~~

The function bind take two actions.

The type is quite intimidating. But stay with me here.
On a line like 

~~~
let (x,w1) = action1 w0 in
let (y,w2) = action2 x w1 in
(y,w2)
~~~

On the first line, action1 is of type `(World -> (a,World))`.
On the second line, action2 is of type `(a -> (World -> (b,World))`.


`bind`:

- take a function similar to all lines as first argument wich returns a `(a,World)`
- take a function with take an `a` as argument and returns a line wich return a `(b,World)`
- return a line wich returns a `(b,World)`.

~~~
(bind action1 action2) w0 =
    let (x, w1) = action1 w0
        (y, w2) = action2 x w1
    in  (y, w2)
~~~


The idea is to hide the World argument with this function. Let's go:
As example imagine if we wanted to simulate:

~~~
let (line1,w1) = getLine w0 in
let ((),w2) = print line1 in
((),w2)
~~~

Now, using the bind function:

~~~
(res,w2) = (bind getLine (\l -> print l)) w0
~~~

As print is of type (World -> ((),World)), we know res = () (null type).
If you didn't saw what was magic here, let's try with three lines this time.


~~~
let (line1,w1) = getLine w0 in
let (line2,w2) = getLine w1 in
let ((),w3) = print (line1 ++ line2) in
((),w3)
~~~

Which is equivalent to:

~~~
(res,w3) = bind getLine (\line1 ->
             bind getLine (\line2 -> 
               print (line1 ++ line2)))
~~~

Didn't you remark something?
Yes, there isn't anymore temporary World variable used anywhere!
This is _MA_. _GIC_.

We can make thinks look better. Let's call bind (>>=) which is an infix function.
Infix is like (+), 3 + 4 <=> "(+) 3 4"

~~~
(res,w3) = getLine >>=
            (\line1 -> getLine >>=
            (\line2 -> print (line1 ++ line2)))
~~~

Ho Ho Ho! Happy Christmas Everyone!
Haskell has made a syntactical sugar for us:

~~~
do
  y <- f x
  z <- g y
  t <- h y z
~~~

Is replaced by:

~~~
    (f     >>= (\y ->
     g y   >>= (\z ->
     h y z >>= (\t ->
      ...
    ))))
~~~

Which is perfect for IO.
Now we also just need a way to remove the last statement containing a World value.
Easy, just write a simple function return
