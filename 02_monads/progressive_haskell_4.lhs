## `IO` trick explained.

Why did we used some strange syntax, and what exactly is this `IO` type.
It looks a bit like magic.

For now let's just forget about all the pure part of our program, and focus
on the impure part:

> askUser :: IO [Integer]
> askUser = do
>   putStrLn "Enter a list of numbers separated with ',' (1,2,3 for example):"
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

You remark the structure looks similar to an imperative language.
If you read me after reading the general references about Haskell.
You should ask, why, but why using something so similar to imperative programming?
Functional is so much better.

Now imagine writting this in a pure and lazy functional language.
Here we go. To simulate purity, we have to change the type of our function.

~~~
main :: World -> ((),World)
~~~

which once compiled will for each change of the World parameter will change
something in the real environment. And in the end it will return something
of type (). Which means returns nothing (or 0).

Now let's see how we should code it:

~~~
main w0 =
    let (list,w1) = askUser w0 in
    let (x,w2) = print (list,w1) in
    x 
~~~

Also remember, the order of evaluation is generally not fixed in Haskell.
When calculating :

`f a b` -> you should first eval `a` then `b` then `f a b`
or eval `b` then `a` then `f a b`.
This is true, because we should work in a pure language.

Now, if you look at the main function, it is clear you must eval the first
line before the second one since, you to evaluate the second line you have
to get a parameter given by the evaluation of the first line.

Such trick works nicely. The compiler will at each step provide a pointer
to a new real world id.
Under the hood, print will be evaluate by:

if print -> print something on the screen, modify the id of the world, return
((),new world id).

Now, if you look at the style of the main function, it is clearly awkward.
Let's try to make the same to the askUser function:

~~~
askUser :: World -> ([Integer],World)
~~~

The type has changed as we will modify the "World" we simulate this by
returning a world value different than the input "World" value.
This way we remain completely "pure" in the language. 
There is no hole of impurity.
You could write a completely pure implementation and it will works.
In the real world, the evaluation will have some side effect each time a function
return another value of the world input.

Before:

> askUser :: IO [Integer]
> askUser = do
>   putStrLn "Enter a list of numbers separated with ',' (1,2,3 for example):"
>   input <- getLine
>   let maybeList = getListFromString input in
>       case maybeList of
>           Just l  -> return l
>           Nothing -> askUser

After:

> askUser w0 =
>     let (_,w1)     = putStrLn "Enter a list of numbers" in
>     let (input,w2) = getLine w1 in
>     let (l,w3)     = case getListFromString input of
>                       Just l   -> (l,w2)
>                       Nothing  -> askUser w2
>     in
>         (l,w3)

This is similar, but awkward. All these `let ... in`. Even if with Haskell
you could remove most, it's still awkard.

But there is a clear pattern. Each line is of the form:

let (x,w') = f w in
...

and the output type is a couple, (answer, newWorldValue).

Guess what. Somebody as found a way to make it nicer by "hidding" a bit of information.
Here it will be the world variable.

How? All lines have the following pattern:

~~~
let (x,w') = f w in ...
~~~

then each function `f` must have a type of kind:

~~~
f :: World -> (a,World)
~~~

Not only this, but we can also remark we use them always with the following general pattern:

~~~
let (x,w0) = f w in
let (y,w1) = g w0 in
...
~~~

We can `bind` the two lines, here is how. Let's define the bind function.

~~~
bind :: (World -> (a,World)) 
        -> (a -> (World -> (b,World))) 
        -> (World -> (b,World)) 
~~~

The type is quite intimidating. But stay with me here:

bind ::
    take a function similar to all lines as first argument with return a (a,World)
    take a function with take an a as argument and returns a line wich return a (b,World)
    return a line wich returns a (b,World).

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
                                bind getLine (\line2 -> print (line1 ++ line2)))
~~~

Didn't you remark something?
Yes, there isn't anymore temporary world variable used anywhere!
This is Ma. Gic.

We can make thinks look better. Let's call bind (>>=) which is an infix function.
Infix is like (+), 3 + 4 <=> "(+) 3 4"

~~~
(res,w3) = getLine >>=
            (\line1 -> getLine >>=
            (\line2 -> print (line1 ++ line2)))
~~~


