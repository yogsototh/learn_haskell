But what occurs if the user enter something strange?
Let's try:

~~~
% runghc 02_progressive_io_example.lhs
Enter a list of numbers (separated by comma):
foo
Prelude.read: no parse
~~~

Argh, an evil error message and a crash! 
Now we just want to put our own, more Human readable message.

For this, we must detect, something went wrong.
Here is one way to do this.
Use the type `Maybe`. It is a very common type in Haskell.

> import Data.Maybe
> 
> maybeRead :: Read a => String -> Maybe a
> maybeRead s = case reads s of
>                   [(x,"")]    -> Just x
>                   _           -> Nothing

What is this thing? Maybe is a type which takes one parameter.
Its definition is:

<code class="haskell">
data Maybe a = Nothing | Just a
</code>

This is a nice way to tell there was an error while trying to create/compute
a value.

The `maybeRead` function is a great example of this. 
This is a function similar to the function `read`[^1],
but if something goes wrong the returned value is `Nothing`.
If the value is right, it returns `Just <the value>`.

[^1]: Which itself is very similar to the javascript `eval` on a string containing JSON).

> getListFromString :: String -> Maybe [Integer]
> getListFromString str = maybeRead $ "[" ++ str ++ "]"

Now to be a bit more readable, we define a function which goes like this:

If the string has the wrong format, it will return `Nothing`.
Otherwise, for example for "1,2,3", it will return `Just [1,2,3]`.

We simply test the value in our main function.

> main = IO ()
> main = do
>   putStrLn "Enter a list of numbers (separated by comma):"
>   input <- getLine
>   let maybeList = getListFromString input in
>       case maybeList of
>           Just l  -> print (sum l)
>           Nothing -> error "Bad format. Good Bye."

In case of error, we prompt a nice error message.

One very important thing to note is the type of all the function used.
There is only one function which contains `IO` in its type: `main`. 
That means, all other functions are pure.

Why purity matters? As a pure function has no side effect, you can for example
evaluate its value in parallel on many different core without any problem.
And you have this by design of Haskell.
