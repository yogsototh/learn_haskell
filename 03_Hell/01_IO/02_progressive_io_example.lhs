Now let's see how this behave.
For example, what occur if the user enter something strange?
Let's try:

~~~
% runghc 02_progressive_io_example.lhs
Enter a list of numbers (separated by comma):
foo
Prelude.read: no parse
~~~

Argh, an evil error message and a crash! 
The first evolution will be to answer with a more friendly message.

For this, we must detect, something went wrong.
Here is one way to do this.
Use the type `Maybe`.
It is a very common type in Haskell.

> import Data.Maybe

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
Don't try to understand too much of this function. 
I use a lower level function than `read`; `reads`.

[^1]: Which itself is very similar to the javascript `eval` on a string containing JSON).

> maybeRead :: Read a => String -> Maybe a
> maybeRead s = case reads s of
>                   [(x,"")]    -> Just x
>                   _           -> Nothing

Now to be a bit more readable, we define a function which goes like this:
If the string has the wrong format, it will return `Nothing`.
Otherwise, for example for "1,2,3", it will return `Just [1,2,3]`.

> getListFromString :: String -> Maybe [Integer]
> getListFromString str = maybeRead $ "[" ++ str ++ "]"


We simply have to test the value in our main function.

> main = IO ()
> main = do
>   putStrLn "Enter a list of numbers (separated by comma):"
>   input <- getLine
>   let maybeList = getListFromString input in
>       case maybeList of
>           Just l  -> print (sum l)
>           Nothing -> error "Bad format. Good Bye."

In case of error, we prompt a nice error message.

Remark the type of each expression in the main's do block remains of the form `IO a`.
The only strange construction is `error`. 
I'll say `error msg` will simply take the needed type (here `IO ()`).

One very important thing to note is the type of all the defined function.
There is only one function which contains `IO` in its type: `main`. 
That means main is impure. 
But main use `getListFromString` which is pure.
It is then clear just by looking at declared types where are pure and impure functions.

Why purity matters? 
I certainly forget many advantages, but the three main reason are:

- It is far easier to think about pure code than impure one.
- Purity protect you from all hard to reproduce bugs due to border effects.
- You can evaluate pure functions in any order or in parallel without risk.

This is why, you should generally put as most code as possible in pure functions.
