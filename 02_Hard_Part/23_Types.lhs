<h4 id="recursive-type">Recursive type</h4>

You already encountered recursive types: lists.
You can re-create lists, but with a more verbose syntax:

<code class="haskell">
data List a = Empty | Cons a (List a)
</code>


If you really want to use an easier syntax you can use infix name for constructors.

<code class="haskell">
infixr 5 :::
data List a = Nil | a ::: (List a)
</code>

The number after `infixr` is the priority.

If you want to be able to print (`Show`), read (`Read`), test equality (`Eq`) and compare (`Ord`) your new data structure you can tell Haskell to derive the appropriate function for you.

> infixr 5 :::
> data List a = Nil | a ::: (List a) 
>               deriving (Show,Read,Eq,Ord)

When you add `deriving (Show)` to your data declaration, Haskell create a `show` function for you.
We'll see soon how you could use your own `show` function.

> convertList [] = Nil
> convertList (x:xs) = x ::: convertList xs

> main = do
>       print (0 ::: 1 ::: Nil)
>       print (convertList [0,1])

This print:

~~~
0 ::: (1 ::: Nil)
0 ::: (1 ::: Nil)
~~~
