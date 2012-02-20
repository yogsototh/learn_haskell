<h3> Higher Level Functions </h3>

To make things even better we should use higher level functions.
What are these beast?
Higher level functions are functions taking another functions as parameters.

Here are some examples:

> import Data.List
> filter :: (a -> Bool) -> [a] -> [a]
> map :: (a -> b) -> [a] -> [b]
> foldl' :: (a -> b -> a) -> a -> [b] -> a

Let's proceed by small steps.

> -- Version 5
> evenSum l = mysum 0 (filter even l)
>     where 
>       mysum n [] = n
>       mysum n (x:xs) = mysum xs (n+x) 

> filter even [1..10] ⇔  [2,4,6,8,10]

Now you can use the `foldl'` to accumulate a value.

> -- Version 6
> import Data.List
> evenSum l = foldl' mysum 0 (filter even l)
>   where mysum acc value = acc + value

For each element of the list, `foldl'` will add it to the next.
And finally add 0.

If you really want to know how the magic works.
Here is the definition of `foldl`.

> foldl f z [] = z
> foldl f z (x:xs) = foldl f (f z x) xs

But as Haskell is lazy, it doesn't evaluate `(f z x)` and push this in the stack.
`foldl'` is a strict version of `foldl`.
If you don't understand what "lazy" and "strict" means,
don't worry, just follow the code as if `fold` and `foldl'` where identical.

Here is what occurs:

~~~
evenSum [1,2,3,4]
⇒ foldl' mysum 0 (filter even [1,2,3,4])
⇒ foldl' mysum 0 [2,4]
⇒ foldl' mysum (mysum 0 2) [4]
⇒ foldl' mysum (0+2) [4]
⇒ foldl' mysum 2 [4]
⇒ foldl' mysum (mysum 2 4) []
⇒ foldl' mysum (2+4) []
⇒ foldl' mysum 6 []
⇒ 6
~~~

Beware! 
Most of the time you want to use `foldl'` and not `foldl`.

This is nice, but as `mysum` is a very simple function, giving it a name is a burden.
We can use anonymous functions or lambdas.

> -- Version 7
> evenSum l = foldl' (\x y -> x+y) (filter even l)

And of course, we remark 

> (\x y -> x+y) ⇔ (+)

