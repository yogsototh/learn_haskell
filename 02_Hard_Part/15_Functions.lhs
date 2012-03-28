<h4 id="higher-level-functions">Higher Level Functions</h4>

To make things even better we should use higher level functions.
What are these beast?
Higher level functions are functions taking another functions as parameters.

Here are some examples:

<div style="display:none">
> import Data.List (foldl')
</div>

> filter :: (a -> Bool) -> [a] -> [a]
> map :: (a -> b) -> [a] -> [b]
> foldl :: (a -> b -> a) -> a -> [b] -> a

Let's proceed by small steps.

> -- Version 5
> evenSum l = mysum 0 (filter even l)
>     where 
>       mysum n [] = n
>       mysum n (x:xs) = mysum xs (n+x) 

where

> filter even [1..10] ⇔  [2,4,6,8,10]

The function `filter` takes a function of type (`a -> Bool`) and a list of type `[a]`. It returns a list containing only elements for which the function returned `true`.

Our next step is to use another way to simulate loop. 
We will use the `foldl` to accumulate a value.
The function `foldl` capture a general coding pattern:

<pre>
myfunc list = foo <span class="blue">initialValue</span> <span class="green">list</span>
    foo accumulated []     = accumulated
    foo tmpValue    (x:xs) = foo (<span class="yellow">bar</span> tmpValue x) xs
</pre>

Which can be replaced by:

<pre>
myfunc list = foldl <span class="yellow">bar</span> <span class="blue">initialValue</span> <span class="green">list</span>
</pre>

If you really want to know how the magic works.
Here is the definition of `foldl`.

> foldl f z [] = z
> foldl f z (x:xs) = foldl f (f z x) xs

~~~
foldl f z [x1,...xn]
⇔  f (... (f (f z x1) x2) ...) xn
~~~

But as Haskell is lazy, it doesn't evaluate `(f z x)` and push this to the stack.
This is why we generally use `foldl'` instead of `foldl`;
`foldl'` is a _strict_ version of `foldl`.
If you don't understand what lazy and strict means,
don't worry, just follow the code as if `foldl` and `foldl'` where identical.

Now our new version of `evenSum` become:

> -- Version 6
> -- foldl' isn't accessible by default
> -- we need to import it from the module Data.List
> import Data.List
> evenSum l = foldl' mysum 0 (filter even l)
>   where mysum acc value = acc + value

Version we can simplify by using directly a lambda notation.
This way we don't have to create the temporary name `mysum`.

> -- Version 7
> -- Generaly it is considered a good practice
> -- to import only the necessary function(s)
> import Data.List (foldl')
> evenSum l = foldl' (\x y -> x+y) (filter even l)

And of course, we remark 

> (\x y -> x+y) ⇔ (+)

