<h4 id="higher-level-functions">Higher Level Functions</h4>

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

where

> filter even [1..10] ⇔  [2,4,6,8,10]

The function `filter` takes a function of type (`a -> Bool`) and a list of type `[a]`. It returns a list containing only elements for which the function returned `true`.

Our next step is to use another way to simulate loop. 
We will use the `foldl'` to accumulate a value.
The function `foldl` capture a general coding pattern:

<code class="haskell">
myfunc list = foo initialValue list
    foo accumulated []     = accumulated
    foo tmpValue    (x:xs) = foo (bar tmpValue x) xs
</code>

Which can be replaced by:

<code class="haskell">
myfunc list = foldl bar initialValue list
</code>

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

<pre>
  <span style="color: #CF6A4C">evenSum [1,2,3,4]</span>
⇒ foldl' mysum 0 (<span style="color: #CF6A4C">filter even [1,2,3,4]</span>)
⇒ <span style="color: #CF6A4C">foldl' mysum 0 <span style="color: #CDA869">[2,4]</span></span>
⇒ <span style="color: #CDA869">foldl' mysum (<span style="color: #CF6A4C">mysum 0 2</span>) [4]</span> 
⇒ foldl' mysum (<span style="color: #CF6A4C">0+2</span>) [4]
⇒ <span style="color: #CF6A4C">foldl' mysum <span style="color: #CDA869">2</span> [4]</span>
⇒ <span style="color: #CDA869">foldl' mysum (<span style="color: #CF6A4C">mysum 2 4</span>) []</span>
⇒ foldl' mysum (<span style="color: #CF6A4C">2+4</span>) []
⇒ <span style="color: #CF6A4C">foldl' mysum <span style="color: #CDA869">6</span> []</span>
⇒ <span style="color: #CDA869">6</span>
</pre>

Beware! 
Most of the time you want to use `foldl'` and not `foldl`.

This is nice, but as `mysum` is a very simple function, giving it a name is a burden.
We can use anonymous functions or lambdas.

> -- Version 7
> evenSum l = foldl' (\x y -> x+y) (filter even l)

And of course, we remark 

> (\x y -> x+y) ⇔ (+)

