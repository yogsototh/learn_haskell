<h4 id="the-list-monad">The list monad</h4>

The list monad help us to simulate non deterministic computation.
Here we go:

> import Control.Monad
>
> allCases = [True,False]
>
> resolve :: [(Bool,Bool,Bool)]
> resolve = do
>               x <- allCases
>               y <- allCases
>               z <- allCases
>               guard $ (x && (not y) && z) || (not x && y && z)
>               return (x,y,z)
> 
> main = do
>   print resolve

MA. GIC. :

~~~
[(True,False,True),(False,True,True)]
~~~
