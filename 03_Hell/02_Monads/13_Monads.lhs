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

We have proved Monad are nice to make our code more elegant.
Note this idea of code organization, in particular for `Maybe` can be used
in most imperative language.
In fact, this is the kind of construction we make naturally.

 > An important remark:
 > 
 > The first element in the sequence being evaluated to `Nothing` will stop
 > the complete evaluation. 
 > That means, you don't execute all lines. 
 > You have this for free, thanks to lazyness.

The `Maybe` monad proved to be useful while being the simplest example.
We saw the utility of the `IO` monad.
But now a cooler example, lists.
