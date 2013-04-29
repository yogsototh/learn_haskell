Not bad, but we can make it even better:

> deposit :: (Num a) => a -> a -> Maybe a
> deposit value account = Just (account + value)
> 
> withdraw :: (Num a,Ord a) => a -> a -> Maybe a
> withdraw value account = if (account < value) 
>                          then Nothing 
>                          else Just (account - value)
> 
> eligible :: (Num a, Ord a) => a -> Maybe Bool
> eligible account =
>   deposit 100 account >>=
>   withdraw 200 >>=
>   deposit 100  >>=
>   withdraw 300 >>=
>   deposit 1000 >>
>   return True
> 
> main = do
>   print $ eligible 300 -- Just True
>   print $ eligible 299 -- Nothing

We have proven that Monads are a good way to make our code more elegant.
Note this idea of code organization, in particular for `Maybe` can be used
in most imperative languages.
In fact, this is the kind of construction we make naturally.

 > An important remark:
 > 
 > The first element in the sequence being evaluated to `Nothing` will stop
 > the complete evaluation. 
 > This means you don't execute all lines.
 > You get this for free, thanks to laziness.

You could also replay these example with the definition of `(>>=)` for `Maybe`
in mind:

<code class="haskell">
instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing  >>= _  = Nothing
    (Just x) >>= f  = f x

    return x = Just x
</code>


The `Maybe` monad proved to be useful while being a very simple example.
We saw the utility of the `IO` monad.
But now for a cooler example, lists.
