Not bad, but we can make it even better:

> deposit :: (Num a) => a -> a -> Maybe a
> deposit value account = Just (account + value)
> withdraw :: (Num a,Ord a) => a -> a -> Maybe a
> withdraw value account = if (account < value) 
>                          then Nothing 
>                          else Just (account - value)
> 
> elligible :: (Num a, Ord a) => a -> Maybe Bool
> elligible account =
>   deposit 100 account >>=
>   withdraw 200 >>=
>   deposit 100  >>=
>   withdraw 300 >>=
>   deposit 1000 >>
>   return True
> 
> main = do
>   print $ elligible 300
>   print $ elligible 299

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
