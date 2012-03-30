Now, let's make it better using Maybe and the fact it is a Monad

> deposit :: (Num a) => a -> a -> Maybe a
> deposit value account = Just (account + value)
> withdraw :: (Num a,Ord a) => a -> a -> Maybe a
> withdraw value account = if (account < value) 
>                          then Nothing 
>                          else Just (account - value)
> 
> elligible :: (Num a, Ord a) => a -> Maybe Bool
> elligible account = do
>   account1 <- deposit 100 account 
>   account2 <- withdraw 200 account1 
>   account3 <- deposit 100 account2 
>   account4 <- withdraw 300 account3 
>   account5 <- deposit 1000 account4
>   Just True
> 
> main = do
>   print $ elligible 300
>   print $ elligible 299


To be a usage monad, your function must obey some rule.
As these rule are undecidable to verify, you have to prove them before creating a new monad.
Here are the rules:

<code class="haskell">
return a >>= k  ==  k a
m >>= return  ==  m
m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
</code>
