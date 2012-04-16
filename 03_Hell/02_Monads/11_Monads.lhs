Now, let's make it better using Maybe and the fact that it is a Monad

> deposit :: (Num a) => a -> a -> Maybe a
> deposit value account = Just (account + value)
> 
> withdraw :: (Num a,Ord a) => a -> a -> Maybe a
> withdraw value account = if (account < value) 
>                          then Nothing 
>                          else Just (account - value)
> 
> eligible :: (Num a, Ord a) => a -> Maybe Bool
> eligible account = do
>   account1 <- deposit 100 account 
>   account2 <- withdraw 200 account1 
>   account3 <- deposit 100 account2 
>   account4 <- withdraw 300 account3 
>   account5 <- deposit 1000 account4
>   Just True
> 
> main = do
>   print $ eligible 300 -- Just True
>   print $ eligible 299 -- Nothing
