Our next evolution will be to ask the user again and again util it enters a valid answer.

We keep the first part:

> import Data.Maybe
> 
> maybeRead :: Read a => String -> Maybe a
> maybeRead s = case reads s of
>                   [(x,"")]    -> Just x
>                   _           -> Nothing
> getListFromString :: String -> Maybe [Integer]
> getListFromString str = maybeRead $ "[" ++ str ++ "]"

Now, we create a function which will ask the user for an integer list
until the input is right.

> askUser :: IO [Integer]
> askUser = do
>   putStrLn "Enter a list of numbers (separated by comma):"
>   input <- getLine
>   let maybeList = getListFromString input in
>       case maybeList of
>           Just l  -> return l
>           Nothing -> askUser

This function is of type `IO [Integer]`. 
Such a type means, that we retrieved a value of this type through some IO actions.
Some people might explain while waving their hands: 

    "This is an `[Integer]` inside an `IO`"

If you want to understand the details behind all of this, you'll have to continue.

Finally our main function is quite simpler:

> main :: IO ()
> main = do
>   list <- askUser
>   print $ sum list
