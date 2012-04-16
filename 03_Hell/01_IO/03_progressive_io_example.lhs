Our next evolution will be to prompt the user again and again until she enters a valid answer.

We keep the first part:

> import Data.Maybe
> 
> maybeRead :: Read a => String -> Maybe a
> maybeRead s = case reads s of
>                   [(x,"")]    -> Just x
>                   _           -> Nothing
> getListFromString :: String -> Maybe [Integer]
> getListFromString str = maybeRead $ "[" ++ str ++ "]"

Now, we create a function which will ask the user for an list of integers
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
Such a type means that we retrieved a value of type `[Integer]` through some IO actions.
Some people might explain while waving their hands: 

 > «This is an `[Integer]` inside an `IO`»

If you want to understand the details behind all of this, you'll have to read the next section.
But sincerely, if you just want to _use_ IO.
Just practice a little and remember to think about the type.

Finally our main function is quite simpler:

> main :: IO ()
> main = do
>   list <- askUser
>   print $ sum list

We have finished with our introduction to `IO`.
This was quite fast. Here are the main things to remember:

- in the `do` bloc, each expression must have the type `IO a`.
  You are then limited in the number of expressions available.
  For example, `getLine`, `print`, `putStrLn`, etc...
- Try to externalize the pure functions as much as possible.
- the `IO a` type means: an IO _action_ which returns an element of type `a`.
  `IO` represents actions; under the hood, `IO a` is the type of a function.
  Read the next section if you are curious.

If you practice a bit, you should be able to _use_ `IO`.

 > _Exercises_:
 > 
 > - Make a program that sums all of its arguments. Hint: use the function `getArgs`.
