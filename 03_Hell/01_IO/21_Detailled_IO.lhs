To finish, let's translate our example:

<code class="haskell">

askUser :: IO [Integer]
askUser = do
  putStrLn "Enter a list of numbers (separated by commas):"
  input <- getLine
  let maybeList = getListFromString input in
      case maybeList of
          Just l  -> return l
          Nothing -> askUser

main :: IO ()
main = do
  list <- askUser
  print $ sum list
</code>

Is translated into:

> import Data.Maybe
> 
> maybeRead :: Read a => String -> Maybe a
> maybeRead s = case reads s of
>                   [(x,"")]    -> Just x
>                   _           -> Nothing
> getListFromString :: String -> Maybe [Integer]
> getListFromString str = maybeRead $ "[" ++ str ++ "]"
> askUser :: IO [Integer]
> askUser = 
>     putStrLn "Enter a list of numbers (sep. by commas):" >>
>     getLine >>= \input ->
>     let maybeList = getListFromString input in
>       case maybeList of
>         Just l -> return l
>         Nothing -> askUser
> 
> main :: IO ()
> main = askUser >>=
>   \list -> print $ sum list

You can compile this code to verify it continues to work.

Left as an exercise to the masochistic reader:

- rewrite everything without using `(>>)`, `(>>=)` and `return`.
