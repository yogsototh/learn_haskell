fr: Pour finir, traduisons notre exemple :
en: To finish, let's translate our example:

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

fr: Se traduit en :
en: Is translated into:

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

fr: Vous pouvez compiler ce code pour vérifier qu'il marche.
en: You can compile this code to verify that it works.

fr: Imaginez à quoi il ressemblerait sans le `(>>)` et `(>>=)`.
en: Imagine what it would look like without the `(>>)` and `(>>=)`.
