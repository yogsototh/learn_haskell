<h3> Deal With IO </h3>

Start by resolving a simple example wich deal with user interaction.

 > Ask a user to enter a list of numbers.
 > Return the sum of the numbers

> toList :: String -> [Integer]
> toList input = read ("[" ++ input ++ "]")
>
> main = do
>   putStrLn "Enter a list of numbers (separated by comma):"
>   input <- getLine
>   print $ sum (toList input)

It should be straightforward.
