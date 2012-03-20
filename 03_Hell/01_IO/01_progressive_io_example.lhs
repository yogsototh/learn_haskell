<h3 id="deal-with-io">Deal With IO</h3>

In this section, I won't explain how IO works.
I just show how to use it by providing some progressive examples.
I will also show you how Haskell separate well pure part from impure part of the program.

What we want:

 > Ask a user to enter a list of numbers.
 > Print the sum of the numbers

> toList :: String -> [Integer]
> toList input = read ("[" ++ input ++ "]")
>
> main = do
>   putStrLn "Enter a list of numbers (separated by comma):"
>   input <- getLine
>   print $ sum (toList input)

It should be straightforward.
