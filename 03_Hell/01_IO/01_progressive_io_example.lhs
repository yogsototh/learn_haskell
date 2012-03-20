<h3 id="deal-with-io">Deal With IO</h3>

In this section, I won't explain how IO works.
I'll just show how to use it by providing some progressive examples.
You'll see how Haskell separate pure from impure part of the program.

A warning, until next section, you shouldn't try to understand the details of
the functions. Just try to understand the meaning.

What to achieve?

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
