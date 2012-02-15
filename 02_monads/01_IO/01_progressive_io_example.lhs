To learn Haskell, let's start directly.

1. The problem, ask a user to enter a list of numbers.
   Return the sum of the numbers

> main = do
>   putStrLn "Enter a list of numbers separated with ',' (1,2,3 for example):"
>   input <- getLine
>   print (sum (read ("[" ++ input ++ "]") :: [Integer]))

Very straightforward.
