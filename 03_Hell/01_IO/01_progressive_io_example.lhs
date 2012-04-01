<h3 id="deal-with-io">Deal With IO</h3>

 > <%=tldr%>
 > 
 > A typical function doing `IO` look a lot like an imperative language:
 >
 > ~~~
 > f :: IO a
 > f = do
 >   x <- action1
 >   action2 x
 >   y <- action3
 >   action4 x y
 > ~~~
 >
 > - To set a value to an object we use `<-` .
 > - The type of each line is `IO *`;
 >   in this example:
 >   - `action1     :: IO b`
 >   - `action2 x   :: IO ()`
 >   - `action3     :: IO c`
 >   - `action4 x y :: IO a`
 >   - `x :: b`, `y :: c`
 > - Few objects have the type `IO a`, this should help you to choose.
 >   In particular you cannot use pure function directly here.
 >   To use pure function you could do `action2 (purefunction x)` for example.

In this section, I will explain how to use IO, not how they work.
You'll see how Haskell separate pure from impure part of the program.

Don't be stoped by trying to understand the details behind the syntax at first.
Answer will come in the next section.

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

It should be straightforward to understand the behavior of this program.
Let's analyze the types in more detail.

~~~
putStrLn :: String -> IO ()
getLine  :: IO String
print    :: Show a => a -> IO ()
~~~

Or more interrestingly, we remark each expression in the `do` block has a type of `IO a`.

<pre>
main = do
  putStrLn "Enter ... " :: <span class="high">IO ()</span>
  getLine               :: <span class="high">IO String</span>
  print Something       :: <span class="high">IO ()</span>
</pre>

We should also remark the effect of the `<-` symbol.

~~~
do
 x <- something
~~~

If `something :: IO a` then `x :: a`.

Another important remark to use `IO`.
All line in a do block must have one of the two forms:

~~~
action1             :: IO a
                    -- in this case, generally a = ()
~~~

or

~~~
value <- action2    -- where
                    -- bar z t :: IO b
                    -- value   :: b
~~~

These two kind of line will correspond to two different way of sequencing actions.
The meaning of this sentence should be clearer at the end of the next section.
