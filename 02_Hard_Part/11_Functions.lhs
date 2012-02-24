Here is a first solution.
The function `evenSum` returns the sum of all even numbers in a list:

> -- Version 1
> evenSum :: [Integer] -> Integer
> 
> evenSum l = accumSum 0 l
> 
> accumSum n l = if l == []
>                   then n
>                   else let x = head l 
>                            xs = tail l 
>                        in if even x
>                               then accumSum (n+x) xs
>                               else accumSum n xs

To test a function you can use `ghci`:

<pre>
% ghci
<span style="color: #AAA">GHCi, version 7.0.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude&gt;</span> :l 11_Functions.lhs 
<span style="color: #AAA">[1 of 1] Compiling Main             ( 11_Functions.lhs, interpreted )
Ok, modules loaded: Main.
*Main&gt;</span> evenSum [1..5]
6
</pre>

Here is an example of execution[^2]: 

[^2]: I know I cheat. But I will talk about non-strict later.

~~~
*Main> evenSum [1..5]
accumSum 0 [1,2,3,4,5]
1 is odd 
accumSum 0 [2,3,4,5]
2 is even
accumSum 2 [3,4,5]
3 is odd 
accumSum 2 [4,5]
4 is even
accumSum 6 [5]
5 is odd 
accumSum 6 []
l == []
6
~~~

Comming from an imperative language all should seems right.
In reality many things can be improved.
First, we can generalize the type.

<code class="haskell">
evenSum :: Integral a => [a] -> a
</code>

