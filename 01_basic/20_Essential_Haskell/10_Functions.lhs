## Functions

Every Haskell object has a type.

~~~
x :: Int            ⇔ x is of type Int
x :: a              ⇔ x can be of any type
x :: Num a => a     ⇔ x can be any type a
                      such that a belongs to Num class 
f :: a -> b         ⇔ f is a function from a to b
f :: a -> b -> c    ⇔ f is a function from a to (b→c)
f :: (a -> b) -> c  ⇔ f is a function from (a→b) to c
~~~

### Basic examples

Defining the type of a function before its declaration isn't mandatory.
Haskell infers the most general type for you.
But it is considered a good practice to do so.

> square :: Num a => a -> a  
> square x = x^2

Note `^` use infix notation. 
For each infix operator there its associated prefix notation.
You just have to put it inside parathesis.

> square x = (^) x 2
> 
> square x = (^2) x

We can remove `x` in the left and right side!
It's called currying.

> square = (^2)

Also you can test values. 
For example an implementation of the absolute function.

> abs x :: Num a => a -> a
> abs = if x >= 0 then x else -x

Note: the `if .. then .. else` Haskell notation is more like the
`¤?¤:¤` C operator. You cannot forget the `else`.

Another notation

> abs x
>     | x >= 0 = x
>     | otherwise = -x



<h3>Functional style</h3>

In this section, we will see how you should code in functional style.
We will refactor the code to make it hopefully better and better.

Let's tackle the following problem:

 > Given a list of integer, return the sum of its even numbers.

The first thing you should note, is there isn't any `for` or `while` loop.
Don't worry, you can use _recursion_.

To answer this problem, we will use the following functions:

> even :: Integral a => a -> Bool
> head :: [a] -> a
> tail :: [a] -> [a]

`even` verify if a number is even.

~~~
even :: Integral a => a -> Bool
even 3  ⇒ False
even 2  ⇒ True
~~~

`head` gives the head of a list.

~~~
head :: [a] -> a
head [1,2,3] ⇒ 1
head []      ⇒ ERROR
~~~

`tail`, returns (surprise!) the tail of a list

~~~
tail :: [a] -> [a]
tail [1,2,3] ⇒ [2,3]
tail [3]     ⇒ []
tail []      ⇒ ERROR
~~~

Remark that:

~~~
l = [1,2,3]
l == (head l):(tail l)
~~~

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

> evenSum :: Integral a => [a] -> a

Next, we can use sub functions using `where` or `let`.
This way our `accumSum` function won't polute the global name space.

> -- Version 2
> evenSum :: Integral a => [a] -> a
> 
> evenSum l = accumSum 0 l
>     where accumSum n l = 
>             if l == []
>                 then n
>                 else let x = head l 
>                          xs = tail l 
>                      in if even x
>                             then accumSum (n+x) xs
>                             else accumSum n xs

Next, we can use pattern matching.

> -- Version 3
> evenSum l = accumSum 0 l
>     where 
>         accumSum n [] = n
>         accumSum n (x:xs) = 
>              if even x
>                 then accumSum (n+x) xs
>                 else accumSum n xs

What is pattern matching? 
Use value instead of general parameter name.

Instead of saying: `foo l = if l == [] then <x> else <y>`
You simply state:  

> foo [] =  <x>
> foo l  =  <y>

But pattern matching go even further. 
It is also able to inspect inside datas. 
We can replace

>  foo l =  let x  = head l 
>               xs = tail l
>           in if even x 
>               then foo (n+x) xs
>               else foo n xs

by

>  foo (x:xs) = if even x 
>                   then foo (n+x) xs
>                   else foo n xs

This is a very useful feature.
It makes our code both terse and easier to read.

We also can currify a bit our definition by removing the `l`:

> -- Version 4
> evenSum :: Integral a => [a] -> a
> 
> evenSum = accumSum 0
>     where 
>         accumSum n [] = n
>         accumSum n (x:xs) = 
>              if even x
>                 then accumSum (n+x) xs
>                 else accumSum n xs

<h3> Higher Level Functions </h3>

To make things even better we should use higher level functions.
What are these beast?
Higher level functions are functions taking another functions as parameters.

Here are some examples:

> import Data.List
> filter :: (a -> Bool) -> [a] -> [a]
> map :: (a -> b) -> [a] -> [b]
> foldl' :: (a -> b -> a) -> a -> [b] -> a

Let's proceed by small steps.

> -- Version 5
> evenSum l = mysum 0 (filter even l)
>     where 
>       mysum n [] = n
>       mysum n (x:xs) = mysum xs (n+x) 

> filter even [1..10] ⇔  [2,4,6,8,10]

Now you can use the `foldl'` to accumulate a value.

> -- Version 6
> import Data.List
> evenSum l = foldl' mysum 0 (filter even l)
>   where mysum acc value = acc + value

For each element of the list, `foldl'` will add it to the next.
And finally add 0.

If you really want to know how the magic works.
Here is the definition of `foldl`.

> foldl f z [] = z
> foldl f z (x:xs) = foldl f (f z x) xs

But as Haskell is lazy, it doesn't evaluate `(f z x)` and push this in the stack.
`foldl'` is a strict version of `foldl`.
If you don't understand what "lazy" and "strict" means,
don't worry, just follow the code as if `fold` and `foldl'` where identical.

Here is what occurs:

~~~
evenSum [1,2,3,4]
⇒ foldl' mysum 0 (filter even [1,2,3,4])
⇒ foldl' mysum 0 [2,4]
⇒ foldl' mysum (mysum 0 2) [4]
⇒ foldl' mysum (0+2) [4]
⇒ foldl' mysum 2 [4]
⇒ foldl' mysum (mysum 2 4) []
⇒ foldl' mysum (2+4) []
⇒ foldl' mysum 6 []
⇒ 6
~~~

Beware! 
Most of the time you want to use `foldl'` and not `foldl`.

This is nice, but as `mysum` is a very simple function, giving it a name is a burden.
We can use anonymous functions or lambdas.

> -- Version 7
> evenSum l = foldl' (\x y -> x+y) (filter even l)

And of course, we remark 

> (\x y -> x+y) 
> ⇔ (\x y -> (+) x y) -- Stop using infix notation
> ⇔ (\x -> (+) x)     -- Currify y
> ⇔ (\ -> (+) )       -- Currify x
> ⇔ (+)               -- Simplify notation 

Finaly

> -- Version 8
> import Data.List
> evenSum :: Integral a => [a] -> a
> evenSum l = foldl' (+) 0 (filter even l)

`foldl'` isn't the easiest function to intuit.
If you are not used to it, you should exercise a bit.

I would like to introduce another higher order function: `(.)`.
The `(.)` function correspond to the mathematical composition.

> (f . g . h) x ⇔  f ( g (h x))

We can take advantage of this operator to curry a bit more our function:

> -- Version 9
> import Data.List
> evenSum :: Integral a => [a] -> a
> evenSum = (foldl' (+) 0) . (filter even)

Also, there already exists a `sum` function.

> -- Version 10 
> import Data.List
> evenSum :: Integral a => [a] -> a
> evenSum = sum . (filter even)

Why is this last definition of evenSum superior to the first one?
Imagine I want not only to make the sum of even numbers of a list.
But now, I want to get the sum of all even square of element of the list.

~~~
[1,2,3,4] ~> [1,4,9,16] ~> [4,16] ~> 20
~~~

Update version 10 is extremely easy:

> squareEvenSum = sum . (filter even) . (map (^2))

We simply had to add another "transformation function".

~~~
map (^2) [1,2,3,4] ⇔ [1,4,9,16]
~~~

To modify version 1 is left as an exercise to the reader.
