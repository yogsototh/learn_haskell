## Functions

Every Haskell object has a type.

~~~
x :: Int            <= Means x is of type Int
f :: Int -> Int     <= Means f is a function from Int to Int
f :: a -> a         <= Means f is a function to any type a to the same type a
f :: a -> b -> c    <= Means f is a function from a to (b -> c)  - Yes Curry -
f :: Num a => a -> a <= Means f is a function from a to a
                        With the constraint
                        a must be in the typeclass Num
~~~

### Basic examples

> square :: Num a => a -> a  -- defining the type isn't mandatory
>                            -- Haskell find the most general type for you.
>                            -- But it is considered a good practice.
> square x = x^2

We can remove `x` in the left and right side!

> square = (^2)      

A test. The absolute function.


> abs x :: Num a => a -> a
> abs = if x >= 0 then x else -x

Another notation

> abs x
>     | x >= 0 = x
>     | otherwise

### Recursivity

Haskell don't have for loop, it has `map`, `filter`, `foldl` and `foldr`.
Note, 98% of the time you don't want to use `foldl` but `foldl'` in `Data.List` instead.

Let's tackle the following problem.

Given a list of integer, return the sum of even numbers.
Beware, the implementation will go from very naive to better and better.

We will use the following functions:

> even :: Integral a => a -> Bool
> head :: [a] -> a
> tail :: [a] -> [a]

~~~
even 3       => False
even 2       => True
head [1,2,3] => 1
head []      => ERROR
tail [1,2,3] => [2,3]
l = [1,2,3]
l == (head l):(tail l)
~~~

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

Many things can be improved.
First, we can generalize the type.

> evenSum :: Integral a => [a] -> a

Next, we can use sub functions using `where` or `let`.

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

After you can use pattern matching.

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
>               then foo xs (n+x)
>               else foo xs n

by

>  foo (x:xs) = if even x 
>                   then foo xs (n+x)
>                   else foo xs n

This is a very useful feature.
It makes our code both tersier and easier to read.

We also can currify a bit our definition

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

### High level functions

To make things even better we should use high level functions.
What are these beast?
High level functions are functions taking functions as parameters.

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

Now you can use the `fold'` to accumlate a value.

> -- Version 6
> import Data.List
> evenSum l = foldl' mysum 0 (filter even l)
>   where mysum acc value = acc + value

For each element of the list, foldl' will add it to the next.
And finally add 0.

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

Beware! 95% of the time you want to use `foldl'` and not `foldl`.
More on that later.

This is nice, but as mysum is a very simple function, giving it a name is a burden.
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

foldl isn't the easiest function to intuit.
There is another higher order function I'd like to introduce: `(.)`.
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

Why is this last for superior to the first one?
Imagine I want not only to make the sum of even numbers of a list.
But now, I want to get the sum of all even square of element of the list.

[1,2,3,4] ~> [1,4,9,16] ~> [4,16] ~> 20

To modify Version 10:

> squareEvenSum = sum . (filter even) . (map (^2))

I simply, add another "transformation function".
map (^2) [1,2,3,4] ⇔ [1,4,9,16]

Modify version 1 is left as an exercise to the reader.

# Types

Haskell has static strong types.

Every value as a type and the type is know at compile time.
Static typing is generally essential to reach fast execution time.
But in common languages static typing has the price of bad generalization.


<code class="c">
// in C
int     int_square(int x) { return x*x; }
float   fl_square(float x) {return x*x; }
complex complex_square (complex z) {
    complex tmp; 
    tmp.real = z.real * z.real - z.img * z.img;
    tmp.img = 2 * z.img * z.real;
}
</code>

To compensate a bit, C++ has templates:

<code class="c++">
class Number<T> {
    T value;
    square() {
        value = value*value;
    }
}

Number<int> i;
i.square;

Number<float> f;
f.square;

class Complex {
    int real;
    int img;
    Complex operator<*>(Complex z) {
        Complex result;
        result.real = real*z.real - img*z.img;
        result.img  = img*z.real + real*z.img;
        return res;
    }
}

Number<Complex> z;
z.square
</code>

In Haskell

<code class="haskell">
square x = x * x

square 2
4
square 2.1
4.41

:m Data.Complex
(2 :+ 1) * (2 :+ 1) 
3.0 :+ 4.0
</code>

`x :+ y` is the notation for the complex (<i>x + ib</i>).
As with `C++`, the code for the multiplication is made inside the module `Data.Complex`.

