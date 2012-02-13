# Basics

## Notations

~~~
3 + 2 * 6 / 3 <=> 3 + ((2*6)/3)
~~~

Logic notation

~~~
True || False => True
True && False => False
True == False => False
True /= False => True  (/=) is the operator for different
~~~

Powers

~~~
x^n     for n an integral
x**y    for y any kind of number (Float for example)
~~~

Integer have no limit except the capacity of your machine:

~~~
4^103   
102844034832575377634685573909834406561420991602098741459288064
~~~

Yeah!
And also rational numbers FTW!

~~~
$ ghci
....
Prelude> :m Data.Ratio
Data.Ratio> (11%15) * (5%3)
11 % 9
~~~

Lists

~~~
[]                      => empty list
[1,2,3]                 => List of integral
["foo","bar","baz"]     => List of string
1:[2,3]                 => [1,2,3], (:) prepend one element
[1,2,3] ++ [4]          => [1,2,3,4], (++) concatenate two lists
[1,2,3] ++ ["foo"]      => ERROR String /= Integral
[1..10]                 => [1,2,3,4,5,6,7,8,9,10]
[1,3..10]               => [1,3,5,7,9]
[2,3,5,7,11..100]       => ERROR! I am not so smart!
[10,9..1]               => [10,9,8,7,6,5,4,3,2,1]
~~~

Strings. 

In Haskell strings are list of `Char`.

~~~
'a' :: Char
"a" :: [Char]
""  <=> []
"ab" <=> ['a','b'] <=>  'a':"b" <=> 'a':['b'] <=> 'a':'b':[]
"abc" <=> "ab"++"c"
~~~

In real code you shouldn't use list of char to represent text.
You should mostly use `Data.Text` instead.

Tuples

The type of couple is `(a,b)`. 
Elements in a tuple can have different type.

~~~
(2,"foo")           is valid
(3,'a',[2,3])       is also valid
((2,"a"),"c",3)     is also valid

fst (x,y) = x
snd (x,y) = y

fst (x,y,z)         ERROR, fst :: (a,b) -> a
snd (x,y,z)         ERROR, snd :: (a,b) -> b
~~~

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
> evenSum l = accumSum l 0 
> 
> accumSum l n = if l == [] 
>                   then n
>                   else let x = head l in
>                       if even x 
>                           then foo xs (n+x)
>                           else foo xs n

Many things can be improved.
First, we can generalize the type.

> evenSum :: Integral a => [a] -> a

Next, we can use sub functions using `where` or `let`.

> -- Version 2
> evenSum l = accumSum l 0 
>   where
>     accumSum l n = if l == [] 
>                       then n
>                       else let x  = head l 
>                                xs = tail l
>                            in if even x 
>                                   then foo xs (n+x)
>                                   else foo xs n

After you can use pattern matching.

> -- Version 3
> evenSum l = accumSum l 0 
>   where
>       accumSum []     n = n
>       accumSum (x:xs) n = if even x 
>                              then foo xs (n+x)
>                              else foo xs n

What is patter matching? 
Use value instead of general parameter name.

Instead of saying: `foo l = if l == [] then <x> else <y>`
You simply state:  

> foo [] =  <x>
> foo l  =  <y>

But pattern matching go even further, it is also capable of inspecting inside datas. We can replace

>  foo l =  let x  = head l 
>               xs = tail l
>           in if even x 
>               then foo xs (n+x)
>               else foo xs n

by

>  foo (x:xs) = if even x 
>                   then foo xs (n+x)
>                   else foo xs n

This is a very useful feature. It makes our code both tersier and easier to read.

### High level functions

Now, we can make things better using high level functions.
What are these beast?
High level functions are functions taking functions as parameters.





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

