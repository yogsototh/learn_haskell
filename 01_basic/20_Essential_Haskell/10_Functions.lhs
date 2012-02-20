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
Don't worry, you can use _recursion_[^0120101].

[^0120101]: Don't worry if you comme from imperative programming. Generally Haskell handles recursion efficiently.

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

