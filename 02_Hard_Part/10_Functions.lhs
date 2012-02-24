## Hard Part

The hard part could now begins.

### Functional style

In this section, I give a short example of the impressive refactoring ability provided by Haskell.
We will choose a problem and resolve it the standard way. 
Then I will make the code evolve.
The end result will be both more elegant and easier to adapt. 

Let's resolve the following problem:

 > Given a list of integer, return the sum of its even numbers.

The first thing you should note, is there isn't any `for` or `while` loop.
Don't worry, there is _recursion_[^0120101].

[^0120101]: Don't worry if you comme from imperative programming. Generally Haskell handles recursion efficiently.

To answer this problem, we will use the following functions (which are defined in `Prelude`):

> even :: Integral a => a -> Bool
> head :: [a] -> a
> tail :: [a] -> [a]

`even` verify if a number is even.

~~~
even :: Integral a => a -> Bool
even 3  ⇒ False
even 2  ⇒ True
~~~

`head` returns the first element of a list:

~~~
head :: [a] -> a
head [1,2,3] ⇒ 1
head []      ⇒ ERROR
~~~

`tail`, returns all element except the first of a list:

~~~
tail :: [a] -> [a]
tail [1,2,3] ⇒ [2,3]
tail [3]     ⇒ []
tail []      ⇒ ERROR
~~~

Remark that for any list `l`, 
`l ⇔ (head l):(tail l)`
