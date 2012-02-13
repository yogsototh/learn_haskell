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

