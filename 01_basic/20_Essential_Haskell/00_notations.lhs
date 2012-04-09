<h2 id="essential-haskell">Essential Haskell</h2>

<%= blogimage("kandinsky_gugg.jpg","Kandinsky Gugg") %>

I suggest you to skim this part. 
Think of it like a reference.
Haskell has a lot of features. 
Many informations are missing here.
Get back here if notation feels strange.

I use the `⇔` symbol to state that two expression are equivalent.
It is a meta notation, `⇔` does not exists in Haskell.
I will also use `⇒` to show what is the return of an expression.

<h3 id="notations">Notations</h3>

<h5 id="arithmetic">Arithmetic</h5>

~~~
3 + 2 * 6 / 3 ⇔ 3 + ((2*6)/3)
~~~

<h5 id="logic">Logic</h5>

~~~
True || False ⇒ True
True && False ⇒ False
True == False ⇒ False
True /= False ⇒ True  (/=) is the operator for different
~~~

<h5 id="powers">Powers</h5>

~~~
x^n     for n an integral (understand Int or Integer)
x**y    for y any kind of number (Float for example)
~~~

`Integer` have no limit except the capacity of your machine:

~~~
4^103   
102844034832575377634685573909834406561420991602098741459288064
~~~

Yeah!
And also rational numbers FTW!
But you need to import the module `Data.Ratio`:

~~~
$ ghci
....
Prelude> :m Data.Ratio
Data.Ratio> (11 % 15) * (5 % 3)
11 % 9
~~~

<h5 id="lists">Lists</h5>

~~~
[]                      ⇔ empty list
[1,2,3]                 ⇔ List of integral
["foo","bar","baz"]     ⇔ List of String
1:[2,3]                 ⇔ [1,2,3], (:) prepend one element
1:2:[]                  ⇔ [1,2]
[1,2] ++ [3,4]          ⇔ [1,2,3,4], (++) concatenate
[1,2,3] ++ ["foo"]      ⇔ ERROR String ≠ Integral
[1..4]                  ⇔ [1,2,3,4]
[1,3..10]               ⇔ [1,3,5,7,9]
[2,3,5,7,11..100]       ⇔ ERROR! I am not so smart!
[10,9..1]               ⇔ [10,9,8,7,6,5,4,3,2,1]
~~~

<h5 id="strings">Strings</h5>

In Haskell strings are list of `Char`.

~~~
'a' :: Char
"a" :: [Char]
""  ⇔ []
"ab" ⇔ ['a','b'] ⇔  'a':"b" ⇔ 'a':['b'] ⇔ 'a':'b':[]
"abc" ⇔ "ab"++"c"
~~~

 > _Remark_:
 > In real code you shouldn't use list of char to represent text.
 > You should mostly use `Data.Text` instead.
 > If you want to represent stream of ASCII char, you should use `Data.ByteString`.

<h5 id="tuples">Tuples</h5>

The type of couple is `(a,b)`. 
Elements in a tuple can have different type.

~~~
-- All these tuple are valid
(2,"foo")
(3,'a',[2,3])
((2,"a"),"c",3)

fst (x,y)       ⇒  x
snd (x,y)       ⇒  y

fst (x,y,z)     ⇒  ERROR: fst :: (a,b) -> a
snd (x,y,z)     ⇒  ERROR: snd :: (a,b) -> b
~~~

<h5 id="deal-with-parentheses">Deal with parentheses</h5>

To remove some parentheses you can use two functions: `($)` and `(.)`.

~~~
-- By default:
f g h x         ⇔  (((f g) h) x)

-- the $ replace parenthesis from the $
-- to the end of the expression 
f g $ h x       ⇔  f g (h x) ⇔ (f g) (h x)
f $ g h x       ⇔  f (g h x) ⇔ f ((g h) x)
f $ g $ h x     ⇔  f (g (h x))

-- (.) the composition function
(f . g) x       ⇔  f (g x)
(f . g . h) x   ⇔  f (g (h x))
~~~
