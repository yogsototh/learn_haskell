### Very basic Haskell

Before continuing you need be warned.
You have to know some essential properties of Haskell. 

_Functional_

Haskell is a functional language.
If you come from imperative language, you'll have to re-learn everything.
But you will discover a lot of new concepts!

_Smart Static Typing_

Instead of being in your way like in `C`, `C++` or `Java`.
The type system is here to help you.

_Purity_

Generally your function won't modify anything of the outside world.
This means, it can't modify the value of a variable, can't get user input, can't write on the screen, can't launch a missile.
On the other hand, parallelism will be very easy to achieve.
Haskell makes it clear where effects occurs and where you are pure.

Furthermore there is an essential respected law in Haskell:

 > Applying a function with the same parameter always return the same value.

_Lazyness_

You can manipulate infinite structures. 
Inifinte lists, infinite trees etc...

A last warning.

Once you know Haskell, most (all?) other languages will have a taste of "not powerful enough", "not the right way"...

### Function declaration

Function are first class object in Haskell.
That means, you can use function as argument, as any other object (like Integer, data structure, etc...)

You might be used to declare functions like this:

In `C`:

<code class="c">
int f(int x, int y) {
    return x*x + y*y;
}
</code>

In javascript:

<code class="javascript">
function f(x,y) {
    return x*x + y*y;
}
</code>

in Python:

<code class="python">
def f(x,y) =
    return x*x + y*y;
</code>

in Ruby:

<code class="ruby">
def f(x,y)
    x*x + y*y
end
</code>

In Scheme:

<code class="scheme">
(define (f x y)
    (+ (* x x) (* y y)))
</code>

Finaly, the Haskell way is:

<code class="haskell">
f x y = x*x + y*y
</code>

Very clean. No parenthesis, no `def`.

Don't forget, Haskell is mainly built on function and types.
It is thus very easy to define functions and types.
The syntax was particularly well thought for these objects.

Generally it is a good idea to write the type of your function before its definition. But it is not mandatory. The compiler is smart enough to discover it for you.

<code class="haskell">
f :: Int -> Int -> Int
f x y = x*x + y*y
</code>

Let's play a little.

> f :: Int -> Int -> Int
> f x y = x*x + y*y
>
> main = print (f 2 3)

