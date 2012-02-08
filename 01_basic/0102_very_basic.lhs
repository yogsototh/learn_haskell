## Very basic Haskell

Before some example, some essential concepts. These are more some warning.

_Functional_

Haskell is a functional language.
If you come from imperative language, you'll have to re-learn everything.
But you will discover a lot of new concepts!

_Smart Static Typing_

Instead of being in your way like in `C`, `C++` or `Java`.
The typing is here to help you.
It is never in your way.

_Purity_

Generally your function won't modify anything of the outside world.
This means, it can't modify the value of a variable, can't get user input, can't write on the screen, can't launch a missile.
On the other hand, parallelism will be very easy to achieve.
The fact is Haskell make it clear where effects occurs and where you are pure.

_Lazyness_

You can manipulate infinite structures. 
Inifinte lists, infinite trees etc...
Tail recursion has generally few importance in Haskell.

A last warning though. Once you know Haskell, all other language will have a taste of "not powerful enough", "not the right way"...
But you will know why.

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

Not bad, but what should we do if we want to add Float instead? 
Or some strange type like complex?

Don't worry, here is the solution:

<code class="haskell">
f :: Num a => a -> a -> a
f x y = x*x + y*y
</code>

First, `a` is a type variable. 
It means, that the first and the second argument will have the same type.
And furthermore, the result will also be of the same type.

So instead of having a forced type like in `C` with declaring the function for int, for long, for float, for double, etc... 
We declare only one function.
But remember our type is statically typed! Great.
It feels like dynamic typed language, and for this, this is good.

But what is this `Num a => ` part about?

`Num` is a typeclass.
Which means we are authorized to use any type which belongs to the class Num.
If a type declare himself to be a num, he must provide a definition for some functions. 
And for this example, the Num class ensure there will exists a `+` and a `*` for our type.

In very few lines I had given _a lot_ of informations.

Let's play a little.

> f :: Int -> Int -> Int
> f x y = x*x + y*y
>
> main = print (f 2 3)

