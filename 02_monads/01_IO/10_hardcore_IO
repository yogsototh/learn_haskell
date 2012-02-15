First, let's take a look at the IO type.

> newtype IO a = IO (State# RealWorld -> (# State# RealWorld,a #))

Ho gosh! What is this notation?

I said it will be Haskell the hard way. It is.
Now make it easier.

First, we need to understand basic concepts.
You can just forget about all these sharp sybols.

> newtype IO a = IO (State RealWorld -> (State RealWorld, a))

OK, let's start here. What is `RealWorld`?
From the doc

> data RealWorld

RealWorld is deeply magical. 
It is primitive, but it is not unlifted (hence ptrArg).
We never manipulate values of type RealWorld; it's only used in the type system, to parameterise `State#`.

Hu? It is a type with nothing inside it. 
No representation for it at all. 
It is just a name.

Now what is `State`?

It is a data with one parameter, a type of state.

> data State s

The only purpose of the type parameter (`s`) is to keep different state threads separate.

In fact, let's try to traduce 

> newtype IO a = IO (State RealWorld -> (State RealWorld, a))

In a more Human and intuitive terms with an example.

> IO String = IO (State RealWorld -> (State RealWorld, String))

`IO String` is a function from a type (State RealWorld) to a couple
of type (State RealWorld, String).

Which if we simplify another time can be said as:

IO String is a function for a state of the world to anotherstate of the world and an String value.

It seems it fit nicely a function like `getLine`.

getLine can be functino in which we provide a state of the real world.
Then getLine do his job, then after that, it provide us a couple.
The String containing the content of the line read, and the changed world state.
Changed because, when we had read something, the state of the world changed.

Congratulation to have followed the first _Hardcore Haskell IO_ level 1.

Now, it will be time to go to level 2.

To make things clearer, I will augment the verbosity of the type. And instead of writting `IO a`. I will write `World -> (World,a)`.

It was a nice help for me. 
It is often hard to remember that "IO a" is in fact a function.

