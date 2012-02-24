Now, a program that ask your name and display `Hello <your name>!`.

> main = do
>     print "What is your name?"
>     name <- getLine
>     print ("Hello " ++ name ++ "!")

First, let us compare with a similar program in other imperative languages:

<code class="python">
# Python
print "What is your name?"
name = raw_input()
print "Hello %s!" % name
</code>

<code class="ruby">
# Ruby
puts "What is your name?"
name = gets.chomp
puts "Hello #{name}!"
</code>

<code class="c">
// In C
#include <stdio.h>
int main (int argc, char **argv) {
    char name[666]; // <- An Evil Number!
    // What if my name is more than 665 character long?
    printf("What is your name?\n"); 
    scanf("%s", name);
    printf("Hello %s!\n", name);
    return 0;
}
</code>

The structure is the same, but there are some syntax differences.
A major part of this tutorial will explain why.

In Haskell, there is a `main` function.
In Haskell every object has a type.
The type of `main` is `IO ()`.
This means, `main` will cause side effects.
`IO` is a ... . 
Wait! No! I won't say it now!
I am afraid to terrify you.
You might run away crying.
For now, I won't talk about what `IO` really is.

Just remember, Haskell can look a lot like other imperative languages.
