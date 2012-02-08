Let's write a program that ask your name and display "Hello 'your name'!".

> main = do
>     print "What is your name?"
>     name <- getLine
>     print ("Hello " ++ name ++ "!")

First, let us compare with a similar program in Python and Ruby: 

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

The structure is the same, but there are some syntax differences:

- The `main` function. Haskell isn't an imperative scripting language. All your program execution is in fact simply the evaluation of the `main` function. 
- A `do` keyword, that wasn't needed for the "Hello World!" example.
- The use of "<-" instead of "=".

Haskell makes it clear in which part of your program
there will be side effect (like writting on the screen, read input, launch the missile) and which part remains pure (no side effect, only computing).

If we look at the type of main it is `IO ()`.
It is a way to say, main can cause side effects.
`IO` is a ... . No, I won't say it now. If I say it, I'm afraid you will run away crying. For now, I won't talk about what really is `IO`.
