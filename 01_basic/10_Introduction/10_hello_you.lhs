en: Now, a program asking your name and replying "Hello" using the name you entered:
fr: Maintenant, un programme qui demande votre nom et répond "Hello" suivit du nom que vous avez entré:

> main = do
>     print "What is your name?"
>     name <- getLine
>     print ("Hello " ++ name ++ "!")

en: First, let us compare this with similar programs in a few imperative languages:
fr: Premièrement, comparons ce code avec ceux de quelques langages de programmation impératif:

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

en: The structure is the same, but there are some syntax differences.
en: The main part of this tutorial will be dedicated to explaining why.
fr: La structure est la même, mais il y a quelques différences de syntaxe.
fr: La partie principale de ce tutoriel sera consacrée à expliquer cela.

en: In Haskell there is a `main` function and every object has a type.
en: The type of `main` is `IO ()`.
en: This means `main` will cause side effects.
fr: En Haskell il y a une fonction `main` tous les objets ont un type.
fr: Le type de `main` est `IO ()`.
fr: Cela veut dire que `main` causera des effets secondaires.

en: Just remember that Haskell can look a lot like mainstream imperative languages.
fr: Rappelez-vous just que Haskell peut ressembler énormément aux principaux langages impératifs.
