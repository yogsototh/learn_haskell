en: <h3 id="types">Types</h3>
fr: <h3 id="types">Les types</h3>

blogimage("salvador-dali-the-madonna-of-port-lligat.jpg","Dali, the madonna of port Lligat")

 > %tldr
 >
en: > - `type Name = AnotherType` is just an alias and the compiler doesn't mark any difference between `Name` and `AnotherType`.
en: > - `data Name = NameConstructor AnotherType` does mark a difference.
en: > - `data` can construct structures which can be recursives.
en: > - `deriving` is magic and creates functions for you.
fr: > - `type Name = AnotherType` n'est qu'un alias de type, le compilateur ne fera pas la différence entre les deux.
fr: > - `data Name = NameConstructor AnotherType` le compilateur fera la différence.
fr: > - `data` permet de construire de nouvelles structures qui peuvent être récursives.
fr: > - `deriving` est magique et créé automatiquement des fonctions pour vous.

en: In Haskell, types are strong and static.
fr: En Haskell, les types sont forts et statiques.

en: Why is this important? It will help you _greatly_ to avoid mistakes.
en: In Haskell, most bugs are caught during the compilation of your program.
en: And the main reason is because of the type inference during compilation.
en: Type inference makes it easy to detect where you used the wrong parameter at the wrong place, for example.
fr: Pourquoi est-ce important? Cela vous aidera a éviter _beaucoup_ d'erreurs.
fr: En Haskell, la majorité des bugs est repérée durant la compilation de votre programme.
fr: Et la raison principale de cela est l'inférence de type durant la compilation.
fr: L'inférence de type permet de détecter plus facilement lorsque vous utilisez le mauvais paramètre au mauvais endroit, par exemple.

en: <h4 id="type-inference">Type inference</h4>
fr: <h4 id="type-inference">Inférence de type</h4>

en: Static typing is generally essential for fast execution.
en: But most statically typed languages are bad at generalizing concepts.
en: Haskell's saving grace is that it can _infer_ types.
fr: Le typage statique est généralement essentiel pour une exécution rapide.
fr: Mais la plupart des langages typés statiquement ont du mal à généraliser des concepts.
fr: La "grâce salvatrice" de Haskell est qu'il peut _inférer_ des types.

en: Here is a simple example, the `square` function in Haskell:
fr: Voici un exemple simple, la fonction `square` en Haskell:

<code class="haskell">
square x = x * x
</code>

en: This function can `square` any Numeral type.
en: You can provide `square` with an `Int`, an `Integer`, a `Float` a `Fractional` and even `Complex`. Proof by example:
fr: Cette fonction peut mettre au carré n'importe quel type `Numeral`.
fr: Vous pouvez l'utilser avec un `Int`, un `Integer`, un `Float`, un `Fractional` ou même un `Complex`. Preuve par l'exemple:

~~~
% ghci
GHCi, version 7.0.4:
...
Prelude> let square x = x*x
Prelude> square 2
4
Prelude> square 2.1
4.41
en: Prelude> -- load the Data.Complex module
fr: Prelude> -- charge le module Data.Complex
Prelude> :m Data.Complex
Prelude Data.Complex> square (2 :+ 1)
3.0 :+ 4.0
~~~

en: `x :+ y` is the notation for the complex (<i>x + iy</i>).
fr: `x :+ y` est la notation pour le complexe (<i>x + iy</i>)

en: Now compare with the amount of code necessary in C:
fr: Comparons maintenant avec la quantité de code nécessaire pour le faire en C:

<code class="c">
int     int_square(int x) { return x*x; }

float   float_square(float x) {return x*x; }

complex complex_square (complex z) {
    complex tmp;
    tmp.real = z.real * z.real - z.img * z.img;
    tmp.img = 2 * z.img * z.real;
}

complex x,y;
y = complex_square(x);
</code>

en: For each type, you need to write a new function.
en: The only way to work around this problem is to use some meta-programming trick, for example using the pre-processor.
en: In C++ there is a better way, C++ templates:
fr: Pour chaque type, vous avez besoin d'écrire une nouvelle fonction.
fr: Le seul moyen de se débarrasser de ce problème est d'utiliser des astuces de méta-programmation, par exemple en utilisant le pré-processeur.
fr: en C++ il y a un meilleur moyen, les _templates_:

<code class="c++">
#include <iostream>
#include <complex>
using namespace std;

template<typename T>
T square(T x)
{
    return x*x;
}

int main() {
    // int
    int sqr_of_five = square(5);
    cout << sqr_of_five << endl;
    // double
    cout << (double)square(5.3) << endl;
    // complex
    cout << square( complex<double>(5,3) )
         << endl;
    return 0;
}
</code>

en: C++ does a far better job than C in this regard.
en: But for more complex functions the syntax can be hard to follow:
en: see [this article](http://bartoszmilewski.com/2009/10/21/what-does-haskell-have-to-do-with-c/) for example.
fr: C++ fait un bien meilleur travail que C ici.
fr: Mais pour des fonctions plus complexes, la syntaxe sera difficile à suivre.
fr: Voyez [cet article](http://bartoszmilewski.com/2009/10/21/what-does-haskell-have-to-do-with-c/) pour quelques exemples. (_NDT: toujours en anglais)

en: In C++ you must declare that a function can work with different types.
en: In Haskell, the opposite is the case.
en: The function will be as general as possible by default.
fr: En C++ vous devez déclarer qu'une fonction peut marcher avec différents types.
fr: En Haskell, c'est le contraire.
fr: La fonction sera aussi générale que possible par défaut.

en: Type inference gives Haskell the feeling of freedom that dynamically typed languages provide.
en: But unlike dynamically typed languages, most errors are caught before run time.
en: Generally, in Haskell:
fr: L'inférence de type donne à Haskell le sentiment de liberté que les langages dynamiquement typés proposent.
fr: Mais contrairement aux langages dynamiquement typés, la majorité des erreurs est détectée avant de lancer le programme.
fr: Généralement, en Haskell:

en:  > "if it compiles it certainly does what you intended"
fr:  > "Si ça compile, ça fait certainement ce que vous attendiez."
