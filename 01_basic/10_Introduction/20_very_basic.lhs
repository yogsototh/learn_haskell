en: <h3 id="very-basic-haskell">Very basic Haskell</h3>
fr: <h3 id="very-basic-haskell">Les bases de Haskell</h3>

blogimage("picasso_owl.jpg","Picasso minimal owl")

en: Before continuing you need to be warned about some essential properties of Haskell.
fr: Avant de continuer, vous devez êtres avertis à propos de propriétés essentielles de Haskell.

en: _Functional_
fr: _Fonctionnel_

en: Haskell is a functional language.
en: If you have an imperative language background, you'll have to learn a lot of new things.
en: Hopefully many of these new concepts will help you to program even in imperative languages.
fr: Haskell est un langage fonctionnel
fr: Si vous avez déjà travaillé avec un langage impératif, vous devrez apprendre beaucoup de nouvelles choses.
fr: Heureusement beaucoup de ces nouveaux concepts vous aidera à programmer même dans un langage impératif.

en: _Smart Static Typing_
fr: _Typage Statique Intelligent_

en: Instead of being in your way like in `C`, `C++` or `Java`, the type system is here to help you.
fr: Au lieu de bloquer votre chemin comme en `C`, `C++` ou `Java`, le système de typage est ici pour vous aider.

en: _Purity_
fr: _Pureté_

en: Generally your functions won't modify anything in the outside world.
en: This means they can't modify the value of a variable, can't get user input, can't write on the screen, can't launch a missile.
en: On the other hand, parallelism will be very easy to achieve.
en: Haskell makes it clear where effects occur and where your code is pure.
en: Also, it will be far easier to reason about your program.
en: Most bugs will be prevented in the pure parts of your program.
fr: Généralement vos fonctions ne modifieront rien dans le monde extérieur.
fr: Cela veut dire qu'elles ne peuvent pas modifier la valeur d'une 

en: Furthermore, pure functions follow a fundamental law in Haskell:
fr: En outre, les fonctions pures suivent une loi fondamentale en Haskell:

en: > Applying a function with the same parameters always returns the same value.
fr: > Appliquer une fonction avec les mêmes paramètres retourne toujours la même valeur.

en: _Laziness_
fr: _Paresse_

en: Laziness by default is a very uncommon language design.
en: By default, Haskell evaluates something only when it is needed.
en: In consequence, it provides a very elegant way to manipulate infinite structures, for example.
fr: La paresse par défaut est un choix de conception de langage très rare.
fr: Par défaut, Haskell évalue quelque chose seulement lorsque cela est nécessaire.
fr: En conséquence, cela fournit un moyen très élégant de manipuler des structures infinies, par exemple.

en: A last warning about how you should read Haskell code.
en: For me, it is like reading scientific papers.
en: Some parts are very clear, but when you see a formula, just focus and read slower.
en: Also, while learning Haskell, it _really_ doesn't matter much if you don't understand syntax details.
en: If you meet a `>>=`, `<$>`, `<-` or any other weird symbol, just ignore them and follows the flow of the code.
fr: Un dernier avertissement sur comment vous devriez lire le code Haskell.
fr: Pour moi, c'est comme lire des papiers scientifiques.
fr: Quelques parties sont très claires, mais quand vous voyez une formule, concentrez-vous dessus et lisez plus lentement.
fr: De plus, lorsque vous apprenez Haskell, cela n'importe _vraiment_ pas si vous ne comprenez pas les détails syntaxiques.
fr: Si vous voyez un `>>=`, `<$>`, `<-` ou n'importe quel symbole bizarre, ignorez-les et suivez le déroulement du code.

en: <h4 id="function-declaration">Function declaration</h4>
fr: <h4 id="function-declaration">Déclaration de fonctions</h4>

en: You might be used to declaring functions like this:
fr: Vous avez déjà dû déclarer des fonctions comme cela:

en: In `C`:
fr: En `C`:

<code class="c">
int f(int x, int y) {
    return x*x + y*y;
}
</code>

en: In JavaScript:
fr: En JavaScript:

<code class="javascript">
function f(x,y) {
    return x*x + y*y;
}
</code>

en: in Python:
fr: En Python:

<code class="python">
def f(x,y):
    return x*x + y*y
</code>

en: in Ruby:
fr: En Ruby:

<code class="ruby">
def f(x,y)
    x*x + y*y
end
</code>

en: In Scheme:
fr: En Scheme:

<code class="scheme">
(define (f x y)
    (+ (* x x) (* y y)))
</code>

en: Finally, the Haskell way is:
fr: Finalement, la manière de faire de Haskell est:

<code class="haskell">
f x y = x*x + y*y
</code>

en: Very clean. No parenthesis, no `def`.
fr: Très propre. Aucune parenthèse, aucun `def`.

en: Don't forget, Haskell uses functions and types a lot.
en: It is thus very easy to define them.
en: The syntax was particularly well thought out for these objects.
fr: N'oubliez pas, Haskell utilise beaucoup les fonctions et les types.
fr: C'est très facile de les définir.
fr: La syntaxe a été particulièrement réfléchie pour ces objets.

en: <h4 id="a-type-example">A Type Example</h4>
fr: <h4 id="a-type-example">Un exemple de type</h4>

en: Although it is not mandatory, type information for functions is usually made
en: explicit. It's not mandatory because the compiler is smart enough to discover
en: it for you. It's a good idea because it indicates intent and understanding.
fr: Même si ce n'est pas obligatoire, les informations de type pour les fonctions sont habituellement déclarées
fr: explicitement. Ce n'est pas indispensable car le compilateur est suffisament intelligent pour le déduire
fr: à votre place. Cependant, c'est une bonne idée car cela montre bien l'intention du développeur et facilite la compréhension.

en: Let's play a little.
fr: Jouons un peu.

en: > -- We declare the type using ::
fr: > -- On déclare le type en utilisant:
> f :: Int -> Int -> Int
> f x y = x*x + y*y
>
> main = print (f 2 3)

~~~
~ runhaskell 20_very_basic.lhs
13
~~~
