#!/usr/bin/env zsh

cat <<END
-----
theme: scientific
image: /Scratch/img/blog/Haskell-the-Hard-Way/magritte_pleasure_principle.jpg
menupriority:   1
kind: article
published: 2012-02-08
en: title: Learn Haskell Fast and Hard
en: subtitle: Blow your mind with Haskell
fr: title: Haskell comme un vrai!
fr: subtitle: Haskell à s'en faire griller les neurones
author: Yann Esposito
authoruri: yannesposito.com
tags: Haskell, programming, functional, tutorial
-----
blogimage("magritte_pleasure_principle.jpg","Magritte pleasure principle")

<div class="intro">

en: %tldr A very short and dense tutorial for learning Haskell.

fr: %tlal Un tutoriel très court mais très dense pour apprendre Haskell.

en: Thanks to [Oleg Taykalo](https://plus.google.com/u/0/113751420744109290534) you can find a Russian translation here: [Part 1](http://habrahabr.ru/post/152889/) _&_ [Part 2](http://habrahabr.ru/post/153383/) ; 
fr: Merci à [Oleg Taykalo](https://plus.google.com/u/0/113751420744109290534) vous pouvez trouver une traduction russe ici: [Partie 1](http://habrahabr.ru/post/152889/) _&_ [Partie 2](http://habrahabr.ru/post/153383/) ; 
fr: Un grand merci à [lepereceval](https://github.com/lepereceval) pour sa traduction française que je n'ai pas eu le courage de faire moi-même !

> <center><hr style="width:30%;float:left;border-color:#CCCCD0;margin-top:1em"/><span class="sc"><b>Table of Content</b></span><hr style="width:30%;float:right;border-color:#CCCCD0;margin-top:1em"/></center>
>
> <div class="toc">
>
END

# Create the TOC

# get a list of
# depth anchor name
grep -e '<h.' **/*.lhs | perl -pe 'if (/^.*?:..:/) {s#^.*?:(..):.*<h([2-6]) id="#$1 $2 #; } else {s#.*<h([2-6]) id="#all $1 #;} ; s#"[^>]*># "#; s#<.*#"#' |
while read language num anchor title; do
    if [[ $language = "all" ]]; then
        echo -n '> '
    else
        echo -n $language': > '
    fi
        while ((num-->2)); do echo -n "  "; done
        echo '* <a href="#'$anchor'">'${title[2,-2]}'</a>'
done


cat <<END
>
> </div>

</div>
END


for fic in **/*.lhs; do
    contains_haskell=$(( $( egrep '^>' $fic | wc -l) > 0 ))
    ((contains_haskell)) && \
        echo "\n<hr/><a href=\"code/$fic\" class=\"cut\">${fic:h}/<strong>${fic:t}</strong></a>\n"
    cat $fic | \
    perl -pe 's#begindiv\(([^)]*)\)#<div class="$1">#' | \
    perl -pe 's#enddiv#</div>#' | \
    perl -pe 's#^<code class="([^"]*)">#~~~~~~ {.$1}#' | \
    perl -pe 's#^</code>#~~~~~~#'
    ((contains_haskell)) && \
        echo "\n<a href=\"code/$fic\" class=\"cut\">${fic:h}/<strong>${fic:t}</strong> </a>\n"
done | perl -pe 'BEGIN{$/="";} s#((^>.*\n)+)#<div class="codehighlight">\n~~~~~~ {.haskell}\n$1~~~~~~\n</div>#mg' | perl -pe 's#^> ?##' | perl -pe 's/^ #/#/'
