#!/usr/bin/env zsh

cat <<END
-----
isHidden:       false
menupriority:   1
kind:           article
created_at:     2012-02-08T15:17:53+02:00
en: title: Haskell the Hard Way
fr: title: Haskell the Hard Way
author_name: Yann Esposito
author_uri: yannesposito.com
# tags:
-----
<%= blogimage("main.png","Title image") %>

begindiv(intro)

en: <%= tldr %>

fr: <%= tlal %>

enddiv

END

cat **/*.lhs | perl -pe 'BEGIN{$/="";} s#((^>.*\n)+)#<code class="haskell">\n$1</code>\n#mg' | perl -pe 's#^> ?##'
