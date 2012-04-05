#!/usr/bin/env zsh
for fic in **/*.lhs; do
    contains_haskell=$(( $( egrep '^>' $fic | wc -l) > 0 ))
    ((contains_haskell)) && \
        print "$fic" && { echo "0,1" | runhaskell $fic >/dev/null }
done
