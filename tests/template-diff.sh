#!/usr/bin/env zsh
# The markup's and templates' differential test against xmobar 0.50's own
# parseString, splitTemplate and parseTemplate:
#   tests/template-diff.sh SEED COUNT
# GHC: a ghc with xmobar's library, every module exposed, and QuickCheck
# (.#ghc-xmobar); BEND: bend.
set -eu
here=${0:A:h}
seed=${1:-1} count=${2:-100}
: ${GHC:?a ghc with xmobar and QuickCheck (.#ghc-xmobar)} ${BEND:=bend}
out=$(mktemp -d)
$GHC -O0 -outputdir $out -o $out/gen $here/gen/GenTemplate.hs > $out/ghc.log 2>&1 || { cat $out/ghc.log; exit 1 }
$out/gen $seed $count > $here/TemplateCases.bend
(cd $here && BEND_NO_TELEMETRY=1 $BEND TemplateDiff.bend | grep -v '^Hint')
rm -rf $out
