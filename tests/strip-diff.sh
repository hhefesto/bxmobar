#!/usr/bin/env zsh
# StdinReader's stripActions against xmobar 0.50's (gen/GenStrip.hs):
#   tests/strip-diff.sh SEED COUNT
# GHC: .#ghc-xmobar; BEND: bend.
set -eu
here=${0:A:h}
seed=${1:-1} count=${2:-500}
: ${GHC:?a ghc with xmobar (.#ghc-xmobar)} ${BEND:=bend}
out=$(mktemp -d)
$GHC -O0 -i$here/gen -outputdir $out/o -o $out/genstrip $here/gen/GenStrip.hs > $out/ghc.log 2>&1 || { cat $out/ghc.log; exit 1 }
LANG=C.UTF-8 LC_ALL=C.UTF-8 $out/genstrip $seed $count > $here/StripCases.bend
(cd $here && BEND_NO_TELEMETRY=1 $BEND StripDiff.bend | grep -v '^Hint')
rm -rf $out
