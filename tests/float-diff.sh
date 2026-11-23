#!/usr/bin/env zsh
# Float's differential test against GHC's own showFFloat, show, round and
# fromInteger at Float:
#   tests/float-diff.sh SEED COUNT
set -eu
here=${0:A:h}
seed=${1:-1} count=${2:-100}
: ${GHC:?a ghc with QuickCheck (.#ghc-xmobar)} ${BEND:=bend}
out=$(mktemp -d)
$GHC -O0 -outputdir $out -o $out/gen $here/gen/GenFloat.hs > $out/ghc.log 2>&1 || { cat $out/ghc.log; exit 1 }
$out/gen $seed $count > $here/FloatCases.bend
(cd $here && BEND_NO_TELEMETRY=1 $BEND FloatDiff.bend | grep -v '^Hint')
rm -rf $out
