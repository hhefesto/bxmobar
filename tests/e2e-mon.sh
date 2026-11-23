#!/usr/bin/env zsh
# The bar end to end against xmobar 0.50's own monitors (tests/gen/GenE2E.hs
# says how): xmobar-bend built from Main.bend, then COUNT random cases:
#   tests/e2e-mon.sh SEED COUNT
# GHC: .#ghc-xmobar; BWRAP: bubblewrap's bwrap; BEND: bend (or XB: a built
# xmobar-bend).
set -eu
here=${0:A:h}
seed=${1:-1} count=${2:-100}
: ${GHC:?a ghc with xmobar (.#ghc-xmobar)} ${BWRAP:?bubblewrap} ${BEND:=bend}
out=$(mktemp -d)
$GHC -O1 -threaded -i$here/gen -outputdir $out/o1 -o $out/monref $here/gen/MonRef.hs > $out/ghc.log 2>&1 || { cat $out/ghc.log; exit 1 }
$GHC -O1 -i$here/gen -outputdir $out/o2 -o $out/gene2e $here/gen/GenE2E.hs >> $out/ghc.log 2>&1 || { cat $out/ghc.log; exit 1 }
if [[ -z ${XB:-} ]]; then
  (cd $here/.. && BEND_NO_TELEMETRY=1 $BEND Main.bend -o $out/xmobar-bend)
  XB=$out/xmobar-bend
fi
LANG=C.UTF-8 $out/gene2e $seed $count $out/monref $BWRAP $XB
rm -rf $out
