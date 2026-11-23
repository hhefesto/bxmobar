#!/usr/bin/env zsh
# The monitors' differential test against xmobar 0.50's own MultiCpu,
# Memory and Swap, each run as xmobar starts it, inside bwrap with random
# /proc files bound over the real ones:
#   tests/mon-diff.sh SEED COUNT
# GHC: .#ghc-xmobar; BWRAP: bubblewrap's bwrap; BEND: bend.
set -eu
here=${0:A:h}
seed=${1:-1} count=${2:-50}
: ${GHC:?a ghc with xmobar (.#ghc-xmobar)} ${BWRAP:?bubblewrap} ${BEND:=bend}
out=$(mktemp -d)
$GHC -O1 -threaded -outputdir $out/o1 -o $out/monref $here/gen/MonRef.hs > $out/ghc.log 2>&1 || { cat $out/ghc.log; exit 1 }
$GHC -O1 -outputdir $out/o2 -o $out/genmon $here/gen/GenMon.hs >> $out/ghc.log 2>&1 || { cat $out/ghc.log; exit 1 }
$out/genmon $seed $count $out/monref $BWRAP > $here/MonCases.bend
(cd $here && BEND_NO_TELEMETRY=1 $BEND MonDiff.bend | grep -v '^Hint')
rm -rf $out
