#!/usr/bin/env zsh
# The monitors' differential test against xmobar 0.50's own MultiCpu,
# Memory, Swap, Network and DynNetwork, each run as xmobar starts it,
# inside bwrap with random /proc and /sys files bound over the real ones,
# its wall clock stopped (libfaketime):
#   tests/mon-diff.sh SEED COUNT
# GHC: .#ghc-xmobar; BWRAP: bubblewrap's bwrap (.#bubblewrap); FAKETIME_LIB:
# libfaketime.so.1 (.#libfaketime's lib/); BEND: bend.
set -eu
here=${0:A:h}
seed=${1:-1} count=${2:-50}
: ${GHC:?a ghc with xmobar (.#ghc-xmobar)} ${BWRAP:?bubblewrap} ${FAKETIME_LIB:?libfaketime.so.1} ${BEND:=bend}
out=$(mktemp -d)
$GHC -O1 -threaded -i$here/gen -outputdir $out/o1 -o $out/monref $here/gen/MonRef.hs > $out/ghc.log 2>&1 || { cat $out/ghc.log; exit 1 }
$GHC -O1 -i$here/gen -outputdir $out/o2 -o $out/genmon $here/gen/GenMon.hs >> $out/ghc.log 2>&1 || { cat $out/ghc.log; exit 1 }
$out/genmon $seed $count $out/monref $BWRAP $FAKETIME_LIB > $here/MonCases.bend
(cd $here && BEND_NO_TELEMETRY=1 $BEND MonDiff.bend | grep -v '^Hint')
rm -rf $out
