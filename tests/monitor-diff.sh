#!/usr/bin/env zsh
# The monitors' common part's differential test against xmobar 0.50's own
# Common.Output and Common.Parsers, run as a monitor runs them:
#   tests/monitor-diff.sh SEED COUNT
set -eu
here=${0:A:h}
seed=${1:-1} count=${2:-100}
: ${GHC:?a ghc with QuickCheck (.#ghc-xmobar)} ${BEND:=bend}
out=$(mktemp -d)
$GHC -O0 -outputdir $out -o $out/gen $here/gen/GenMonitor.hs > $out/ghc.log 2>&1 || { cat $out/ghc.log; exit 1 }
$out/gen $seed $count > $here/MonitorCases.bend
(cd $here && BEND_NO_TELEMETRY=1 $BEND MonitorDiff.bend | grep -v '^Hint')
rm -rf $out
