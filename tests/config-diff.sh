#!/usr/bin/env zsh
# The configuration's differential test against xmobar 0.50's own
# parseConfig, on random xmobarrc texts:
#   tests/config-diff.sh SEED COUNT
set -eu
here=${0:A:h}
seed=${1:-1} count=${2:-100}
: ${GHC:?a ghc with QuickCheck (.#ghc-xmobar)} ${BEND:=bend}
out=$(mktemp -d)
$GHC -O0 -outputdir $out -o $out/gen $here/gen/GenConfig.hs > $out/ghc.log 2>&1 || { cat $out/ghc.log; exit 1 }
$out/gen $seed $count > $here/ConfigCases.bend
(cd $here && BEND_NO_TELEMETRY=1 $BEND ConfigDiff.bend | grep -v '^Hint')
rm -rf $out
