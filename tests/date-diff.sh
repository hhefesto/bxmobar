#!/usr/bin/env zsh
# The Date plugin's differential test against time 1.12.2's own
# formatTime defaultTimeLocale, on random times and formats:
#   tests/date-diff.sh SEED COUNT
set -eu
here=${0:A:h}
seed=${1:-1} count=${2:-100}
: ${GHC:?a ghc with QuickCheck (.#ghc-xmobar)} ${BEND:=bend}
out=$(mktemp -d)
$GHC -O0 -outputdir $out -o $out/gen $here/gen/GenDate.hs > $out/ghc.log 2>&1 || { cat $out/ghc.log; exit 1 }
$out/gen $seed $count > $here/DateCases.bend
(cd $here && BEND_NO_TELEMETRY=1 $BEND DateDiff.bend | grep -v '^Hint')
rm -rf $out
