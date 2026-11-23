#!/usr/bin/env zsh
# The X11 bar's differential test: COUNT random configurations and lines
# (tests/gen/GenX.hs), each shown by xmobar 0.50 and by xmobar-bend on a
# fresh Xvfb screen; the screens are compared pixel by pixel, and the
# windows' geometry and properties (_NET_WM_PID left out):
#
#   tests/x-diff.sh SEED COUNT [JOBS]
#
# Run in `nix develop .#x` (bend, GHC with xmobar, xmobar, Xvfb, xwd,
# ImageMagick, xwininfo, xprop, the DejaVu fonts as FONTS). XB: a built
# xmobar-bend (else Main.bend is built). KEEP=1 keeps the cases' files.
set -u
here=${0:A:h}
seed=${1:-1} count=${2:-20} jobs=${3:-4}
: ${GHC:=ghc} ${FONTS:?the fonts directory (DejaVu)}
out=$(mktemp -d)
$GHC -O0 -i$here/gen -outputdir $out/o -o $out/genx $here/gen/GenX.hs > $out/ghc.log 2>&1 || { cat $out/ghc.log; exit 1 }
if [[ -z ${XB:-} ]]; then
  (cd $here/.. && BEND_NO_TELEMETRY=1 bend Main.bend -o $out/xmobar-bend) || exit 1
  XB=$out/xmobar-bend
fi
REF=$(command -v xmobar)
$out/genx $seed $count $out/c
# the fonts the two see: DejaVu only, a cache of their own
cat > $out/fonts.conf <<EOF
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig><dir>$FONTS</dir><cachedir>$out/fc</cachedir></fontconfig>
EOF
export FONTCONFIG_FILE=$out/fonts.conf LANG=C.UTF-8 LC_ALL=C.UTF-8
fc-cache $FONTS > /dev/null 2>&1 || true

# shot BAR CASE NAME: BAR on the display with the case's line on its
# standard input; the screen, the window's geometry and properties
shot() {
  local bar=$1 c=$2 name=$3
  ( cat $c/line; sleep 3 ) | $bar $c/config > $c/$name.log 2>&1 &
  local pid=$!
  sleep 1.8
  xwd -root -silent | magick xwd:- $c/$name.png
  local win=$(xwininfo -root -tree | awk '/"xmobar"/ {print $1; exit}')
  if [[ -n $win ]]; then
    xwininfo -id $win | grep -E 'Absolute|Width|Height|Map State|Override' > $c/$name.geom
    xprop -id $win | grep -v '_NET_WM_PID' | sort > $c/$name.props
  else
    print "no window" > $c/$name.geom
    : > $c/$name.props
  fi
  kill $pid 2> /dev/null
  pkill -P $pid 2> /dev/null
  wait $pid 2> /dev/null
  sleep 0.2
}

# worker K: the cases K, K+JOBS, ... on display :(140+K)
worker() {
  local k=$1 d=$((140 + $1))
  Xvfb :$d -screen 0 1024x240x24 -nolisten tcp > $out/xvfb$k.log 2>&1 &
  local xv=$!
  sleep 1
  export DISPLAY=:$d
  for ((i = k; i <= count; i += jobs)); do
    local c=$out/c/$i
    shot $REF $c ref
    shot $XB $c bend
    local px=$(magick compare -metric AE $c/ref.png $c/bend.png $c/diff.png 2>&1)
    local same=1
    [[ $px == 0* ]] || same=0
    cmp -s $c/ref.geom $c/bend.geom || same=0
    cmp -s $c/ref.props $c/bend.props || same=0
    if (( same )); then
      print "$i same" >> $out/result$k
    else
      print "$i differ: pixels $px" >> $out/result$k
      diff $c/ref.geom $c/bend.geom >> $out/result$k
      diff $c/ref.props $c/bend.props >> $out/result$k
    fi
  done
  kill $xv
}

for ((k = 1; k <= jobs; k++)); do worker $k & done
wait
cat $out/result* | grep -v ' same$'
print "same $(cat $out/result* | grep -c ' same$'), differ $(cat $out/result* | grep -c ' differ')"
if [[ -n ${KEEP:-} ]]; then print "kept: $out"; else rm -rf $out; fi
