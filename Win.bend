# The bar's window, what is decided about it (Xmobar.X11.Window): where it
# goes on the screens (setPosition), its struts (getStrutValues) and its
# height from the font. X11 itself (creating it, its properties) is in
# x11.c, through X.bend.
#
# X's numbers keep their types' arithmetic as U32s: a Position (Int32) in
# two's complement, a Dimension (Word32), both wrapping as in Haskell; the
# config's Ints are their low 32 bits (all xmobar sends X is 32 bits). The
# struts are Ints in Haskell, sent as 32-bit CARDINALs: their low 32 bits
# are what X gets, so U32 arithmetic gives the same; only Static's
# comparison is signed.

import Base
import ./Config.bend as C
import ./Template.bend as T

# X.Rectangle: x, y Positions, width, height Dimensions
type Rect is Data:
  Rect{x: U32, y: U32, w: U32, h: U32}

def rect.w(r: Rect) -> U32:
  match r:
    case Rect{_, _, w, _}:
      w

# Signed (Int32) order on two's complement words
def s.lt(+a: U32, +b: U32) -> Bool:
  U32.is_lt(U32.xor(a, 2147483648), U32.xor(b, 2147483648))

def s.max(+a: U32, +b: U32) -> U32:
  Bool.pick(U32, s.lt(a, b), b, a)

# div at Int: floor division by 2, an arithmetic shift
def s.div2(+a: U32) -> U32:
  U32.or(U32.shr(a), U32.and(a, 2147483648))

# The screen (picker) -------------------------------------------------------------------

# maximumBy (compare `on` rect_width): the last of the widest
def broadest.go(rs: List<&2, Rect>, +acc: Rect) -> Rect:
  match rs:
    case Nil{}:
      acc
    case Con{+r, rest}:
      broadest.go(rest, Bool.pick(Rect, U32.is_gt(rect.w(acc), rect.w(r)), acc, r))

# picker: the broadest when pickBroadest, else the first (Xinerama always
# answers one at least; none is head []'s error, here a 0 rectangle)
def picker(+broadest: Bool, rs: List<&2, Rect>) -> Rect:
  match rs:
    case Nil{}:
      Rect{0, 0, 0, 0}
    case Con{+r, rest}:
      Bool.pick(Rect, broadest, broadest.go(rest, r), r)

# safeIndex i rs
def safe.index(rs: List<&2, Rect>, +i: U32) -> Maybe<&2, Rect>:
  match rs:
    case Nil{}:
      None{}
    case Con{r, rest}:
      Bool.pick(Maybe<&2, Rect>, U32.is_eq(i, 0), Some{r}, safe.index(rest, U32.sub(i, 1)))

def or.pick(m: Maybe<&2, Rect>, +broadest: Bool, rs: List<&2, Rect>) -> Rect:
  match m:
    case Some{r}:
      r
    case None{}:
      picker(broadest, rs)

# setPosition ------------------------------------------------------------------------------

# pw i = rw * min 100 i `div` 100 (Word32s)
def pw(+rw: U32, +i: U32) -> U32:
  U32.div(U32.mul(rw, U32.min(100, i)), 100)

# remwid i = rw - pw (fi i)
def remwid(+rw: U32, +i: U32) -> U32:
  U32.sub(rw, pw(rw, i))

def ax(a: T.Align, +rx: U32, +rw: U32, +i: U32) -> U32:
  match a:
    case T.AL{}:
      rx
    case T.AR{}:
      U32.add(rx, remwid(rw, i))
    case T.AC{}:
      U32.add(rx, U32.div(remwid(rw, i), 2))

# mh h' = max (fi h') h
def mh(+ch: U32, +h: U32) -> U32:
  U32.max(ch, h)

# the position on the screen scr = Rect{rx, ry, rw, rh}
# ny = ry + fi (rh - ht)
def ny(+ry: U32, +rh: U32, +h: U32) -> U32:
  U32.add(ry, U32.sub(rh, h))

def place(p: C.Pos, +rx: U32, +ry: U32, +rw: U32, +rh: U32, +h: U32) -> Rect:
  match p:
    case C.Top{}:
      Rect{rx, ry, rw, h}
    case C.TopP{+l, +r}:
      Rect{U32.add(rx, l), ry, U32.sub(U32.sub(rw, l), r), h}
    case C.TopH{+ch}:
      Rect{rx, ry, rw, mh(ch, h)}
    case C.TopHM{+ch, +l, +r, +t, _}:
      Rect{U32.add(rx, l), U32.add(ry, t), U32.sub(U32.sub(rw, l), r), mh(ch, h)}
    case C.TopW{a, +i}:
      Rect{ax(a, rx, rw, i), ry, pw(rw, i), h}
    case C.TopSize{a, +i, +ch}:
      Rect{ax(a, rx, rw, i), ry, pw(rw, i), mh(ch, h)}
    case C.Bottom{}:
      Rect{rx, ny(ry, rh, h), rw, h}
    case C.BottomH{+ch}:
      Rect{rx, U32.add(ry, U32.sub(rh, mh(ch, h))), rw, mh(ch, h)}
    case C.BottomHM{+ch, +l, +r, _, +b}:
      Rect{U32.add(rx, l), U32.sub(U32.sub(U32.add(ry, rh), b), mh(ch, h)), U32.sub(U32.sub(rw, l), r), mh(ch, h)}
    case C.BottomW{a, +i}:
      Rect{ax(a, rx, rw, i), ny(ry, rh, h), pw(rw, i), h}
    case C.BottomP{+l, +r}:
      Rect{U32.add(rx, l), ny(ry, rh, h), U32.sub(U32.sub(rw, l), r), h}
    case C.BottomSize{a, +i, +ch}:
      Rect{ax(a, rx, rw, i), U32.add(ry, U32.sub(rh, mh(ch, h))), pw(rw, i), mh(ch, h)}
    case C.Static{+cx, +cy, +cw, +ch}:
      Rect{cx, cy, cw, ch}
    # setPosition c p'' [scr] ht: on a list of scr alone, every OnScreen
    # inside is scr again
    case C.OnScreen{_, q}:
      place(q, rx, ry, rw, rh, h)

def place.on(p: C.Pos, s: Rect, +h: U32) -> Rect:
  match s:
    case Rect{+rx, +ry, +rw, +rh}:
      place(p, rx, ry, rw, rh, h)

# setPosition c p rs ht (broadest: pickBroadest c)
def set.position(+broadest: Bool, p: C.Pos, +rs: List<&2, Rect>, +ht: U32) -> Rect:
  match p:
    case C.OnScreen{+i, q}:
      place.on(q, or.pick(safe.index(rs, i), broadest, rs), ht)
    case _:
      place.on(p, picker(broadest, rs), ht)

# the window's height without a position's own: the font's ascent and
# descent of "0", and 4
def height(+ascent: U32, +descent: U32) -> U32:
  U32.add(U32.add(ascent, descent), 4)

# The struts --------------------------------------------------------------------------------

def rect.bottom(r: Rect) -> U32:
  match r:
    case Rect{_, y, _, h}:
      U32.add(y, h)

# getRootWindowHeight: the greatest y + height of the screens
def root.height(rs: List<&2, Rect>) -> U32:
  match rs:
    case Nil{}:
      0
    case Con{r, Nil{}}:
      rect.bottom(r)
    case Con{r, rest}:
      s.max(rect.bottom(r), root.height(rest))

def top.strut(+st: U32, +nx: U32, +nw: U32) -> List<&2, U32>:
  [0, 0, st, 0, 0, 0, 0, 0, nx, nw, 0, 0]

def bottom.strut(+sb: U32, +nx: U32, +nw: U32) -> List<&2, U32>:
  [0, 0, 0, sb, 0, 0, 0, 0, 0, 0, nx, nw]

def static.struts(+cx: U32, +cy: U32, +cw: U32, +ch: U32, +rwh: U32) -> List<&2, U32>:
  +xe = U32.sub(U32.add(cx, cw), 1)
  Bool.pick(List<&2, U32>, s.lt(cy, s.div2(rwh)), top.strut(U32.add(cy, ch), cx, xe), bottom.strut(U32.sub(rwh, cy), cx, xe))

def struts.at(p: C.Pos, +x: U32, +y: U32, +w: U32, +h: U32, +rwh: U32) -> List<&2, U32>:
  match p:
    case C.OnScreen{_, q}:
      struts.at(q, x, y, w, h, rwh)
    case C.TopHM{_, _, _, _, +b}:
      top.strut(U32.add(U32.add(y, h), b), x, U32.sub(U32.add(x, w), 1))
    case C.BottomHM{_, _, _, +t, _}:
      bottom.strut(U32.add(U32.sub(rwh, y), t), x, U32.sub(U32.add(x, w), 1))
    case C.Static{+cx, +cy, +cw, +ch}:
      static.struts(cx, cy, cw, ch, rwh)
    case C.Top{}:
      top.strut(U32.add(y, h), x, U32.sub(U32.add(x, w), 1))
    case C.TopH{_}:
      top.strut(U32.add(y, h), x, U32.sub(U32.add(x, w), 1))
    case C.TopP{_, _}:
      top.strut(U32.add(y, h), x, U32.sub(U32.add(x, w), 1))
    case C.TopW{_, _}:
      top.strut(U32.add(y, h), x, U32.sub(U32.add(x, w), 1))
    case C.TopSize{_, _, _}:
      top.strut(U32.add(y, h), x, U32.sub(U32.add(x, w), 1))
    case _:
      bottom.strut(U32.sub(rwh, y), x, U32.sub(U32.add(x, w), 1))

# getStrutValues r p (getRootWindowHeight rs): _NET_WM_STRUT_PARTIAL's 12
# values (_NET_WM_STRUT's are its first 4)
def struts(r: Rect, p: C.Pos, rs: List<&2, Rect>) -> List<&2, U32>:
  match r:
    case Rect{+x, +y, +w, +h}:
      struts.at(p, x, y, w, h, root.height(rs))
