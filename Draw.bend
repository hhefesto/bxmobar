# The bar drawn, what is decided about it (Xmobar.Draw.Cairo's
# drawSegments, Xmobar.Draw.Boxes, Xmobar.Text.Pango's fixXft): each
# segment's Pango markup, then, from the extents Pango measured for them,
# where each goes, its background, its boxes, the border, and where each
# action is. x11.c does the drawing: Pango lays out the markups (X.measure)
# and draws the ops this module writes (X.draw).
#
# Haskell draws in Doubles that are all whole multiples of 1/2048 pixel
# (Pango's units are 1/1024; a few values are halved), so here every
# coordinate is a whole number of q = 1/2048 pixel, a U32 in two's
# complement: exact, as the Doubles are, while a bar is under 2^20 pixels.
#
# Icons are not drawn yet: an icon has no bitmap in the cache, so it is as
# xmobar draws an icon whose file it did not load (width 0, nothing drawn).

import Base
import ./Hs.bend as H
import ./Big.bend as B
import ./Config.bend as C
import ./Template.bend as T
import ./Colour.bend as K

# q arithmetic -----------------------------------------------------------------------

def s.lt(+a: U32, +b: U32) -> Bool:
  U32.is_lt(U32.xor(a, 2147483648), U32.xor(b, 2147483648))

def s.min(+a: U32, +b: U32) -> U32:
  Bool.pick(U32, s.lt(b, a), b, a)

def s.max(+a: U32, +b: U32) -> U32:
  Bool.pick(U32, s.lt(a, b), b, a)

def s.ge0(+a: U32) -> Bool:
  U32.is_lt(a, 2147483648)

# halving an even q (an arithmetic shift)
def half(+a: U32) -> U32:
  U32.or(U32.shr(a), U32.and(a, 2147483648))

# a whole number of pixels (an Int's low 32 bits) in q
def px(+n: U32) -> U32:
  U32.mul(n, 2048)

def show.s(+a: U32) -> String:
  Bool.pick(String, s.ge0(a), U32.show(a), "-" ++ U32.show(U32.sub(0, a)))

# Markup -------------------------------------------------------------------------------

def hex.digit(+d: U32) -> String:
  SCon{Char.from_u32(Bool.pick(U32, U32.is_lt(d, 10), U32.add(d, 48), U32.add(d, 87))), SNil{}}

def hex.go(fuel: Nat, +u: U32, +acc: String) -> String:
  match fuel:
    case 0n:
      acc
    case 1n+f:
      H.Bool.if(String, U32.is_lt(u, 16), _ => hex.digit(u) ++ acc, _ => hex.go(f, U32.shrn(u, 4n), hex.digit(U32.and(u, 15)) ++ acc))

# printf "%x"
def hex(+u: U32) -> String:
  hex.go(8n, u, "")

# g_markup_escape_text on one character: & < > ' " by entity, the C0 and
# C1 controls but \t \n \r and U+0085 as &#xN;
def esc.char(c: Char) -> String:
  +u = Char.to_u32(c)
  H.Bool.if(String, U32.is_eq(u, 38), _ => "&amp;", _ => H.Bool.if(String, U32.is_eq(u, 60), _ => "&lt;", _ => H.Bool.if(String, U32.is_eq(u, 62), _ => "&gt;", _ => H.Bool.if(String, U32.is_eq(u, 39), _ => "&#39;", _ => H.Bool.if(String, U32.is_eq(u, 34), _ => "&quot;", _ => H.Bool.if(String, Bool.or(Bool.or(H.in(u, 1, 8), H.in(u, 11, 12)), Bool.or(Bool.or(H.in(u, 14, 31), H.in(u, 127, 132)), H.in(u, 134, 159))), _ => "&#x" ++ hex(u) ++ ";", _ => SCon{Char.from_u32(u), SNil{}}))))))

# escapeMarkup
def escape(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{c, rest}:
      esc.char(c) ++ escape(rest)

def dash.space(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{+c, rest}:
      SCon{Bool.pick(Char, H.c.is(c, 45), Char.from_u32(32), c), dash.space(rest)}

# fixXft: "xft:" dropped and its dashes spaces
def fix.xft(+f: String) -> String:
  Bool.pick(String, String.starts_with(f, "xft:"), dash.space(String.drop(f, 4n)), f)

# the fg and bg of "fg,bg", bg the config's when there is no comma
type FB is Data:
  FB{fg: String, bg: String}

def comma.next(m: Maybe<&2, Nat>) -> Maybe<&2, Nat>:
  match m:
    case Some{n}:
      Some{1n+n}
    case None{}:
      None{}

def comma.at(s: String) -> Maybe<&2, Nat>:
  match s:
    case SNil{}:
      None{}
    case SCon{c, rest}:
      H.Bool.if(Maybe<&2, Nat>, H.c.is(c, 44), _ => Some{0n}, _ => comma.next(comma.at(rest)))

def fb.at(m: Maybe<&2, Nat>, +c: String, +bg: String) -> FB:
  match m:
    case Some{+n}:
      FB{String.take(c, n), String.drop(c, 1n+n)}
    case None{}:
      FB{c, bg}

# colorComponents conf c
def components(+bg: String, +c: String) -> FB:
  fb.at(comma.at(c), c, bg)

# an Int index (a Big below 2^64, two's complement) as a place in a list
# of n: Some i (0-based) when 1 <= idx <= n
def index.at(m: Maybe<&2, U32>, +n: Nat) -> Maybe<&2, Nat>:
  match m:
    case Some{+i}:
      H.Bool.if(Maybe<&2, Nat>, Bool.and(U32.is_ge(i, 1), Nat.is_le(U32.to_nat(i), n)), _ => Some{U32.to_nat(U32.sub(i, 1))}, _ => None{})
    case None{}:
      None{}

def index.of(+idx: List<&2, U32>, +n: Nat) -> Maybe<&2, Nat>:
  index.at(B.to.u32(idx), n)

def nth.str(n: Nat, xs: List<&2, String>, +d: String) -> String:
  match n xs:
    case 0n Con{x, _}:
      x
    case 1n+m Con{_, rest}:
      nth.str(m, rest, d)
    case _ _:
      d

def nth.u32(n: Nat, xs: List<&2, U32>, +d: U32) -> U32:
  match n xs:
    case 0n Con{x, _}:
      x
    case 1n+m Con{_, rest}:
      nth.u32(m, rest, d)
    case _ _:
      d

def font.at(m: Maybe<&2, Nat>, +font: String, fonts: List<&2, String>) -> String:
  match m:
    case Some{i}:
      nth.str(i, fonts, font)
    case None{}:
      font

def offset.at(m: Maybe<&2, Nat>, +off: U32, offs: List<&2, U32>) -> U32:
  match m:
    case Some{i}:
      nth.u32(i, offs, off)
    case None{}:
      off

# indexedFont conf idx
def indexed.font(+font: String, +fonts: List<&2, String>, +idx: List<&2, U32>) -> String:
  font.at(index.of(idx, List.length(&2, String, fonts)), font, fonts)

# indexedOffset conf idx
def indexed.offset(+off: U32, +offs: List<&2, U32>, +idx: List<&2, U32>) -> U32:
  offset.at(index.of(idx, List.length(&2, U32, offs)), off, offs)

# what drawing needs of the config
type Paint is Data:
  Paint{font: String, fonts: List<&2, String>, bg: String, off: U32, offs: List<&2, U32>, icon: U32, border: C.Border, bcolor: String, bwidth: U32}

def look(c: C.Config) -> Paint:
  match c:
    case C.Config{font, fonts, _, _, bg, _, _, _, _, off, offs, icon, border, bcolor, bwidth, _, _, _, _, _, _, _, _, _, _, _, _, _, _}:
      Paint{font, fonts, bg, off, offs, icon, border, bcolor, bwidth}

def info.colors(i: T.Info) -> String:
  match i:
    case T.Info{c, _, _, _}:
      c

# segmentMarkup: markSpan [FontBackground bg (unless the config's), FontDescr
# font, FontForeground fg] (escapeMarkup text); markSpan shows each value
def markup.of(+text: String, fb: FB, +cbg: String, +font: String) -> String:
  match fb:
    case FB{+fg, +bg}:
      "<span" ++ Bool.pick(String, String.eq(bg, cbg), "", " background=" ++ H.show.str(bg)) ++ " font_desc=" ++ H.show.str(font) ++ " foreground=" ++ H.show.str(fg) ++ ">" ++ escape(text) ++ "</span>"

def markup(+l: Paint, s: T.Seg) -> Maybe<&2, String>:
  match l s:
    case Paint{+font, +fonts, +bg, _, _, _, _, _, _} T.Seg{T.WText{+t}, +info, +idx, _}:
      Some{markup.of(t, components(bg, info.colors(info)), bg, fix.xft(indexed.font(font, fonts, idx)))}
    case _ _:
      None{}

def cons.some(m: Maybe<&2, String>, more: List<&2, String>) -> List<&2, String>:
  match m:
    case Some{x}:
      x <> more
    case None{}:
      more

def markups.go(+l: Paint, ss: List<&2, T.Seg>) -> List<&2, String>:
  match ss:
    case Nil{}:
      Nil{}
    case Con{s, rest}:
      cons.some(markup(l, s), markups.go(l, rest))

# the markups of the text segments, left's, center's, then right's, in
# order: what X.measure lays out
def markups(+l: Paint, secs: List<&2, List<&2, T.Seg>>) -> List<&2, String>:
  match secs:
    case Nil{}:
      Nil{}
    case Con{ss, rest}:
      List.append(&2, String, markups.go(l, ss), markups(l, rest))

# Layout (drawSegments) ------------------------------------------------------------------

# Pango's logical extents of a layout (layoutGetExtents' second): x, y,
# width, height, in Pango units (1/1024 pixel), two's complement
type Ext is Data:
  Ext{o: U32, u: U32, w: U32, h: U32}

# an icon in the cache (updateCache's): its path as the markup names it,
# x11.c's place for its bitmap, its width and height
type Icon is Data:
  Icon{path: String, id: U32, w: U32, h: U32}

def icon.find(+p: String, xs: List<&2, Icon>) -> Maybe<&2, Icon>:
  match xs:
    case Nil{}:
      None{}
    case Con{Icon{+q, +i, +w, +h}, rest}:
      H.Bool.if(Maybe<&2, Icon>, String.eq(q, p), _ => Some{Icon{q, i, w, h}}, _ => icon.find(p, rest))

def icon.paths.go(ss: List<&2, T.Seg>, acc: List<&2, String>) -> List<&2, String>:
  match ss:
    case Nil{}:
      acc
    case Con{T.Seg{T.WIcon{p}, _, _, _}, rest}:
      icon.paths.go(rest, p <> acc)
    case Con{_, rest}:
      icon.paths.go(rest, acc)

# the icons' paths, in the segments' order (updateIconCache's paths)
def icon.paths(secs: List<&2, List<&2, T.Seg>>, acc: List<&2, String>) -> List<&2, String>:
  match secs:
    case Nil{}:
      List.reverse(&2, String, acc)
    case Con{ss, rest}:
      icon.paths(rest, icon.paths.go(ss, acc))

# round :: Double -> Position of a q: to the nearest pixel, a half to the
# even one
def round.q(+q: U32) -> U32:
  +k = U32.or(U32.shrn(q, 11n), Bool.pick(U32, s.ge0(q), 0, 4292870144))
  +r = U32.and(q, 2047)
  H.Bool.if(U32, U32.is_lt(r, 1024), _ => k, _ => H.Bool.if(U32, U32.is_gt(r, 1024), _ => U32.add(k, 1), _ => Bool.pick(U32, U32.is_zero(U32.and(k, 1)), k, U32.add(k, 1))))

# a string as the ops carry it: its characters' codes, commas between
# ("-" when empty)
def codes.go(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{c, SNil{}}:
      U32.show(Char.to_u32(c))
    case SCon{c, rest}:
      U32.show(Char.to_u32(c)) ++ "," ++ codes.go(rest)

def codes(+s: String) -> String:
  Bool.pick(String, String.is_empty(s), "-", codes.go(s))

# Renderinfo's renderer: a text's layout (its place in X.measure's list),
# its logical width and its vertical offset, in q; an icon's bitmap, its
# width (q), the pixel row of its middle, its colours; an Hspace's, or an
# icon's not loaded (nothing)
type RK is Data:
  RText{layout: U32, w: U32, voff: U32}
  RIcon{id: U32, w: U32, y: U32, fg: String, bg: String}
  RNone{}

type RI is Data:
  RI{k: RK, width: U32, info: T.Info, acts: Maybe<&2, List<&2, T.Action>>}

# the segments' Renderinfos, the next layout's place, the extents left
type RIs is Data:
  RIs{ris: List<&2, RI>, n: U32, exts: List<&2, Ext>}

def ris.cons(+r: RI, rs: RIs) -> RIs:
  match rs:
    case RIs{xs, n, es}:
      RIs{r <> xs, n, es}

def paint.off(p: Paint) -> U32:
  match p:
    case Paint{_, _, _, off, _, _, _, _, _}:
      off

def paint.offs(p: Paint) -> List<&2, U32>:
  match p:
    case Paint{_, _, _, _, offs, _, _, _, _}:
      offs

def paint.bg(p: Paint) -> String:
  match p:
    case Paint{_, _, bg, _, _, _, _, _, _}:
      bg

def paint.icon(p: Paint) -> U32:
  match p:
    case Paint{_, _, _, _, _, icon, _, _, _}:
      icon

def fb.fg(f: FB) -> String:
  match f:
    case FB{fg, _}:
      fg

def fb.bg.of(f: FB) -> String:
  match f:
    case FB{_, bg}:
      bg

# an icon's Renderinfo: its bitmap's width, drawn at the bar's middle plus
# iconOffset in its colours; one not in the cache is nothing, 0 wide
def icon.ri(m: Maybe<&2, Icon>, +info: T.Info, acts: Maybe<&2, List<&2, T.Action>>, +cbg: String, +ioff: U32, +ht: U32) -> RI:
  match m:
    case Some{Icon{_, +i, +w, _}}:
      +fb = components(cbg, info.colors(info))
      RI{RIcon{i, px(w), round.q(U32.add(U32.mul(ht, 1024), px(ioff))), fb.fg(fb), fb.bg.of(fb)}, px(w), info, acts}
    case None{}:
      RI{RNone{}, 0, info, acts}

# withRenderinfo: a text's width is its logical width less its x; it is
# drawn at the font's offset plus (height - h + y) / 2
def infos(ss: List<&2, T.Seg>, es: List<&2, Ext>, +p: Paint, +icons: List<&2, Icon>, +ht: U32, +n: U32) -> RIs:
  match ss es:
    case Nil{} es:
      RIs{Nil{}, n, es}
    case Con{T.Seg{T.WText{_}, info, idx, acts}, rest} Con{Ext{+o, +u, +w, +h}, er}:
      +voff = U32.add(U32.sub(U32.add(px(indexed.offset(paint.off(p), paint.offs(p), idx)), U32.mul(ht, 1024)), h), u)
      ris.cons(RI{RText{n, U32.mul(w, 2), voff}, U32.mul(U32.sub(w, o), 2), info, acts}, infos(rest, er, p, icons, ht, U32.inc(n)))
    case Con{T.Seg{T.WHspace{+k}, info, _, acts}, rest} es:
      ris.cons(RI{RNone{}, px(k), info, acts}, infos(rest, es, p, icons, ht, n))
    case Con{T.Seg{T.WIcon{+path}, info, _, acts}, rest} es:
      ris.cons(icon.ri(icon.find(path, icons), info, acts, paint.bg(p), paint.icon(p), ht), infos(rest, es, p, icons, ht, n))
    case Con{T.Seg{_, info, _, acts}, rest} es:
      ris.cons(RI{RNone{}, 0, info, acts}, infos(rest, es, p, icons, ht, n))

# a segment's actions and where it is
type ActPos is Data:
  ActPos{acts: List<&2, T.Action>, from: U32, to: U32}

# a segment's boxes and where it is
type BB is Data:
  BB{from: U32, to: U32, boxes: List<&2, T.Box>}

# drawSegment's accumulator, and the ops written so far
type Acc is Data:
  Acc{off: U32, acts: List<&2, ActPos>, boxes: List<&2, BB>, ops: String}

def alpha.code(a: K.Alpha) -> String:
  match a:
    case K.Opaque{}:
      "0"
    case K.Clear{}:
      "1"
    case K.AlphaBad{}:
      "2"

# a colour as the ops write it: r g b alpha (0 opaque, 1 alpha 0, 2 read's
# error)
def colour.op(c: K.Colour) -> String:
  match c:
    case K.Colour{K.RGB{r, g, b}, a}:
      U32.show(r) ++ " " ++ U32.show(g) ++ " " ++ U32.show(b) ++ " " ++ alpha.code(a)

def nums(xs: List<&2, U32>) -> String:
  match xs:
    case Nil{}:
      ""
    case Con{+x, rest}:
      " " ++ show.s(x) ++ nums(rest)

def fb.bg(f: FB) -> String:
  match f:
    case FB{_, bg}:
      bg

def info.boxes(i: T.Info) -> List<&2, T.Box>:
  match i:
    case T.Info{_, _, _, bs}:
      bs

# drawSegmentBackground: R colour x y w h, filled
def seg.bg(+cbg: String, +ht: U32, i: T.Info, +x0: U32, +x1: U32) -> String:
  match i:
    case T.Info{+c, +top, +bot, _}:
      +bg = fb.bg(components(cbg, c))
      H.Bool.if(String, Bool.and(Bool.not(String.eq(bg, cbg)), Bool.or(s.ge0(top), s.ge0(bot))), _ => "R " ++ colour.op(K.read(bg)) ++ nums([x0, px(top), U32.sub(x1, x0), U32.sub(U32.sub(px(ht), px(bot)), px(top))]) ++ "\n", _ => "")

# the text's layout drawn at off: T layout x y width, the width the
# ellipsis cuts it at (puToInt (mx - off)), -1 when it fits
def seg.render(k: RK, +off: U32, +mx: U32) -> String:
  match k:
    case RText{+i, +w, +voff}:
      "T " ++ U32.show(i) ++ nums([off, voff]) ++ " " ++ Bool.pick(String, s.lt(mx, U32.add(off, w)), U32.show(U32.shr(U32.sub(mx, off))), "-1") ++ "\n"
    case RIcon{+i, +w, +y, +fg, +bg}:
      H.Bool.if(String, s.lt(mx, U32.add(off, w)), _ => "", _ => "I " ++ U32.show(i) ++ nums([round.q(off), y]) ++ " " ++ codes(fg) ++ " " ++ codes(bg) ++ "\n")
    case RNone{}:
      ""

def act.cons(m: Maybe<&2, List<&2, T.Action>>, +from: U32, +to: U32, as: List<&2, ActPos>) -> List<&2, ActPos>:
  match m:
    case Some{xs}:
      ActPos{xs, from, to} <> as
    case None{}:
      as

def box.cons(bs: List<&2, T.Box>, +from: U32, +to: U32, bbs: List<&2, BB>) -> List<&2, BB>:
  match bs:
    case Nil{}:
      bbs
    case Con{b, rest}:
      BB{from, to, Con{b, rest}} <> bbs

# drawSegment dctx surface maxoff
def draw.seg(r: RI, acc: Acc, +cbg: String, +ht: U32, +mx: U32) -> Acc:
  match r acc:
    case RI{k, +lw, +info, acts} Acc{+off, as, bs, ops}:
      +end = s.min(mx, U32.add(off, lw))
      Acc{U32.add(off, lw), act.cons(acts, off, end, as), box.cons(info.boxes(info), off, end, bs), ops ++ H.Bool.if(String, s.lt(off, end), _ => seg.bg(cbg, ht, info, off, end) ++ seg.render(k, off, mx), _ => "")}

def draw.segs(rs: List<&2, RI>, acc: Acc, +cbg: String, +ht: U32, +mx: U32) -> Acc:
  match rs:
    case Nil{}:
      acc
    case Con{r, rest}:
      draw.segs(rest, draw.seg(r, acc, cbg, ht, mx), cbg, ht, mx)

def widths(rs: List<&2, RI>) -> U32:
  match rs:
    case Nil{}:
      0
    case Con{RI{_, w, _, _}, rest}:
      U32.add(w, widths(rest))

# Boxes ----------------------------------------------------------------------------------

def border.code(b: T.BoxBorder) -> U32:
  match b:
    case T.BBTop{}:
      0
    case T.BBBottom{}:
      1
    case T.BBVBoth{}:
      2
    case T.BBLeft{}:
      3
    case T.BBRight{}:
      4
    case T.BBHBoth{}:
      5
    case T.BBFull{}:
      6

def align.code(a: T.Align) -> U32:
  match a:
    case T.AL{}:
      0
    case T.AR{}:
      1
    case T.AC{}:
      2

def box.eq(a: T.Box, b: T.Box) -> Bool:
  match a b:
    case T.Box{b1, a1, +o1, +w1, +c1, +t1, +r1, +bo1, +l1} T.Box{b2, a2, +o2, +w2, +c2, +t2, +r2, +bo2, +l2}:
      Bool.and(Bool.and(Bool.and(U32.is_eq(border.code(b1), border.code(b2)), U32.is_eq(align.code(a1), align.code(a2))), Bool.and(U32.is_eq(o1, o2), U32.is_eq(w1, w2))), Bool.and(Bool.and(String.eq(c1, c2), U32.is_eq(t1, t2)), Bool.and(U32.is_eq(r1, r2), Bool.and(U32.is_eq(bo1, bo2), U32.is_eq(l1, l2)))))

def boxes.eq(as: List<&2, T.Box>, bs: List<&2, T.Box>) -> Bool:
  match as bs:
    case Nil{} Nil{}:
      True{}
    case Con{a, ar} Con{b, br}:
      Bool.and(box.eq(a, b), boxes.eq(ar, br))
    case _ _:
      False{}

# a line: x0 y0 x1 y1
def line(+a: U32, +b: U32, +c: U32, +d: U32) -> List<&2, U32>:
  [a, b, c, d]

def box.pick(bd: T.BoxBorder, +rtop: List<&2, U32>, +rbot: List<&2, U32>, +rleft: List<&2, U32>, +rright: List<&2, U32>) -> List<&2, List<&2, U32>>:
  match bd:
    case T.BBTop{}:
      [rtop]
    case T.BBBottom{}:
      [rbot]
    case T.BBVBoth{}:
      [rtop, rbot]
    case T.BBLeft{}:
      [rleft]
    case T.BBRight{}:
      [rright]
    case T.BBHBoth{}:
      [rleft, rright]
    case T.BBFull{}:
      [rtop, rbot, rleft, rright]

def box.lines(bd: T.BoxBorder, +xmin: U32, +xmax: U32, +ymin: U32, +ymax: U32, +p0: U32, +p1: U32) -> List<&2, List<&2, U32>>:
  +rtop = line(U32.add(xmin, p0), ymin, U32.add(xmax, p1), ymin)
  +rbot = line(U32.add(xmin, p0), ymax, U32.add(xmax, p1), ymax)
  +rleft = line(xmin, U32.add(ymin, p0), xmin, U32.add(ymax, p1))
  +rright = line(xmax, U32.add(ymin, p0), xmax, U32.add(ymax, p1))
  box.pick(bd, rtop, rbot, rleft, rright)

def lines.nums(ls: List<&2, List<&2, U32>>) -> String:
  match ls:
    case Nil{}:
      ""
    case Con{l, rest}:
      nums(l) ++ lines.nums(rest)

# p0, p1 of a BoxOffset: L (0, -m), C (m, -m), R (m, 0)
def p0(a: T.Align, +ma: U32) -> U32:
  match a:
    case T.AL{}:
      0
    case _:
      ma

def p1(a: T.Align, +ma: U32) -> U32:
  match a:
    case T.AR{}:
      0
    case _:
      U32.sub(0, ma)

# drawBox: renderLines color (fi w) (boxLines box ht x0 x1): L colour width
# n, then n lines
def box.op(b: T.Box, +ht: U32, +x0: U32, +x1: U32) -> String:
  match b:
    case T.Box{bd, +al, +m, +lw, +color, +mt, +mr, +mb, +ml}:
      +lc = U32.mul(lw, 1024)
      +ls = box.lines(bd, U32.sub(U32.sub(x0, px(ml)), lc), U32.add(U32.add(x1, px(mr)), lc), U32.add(px(mt), lc), U32.sub(U32.sub(px(ht), px(mb)), lc), p0(al, px(m)), p1(al, px(m)))
      "L " ++ colour.op(K.read(color)) ++ " " ++ show.s(px(lw)) ++ " " ++ Nat.show(List.length(&2, List<&2, U32>, ls)) ++ lines.nums(ls) ++ "\n"

def bbox(bs: List<&2, T.Box>, +ht: U32, +x0: U32, +x1: U32) -> String:
  match bs:
    case Nil{}:
      ""
    case Con{b, rest}:
      box.op(b, ht, x0, x1) ++ bbox(rest, ht, x0, x1)

def bb.op(bb: BB, +ht: U32) -> String:
  match bb:
    case BB{+f, +t, bs}:
      bbox(bs, ht, f, t)

# drawBoxes: a box that goes on where the one before ends (the same
# boxes, not after it) is drawn as one with it
def boxes.go(bbs: List<&2, BB>, cur: BB, +ht: U32) -> String:
  match bbs cur:
    case Nil{} cur:
      bb.op(cur, ht)
    case Con{BB{+f2, +t2, +b2}, +rest} BB{+f, +t, +b}:
      H.Bool.if(String, Bool.or(s.lt(t, f2), Bool.not(boxes.eq(b, b2))), _ => bb.op(BB{f, t, b}, ht) ++ boxes.go(rest, BB{f2, t2, b2}, ht), _ => boxes.go(rest, BB{f, t2, b2}, ht))

def draw.boxes(bbs: List<&2, BB>, +ht: U32) -> String:
  match bbs:
    case Nil{}:
      ""
    case Con{b, rest}:
      boxes.go(rest, b, ht)

# The border ------------------------------------------------------------------------------

def border.rect.at(b: C.Border, +w: U32, +h: U32, +w1: U32) -> List<&2, U32>:
  match b:
    case C.TopB{}:
      [0, 0, w1, 0]
    case C.BottomB{}:
      [0, U32.sub(h, 2048), w1, 0]
    case C.FullB{}:
      [0, 0, w1, U32.sub(h, 2048)]
    case C.TopBM{+m}:
      [0, px(m), w1, 0]
    case C.BottomBM{+m}:
      [0, U32.sub(h, px(m)), w1, 0]
    case C.FullBM{+m}:
      [px(m), px(m), U32.sub(w, U32.mul(px(m), 2)), U32.sub(h, U32.mul(px(m), 2))]
    case C.NoBorder{}:
      []

# borderRect bdr w h (in q)
def border.rect(b: C.Border, +w: U32, +h: U32) -> List<&2, U32>:
  +w1 = U32.sub(w, 2048)
  border.rect.at(b, w, h, w1)

# drawBorder, when borderWidth > 0: S colour width x y w h, stroked
def border.op(+p: Paint, +w: U32, +h: U32) -> String:
  match p:
    case Paint{_, _, _, _, _, _, border, +bc, +bw}:
      +r = border.rect(border, px(w), px(h))
      H.Bool.if(String, Bool.and(s.lt(0, bw), Bool.not(List.is_empty(&2, U32, r))), _ => "S " ++ colour.op(K.read(bc)) ++ " " ++ show.s(px(bw)) ++ nums(r) ++ "\n", _ => "")

# The frame ------------------------------------------------------------------------------

# what X.draw draws (its ops, one a line), and where the actions are
type Frame is Data:
  Frame{ops: String, acts: List<&2, ActPos>}

def acc.fin(a: Acc, +p: Paint, +w: U32, +h: U32) -> Frame:
  match a:
    case Acc{_, as, bs, ops}:
      Frame{ops ++ draw.boxes(List.reverse(&2, BB, bs), h) ++ border.op(p, w, h), as}

def acc.off(a: Acc) -> U32:
  match a:
    case Acc{off, _, _, _}:
      off

# the center, drawn up to the right's start, when it has a width
def center(+cw: U32, +cs: List<&2, RI>, l: Acc, +cstart: U32, +rstart: U32, +cbg: String, +ht: U32) -> Acc:
  match l:
    case Acc{_, +as, +bs, +ops}:
      H.Bool.if(Acc, s.lt(0, cw), _ => draw.segs(cs, Acc{cstart, as, bs, ops}, cbg, ht, rstart), _ => Acc{0, as, bs, ops})

def right(rs: List<&2, RI>, c: Acc, +rstart: U32, +cbg: String, +ht: U32, +mx: U32) -> Acc:
  match c:
    case Acc{_, as, bs, ops}:
      draw.segs(rs, Acc{rstart, as, bs, ops}, cbg, ht, mx)

def place.all(+l: Acc, +cs: List<&2, RI>, +rs: List<&2, RI>, +p: Paint, +w: U32, +h: U32) -> Frame:
  +lend = acc.off(l)
  +dw = px(w)
  +rw = widths(rs)
  +cw = widths(cs)
  +rstart = s.max(lend, U32.sub(dw, rw))
  +cstart = Bool.pick(U32, Bool.or(s.lt(2048, lend), U32.is_eq(rw, 0)), s.max(lend, half(U32.sub(dw, cw))), lend)
  acc.fin(right(rs, center(cw, cs, l, cstart, rstart, paint.bg(p), h), rstart, paint.bg(p), h, dw), p, w, h)

def ris.of(r: RIs) -> List<&2, RI>:
  match r:
    case RIs{xs, _, _}:
      xs

def ris.n(r: RIs) -> U32:
  match r:
    case RIs{_, n, _}:
      n

def ris.exts(r: RIs) -> List<&2, Ext>:
  match r:
    case RIs{_, _, es}:
      es

def sec(n: Nat, secs: List<&2, List<&2, T.Seg>>) -> List<&2, T.Seg>:
  match n secs:
    case 0n Con{s, _}:
      s
    case 1n+m Con{_, rest}:
      sec(m, rest)
    case _ _:
      Nil{}

def frame.r(+p: Paint, +icons: List<&2, Icon>, +w: U32, +h: U32, l: RIs, +c: RIs, +rsegs: List<&2, T.Seg>) -> Frame:
  +r = infos(rsegs, ris.exts(c), p, icons, h, ris.n(c))
  place.all(draw.segs(ris.of(l), Acc{0, Nil{}, Nil{}, ""}, paint.bg(p), h, px(w)), ris.of(c), ris.of(r), p, w, h)

def frame.c(+p: Paint, +icons: List<&2, Icon>, +w: U32, +h: U32, +l: RIs, +csegs: List<&2, T.Seg>, +rsegs: List<&2, T.Seg>) -> Frame:
  frame.r(p, icons, w, h, l, infos(csegs, ris.exts(l), p, icons, h, ris.n(l)), rsegs)

# drawSegments: the bar of width w and height h (pixels), its left, center
# and right segments, the extents of their texts' markups (markups' order),
# the icons in the cache
def frame(+p: Paint, +w: U32, +h: U32, +secs: List<&2, List<&2, T.Seg>>, exts: List<&2, Ext>, +icons: List<&2, Icon>) -> Frame:
  frame.c(p, icons, w, h, infos(sec(0n, secs), exts, p, icons, h, 0), sec(1n, secs), sec(2n, secs))

# Actions --------------------------------------------------------------------------------

def has.button(+b: U32, bs: List<&2, U32>) -> Bool:
  match bs:
    case Nil{}:
      False{}
    case Con{+x, rest}:
      Bool.or(U32.is_eq(x, b), has.button(b, rest))

def spawns(+b: U32, xs: List<&2, T.Action>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case Con{T.Spawn{bs, +cmd}, rest}:
      +more = spawns(b, rest)
      Bool.pick(List<&2, String>, has.button(b, bs), cmd <> more, more)

# runActions actions button x: the commands of the actions where the click
# is (from <= x <= to) for that button, in order
def clicked(as: List<&2, ActPos>, +b: U32, +x: U32) -> List<&2, String>:
  match as:
    case Nil{}:
      Nil{}
    case Con{ActPos{xs, +from, +to}, rest}:
      +more = clicked(rest, b, x)
      H.Bool.if(List<&2, String>, Bool.and(Bool.not(s.lt(px(x), from)), Bool.not(s.lt(to, px(x)))), _ => List.append(&2, String, spawns(b, xs), more), _ => more)
