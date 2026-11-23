# xmobar's markup and templates in Bend: Xmobar.Config.Template's
# parseString (the text of a bar cut into segments, each with its colors,
# boxes, font and actions), Xmobar.Run.Template's splitTemplate (left,
# center, right) and parseTemplate (the %command% of a template), and
# Xmobar.Text.Output's plain text.
#
# parseString is a parsec parser in xmobar; here it is the same choices,
# made by hand. At each place: the closing tag of the element it is in
# ends the element (manyTill tries it first); else text, as long as no <
# starts one of the tags' names (icon= hspace= raw= action= /action> fn=
# /fn> box /box> fc= /fc>); else the tag whose name starts there (<icon=,
# <hspace=, <raw=, <action=, <fn=, <box, <fc=). Every tag parser is under
# parsec's try, and text never fails once it has read a character, so a
# tag that does not parse, a closing tag out of place or an element left
# open fails the whole string: parseString then answers its one segment,
# "Could not parse string: " and the string.
#
# Bend matches only parameters, so the parser, items, is one def with a
# mode: Look classifies the place (classify), Go acts on the class, and
# the M modes act on what a tag's own (pure) parser answered. Each element
# is items again, with the element's closing tag as its end, and a
# continuation goes on after it; a fuel (three steps a character) bounds
# every chain of calls.

import Base
import ./Hs.bend as H
import ./Big.bend as B

# The segments (Xmobar.Config.Types) -----------------------------------------------

# a button is a Word32
type Action is Data:
  Spawn{buttons: List<&2, U32>, cmd: String}

# Hspace's width is an Int32 (a U32 in two's complement)
type Widget is Data:
  WIcon{path: String}
  WText{text: String}
  WHspace{n: U32}

type BoxBorder is Data:
  BBTop{}
  BBBottom{}
  BBVBoth{}
  BBLeft{}
  BBRight{}
  BBHBoth{}
  BBFull{}

type Align is Data:
  AL{}
  AR{}
  AC{}

# Box{border, BoxOffset align offset, width (CInt), color, BoxMargins top
# right bottom left}: Int32s as U32s
type Box is Data:
  Box{border: BoxBorder, align: Align, offset: U32, width: U32, color: String, mt: U32, mr: U32, mb: U32, ml: U32}

# TextRenderInfo: the colors ("fg" or "fg,bg"), the background's top and
# bottom offsets (Int32), the boxes (the innermost first)
type Info is Data:
  Info{colors: String, top: U32, bottom: U32, boxes: List<&2, Box>}

# a segment: its widget, how it is drawn, its font index (an Int, 64 bits:
# a Big below 2^64) and its actions (the innermost first)
type Seg is Data:
  Seg{w: Widget, info: Info, font: List<&2, U32>, acts: Maybe<&2, List<&2, Action>>}

type Ctx is Data:
  Ctx{info: Info, font: List<&2, U32>, acts: Maybe<&2, List<&2, Action>>}

def Res() -> Data:
  Maybe<&2, List<&2, Seg>>

def seg(ctx: Ctx, w: Widget) -> Seg:
  match ctx:
    case Ctx{i, f, a}:
      Seg{w, i, f, a}

def minus1() -> U32:
  4294967295

# Characters ------------------------------------------------------------------------

# the classes the tags read: 0 a digit; 1 not >; 2 not `; 3 fc's (alphaNum
# or , : #); 4 box's (alphaNum or = space # ,); 5 isSpace; 6 a button (1-5)
def class.ok(+k: U32, c: Char) -> Bool:
  +u = Char.to_u32(c)
  H.Bool.if(Bool, U32.is_eq(k, 0), _ => H.is.digit(u), _ => H.Bool.if(Bool, U32.is_eq(k, 1), _ => U32.is_ne(u, 62), _ => H.Bool.if(Bool, U32.is_eq(k, 2), _ => U32.is_ne(u, 96), _ => H.Bool.if(Bool, U32.is_eq(k, 3), _ => Bool.or(H.is.alnum(u), Bool.or(U32.is_eq(u, 44), Bool.or(U32.is_eq(u, 58), U32.is_eq(u, 35)))), _ => H.Bool.if(Bool, U32.is_eq(k, 4), _ => Bool.or(H.is.alnum(u), Bool.or(U32.is_eq(u, 61), Bool.or(U32.is_eq(u, 32), Bool.or(U32.is_eq(u, 35), U32.is_eq(u, 44))))), _ => H.Bool.if(Bool, U32.is_eq(k, 5), _ => H.is.space(u), _ => H.in(u, 49, 53)))))))

# how many characters of s, from its start, are in class k
def span(+k: U32, s: String) -> Nat:
  match s:
    case SNil{}:
      0n
    case SCon{c, rest}:
      H.Bool.if(Nat, class.ok(k, c), _ => 1n+span(k, rest), _ => 0n)

def starts(+s: String, p: String) -> Bool:
  String.starts_with(s, p)

def first.is(s: String, +u: U32) -> Bool:
  match s:
    case SCon{c, _}:
      H.c.is(c, u)
    case SNil{}:
      False{}

def tail(s: String) -> String:
  match s:
    case SCon{_, r}:
      r
    case SNil{}:
      ""

# Text ------------------------------------------------------------------------------

# what, after a <, keeps text from taking it
def text.suffix(+r: String) -> Bool:
  Bool.or(starts(r, "icon="), Bool.or(starts(r, "hspace="), Bool.or(starts(r, "raw="), Bool.or(starts(r, "action="), Bool.or(starts(r, "/action>"), Bool.or(starts(r, "fn="), Bool.or(starts(r, "/fn>"), Bool.or(starts(r, "box"), Bool.or(starts(r, "/box>"), Bool.or(starts(r, "fc="), starts(r, "/fc>")))))))))))

# how long the text at the start of s is
def text.len(s: String) -> Nat:
  match s:
    case SNil{}:
      0n
    case SCon{c, +r}:
      H.Bool.if(Nat, Bool.and(H.c.is(c, 60), text.suffix(r)), _ => 0n, _ => 1n+text.len(r))

# The tags' own parsers -------------------------------------------------------------

type SS is Data:
  SS{a: String, rest: String}

type SU is Data:
  SU{n: U32, rest: String}

type ActR is Data:
  ActR{a: Action, rest: String}

type FR is Data:
  FR{i: List<&2, U32>, rest: String}

def ss.cons(c: Char, m: Maybe<&2, SS>) -> Maybe<&2, SS>:
  match m:
    case Some{SS{a, r}}:
      Some{SS{SCon{c, a}, r}}
    case None{}:
      None{}

# manyTill (noneOf ">") (try (string "/>")) with k = 1, or with digits (k = 0)
def till.close(+k: U32, +s: String) -> Maybe<&2, SS>:
  match s:
    case SNil{}:
      None{}
    case SCon{+c, r}:
      H.Bool.if(Maybe<&2, SS>, starts(s, "/>"), _ => Some{SS{"", String.drop(s, 2n)}}, _ => H.Bool.if(Maybe<&2, SS>, class.ok(k, c), _ => ss.cons(c, till.close(k, r)), _ => None{}))

# s after "<icon="
def icon.p(+s: String) -> Maybe<&2, SS>:
  till.close(1, s)

def su.of(m: Maybe<&2, SS>) -> Maybe<&2, SU>:
  match m:
    case Some{SS{d, r}}:
      Some{SU{H.digits.val(10, d, 0), r}}
    case None{}:
      None{}

# s after "<hspace=": the width (readMaybe at Int32; none read: 0)
def hspace.p(+s: String) -> Maybe<&2, SU>:
  su.of(till.close(0, s))

# n characters of s, then "/>"
def raw.take(+n: Nat, +s: String) -> Maybe<&2, SS>:
  +r = String.drop(s, n)
  H.Bool.if(Maybe<&2, SS>, starts(r, "/>"), _ => Some{SS{String.take(s, n), String.drop(r, 2n)}}, _ => None{})

def raw.len(m: Maybe<&2, U32>, +s: String) -> Maybe<&2, SS>:
  match m:
    case Some{+n}:
      raw.take(U32.to_nat(n), s)
    case None{}:
      None{}

# the length read (an Integer: no more than what is left, else count fails)
def raw.count(+d: String, +s: String) -> Maybe<&2, SS>:
  +v = B.norm(B.dec.big(d))
  H.Bool.if(Maybe<&2, SS>, B.is.le(v, B.of.u32(U32.from_nat(String.length(s)))), _ => raw.len(B.to.u32(v), s), _ => None{})

# s after "<raw=": digits, a colon, that many characters, "/>"
def raw.p(+s: String) -> Maybe<&2, SS>:
  +n = span(0, s)
  +r = String.drop(s, n)
  H.Bool.if(Maybe<&2, SS>, Bool.and(Nat.is_gt(n, 0n), first.is(r, 58)), _ => raw.count(String.take(s, n), tail(r)), _ => None{})

# the buttons' digits as Word32s
def buttons.of(s: String) -> List<&2, U32>:
  match s:
    case SNil{}:
      Nil{}
    case SCon{c, rest}:
      U32.sub(Char.to_u32(c), 48) <> buttons.of(rest)

def ar.of(+cmd: String, m: Maybe<&2, SS>) -> Maybe<&2, ActR>:
  match m:
    case Some{SS{b, r}}:
      Some{ActR{Spawn{buttons.of(b), cmd}, r}}
    case None{}:
      None{}

# space, spaces, "button=", one or more of 1-5, ">"
def action.buttons.more(+s: String) -> Maybe<&2, SS>:
  +t = String.drop(s, span(5, s))
  +n = span(6, String.drop(t, 7n))
  +r = String.drop(t, Nat.add(7n, n))
  H.Bool.if(Maybe<&2, SS>, Bool.and(starts(t, "button="), Bool.and(Nat.is_gt(n, 0n), first.is(r, 62))), _ => Some{SS{String.take(String.drop(t, 7n), n), tail(r)}}, _ => None{})

# after the command: ">" (button 1), or a space and button=...>
def action.buttons(+s: String) -> Maybe<&2, SS>:
  match s:
    case SCon{c, +r}:
      +u = Char.to_u32(c)
      H.Bool.if(Maybe<&2, SS>, U32.is_eq(u, 62), _ => Some{SS{"1", r}}, _ => H.Bool.if(Maybe<&2, SS>, H.is.space(u), _ => action.buttons.more(r), _ => None{}))
    case SNil{}:
      None{}

# s after "<action=": `command` or a command to the >, then the buttons
def action.p(+s: String) -> Maybe<&2, ActR>:
  match s:
    case SCon{c, +r}:
      +bq = span(2, r)
      +after = String.drop(r, bq)
      +n = span(1, s)
      H.Bool.if(Maybe<&2, ActR>, H.c.is(c, 96), _ => H.Bool.if(Maybe<&2, ActR>, Bool.and(Nat.is_gt(bq, 0n), first.is(after, 96)), _ => ar.of(String.take(r, bq), action.buttons(tail(after))), _ => None{}), _ => H.Bool.if(Maybe<&2, ActR>, Nat.is_gt(n, 0n), _ => ar.of(String.take(s, n), action.buttons(String.drop(s, n))), _ => None{}))
    case SNil{}:
      None{}

# s after "<fn=": digits and ">" (readMaybe at Int: modulo 2^64)
def fn.p(+s: String) -> Maybe<&2, FR>:
  +n = span(0, s)
  +r = String.drop(s, n)
  H.Bool.if(Maybe<&2, FR>, Bool.and(Nat.is_gt(n, 0n), first.is(r, 62)), _ => Some{FR{B.dec64(String.take(s, n)), tail(r)}}, _ => None{})

# s after "<box": its parameters (maybe none) and ">"
def box.p(+s: String) -> Maybe<&2, SS>:
  +n = span(4, s)
  +r = String.drop(s, n)
  H.Bool.if(Maybe<&2, SS>, first.is(r, 62), _ => Some{SS{String.take(s, n), tail(r)}}, _ => None{})

# s after "<fc=": the colors and ">"
def fc.p(+s: String) -> Maybe<&2, SS>:
  +n = span(3, s)
  +r = String.drop(s, n)
  H.Bool.if(Maybe<&2, SS>, Bool.and(Nat.is_gt(n, 0n), first.is(r, 62)), _ => Some{SS{String.take(s, n), tail(r)}}, _ => None{})

# A tag's effect on the context --------------------------------------------------

def or.default(m: Maybe<&2, U32>, +d: U32) -> U32:
  match m:
    case Some{x}:
      x
    case None{}:
      d

# the text before the first c, and the rest after it (None: no c)
type Brk is Data:
  Brk{before: String, after: Maybe<&2, String>}

def brk.cons(c: Char, b: Brk) -> Brk:
  match b:
    case Brk{x, a}:
      Brk{SCon{c, x}, a}

def brk(+u: U32, s: String) -> Brk:
  match s:
    case SNil{}:
      Brk{"", None{}}
    case SCon{+c, +r}:
      H.Bool.if(Brk, H.c.is(c, u), _ => Brk{"", Some{r}}, _ => brk.cons(c, brk(u, r)))

def after.or(m: Maybe<&2, String>, +d: String) -> String:
  match m:
    case Some{x}:
      x
    case None{}:
      d

# ot and ob of "fg:ot,ob" (one number: both)
def fc.offsets(b: Brk) -> Info:
  match b:
    case Brk{+top, m}:
      Info{"", or.default(H.read.num(top), minus1()), or.default(H.read.num(after.or(m, top)), minus1()), Nil{}}

def fc.info.go(fg: String, o: Info, bs: List<&2, Box>) -> Info:
  match o:
    case Info{_, t, b, _}:
      Info{fg, t, b, bs}

def fc.info(b: Brk, bs: List<&2, Box>) -> Info:
  match b:
    case Brk{fg, m}:
      fc.info.go(fg, fc.offsets(brk(44, after.or(m, ""))), bs)

def ctx.fc(ctx: Ctx, c: String) -> Ctx:
  match ctx:
    case Ctx{Info{_, _, _, bs}, f, a}:
      Ctx{fc.info(brk(58, c), bs), f, a}

def ctx.fn(ctx: Ctx, i: List<&2, U32>) -> Ctx:
  match ctx:
    case Ctx{info, _, a}:
      Ctx{info, i, a}

def acts.push(x: Action, m: Maybe<&2, List<&2, Action>>) -> Maybe<&2, List<&2, Action>>:
  match m:
    case None{}:
      Some{[x]}
    case Some{xs}:
      Some{x <> xs}

def ctx.action(ctx: Ctx, x: Action) -> Ctx:
  match ctx:
    case Ctx{info, f, a}:
      Ctx{info, f, acts.push(x, a)}

# Box parameters (boxReader, boxParamReader) --------------------------------------

def border.of(+s: String) -> Maybe<&2, BoxBorder>:
  H.Bool.if(Maybe<&2, BoxBorder>, String.eq(s, "Top"), _ => Some{BBTop{}}, _ => H.Bool.if(Maybe<&2, BoxBorder>, String.eq(s, "Bottom"), _ => Some{BBBottom{}}, _ => H.Bool.if(Maybe<&2, BoxBorder>, String.eq(s, "VBoth"), _ => Some{BBVBoth{}}, _ => H.Bool.if(Maybe<&2, BoxBorder>, String.eq(s, "Left"), _ => Some{BBLeft{}}, _ => H.Bool.if(Maybe<&2, BoxBorder>, String.eq(s, "Right"), _ => Some{BBRight{}}, _ => H.Bool.if(Maybe<&2, BoxBorder>, String.eq(s, "HBoth"), _ => Some{BBHBoth{}}, _ => H.Bool.if(Maybe<&2, BoxBorder>, String.eq(s, "Full"), _ => Some{BBFull{}}, _ => None{})))))))

def border.or(m: Maybe<&2, BoxBorder>, d: BoxBorder) -> BoxBorder:
  match m:
    case Some{x}:
      x
    case None{}:
      d

# readMaybe [a] at Align
def align.of(+u: U32, a: Align) -> Align:
  H.Bool.if(Align, U32.is_eq(u, 76), _ => AL{}, _ => H.Bool.if(Align, U32.is_eq(u, 82), _ => AR{}, _ => H.Bool.if(Align, U32.is_eq(u, 67), _ => AC{}, _ => a)))

def box.offset(+b: Box, v: String) -> Box:
  match b v:
    case Box{bb, al, off, w, c, t, r, bo, l} SCon{a, +o}:
      Box{bb, align.of(Char.to_u32(a), al), or.default(H.read.num(o), off), w, c, t, r, bo, l}
    case _ _:
      b

def box.margin(+b: Box, +pos: String, +v: String) -> Box:
  match b:
    case Box{bb, al, off, w, c, t, r, bo, l}:
      H.Bool.if(Box, String.eq(pos, "t"), _ => Box{bb, al, off, w, c, or.default(H.read.num(v), t), r, bo, l}, _ => H.Bool.if(Box, String.eq(pos, "r"), _ => Box{bb, al, off, w, c, t, or.default(H.read.num(v), r), bo, l}, _ => H.Bool.if(Box, String.eq(pos, "b"), _ => Box{bb, al, off, w, c, t, r, or.default(H.read.num(v), bo), l}, _ => H.Bool.if(Box, String.eq(pos, "l"), _ => Box{bb, al, off, w, c, t, r, bo, or.default(H.read.num(v), l)}, _ => Box{bb, al, off, w, c, t, r, bo, l}))))

def box.type(b: Box, +v: String) -> Box:
  match b:
    case Box{bb, al, off, w, c, t, r, bo, l}:
      Box{border.or(border.of(v), bb), al, off, w, c, t, r, bo, l}

def box.width(b: Box, +v: String) -> Box:
  match b:
    case Box{bb, al, off, w, c, t, r, bo, l}:
      Box{bb, al, off, or.default(H.read.num(v), w), c, t, r, bo, l}

def box.color(b: Box, v: String) -> Box:
  match b:
    case Box{bb, al, off, w, _, t, r, bo, l}:
      Box{bb, al, off, w, v, t, r, bo, l}

def box.param(+b: Box, +p: String, +v: String) -> Box:
  H.Bool.if(Box, String.is_empty(v), _ => b, _ => H.Bool.if(Box, String.eq(p, "type"), _ => box.type(b, v), _ => H.Bool.if(Box, String.eq(p, "offset"), _ => box.offset(b, v), _ => H.Bool.if(Box, String.eq(p, "width"), _ => box.width(b, v), _ => H.Bool.if(Box, String.eq(p, "color"), _ => box.color(b, v), _ => H.Bool.if(Box, first.is(p, 109), _ => box.margin(b, tail(p), v), _ => b))))))

def box.word(+b: Box, br: Brk) -> Box:
  match br:
    case Brk{p, m}:
      box.param(b, p, after.or(m, ""))

def box.read(ws: List<&2, String>, +b: Box) -> Box:
  match ws:
    case Nil{}:
      b
    case w <> rest:
      box.read(rest, box.word(b, brk(61, w)))

def ctx.box(ctx: Ctx, +params: String) -> Ctx:
  match ctx:
    case Ctx{Info{+cs, ot, ob, bs}, f, a}:
      Ctx{Info{cs, ot, ob, box.read(H.words(params), Box{BBFull{}, AC{}, 0, 1, cs, 0, 0, 0, 0}) <> bs}, f, a}

# The parser ------------------------------------------------------------------------

type Cls is Data:
  KEnd{}
  KFail{}
  KText{}
  KIcon{}
  KHspace{}
  KRaw{}
  KAction{}
  KFn{}
  KBox{}
  KFc{}

def classify.tag(+s: String) -> Cls:
  H.Bool.if(Cls, starts(s, "<icon="), _ => KIcon{}, _ => H.Bool.if(Cls, starts(s, "<hspace="), _ => KHspace{}, _ => H.Bool.if(Cls, starts(s, "<raw="), _ => KRaw{}, _ => H.Bool.if(Cls, starts(s, "<action="), _ => KAction{}, _ => H.Bool.if(Cls, starts(s, "<fn="), _ => KFn{}, _ => H.Bool.if(Cls, starts(s, "<box"), _ => KBox{}, _ => H.Bool.if(Cls, starts(s, "<fc="), _ => KFc{}, _ => KFail{})))))))

# top: the string's end ends it (eof); else the end of the string is an
# element left open
def classify.at(top: Bool, +s: String) -> Cls:
  match s:
    case SNil{}:
      Bool.pick(Cls, top, KEnd{}, KFail{})
    case SCon{c, +r}:
      H.Bool.if(Cls, Bool.and(H.c.is(c, 60), text.suffix(r)), _ => classify.tag(s), _ => KText{})

def classify(end: Maybe<&2, String>, +s: String) -> Cls:
  match end:
    case Some{e}:
      H.Bool.if(Cls, starts(s, e), _ => KEnd{}, _ => classify.at(False{}, s))
    case None{}:
      classify.at(True{}, s)

def end.drop(end: Maybe<&2, String>, +s: String) -> String:
  match end:
    case Some{e}:
      String.drop(s, String.length(e))
    case None{}:
      s

type Mode is Data:
  MLook{}
  MGo{k: Cls}
  MIcon{m: Maybe<&2, SS>}
  MHspace{m: Maybe<&2, SU>}
  MRaw{m: Maybe<&2, SS>}
  MAction{m: Maybe<&2, ActR>}
  MFn{m: Maybe<&2, FR>}
  MBox{m: Maybe<&2, SS>}
  MFc{m: Maybe<&2, SS>}

def items(fuel: Nat, m: Mode, +ctx: Ctx, +end: Maybe<&2, String>, +s: String, k: List<&2, Seg> -> String -> Res()) -> Res():
  match fuel m:
    case 0n _:
      None{}
    case 1n+f MLook{}:
      items(f, MGo{classify(end, s)}, ctx, end, s, k)
    case 1n+f MGo{KEnd{}}:
      k(Nil{}, end.drop(end, s))
    case 1n+f MGo{KFail{}}:
      None{}
    case 1n+f MGo{KText{}}:
      +n = text.len(s)
      items(f, MLook{}, ctx, end, String.drop(s, n), segs => r => k(seg(ctx, WText{String.take(s, n)}) <> segs, r))
    case 1n+f MGo{KIcon{}}:
      items(f, MIcon{icon.p(String.drop(s, 6n))}, ctx, end, s, k)
    case 1n+f MGo{KHspace{}}:
      items(f, MHspace{hspace.p(String.drop(s, 8n))}, ctx, end, s, k)
    case 1n+f MGo{KRaw{}}:
      items(f, MRaw{raw.p(String.drop(s, 5n))}, ctx, end, s, k)
    case 1n+f MGo{KAction{}}:
      items(f, MAction{action.p(String.drop(s, 8n))}, ctx, end, s, k)
    case 1n+f MGo{KFn{}}:
      items(f, MFn{fn.p(String.drop(s, 4n))}, ctx, end, s, k)
    case 1n+f MGo{KBox{}}:
      items(f, MBox{box.p(String.drop(s, 4n))}, ctx, end, s, k)
    case 1n+f MGo{KFc{}}:
      items(f, MFc{fc.p(String.drop(s, 4n))}, ctx, end, s, k)
    case 1n+f MIcon{Some{SS{p, rest}}}:
      items(f, MLook{}, ctx, end, rest, segs => r => k(seg(ctx, WIcon{p}) <> segs, r))
    case 1n+f MHspace{Some{SU{w, rest}}}:
      items(f, MLook{}, ctx, end, rest, segs => r => k(seg(ctx, WHspace{w}) <> segs, r))
    case 1n+f MRaw{Some{SS{t, rest}}}:
      items(f, MLook{}, ctx, end, rest, segs => r => k(seg(ctx, WText{t}) <> segs, r))
    case 1n+f MAction{Some{ActR{x, rest}}}:
      +g = f
      items(g, MLook{}, ctx.action(ctx, x), Some{"</action>"}, rest, inner => r => items(g, MLook{}, ctx, end, r, segs => r2 => k(List.append(&2, Seg, inner, segs), r2)))
    case 1n+f MFn{Some{FR{i, rest}}}:
      +g = f
      items(g, MLook{}, ctx.fn(ctx, i), Some{"</fn>"}, rest, inner => r => items(g, MLook{}, ctx, end, r, segs => r2 => k(List.append(&2, Seg, inner, segs), r2)))
    case 1n+f MBox{Some{SS{p, rest}}}:
      +g = f
      items(g, MLook{}, ctx.box(ctx, p), Some{"</box>"}, rest, inner => r => items(g, MLook{}, ctx, end, r, segs => r2 => k(List.append(&2, Seg, inner, segs), r2)))
    case 1n+f MFc{Some{SS{c, rest}}}:
      +g = f
      items(g, MLook{}, ctx.fc(ctx, c), Some{"</fc>"}, rest, inner => r => items(g, MLook{}, ctx, end, r, segs => r2 => k(List.append(&2, Seg, inner, segs), r2)))
    case 1n+f _:
      None{}

def info.plain(fg: String) -> Info:
  Info{fg, 0, 0, Nil{}}

def parse.or(r: Res(), +fg: String, +s: String) -> List<&2, Seg>:
  match r:
    case Some{segs}:
      segs
    case None{}:
      [Seg{WText{"Could not parse string: " ++ s}, info.plain(fg), Nil{}, None{}}]

# parseString: s's segments, fg the config's fgColor
def parse(+fg: String, +s: String) -> List<&2, Seg>:
  +fuel = Nat.add(Nat.mul(3n, String.length(s)), 3n)
  parse.or(items(fuel, MLook{}, Ctx{info.plain(fg), Nil{}, None{}}, None{}, s, segs => _ => Some{segs}), fg, s)

# Show (Haskell's derived Show at Segment) -----------------------------------------

def show.widget(w: Widget) -> String:
  match w:
    case WIcon{p}:
      "Icon " ++ H.show.str(p)
    case WText{t}:
      "Text " ++ H.show.str(t)
    case WHspace{n}:
      "Hspace " ++ H.show.i32(11n, n)

def show.border(b: BoxBorder) -> String:
  match b:
    case BBTop{}:
      "BBTop"
    case BBBottom{}:
      "BBBottom"
    case BBVBoth{}:
      "BBVBoth"
    case BBLeft{}:
      "BBLeft"
    case BBRight{}:
      "BBRight"
    case BBHBoth{}:
      "BBHBoth"
    case BBFull{}:
      "BBFull"

def show.align(a: Align) -> String:
  match a:
    case AL{}:
      "L"
    case AR{}:
      "R"
    case AC{}:
      "C"

def show.box(b: Box) -> String:
  match b:
    case Box{bb, al, off, w, c, t, r, bo, l}:
      "Box {bBorder = " ++ show.border(bb) ++ ", bOffset = BoxOffset " ++ show.align(al) ++ " " ++ H.show.i32(11n, off) ++ ", bWidth = " ++ H.show.i32(0n, w) ++ ", bColor = " ++ H.show.str(c) ++ ", bMargins = BoxMargins " ++ H.show.i32(11n, t) ++ " " ++ H.show.i32(11n, r) ++ " " ++ H.show.i32(11n, bo) ++ " " ++ H.show.i32(11n, l) ++ "}"

def show.boxes(bs: List<&2, Box>) -> List<&2, String>:
  match bs:
    case Nil{}:
      Nil{}
    case b <> rest:
      show.box(b) <> show.boxes(rest)

def show.info(i: Info) -> String:
  match i:
    case Info{cs, t, b, bs}:
      "TextRenderInfo {tColorsString = " ++ H.show.str(cs) ++ ", tBgTopOffset = " ++ H.show.i32(0n, t) ++ ", tBgBottomOffset = " ++ H.show.i32(0n, b) ++ ", tBoxes = " ++ H.show.list(show.boxes(bs)) ++ "}"

def show.words(ws: List<&2, U32>) -> List<&2, String>:
  match ws:
    case Nil{}:
      Nil{}
    case w <> rest:
      H.show.u32(w) <> show.words(rest)

def show.action(a: Action) -> String:
  match a:
    case Spawn{bs, c}:
      "Spawn " ++ H.show.list(show.words(bs)) ++ " " ++ H.show.str(c)

def show.actions(xs: List<&2, Action>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      show.action(x) <> show.actions(rest)

def show.acts(m: Maybe<&2, List<&2, Action>>) -> String:
  match m:
    case None{}:
      "Nothing"
    case Some{xs}:
      "Just " ++ H.show.list(show.actions(xs))

def show.seg(s: Seg) -> String:
  match s:
    case Seg{w, i, f, a}:
      "(" ++ show.widget(w) ++ "," ++ show.info(i) ++ "," ++ B.i64.show(0n, f) ++ "," ++ show.acts(a) ++ ")"

def show.segs.go(xs: List<&2, Seg>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      show.seg(x) <> show.segs.go(rest)

def show.segs(xs: List<&2, Seg>) -> String:
  H.show.list(show.segs.go(xs))

# Templates (Xmobar.Run.Template) ---------------------------------------------------

def char.at(s: String, +i: Nat) -> U32:
  match s:
    case SNil{}:
      0
    case SCon{c, r}:
      match i:
        case 0n:
          Char.to_u32(c)
        case 1n+j:
          char.at(r, j)

def split.r(le: String, b: Brk, +t: String) -> List<&2, String>:
  match b:
    case Brk{ce, Some{ri}}:
      [le, ce, ri]
    case Brk{_, None{}}:
      [t, "", ""]

def split.l(b: Brk, +r: U32, +t: String) -> List<&2, String>:
  match b:
    case Brk{le, Some{re}}:
      split.r(le, brk(r, re), t)
    case Brk{_, None{}}:
      [t, "", ""]

# splitTemplate: the left, center and right of a template; alignSep's two
# characters cut it ("}{" when alignSep is not two long)
def split(+sep: String, +t: String) -> List<&2, String>:
  +s = Bool.pick(String, Nat.is_eq(String.length(sep), 2n), sep, "}{")
  split.l(brk(char.at(s, 0n), t), char.at(s, 1n), t)

# a template's command, the text before it and the text after it
type Part is Data:
  Part{com: String, before: String, after: String}

def not.sep(+sep: String, s: String) -> Nat:
  match s:
    case SNil{}:
      0n
    case SCon{+c, rest}:
      H.Bool.if(Nat, String.contains(sep, SCon{c, SNil{}}), _ => 0n, _ => 1n+not.sep(sep, rest))

def parts.cons(p: Part, m: Maybe<&2, List<&2, Part>>) -> Maybe<&2, List<&2, Part>>:
  match m:
    case Some{ps}:
      Some{p <> ps}
    case None{}:
      None{}

# many templateStringParser: s (no separator), the command between two
# sepChars, ss; a first part that reads text and then no sepChar fails (it
# has consumed), one that reads nothing stops the list (what is left is not
# read: parse has no eof)
def parts(fuel: Nat, +sep: String, +c0: U32, +t: String) -> Maybe<&2, List<&2, Part>>:
  match fuel:
    case 0n:
      None{}
    case 1n+f:
      +n = not.sep(sep, t)
      +a = String.drop(t, n)
      +m = not.sep(sep, tail(a))
      +b = String.drop(tail(a), m)
      +q = not.sep(sep, tail(b))
      H.Bool.if(Maybe<&2, List<&2, Part>>, first.is(a, c0), _ => H.Bool.if(Maybe<&2, List<&2, Part>>, first.is(b, c0), _ => parts.cons(Part{String.take(tail(a), m), String.take(t, n), String.take(tail(b), q)}, parts(f, sep, c0, String.drop(tail(b), q))), _ => None{}), _ => Bool.pick(Maybe<&2, List<&2, Part>>, Nat.is_eq(n, 0n), Some{Nil{}}, None{}))

def parts.or(m: Maybe<&2, List<&2, Part>>, +t: String) -> List<&2, Part>:
  match m:
    case Some{ps}:
      ps
    case None{}:
      [Part{"", t, ""}]

# parseTemplate's parts (before combine looks the commands up): a template
# that does not parse is one part, all text
def template(+sep: String, +t: String) -> List<&2, Part>:
  parts.or(parts(Nat.add(String.length(t), 1n), sep, char.at(sep, 0n), t), t)

# Text output (Xmobar.Text.Output's format, Plain) ---------------------------------

def plain.seg(s: Seg) -> String:
  match s:
    case Seg{WText{t}, _, _, _}:
      t
    case Seg{WHspace{+n}, _, _, _}:
      H.Bool.if(String, H.is.neg(n), _ => "", _ => String.repeat(" ", U32.to_nat(n)))
    case Seg{WIcon{_}, _, _, _}:
      ""

def plain.go(xs: List<&2, Seg>) -> String:
  match xs:
    case Nil{}:
      ""
    case x <> rest:
      plain.seg(x) ++ plain.go(rest)

# format at Plain: the segments' text, an Hspace as that many spaces
def plain(+fg: String, +s: String) -> String:
  plain.go(parse(fg, s))
