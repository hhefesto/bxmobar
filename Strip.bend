# StdinReader's stripActions (Xmobar.Run.Actions): while the line matches
#
#   <action=`?([^>`]*)`?( +button=[12345]+)?>(.+)</action>
#
# (a POSIX extended regex: the leftmost match, the longest there), the
# match is replaced by [action=\1\2]\3[/action], one level of actions a
# time. Here the regex is decided by hand: at the leftmost "<action=" it
# matches from, its head runs to the first > (neither group 1 nor 2 can
# hold one): at most one backtick before group 1 and one after it, and
# after that second one only nothing or group 2; the longest (.+) then
# runs to the last </action> at least one character on. Groups 1 and 2
# side by side are the head without its backticks.

import Base
import ./Hs.bend as H

def inc(m: Maybe<&2, Nat>) -> Maybe<&2, Nat>:
  match m:
    case Some{n}:
      Some{1n+n}
    case None{}:
      None{}

# where p first starts in s
def find(s: String, +p: String) -> Maybe<&2, Nat>:
  match s:
    case SNil{}:
      Bool.pick(Maybe<&2, Nat>, String.is_empty(p), Some{0n}, None{})
    case SCon{+c, +rest}:
      H.Bool.if(Maybe<&2, Nat>, String.starts_with(SCon{c, rest}, p), _ => Some{0n}, _ => inc(find(rest, p)))

def last.or(m: Maybe<&2, Nat>, +here: Bool) -> Maybe<&2, Nat>:
  match m:
    case Some{n}:
      Some{1n+n}
    case None{}:
      Bool.pick(Maybe<&2, Nat>, here, Some{0n}, None{})

# where p last starts in s
def find.last(s: String, +p: String) -> Maybe<&2, Nat>:
  match s:
    case SNil{}:
      None{}
    case SCon{+c, +rest}:
      last.or(find.last(rest, p), String.starts_with(SCon{c, rest}, p))

def no.ticks(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{+c, rest}:
      +more = no.ticks(rest)
      Bool.pick(String, H.c.is(c, 96), more, SCon{c, more})

def all.in(s: String, +lo: U32, +hi: U32) -> Bool:
  match s:
    case SNil{}:
      True{}
    case SCon{c, rest}:
      Bool.and(H.in(Char.to_u32(c), lo, hi), all.in(rest, lo, hi))

# group 2 whole: one space or more, button=, a digit 1-5 or more
def is.button(s: String) -> Bool:
  match s:
    case SCon{+c, +rest}:
      H.Bool.if(Bool, H.c.is(c, 32), _ => Bool.or(is.button(rest), Bool.and(String.starts_with(rest, "button="), Bool.and(Bool.not(String.is_empty(String.drop(rest, 7n))), all.in(String.drop(rest, 7n), 49, 53)))), _ => False{})
    case SNil{}:
      False{}

# after a backtick that ends group 1: nothing, or group 2 alone
def after.tick(+s: String) -> Bool:
  Bool.or(String.is_empty(s), is.button(s))

def head.body(s: String) -> Bool:
  match s:
    case SNil{}:
      True{}
    case SCon{+c, +rest}:
      H.Bool.if(Bool, H.c.is(c, 96), _ => after.tick(rest), _ => head.body(rest))

# the head between <action= and the first >
def head.ok(s: String) -> Bool:
  match s:
    case SCon{+c, +rest}:
      Bool.pick(Bool, H.c.is(c, 96), head.body(rest), head.body(SCon{c, rest}))
    case SNil{}:
      True{}

# What the regex sees ----------------------------------------------------------------------
#
# regex-posix hands the line to glibc's regexec as withCAString makes it:
# each character's low 8 bits, cut at the first NUL. xmobar runs in the
# environment's locale (UTF-8 here), so glibc reads those bytes as UTF-8:
# a valid sequence is one character; a byte that starts none (or whose
# sequence is cut, overlong, a surrogate or past U+10FFFF) matches nothing,
# neither . nor [^>`]. The mask is those bytes (as characters), each bad
# one replaced by U+FFFF, up to the first NUL: a character past U+00FF can
# so be a > or a backtick to the regex.

def byte(c: Char) -> U32:
  U32.and(Char.to_u32(c), 255)

def cont(s: String, +lo: U32, +hi: U32) -> Bool:
  match s:
    case SCon{c, _}:
      H.in(byte(c), lo, hi)
    case SNil{}:
      False{}

def tail.of(s: String) -> String:
  match s:
    case SCon{_, r}:
      r
    case SNil{}:
      ""

# the length of the UTF-8 sequence s starts (b its first byte), 0 when it
# is not a valid one
def seq.len(+b: U32, +s: String) -> Nat:
  +s2 = tail.of(s)
  +s3 = tail.of(s2)
  H.Bool.if(Nat, H.in(b, 194, 223), _ => Bool.pick(Nat, cont(s, 128, 191), 2n, 0n), _ => H.Bool.if(Nat, H.in(b, 224, 239), _ => Bool.pick(Nat, Bool.and(cont(s, Bool.pick(U32, U32.is_eq(b, 224), 160, 128), Bool.pick(U32, U32.is_eq(b, 237), 159, 191)), cont(s2, 128, 191)), 3n, 0n), _ => H.Bool.if(Nat, H.in(b, 240, 244), _ => Bool.pick(Nat, Bool.and(cont(s, Bool.pick(U32, U32.is_eq(b, 240), 144, 128), Bool.pick(U32, U32.is_eq(b, 244), 143, 191)), Bool.and(cont(s2, 128, 191), cont(s3, 128, 191))), 4n, 0n), _ => 0n)))

# a byte 128 or more: its valid sequence as it is, else U+FFFF
def mask.seq(+n: Nat, +c: Char, +rest: String) -> String:
  Bool.pick(String, Nat.is_eq(n, 0n), SCon{Char.from_u32(65535), SNil{}}, String.take(SCon{c, rest}, n))

def mask.go(fuel: Nat, s: String) -> String:
  match fuel s:
    case 1n+f SCon{+c, +rest}:
      +b = byte(c)
      +ascii = U32.is_lt(b, 128)
      +n = Bool.pick(Nat, ascii, 1n, Nat.max(seq.len(b, rest), 1n))
      +emit = Bool.pick(String, ascii, SCon{c, SNil{}}, mask.seq(seq.len(b, rest), c, rest))
      H.Bool.if(String, U32.is_eq(b, 0), _ => "", _ => emit ++ mask.go(f, String.drop(SCon{c, rest}, n)))
    case _ _:
      ""

def to.bytes(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{c, rest}:
      SCon{Char.from_u32(byte(c)), to.bytes(rest)}

def mask(+s: String) -> String:
  mask.go(String.length(s), to.bytes(s))

def is.bad(c: Char) -> Bool:
  U32.is_eq(Char.to_u32(c), 65535)

def any.bad(s: String) -> Bool:
  match s:
    case SNil{}:
      False{}
    case SCon{c, rest}:
      Bool.or(is.bad(c), any.bad(rest))

# where the first bad character is (the length when none)
def first.bad(s: String) -> Nat:
  match s:
    case SNil{}:
      0n
    case SCon{+c, rest}:
      Bool.pick(Nat, is.bad(c), 0n, 1n+first.bad(rest))

# The match --------------------------------------------------------------------------------

# a match in the mask: where it starts, its head's length (between
# <action= and >), group 3's length
type Pos is Data:
  Pos{i: Nat, h: Nat, q: Nat}

def pos.tail(m: Maybe<&2, Nat>, +h: Nat) -> Maybe<&2, Pos>:
  match m:
    case Some{+q}:
      Bool.pick(Maybe<&2, Pos>, Nat.is_gt(q, 0n), Some{Pos{0n, h, q}}, None{})
    case None{}:
      None{}

# the head, all of it characters the regex sees, then group 3 up to the
# last </action> before the first bad character
def pos.head(m: Maybe<&2, Nat>, +rest: String) -> Maybe<&2, Pos>:
  match m:
    case Some{+gt}:
      +h = String.take(rest, gt)
      +after = String.drop(rest, 1n+gt)
      H.Bool.if(Maybe<&2, Pos>, Bool.and(head.ok(h), Bool.not(any.bad(h))), _ => pos.tail(find.last(String.take(after, first.bad(after)), "</action>"), gt), _ => None{})
    case None{}:
      None{}

def pos.at(+t: String) -> Maybe<&2, Pos>:
  pos.head(find(String.drop(t, 8n), ">"), String.drop(t, 8n))

def pos.next(m: Maybe<&2, Pos>) -> Maybe<&2, Pos>:
  match m:
    case Some{Pos{i, h, q}}:
      Some{Pos{1n+i, h, q}}
    case None{}:
      None{}

def pos.else(m: Maybe<&2, Pos>, k: Unit -> Maybe<&2, Pos>) -> Maybe<&2, Pos>:
  match m:
    case Some{p}:
      Some{p}
    case None{}:
      pos.next(k(Unit{}))

# the leftmost match in the mask
def pos(s: String) -> Maybe<&2, Pos>:
  match s:
    case SNil{}:
      None{}
    case SCon{+c, +rest}:
      H.Bool.if(Maybe<&2, Pos>, String.starts_with(SCon{c, rest}, "<action="), _ => pos.else(pos.at(SCon{c, rest}), _ => pos(rest)), _ => pos.next(pos(rest)))

# subRegex's one replacement: [action=\1\2]\3[/action] (the match runs
# to the last </action> there is, so no other follows)
def replaced(p: Pos, +s: String) -> String:
  match p:
    case Pos{+i, +h, +q}:
      +hd = String.drop(s, Nat.add(i, 8n))
      +after = String.drop(hd, 1n+h)
      String.take(s, i) ++ "[action=" ++ no.ticks(String.take(hd, h)) ++ "]" ++ String.take(after, q) ++ "[/action]" ++ String.drop(after, Nat.add(q, 9n))

def strip.go(fuel: Nat, m: Maybe<&2, Pos>, +s: String) -> String:
  match fuel m:
    case 1n+f Some{p}:
      +t = replaced(p, s)
      strip.go(f, pos(mask(t)), t)
    case _ _:
      s

# stripActions
def strip(+s: String) -> String:
  strip.go(String.length(s), pos(mask(s)), s)
