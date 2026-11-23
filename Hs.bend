# Haskell's meanings that xmobar's code leans on, in Bend: Data.Char's
# classes (isSpace, isDigit, isAlphaNum by GHC's Unicode tables), Show's
# text for strings and Int32s, and Read's numbers as GHC's lexer reads them
# (readMaybe at Int32, CInt and Word32). Each def says which Haskell it is.

import Base
import ./Unicode.bend as U

def Bool.if(-R: Type, c: Bool, t: Unit -> R, e: Unit -> R) -> R:
  match c:
    case True{}:
      t(Unit{})
    case False{}:
      e(Unit{})

def hs.paren(on: Bool, s: String) -> String:
  match on:
    case True{}:
      "(" ++ s ++ ")"
    case False{}:
      s

# Data.Char ------------------------------------------------------------------------

def in.ranges(+u: U32, rs: List<&2, U.Range>) -> Bool:
  match rs:
    case Nil{}:
      False{}
    case U.Range{+lo, hi} <> rest:
      Bool.if(Bool, U32.is_lt(u, lo), _ => False{}, _ => Bool.if(Bool, U32.is_le(u, hi), _ => True{}, _ => in.ranges(u, rest)))

def in(+u: U32, +lo: U32, +hi: U32) -> Bool:
  Bool.and(U32.is_ge(u, lo), U32.is_le(u, hi))

# isDigit: ASCII's digits only
def is.digit(+u: U32) -> Bool:
  in(u, 48, 57)

def is.hex(+u: U32) -> Bool:
  Bool.or(in(u, 48, 57), Bool.or(in(u, 97, 102), in(u, 65, 70)))

def is.oct(+u: U32) -> Bool:
  in(u, 48, 55)

# isSpace: space, \t \n \v \f \r, NBSP, and Unicode's Space separators
def is.space(+u: U32) -> Bool:
  Bool.if(Bool, U32.is_lt(u, 256), _ => Bool.or(U32.is_eq(u, 32), Bool.or(in(u, 9, 13), U32.is_eq(u, 160))), _ => Bool.or(U32.is_eq(u, 5760), Bool.or(in(u, 8192, 8202), Bool.or(U32.is_eq(u, 8239), Bool.or(U32.is_eq(u, 8287), U32.is_eq(u, 12288))))))

# isAlphaNum: letters and numbers of every script (L* and N*)
def is.alnum(+u: U32) -> Bool:
  Bool.if(Bool, U32.is_lt(u, 128), _ => Bool.or(in(u, 48, 57), Bool.or(in(u, 65, 90), in(u, 97, 122))), _ => in.ranges(u, U.alnum()))

def c.is.space(c: Char) -> Bool:
  is.space(Char.to_u32(c))

def c.is.digit(c: Char) -> Bool:
  is.digit(Char.to_u32(c))

def c.is.alnum(c: Char) -> Bool:
  is.alnum(Char.to_u32(c))

def c.is(c: Char, +u: U32) -> Bool:
  U32.is_eq(Char.to_u32(c), u)

# Show -----------------------------------------------------------------------------

# ASCII's controls by name, 0 to 31, and SP (GHC's asciiTab)
def esc.names() -> List<&2, String>:
  ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US", "SP"]

# the controls 7 to 13 by letter
def esc.letters() -> List<&2, String>:
  ["a", "b", "t", "n", "v", "f", "r"]

def nth(n: Nat, xs: List<&2, String>) -> String:
  match n xs:
    case 0n x <> _:
      x
    case 1n+m _ <> rest:
      nth(m, rest)
    case _ _:
      ""

def protect(on: Bool) -> String:
  match on:
    case True{}:
      "\\&"
    case False{}:
      ""

# whether a text starts with a character from lo to hi
def next.in(t: String, +lo: U32, +hi: U32) -> Bool:
  match t:
    case SCon{c, _}:
      in(Char.to_u32(c), lo, hi)
    case SNil{}:
      False{}

# a character inside a shown string, as GHC's showLitString writes it: \"
# and \\; the controls 7 to 13 by letter, the others by ASCII's names (\SO
# with \& after it before an H), DEL; any other non-ASCII character as \ and
# its decimal code (with \& after it before a digit); the rest as itself.
# t: what follows the character
def show.char.code(+u: U32, +t: String) -> String:
  Bool.if(String, U32.is_eq(u, 34), _ => "\\\"", _ => Bool.if(String, U32.is_eq(u, 92), _ => "\\\\", _ => Bool.if(String, U32.is_eq(u, 127), _ => "\\DEL", _ => Bool.if(String, U32.is_gt(u, 127), _ => "\\" ++ Nat.show(U32.to_nat(u)) ++ protect(next.in(t, 48, 57)), _ => Bool.if(String, U32.is_ge(u, 32), _ => SCon{Char.from_u32(u), SNil{}}, _ => Bool.if(String, in(u, 7, 13), _ => "\\" ++ nth(Nat.sub(U32.to_nat(u), 7n), esc.letters()), _ => Bool.if(String, U32.is_eq(u, 14), _ => "\\SO" ++ protect(next.in(t, 72, 72)), _ => "\\" ++ nth(U32.to_nat(u), esc.names()))))))))

def show.str.go(s: String) -> String:
  match s:
    case SNil{}:
      "\""
    case SCon{c, +t}:
      show.char.code(Char.to_u32(c), t) ++ show.str.go(t)

# show at a String
def show.str(s: String) -> String:
  "\"" ++ show.str.go(s)

def show.list.go(xs: List<&2, String>) -> String:
  match xs:
    case Nil{}:
      ""
    case x <> Nil{}:
      x
    case x <> rest:
      x ++ "," ++ show.list.go(rest)

# show at a list, its elements already shown
def show.list(xs: List<&2, String>) -> String:
  "[" ++ show.list.go(xs) ++ "]"

def neg(+w: U32) -> U32:
  U32.add(U32.not(w), 1)

def is.neg(+w: U32) -> Bool:
  U32.is_ge(w, 2147483648)

# showsPrec p at an Int32 (a U32 read as two's complement): a negative one
# in parentheses above precedence 6
def show.i32(+p: Nat, +w: U32) -> String:
  Bool.if(String, is.neg(w), _ => hs.paren(Nat.is_gt(p, 6n), "-" ++ Nat.show(U32.to_nat(neg(w)))), _ => Nat.show(U32.to_nat(w)))

def show.u32(+w: U32) -> String:
  Nat.show(U32.to_nat(w))

# Read -----------------------------------------------------------------------------

def digit.val(+u: U32) -> U32:
  Bool.if(U32, U32.is_le(u, 57), _ => U32.sub(u, 48), _ => Bool.if(U32, U32.is_le(u, 70), _ => U32.sub(u, 55), _ => U32.sub(u, 87)))

# base 8 10 or 16; which: 0 octal, 1 decimal, 2 hexadecimal
def digit.ok(+which: U32, +u: U32) -> Bool:
  Bool.if(Bool, U32.is_eq(which, 0), _ => is.oct(u), _ => Bool.if(Bool, U32.is_eq(which, 1), _ => is.digit(u), _ => is.hex(u)))

def all.digits(+which: U32, s: String) -> Bool:
  match s:
    case SNil{}:
      True{}
    case SCon{c, rest}:
      Bool.and(digit.ok(which, Char.to_u32(c)), all.digits(which, rest))

# the value of a string of digits in a base, modulo 2^32
def digits.val(+base: U32, s: String, +acc: U32) -> U32:
  match s:
    case SNil{}:
      acc
    case SCon{c, rest}:
      digits.val(base, rest, U32.add(U32.mul(acc, base), digit.val(Char.to_u32(c))))

def nonempty(s: String) -> Bool:
  Bool.not(String.is_empty(s))

def read.num.dec(+s: String) -> Maybe<&2, U32>:
  Bool.pick(Maybe<&2, U32>, Bool.and(nonempty(s), all.digits(1, s)), Some{digits.val(10, s, 0)}, None{})

# after a 0: x or X and hexadecimal digits, o or O and octal ones
def read.num.prefixed(+s: String, +whole: String) -> Maybe<&2, U32>:
  match s:
    case SCon{x, +r}:
      +u = Char.to_u32(x)
      Bool.if(Maybe<&2, U32>, Bool.and(Bool.or(U32.is_eq(u, 120), U32.is_eq(u, 88)), Bool.and(nonempty(r), all.digits(2, r))), _ => Some{digits.val(16, r, 0)}, _ => Bool.if(Maybe<&2, U32>, Bool.and(Bool.or(U32.is_eq(u, 111), U32.is_eq(u, 79)), Bool.and(nonempty(r), all.digits(0, r))), _ => Some{digits.val(8, r, 0)}, _ => read.num.dec(whole)))
    case SNil{}:
      read.num.dec(whole)

# readMaybe at Int32, CInt or Word32 of a text with no sign, space or
# parenthesis (what xmobar's markup lets through): GHC's lexer reads a
# decimal number, or 0x/0X and hexadecimal digits, or 0o/0O and octal ones,
# and nothing may follow; fromInteger keeps the value modulo 2^32. A
# fraction or an exponent (1e3) is a number but not an integer: Nothing.
def read.num(+s: String) -> Maybe<&2, U32>:
  match s:
    case SCon{c, +r}:
      Bool.if(Maybe<&2, U32>, c.is(c, 48), _ => read.num.prefixed(r, s), _ => read.num.dec(s))
    case SNil{}:
      None{}

# Data.List's words: the runs of non-space characters
def space.len(+want: Bool, s: String) -> Nat:
  match s:
    case SNil{}:
      0n
    case SCon{c, rest}:
      Bool.if(Nat, Bool.not(Bool.xor(c.is.space(c), want)), _ => 1n+space.len(want, rest), _ => 0n)

def words.go(fuel: Nat, +s: String) -> List<&2, String>:
  match fuel:
    case 0n:
      Nil{}
    case 1n+f:
      +t = String.drop(s, space.len(True{}, s))
      +n = space.len(False{}, t)
      Bool.if(List<&2, String>, String.is_empty(t), _ => Nil{}, _ => String.take(t, n) <> words.go(f, String.drop(t, n)))

def words(+s: String) -> List<&2, String>:
  words.go(Nat.add(String.length(s), 1n), s)

# read at Int ---------------------------------------------------------------------

def drop.space(+s: String) -> String:
  String.drop(s, space.len(True{}, s))

def first.is(s: String, +u: U32) -> Bool:
  match s:
    case SCon{c, _}:
      c.is(c, u)
    case SNil{}:
      False{}

def tail(s: String) -> String:
  match s:
    case SCon{_, r}:
      r
    case SNil{}:
      ""

# how many characters of s are digits of a base (which: 0 octal, 1
# decimal, 2 hexadecimal)
def digits.len(+which: U32, s: String) -> Nat:
  match s:
    case SNil{}:
      0n
    case SCon{c, rest}:
      Bool.if(Nat, digit.ok(which, Char.to_u32(c)), _ => 1n+digits.len(which, rest), _ => 0n)

type NR is Data:
  NR{v: U32, rest: String}

def nr.neg(m: Maybe<&2, NR>) -> Maybe<&2, NR>:
  match m:
    case Some{NR{v, r}}:
      Some{NR{neg(v), r}}
    case None{}:
      None{}

# a number token at the start of s (no sign): 0x.. 0o.. or decimal; what
# follows must end the token (space, a parenthesis, the end), else the
# lexer reads more (a fraction, an exponent, a name) and Int's read fails
def number.ends(s: String) -> Bool:
  match s:
    case SNil{}:
      True{}
    case SCon{c, _}:
      +u = Char.to_u32(c)
      Bool.or(is.space(u), Bool.or(U32.is_eq(u, 40), U32.is_eq(u, 41)))

def number.with(+which: U32, +base: U32, +s: String) -> Maybe<&2, NR>:
  +n = digits.len(which, s)
  +r = String.drop(s, n)
  Bool.pick(Maybe<&2, NR>, Bool.and(Nat.is_gt(n, 0n), number.ends(r)), Some{NR{digits.val(base, String.take(s, n), 0), r}}, None{})

def char0(s: String) -> U32:
  match s:
    case SCon{c, _}:
      Char.to_u32(c)
    case SNil{}:
      0

def number.hex(+s: String) -> Maybe<&2, NR>:
  number.with(2, 16, s)

def number.oct(+s: String) -> Maybe<&2, NR>:
  number.with(0, 8, s)

def number(+s: String) -> Maybe<&2, NR>:
  +t = tail(s)
  +x = char0(t)
  Bool.if(Maybe<&2, NR>, Bool.and(first.is(s, 48), Bool.or(U32.is_eq(x, 120), U32.is_eq(x, 88))), _ => number.hex(String.drop(s, 2n)), _ => Bool.if(Maybe<&2, NR>, Bool.and(first.is(s, 48), Bool.or(U32.is_eq(x, 111), U32.is_eq(x, 79))), _ => number.oct(String.drop(s, 2n)), _ => number.with(1, 10, s)))

def close.paren.go(+v: U32, +r: String) -> Maybe<&2, NR>:
  Bool.pick(Maybe<&2, NR>, first.is(r, 41), Some{NR{v, tail(r)}}, None{})

def close.paren(m: Maybe<&2, NR>) -> Maybe<&2, NR>:
  match m:
    case Some{NR{v, r}}:
      close.paren.go(v, drop.space(r))
    case None{}:
      None{}

# parens (number, or - and a number), spaces between tokens
def int.expr(fuel: Nat, +s: String) -> Maybe<&2, NR>:
  match fuel:
    case 0n:
      None{}
    case 1n+f:
      +t = drop.space(s)
      Bool.if(Maybe<&2, NR>, first.is(t, 40), _ => close.paren(int.expr(f, tail(t))), _ => Bool.if(Maybe<&2, NR>, first.is(t, 45), _ => nr.neg(number(drop.space(tail(t)))), _ => number(t)))

def int.fin(m: Maybe<&2, NR>) -> Maybe<&2, U32>:
  match m:
    case Some{NR{v, r}}:
      Bool.pick(Maybe<&2, U32>, String.is_empty(drop.space(r)), Some{v}, None{})
    case None{}:
      None{}

# read at Int: None where Haskell's read fails (Prelude.read: no parse);
# the value modulo 2^32 (xmobar's numbers fit)
def read.int(+s: String) -> Maybe<&2, U32>:
  int.fin(int.expr(Nat.add(String.length(s), 1n), s))

def drop.last.empty(xs: List<&2, String>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case Con{+x, Nil{}}:
      Bool.pick(List<&2, String>, String.is_empty(x), Nil{}, [x])
    case Con{x, rest}:
      x <> drop.last.empty(rest)

# Data.List's lines (ByteString's too): no last empty line after a final
# newline
def lines(s: String) -> List<&2, String>:
  drop.last.empty(String.split(s, '\n'))
