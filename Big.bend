# Natural numbers of any size, for what Haskell does with Integer: GHC's
# floatToDigits (showFFloat's digits) and Int's 64 bits. A number is its
# 16-bit limbs, lowest first, with no zero limb on top (0 is the empty
# list), so a limb times a limb plus a carry fits a U32.

import Base
import ./Hs.bend as H

def Big() -> Data:
  List<&2, U32>

type QR is Data:
  QR{q: List<&2, U32>, r: U32}

def lo(+u: U32) -> U32:
  U32.and(u, 65535)

def hi(+u: U32) -> U32:
  U32.shrn(u, 16n)

def norm.go(rev: List<&2, U32>) -> List<&2, U32>:
  match rev:
    case Nil{}:
      Nil{}
    case Con{+x, +rest}:
      H.Bool.if(List<&2, U32>, U32.is_eq(x, 0), _ => norm.go(rest), _ => x <> rest)

# without zero limbs on top
def norm(a: List<&2, U32>) -> List<&2, U32>:
  List.reverse(&2, U32, norm.go(List.reverse(&2, U32, a)))

def of.u32(+u: U32) -> List<&2, U32>:
  norm([lo(u), hi(u)])

def is.zero(a: List<&2, U32>) -> Bool:
  List.is_empty(&2, U32, a)

# a + c, c below 2^16
def add.c(a: List<&2, U32>, +c: U32) -> List<&2, U32>:
  match a:
    case Nil{}:
      H.Bool.if(List<&2, U32>, U32.is_eq(c, 0), _ => Nil{}, _ => c <> Nil{})
    case Con{x, xs}:
      +s = U32.add(x, c)
      lo(s) <> add.c(xs, hi(s))

def add.go(a: List<&2, U32>, b: List<&2, U32>, +c: U32) -> List<&2, U32>:
  match a b:
    case Nil{} _:
      add.c(b, c)
    case Con{x, xs} Nil{}:
      add.c(x <> xs, c)
    case Con{x, xs} Con{y, ys}:
      +s = U32.add(U32.add(x, y), c)
      lo(s) <> add.go(xs, ys, hi(s))

def add(a: List<&2, U32>, b: List<&2, U32>) -> List<&2, U32>:
  add.go(a, b, 0)

# a * m + c, m and c below 2^16
def mul.small.go(a: List<&2, U32>, +m: U32, +c: U32) -> List<&2, U32>:
  match a:
    case Nil{}:
      H.Bool.if(List<&2, U32>, U32.is_eq(c, 0), _ => Nil{}, _ => c <> Nil{})
    case Con{x, xs}:
      +s = U32.add(U32.mul(x, m), c)
      lo(s) <> mul.small.go(xs, m, hi(s))

def mul.small(a: List<&2, U32>, +m: U32) -> List<&2, U32>:
  norm(mul.small.go(a, m, 0))

# a * 2^n
def shl(n: Nat, a: List<&2, U32>) -> List<&2, U32>:
  match n:
    case 0n:
      a
    case 1n+k:
      shl(k, mul.small(a, 2))

# 10^n
def pow10(n: Nat) -> List<&2, U32>:
  match n:
    case 0n:
      [1]
    case 1n+k:
      mul.small(pow10(k), 10)

def cmp.then(c: Cmp, +x: U32, +y: U32) -> Cmp:
  match c:
    case EQ{}:
      U32.cmp(x, y)
    case o:
      o

def cmp(a: List<&2, U32>, b: List<&2, U32>) -> Cmp:
  match a b:
    case Nil{} Nil{}:
      EQ{}
    case Nil{} _:
      LT{}
    case _ Nil{}:
      GT{}
    case Con{x, xs} Con{y, ys}:
      cmp.then(cmp(xs, ys), x, y)

def is.le(a: List<&2, U32>, b: List<&2, U32>) -> Bool:
  Cmp.is_le(cmp(a, b))

def is.lt(a: List<&2, U32>, b: List<&2, U32>) -> Bool:
  Cmp.is_lt(cmp(a, b))

# a - b with a borrow, for a >= b
def sub.c(a: List<&2, U32>, +bw: U32) -> List<&2, U32>:
  match a:
    case Nil{}:
      Nil{}
    case Con{+x, +xs}:
      H.Bool.if(List<&2, U32>, U32.is_ge(x, bw), _ => U32.sub(x, bw) <> xs, _ => U32.sub(U32.add(x, 65536), bw) <> sub.c(xs, 1))

def sub.go(a: List<&2, U32>, b: List<&2, U32>, +bw: U32) -> List<&2, U32>:
  match a b:
    case Nil{} _:
      Nil{}
    case Con{x, xs} Nil{}:
      sub.c(x <> xs, bw)
    case Con{+x, +xs} Con{y, +ys}:
      +t = U32.add(y, bw)
      H.Bool.if(List<&2, U32>, U32.is_ge(x, t), _ => U32.sub(x, t) <> sub.go(xs, ys, 0), _ => U32.sub(U32.add(x, 65536), t) <> sub.go(xs, ys, 1))

def sub(a: List<&2, U32>, b: List<&2, U32>) -> List<&2, U32>:
  norm(sub.go(a, b, 0))

def qr.cons(+x: U32, r: QR) -> QR:
  match r:
    case QR{q, m}:
      QR{x <> q, m}

# the highest limb first: q's limbs come out highest last, so lowest first
def divmod.go(rev: List<&2, U32>, +d: U32, +r: U32, acc: List<&2, U32>) -> QR:
  match rev:
    case Nil{}:
      QR{norm(acc), r}
    case Con{x, rest}:
      +v = U32.add(U32.mul(r, 65536), x)
      divmod.go(rest, d, U32.mod(v, d), U32.div(v, d) <> acc)

# a divided by d (0 < d <= 2^16): its quotient and remainder
def divmod(a: List<&2, U32>, +d: U32) -> QR:
  divmod.go(List.reverse(&2, U32, a), d, 0, Nil{})

def show.go(fuel: Nat, r: QR, acc: String) -> String:
  match fuel r:
    case 0n _:
      acc
    case 1n+f QR{+q, +m}:
      +s = {SCon{Char.from_u32(U32.add(48, m)), acc} : String}
      H.Bool.if(String, is.zero(q), _ => s, _ => show.go(f, divmod(q, 10), s))

def show(+a: List<&2, U32>) -> String:
  +n = List.length(&2, U32, a)
  show.go(Nat.add(Nat.mul(5n, n), 1n), divmod(a, 10), "")

# Int: 64 bits ----------------------------------------------------------------------

# a decimal number's digits modulo 2^64 (fromInteger at Int), as a Big
def dec64.go(s: String, acc: List<&2, U32>) -> List<&2, U32>:
  match s:
    case SNil{}:
      acc
    case SCon{c, rest}:
      dec64.go(rest, norm(List.take(&2, U32, add.c(mul.small(acc, 10), U32.sub(Char.to_u32(c), 48)), 4n)))

def dec64(s: String) -> List<&2, U32>:
  dec64.go(s, Nil{})

# whether a 64-bit value is negative as an Int (its top bit)
def i64.is.neg(a: List<&2, U32>) -> Bool:
  match a:
    case Con{_, Con{_, Con{_, Con{+x, _}}}}:
      U32.is_ge(x, 32768)
    case _:
      False{}

def two64() -> List<&2, U32>:
  [0, 0, 0, 0, 1]

# showsPrec p at Int of a 64-bit value
def i64.show(+p: Nat, +a: List<&2, U32>) -> String:
  H.Bool.if(String, i64.is.neg(a), _ => H.hs.paren(Nat.is_gt(p, 6n), "-" ++ show(sub(two64(), a))), _ => show(a))

# a value below 2^32, or None
def to.u32(a: List<&2, U32>) -> Maybe<&2, U32>:
  match a:
    case Nil{}:
      Some{0}
    case Con{x, Nil{}}:
      Some{x}
    case Con{x, Con{y, Nil{}}}:
      Some{U32.add(x, U32.mul(y, 65536))}
    case _:
      None{}

# a decimal number's digits, all of them (an Integer)
def dec.big.go(s: String, acc: List<&2, U32>) -> List<&2, U32>:
  match s:
    case SNil{}:
      acc
    case SCon{c, rest}:
      dec.big.go(rest, add.c(mul.small(acc, 10), U32.sub(Char.to_u32(c), 48)))

def dec.big(s: String) -> List<&2, U32>:
  dec.big.go(s, Nil{})

def shift16(a: List<&2, U32>) -> List<&2, U32>:
  match a:
    case Nil{}:
      Nil{}
    case Con{x, xs}:
      0 <> (x <> xs)

# a * b
def mul.big(+a: List<&2, U32>, b: List<&2, U32>) -> List<&2, U32>:
  match b:
    case Nil{}:
      Nil{}
    case Con{y, ys}:
      add(mul.small(a, y), shift16(mul.big(a, ys)))

def or0(m: Maybe<&2, U32>) -> U32:
  match m:
    case Some{x}:
      x
    case None{}:
      0

def to.u32.or0(a: List<&2, U32>) -> U32:
  or0(to.u32(a))

# showsPrec 0 at Int of minus a natural number
def i.show.neg(+a: List<&2, U32>) -> String:
  Bool.pick(String, is.zero(a), "0", "-" ++ show(a))

def any.nonzero(xs: List<&2, U32>) -> Bool:
  match xs:
    case Nil{}:
      False{}
    case Con{x, rest}:
      Bool.or(U32.is_ne(x, 0), any.nonzero(rest))

def pow2.small(n: Nat) -> U32:
  match n:
    case 0n:
      1
    case 1n+m:
      U32.mul(pow2.small(m), 2)

def shr.sticky.bits(r: QR, +lost: Bool) -> QR:
  match r:
    case QR{q, +m}:
      QR{q, Bool.pick(U32, Bool.or(lost, U32.is_ne(m, 0)), 1, 0)}

# a / 2^n, and r 1 if any bit shifted out was set, else 0
def shr.sticky(+a: List<&2, U32>, +n: Nat) -> QR:
  +limbs = Nat.div(n, 16n)
  shr.sticky.bits(divmod(List.drop(&2, U32, a, limbs), pow2.small(Nat.mod(n, 16n))), any.nonzero(List.take(&2, U32, a, limbs)))
