# Haskell's Float as xmobar's monitors print and round it, in Bend: GHC's
# floatToDigits (Burger and Dybvig's shortest digits, GHC.Float's own
# estimate of the exponent and fixup), roundTo (half to even on those
# digits), showFFloat (Just d) (showDigits) and show at Float; round
# (half to even); and an Integer to the nearest Float. A Float is Bend's
# F32 (IEEE single, as GHC's), its bits read through F32.bits.
#
# Int here is an Int32 in a U32 (two's complement): exponents and digit
# counts, all small.

import Base
import ./Hs.bend as H
import ./Big.bend as B

# Int32s ----------------------------------------------------------------------------

def i.neg(+a: U32) -> U32:
  U32.add(U32.not(a), 1)

def i.is.neg(+a: U32) -> Bool:
  U32.is_ge(a, 2147483648)

def i.lt(+a: U32, +b: U32) -> Bool:
  U32.is_lt(U32.xor(a, 2147483648), U32.xor(b, 2147483648))

def i.ge(+a: U32, +b: U32) -> Bool:
  Bool.not(i.lt(a, b))

# the magnitude of a non-negative Int32, as a Nat
def i.nat(+a: U32) -> Nat:
  U32.to_nat(a)

def i.show(+a: U32) -> String:
  H.show.i32(0n, a)

# decodeFloat ------------------------------------------------------------------------

# a Float's mantissa and exponent, as decodeFloat answers them: x = f *
# 2^e, f of 24 bits (a denormal's shifted up, its exponent lowered); 0 is
# (0, 0)
type Dec is Data:
  Dec{f: U32, e: U32}

def bits(+x: F32) -> U32:
  F32.bits(x)

def of.bits(u: U32) -> F32:
  match u:
    case U32{w}:
      F32{w}

def field.e(+b: U32) -> U32:
  U32.and(U32.shrn(b, 23n), 255)

def field.m(+b: U32) -> U32:
  U32.and(b, 8388607)

def denorm.go(n: Nat, +m: U32, +e: U32) -> Dec:
  match n:
    case 0n:
      Dec{m, e}
    case 1n+k:
      H.Bool.if(Dec, U32.is_ge(m, 8388608), _ => Dec{m, e}, _ => denorm.go(k, U32.shln(m, 1n), U32.sub(e, 1)))

def decode(+x: F32) -> Dec:
  +b = bits(x)
  +ef = field.e(b)
  +m = field.m(b)
  H.Bool.if(Dec, U32.is_eq(ef, 0), _ => H.Bool.if(Dec, U32.is_eq(m, 0), _ => Dec{0, 0}, _ => denorm.go(24n, m, i.neg(149))), _ => Dec{U32.or(m, 8388608), U32.sub(ef, 150)})

def is.nan(+x: F32) -> Bool:
  +b = bits(x)
  Bool.and(U32.is_eq(field.e(b), 255), U32.is_ne(field.m(b), 0))

def is.inf(+x: F32) -> Bool:
  +b = bits(x)
  Bool.and(U32.is_eq(field.e(b), 255), U32.is_eq(field.m(b), 0))

def is.neg(+x: F32) -> Bool:
  U32.is_ge(bits(x), 2147483648)

# floatToDigits 10 -------------------------------------------------------------------

# the digits (most significant first) and the exponent k: x = 0.d1d2.. * 10^k
type Digits is Data:
  Digits{ds: List<&2, U32>, k: U32}

# Burger and Dybvig's r, s, m+ and m-
type RS is Data:
  RS{r: List<&2, U32>, s: List<&2, U32>, up: List<&2, U32>, dn: List<&2, U32>}

def pow2(n: Nat) -> List<&2, U32>:
  B.shl(n, [1])

def rs.pos(+fb: List<&2, U32>, edge: Bool, +e: Nat) -> RS:
  match edge:
    case True{}:
      RS{B.shl(2n, B.shl(e, fb)), [4], pow2(1n+e), pow2(e)}
    case False{}:
      +be = pow2(e)
      RS{B.shl(1n, B.shl(e, fb)), [2], be, be}

# -e = ne
def rs.neg(+fb: List<&2, U32>, edge: Bool, +ne: Nat) -> RS:
  match edge:
    case True{}:
      RS{B.shl(2n, fb), pow2(Nat.add(ne, 2n)), [2], [1]}
    case False{}:
      RS{B.shl(1n, fb), pow2(Nat.add(ne, 1n)), [1], [1]}

# f, e after floatToDigits' denormal adjustment; f = 2^23 at a power of two
def rs.of(+f: U32, +e: U32) -> RS:
  +fb = B.of.u32(f)
  +edge = U32.is_eq(f, 8388608)
  H.Bool.if(RS, i.ge(e, 0), _ => rs.pos(fb, edge, i.nat(e)), _ => rs.neg(fb, Bool.and(edge, i.lt(i.neg(149), e)), i.nat(i.neg(e))))

# GHC's estimate: lx = p - 1 + e0, k1 = (lx * 8651) quot 28738, +1 if lx >= 0
def k0(+e0: U32) -> U32:
  +lx = U32.add(23, e0)
  H.Bool.if(U32, i.ge(lx, 0), _ => U32.add(U32.div(U32.mul(lx, 8651), 28738), 1), _ => i.neg(U32.div(U32.mul(i.neg(lx), 8651), 28738)))

def fixup.ok(+n: U32, rs: RS) -> Bool:
  match rs:
    case RS{+r, +s, +up, _}:
      +ru = B.add(r, up)
      H.Bool.if(Bool, i.ge(n, 0), _ => B.is.le(ru, B.mul.big(B.pow10(i.nat(n)), s)), _ => B.is.le(B.mul.big(B.pow10(i.nat(i.neg(n))), ru), s))

def fixup(fuel: Nat, +n: U32, +rs: RS) -> U32:
  match fuel:
    case 0n:
      n
    case 1n+g:
      H.Bool.if(U32, fixup.ok(n, rs), _ => n, _ => fixup(g, U32.add(n, 1), rs))

type DR is Data:
  DR{d: U32, r: List<&2, U32>}

# (a quotRem s) for a < 10 s: the digit by subtraction
def qr10(fuel: Nat, +a: List<&2, U32>, +s: List<&2, U32>, +d: U32) -> DR:
  match fuel:
    case 0n:
      DR{d, a}
    case 1n+g:
      H.Bool.if(DR, B.is.lt(a, s), _ => DR{d, a}, _ => qr10(g, B.sub(a, s), s, U32.add(d, 1)))

# GHC's gen: q the step's (rn * 10) quotRem sN, up and dn already times 10
def gen(fuel: Nat, +ds: List<&2, U32>, q: DR, +sn: List<&2, U32>, +up: List<&2, U32>, +dn: List<&2, U32>) -> List<&2, U32>:
  match fuel q:
    case 0n _:
      ds
    case 1n+g DR{+d, +r}:
      +low = B.is.lt(r, dn)
      +high = B.is.lt(sn, B.add(r, up))
      H.Bool.if(List<&2, U32>, Bool.and(low, Bool.not(high)), _ => d <> ds, _ => H.Bool.if(List<&2, U32>, Bool.and(Bool.not(low), high), _ => U32.add(d, 1) <> ds, _ => H.Bool.if(List<&2, U32>, Bool.and(low, high), _ => Bool.pick(List<&2, U32>, B.is.lt(B.shl(1n, r), sn), d <> ds, U32.add(d, 1) <> ds), _ => gen(g, d <> ds, qr10(11n, B.mul.small(r, 10), sn, 0), sn, B.mul.small(up, 10), B.mul.small(dn, 10)))))

def gen.start(+rn: List<&2, U32>, +sn: List<&2, U32>, +up: List<&2, U32>, +dn: List<&2, U32>) -> List<&2, U32>:
  List.reverse(&2, U32, gen(40n, Nil{}, qr10(11n, B.mul.small(rn, 10), sn, 0), sn, B.mul.small(up, 10), B.mul.small(dn, 10)))

def digits.neg(+k: U32, +bk: List<&2, U32>, +r: List<&2, U32>, +s: List<&2, U32>, +up: List<&2, U32>, +dn: List<&2, U32>) -> Digits:
  Digits{gen.start(B.mul.big(r, bk), s, B.mul.big(up, bk), B.mul.big(dn, bk)), k}

def digits.go(+k: U32, rs: RS) -> Digits:
  match rs:
    case RS{+r, +s, +up, +dn}:
      H.Bool.if(Digits, i.ge(k, 0), _ => Digits{gen.start(r, B.mul.big(s, B.pow10(i.nat(k))), up, dn), k}, _ => digits.neg(k, B.pow10(i.nat(i.neg(k))), r, s, up, dn))

# a denormal's mantissa back to the real minimum exponent, -149
def digits.of(d: Dec) -> Digits:
  match d:
    case Dec{+f0, +e0}:
      +adj = i.lt(e0, i.neg(149))
      +e = Bool.pick(U32, adj, i.neg(149), e0)
      +f = Bool.pick(U32, adj, U32.shrn(f0, i.nat(U32.sub(i.neg(149), e0))), f0)
      +rs = rs.of(f, e)
      digits.go(fixup(64n, k0(e0), rs), rs)

def dec.is.zero(d: Dec) -> Bool:
  match d:
    case Dec{f, _}:
      U32.is_eq(f, 0)

# floatToDigits 10 x, x >= 0
def digits(+x: F32) -> Digits:
  +d = decode(x)
  H.Bool.if(Digits, dec.is.zero(d), _ => Digits{[0], 0}, _ => digits.of(d))

# roundTo 10 ------------------------------------------------------------------------

type RT is Data:
  RT{c: U32, ds: List<&2, U32>}

def rt.cons(+i: U32, r: RT) -> RT:
  match r:
    case RT{+c, +ds}:
      +j = U32.add(c, i)
      Bool.pick(RT, U32.is_eq(j, 10), RT{1, 0 <> ds}, RT{0, j <> ds})

def all.zero(xs: List<&2, U32>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case Con{x, rest}:
      Bool.and(U32.is_eq(x, 0), all.zero(rest))

def even(+i: U32) -> Bool:
  U32.is_eq(U32.and(i, 1), 0)

# GHC's f n e is: n digits kept, e whether the digit before was even (half
# to even only at exactly half)
def rt.f(xs0: List<&2, U32>, n: Nat, e: Bool) -> RT:
  match xs0:
    case Nil{}:
      RT{0, List.replicate(U32, n, 0)}
    case Con{+x, xs}:
      match n:
        case 0n:
          Bool.pick(RT, Bool.and(U32.is_eq(x, 5), Bool.and(e, all.zero(xs))), RT{0, Nil{}}, RT{Bool.pick(U32, U32.is_ge(x, 5), 1, 0), Nil{}})
        case 1n+m:
          rt.cons(x, rt.f(xs, m, even(x)))

def round.to.fin(r: RT) -> RT:
  match r:
    case RT{+c, +ds}:
      Bool.pick(RT, U32.is_eq(c, 0), RT{0, ds}, RT{1, 1 <> ds})

def round.to(+d: Nat, xs0: List<&2, U32>) -> RT:
  round.to.fin(rt.f(xs0, d, True{}))

# showFFloat (Just d) ----------------------------------------------------------------

def chars(ds: List<&2, U32>) -> String:
  match ds:
    case Nil{}:
      ""
    case Con{d, rest}:
      SCon{Char.from_u32(U32.add(48, d)), chars(rest)}

def mk0(+s: String) -> String:
  Bool.pick(String, String.is_empty(s), "0", s)

def dot.rest(+rs: String) -> String:
  Bool.pick(String, String.is_empty(rs), "", "." ++ rs)

def fixed.pos.ge(+e: U32, r: RT) -> String:
  match r:
    case RT{+ei, +ds}:
      +cs = chars(ds)
      +n = i.nat(U32.add(e, ei))
      mk0(String.take(cs, n)) ++ dot.rest(String.drop(cs, n))

def fixed.pos.lt.go(ds: List<&2, U32>) -> String:
  match ds:
    case Con{d, rest}:
      SCon{Char.from_u32(U32.add(48, d)), dot.rest(chars(rest))}
    case Nil{}:
      ""

def fixed.pos.lt(r: RT) -> String:
  match r:
    case RT{+ei, +ds}:
      fixed.pos.lt.go(Bool.pick(List<&2, U32>, U32.is_gt(ei, 0), ds, 0 <> ds))

def fixed.pos(+dec: Nat, d: Digits) -> String:
  match d:
    case Digits{+ds0, +e}:
      H.Bool.if(String, i.ge(e, 0), _ => fixed.pos.ge(e, round.to(Nat.add(dec, i.nat(e)), ds0)), _ => fixed.pos.lt(round.to(dec, List.append(&2, U32, List.replicate(U32, i.nat(i.neg(e)), 0), ds0))))

# showFFloat (Just dec) x "" (xmobar's showDigits dec x)
def fixed(+dec: Nat, +x: F32) -> String:
  H.Bool.if(String, is.nan(x), _ => "NaN", _ => H.Bool.if(String, is.inf(x), _ => Bool.pick(String, is.neg(x), "-Infinity", "Infinity"), _ => H.Bool.if(String, is.neg(x), _ => "-" ++ fixed.pos(dec, digits(F32.neg(x))), _ => fixed.pos(dec, digits(x)))))

# show at Float ----------------------------------------------------------------------

def generic.fixed.go(n: Nat, +s: String, rs: String) -> String:
  match n rs:
    case 0n _:
      mk0(String.reverse(s)) ++ "." ++ mk0(rs)
    case 1n+m SNil{}:
      generic.fixed.go(m, SCon{'0', s}, "")
    case 1n+m SCon{r, rest}:
      generic.fixed.go(m, SCon{r, s}, rest)

def generic.exp(+cs: String, +e: U32) -> String:
  match cs:
    case SCon{d, SNil{}}:
      Bool.pick(String, H.c.is(d, 48), "0.0e0", SCon{d, SNil{}} ++ ".0e" ++ i.show(U32.sub(e, 1)))
    case SCon{d, rest}:
      SCon{d, SNil{}} ++ "." ++ rest ++ "e" ++ i.show(U32.sub(e, 1))
    case SNil{}:
      ""

def generic.pos(d: Digits) -> String:
  match d:
    case Digits{+ds0, +e}:
      +cs = chars(ds0)
      H.Bool.if(String, Bool.or(i.lt(e, 0), i.lt(7, e)), _ => generic.exp(cs, e), _ => H.Bool.if(String, U32.is_eq(e, 0), _ => "0." ++ cs, _ => generic.fixed.go(i.nat(e), "", cs)))

# showsPrec p at Float: a negative one in parentheses above 6
def show(+p: Nat, +x: F32) -> String:
  H.Bool.if(String, is.nan(x), _ => "NaN", _ => H.Bool.if(String, is.inf(x), _ => Bool.pick(String, is.neg(x), H.hs.paren(Nat.is_gt(p, 6n), "-Infinity"), "Infinity"), _ => H.Bool.if(String, is.neg(x), _ => H.hs.paren(Nat.is_gt(p, 6n), "-" ++ generic.pos(digits(F32.neg(x)))), _ => generic.pos(digits(x)))))

# round ------------------------------------------------------------------------------

def is.even.f(+x: F32) -> Bool:
  F32.is_eq((F32.floor((x / 2.0 : F32)) * 2.0 : F32), x)

# round at Float, as a Float: half to even (exact: a Float of 2^23 or more
# is whole already, and below that x - floor x and floor x + 1 are exact)
def round.f(+x: F32) -> F32:
  +fl = F32.floor(x)
  +diff = (x - fl : F32)
  H.Bool.if(F32, F32.is_gt(diff, 0.5), _ => (fl + 1.0 : F32), _ => H.Bool.if(F32, F32.is_lt(diff, 0.5), _ => fl, _ => Bool.pick(F32, is.even.f(fl), fl, (fl + 1.0 : F32))))

# a whole Float's magnitude as an Integer
def whole.big(d: Dec) -> List<&2, U32>:
  match d:
    case Dec{+f, +e}:
      H.Bool.if(List<&2, U32>, i.ge(e, 0), _ => B.shl(i.nat(e), B.of.u32(f)), _ => B.of.u32(U32.shrn(f, i.nat(i.neg(e)))))

# show (round x :: Int), for |x| below 2^63
def show.round(+x: F32) -> String:
  +r = round.f(x)
  H.Bool.if(String, is.neg(r), _ => B.i.show.neg(whole.big(decode(F32.neg(r)))), _ => B.show(whole.big(decode(r))))

# fromInteger at Float ------------------------------------------------------------------

def bitlen.u32(n: Nat, +x: U32) -> Nat:
  match n:
    case 0n:
      0n
    case 1n+m:
      H.Bool.if(Nat, U32.is_eq(x, 0), _ => 0n, _ => 1n+bitlen.u32(m, U32.shrn(x, 1n)))

def bitlen(a: List<&2, U32>) -> Nat:
  match a:
    case Nil{}:
      0n
    case Con{x, Nil{}}:
      bitlen.u32(17n, x)
    case Con{_, rest}:
      Nat.add(16n, bitlen(rest))

# 2^k as a Float, k up to 300 (past 127: Infinity)
def pow2.f(k: Nat) -> F32:
  match k:
    case 0n:
      1.0
    case 1n+m:
      (pow2.f(m) * 2.0 : F32)

def of.big.top(t: B.QR, +k: Nat) -> F32:
  match t:
    case B.QR{q, +sticky}:
      (U32.to_f32(U32.or(B.to.u32.or0(q), Bool.pick(U32, U32.is_eq(sticky, 0), 0, 1))) * pow2.f(k) : F32)

# the nearest Float to a natural number (half to even): its top 32 bits,
# with a 1 below them if any bit further down is set, then scaled
def of.big(+a: List<&2, U32>) -> F32:
  +n = bitlen(a)
  H.Bool.if(F32, Nat.is_le(n, 32n), _ => U32.to_f32(B.to.u32.or0(a)), _ => of.big.top(B.shr.sticky(a, Nat.sub(n, 32n)), Nat.sub(n, 32n)))

