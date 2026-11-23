# Haskell's Read, as far as xmobar's configuration needs it: GHC's lexer
# (Text.Read.Lex: identifiers, symbols, punctuation, numbers, string and
# character literals with their escapes) and a value as derived Read
# instances read one (a constructor applied to atoms, a record in its
# fields' order, a list, a tuple, a literal, a negative number, any of
# them in parentheses), before its type is known: Config.bend then reads
# each field's value at its type. reads leaves what follows unread; read'
# in xmobar takes the one parse there is.

import Base
import ./Hs.bend as H
import ./Big.bend as B

# Lexing -----------------------------------------------------------------------------

type Tok is Data:
  TIdent{s: String}
  TSym{s: String}
  TPunc{c: U32}
  TNum{int: String, frac: String, exp: String}
  TStr{s: String}
  TChar{c: U32}
  TBad{}

def is.ident.start(+u: U32) -> Bool:
  Bool.or(H.is.alnum(u), U32.is_eq(u, 95))

def is.ident(+u: U32) -> Bool:
  Bool.or(H.is.alnum(u), Bool.or(U32.is_eq(u, 95), U32.is_eq(u, 39)))

# GHC's symbol characters (ASCII's): !#$%&*+./<=>?@\^|-~:
def is.sym(+u: U32) -> Bool:
  String.contains("!#$%&*+./<=>?@\\^|-~:", SCon{Char.from_u32(u), SNil{}})

# ()[],;`{}
def is.punc(+u: U32) -> Bool:
  String.contains("()[],;`{}", SCon{Char.from_u32(u), SNil{}})

def run.len(+k: U32, s: String) -> Nat:
  match s:
    case SNil{}:
      0n
    case SCon{c, rest}:
      +u = Char.to_u32(c)
      H.Bool.if(Nat, H.Bool.if(Bool, U32.is_eq(k, 0), _ => is.ident(u), _ => is.sym(u)), _ => 1n+run.len(k, rest), _ => 0n)

# a lexeme and what follows it
type Lx is Data:
  Lx{t: Tok, rest: String}

def esc.name.go(+s: String, ns: List<&2, String>, +i: U32) -> Maybe<&2, U32>:
  match ns:
    case Nil{}:
      Bool.pick(Maybe<&2, U32>, String.starts_with(s, "DEL"), Some{127}, None{})
    case Con{+n, rest}:
      H.Bool.if(Maybe<&2, U32>, String.starts_with(s, n), _ => Some{Bool.pick(U32, U32.is_eq(i, 32), 32, i)}, _ => esc.name.go(s, rest, U32.add(i, 1)))

# the ASCII names of \NUL .. \US, \SP and \DEL in a string or character
def esc.name(+s: String) -> Maybe<&2, U32>:
  esc.name.go(s, H.esc.names(), 0)

type EC is Data:
  EC{c: Maybe<&2, U32>, n: Nat}
  ECBad{}

def esc.letter(+u: U32) -> Maybe<&2, U32>:
  H.Bool.if(Maybe<&2, U32>, U32.is_eq(u, 97), _ => Some{7}, _ => H.Bool.if(Maybe<&2, U32>, U32.is_eq(u, 98), _ => Some{8}, _ => H.Bool.if(Maybe<&2, U32>, U32.is_eq(u, 102), _ => Some{12}, _ => H.Bool.if(Maybe<&2, U32>, U32.is_eq(u, 110), _ => Some{10}, _ => H.Bool.if(Maybe<&2, U32>, U32.is_eq(u, 114), _ => Some{13}, _ => H.Bool.if(Maybe<&2, U32>, U32.is_eq(u, 116), _ => Some{9}, _ => H.Bool.if(Maybe<&2, U32>, U32.is_eq(u, 118), _ => Some{11}, _ => H.Bool.if(Maybe<&2, U32>, U32.is_eq(u, 92), _ => Some{92}, _ => H.Bool.if(Maybe<&2, U32>, U32.is_eq(u, 34), _ => Some{34}, _ => H.Bool.if(Maybe<&2, U32>, U32.is_eq(u, 39), _ => Some{39}, _ => None{}))))))))))

def ec.of.name(m: Maybe<&2, U32>, +s: String) -> EC:
  match m:
    case Some{+c}:
      EC{Some{c}, Bool.pick(Nat, U32.is_eq(c, 127), 3n, String.length(H.nth(U32.to_nat(c), H.esc.names())))}
    case None{}:
      ECBad{}

def ec.digits(+which: U32, +base: U32, +s: String, +skip: Nat) -> EC:
  +t = String.drop(s, skip)
  +n = H.digits.len(which, t)
  +v = H.digits.val(base, String.take(t, n), 0)
  Bool.pick(EC, Bool.and(Nat.is_gt(n, 0n), U32.is_le(v, 1114111)), EC{Some{v}, Nat.add(skip, n)}, ECBad{})

# an escape after its backslash: the character (None: \&, nothing) and how
# many characters it took
def escape(+s: String) -> EC:
  +u = H.char0(s)
  +m = esc.letter(u)
  H.Bool.if(EC, Bool.not(Maybe.is_none(&2, U32, m)), _ => EC{m, 1n}, _ => H.Bool.if(EC, U32.is_eq(u, 38), _ => EC{None{}, 1n}, _ => H.Bool.if(EC, H.is.digit(u), _ => ec.digits(1, 10, s, 0n), _ => H.Bool.if(EC, U32.is_eq(u, 120), _ => ec.digits(2, 16, s, 1n), _ => H.Bool.if(EC, U32.is_eq(u, 111), _ => ec.digits(0, 8, s, 1n), _ => H.Bool.if(EC, Bool.and(U32.is_eq(u, 94), H.in(H.char0(H.tail(s)), 64, 95)), _ => EC{Some{U32.sub(H.char0(H.tail(s)), 64)}, 2n}, _ => ec.of.name(esc.name(s), s)))))))

type SR is Data:
  SR{s: String, rest: String}
  SRBad{}

def sr.cons(+c: Maybe<&2, U32>, r: SR) -> SR:
  match c r:
    case Some{u} SR{s, rest}:
      SR{SCon{Char.from_u32(u), s}, rest}
    case None{} SR{s, rest}:
      SR{s, rest}
    case _ SRBad{}:
      SRBad{}

# a string literal's characters after its opening quote; a gap (\ spaces
# \) is nothing. One def: SLit reads on, SEsc takes an escape read
type SM is Data:
  SLit{s: String}
  SEsc{e: EC, s: String}

def str.lit(fuel: Nat, m: SM) -> SR:
  match fuel m:
    case 0n _:
      SRBad{}
    case 1n+f SLit{+s}:
      +g = f
      +u = H.char0(s)
      +t = H.tail(s)
      +sp = H.space.len(True{}, t)
      H.Bool.if(SR, String.is_empty(s), _ => SRBad{}, _ => H.Bool.if(SR, U32.is_eq(u, 34), _ => SR{"", t}, _ => H.Bool.if(SR, U32.is_eq(u, 92), _ => H.Bool.if(SR, Nat.is_gt(sp, 0n), _ => Bool.pick(SR, H.first.is(String.drop(t, sp), 92), str.lit(g, SLit{H.tail(String.drop(t, sp))}), SRBad{}), _ => str.lit(g, SEsc{escape(t), t})), _ => sr.cons(Some{u}, str.lit(g, SLit{t})))))
    case 1n+f SEsc{EC{c, n}, +t}:
      sr.cons(c, str.lit(f, SLit{String.drop(t, n)}))
    case 1n+f SEsc{ECBad{}, _}:
      SRBad{}

def lx.str(r: SR) -> Lx:
  match r:
    case SR{s, rest}:
      Lx{TStr{s}, rest}
    case SRBad{}:
      Lx{TBad{}, ""}

def lx.char.esc(e: EC, +s: String) -> Lx:
  match e:
    case EC{Some{+c}, +n}:
      Bool.pick(Lx, H.first.is(String.drop(s, n), 39), Lx{TChar{c}, String.drop(s, 1n+n)}, Lx{TBad{}, ""})
    case _:
      Lx{TBad{}, ""}

# 'c' (an escape, or any character but ' and \)
def lx.char(+s: String) -> Lx:
  +u = H.char0(s)
  H.Bool.if(Lx, U32.is_eq(u, 92), _ => lx.char.esc(escape(H.tail(s)), H.tail(s)), _ => Bool.pick(Lx, Bool.and(Bool.not(String.is_empty(s)), Bool.and(U32.is_ne(u, 39), H.first.is(H.tail(s), 39))), Lx{TChar{u}, String.drop(s, 2n)}, Lx{TBad{}, ""}))

# 0x.. or 0o..: the number in decimal and what follows (GHC's lexer reads
# them as one number)
def lx.based(+which: U32, +base: U32, +s: String) -> Lx:
  +n = H.digits.len(which, s)
  Lx{TNum{B.show(B.based(base, String.take(s, n))), "", ""}, String.drop(s, n)}

# e or E, maybe a sign, digits: how long (0: no exponent)
def exp.len(+s: String) -> Nat:
  +t = H.tail(s)
  +signed = Bool.or(H.first.is(t, 43), H.first.is(t, 45))
  +d = H.digits.len(1, Bool.pick(String, signed, H.tail(t), t))
  Bool.pick(Nat, Bool.and(Bool.or(H.first.is(s, 101), H.first.is(s, 69)), Nat.is_gt(d, 0n)), Nat.add(Nat.add(1n, Bool.pick(Nat, signed, 1n, 0n)), d), 0n)

def lx.dec(+s: String) -> Lx:
  +n = H.digits.len(1, s)
  +r = String.drop(s, n)
  +m = H.digits.len(1, H.tail(r))
  +hasf = Bool.and(H.first.is(r, 46), Nat.is_gt(m, 0n))
  +r2 = Bool.pick(String, hasf, String.drop(H.tail(r), m), r)
  +e = exp.len(r2)
  Lx{TNum{String.take(s, n), Bool.pick(String, hasf, String.take(H.tail(r), m), ""), String.take(r2, e)}, String.drop(r2, e)}

# a number: digits, a fraction, an exponent; 0x and 0o ones in decimal
def lx.num(+s: String) -> Lx:
  +x = U32.or(H.char0(H.tail(s)), 32)
  +b = String.drop(s, 2n)
  H.Bool.if(Lx, Bool.and(H.first.is(s, 48), Bool.and(U32.is_eq(x, 120), Nat.is_gt(H.digits.len(2, b), 0n))), _ => lx.based(2, 16, b), _ => H.Bool.if(Lx, Bool.and(H.first.is(s, 48), Bool.and(U32.is_eq(x, 111), Nat.is_gt(H.digits.len(0, b), 0n))), _ => lx.based(0, 8, b), _ => lx.dec(s)))

# the next lexeme after spaces
def lex(+s0: String) -> Lx:
  +s = H.drop.space(s0)
  +u = H.char0(s)
  H.Bool.if(Lx, String.is_empty(s), _ => Lx{TBad{}, ""}, _ => H.Bool.if(Lx, U32.is_eq(u, 34), _ => lx.str(str.lit(Nat.add(Nat.mul(2n, String.length(s)), 2n), SLit{H.tail(s)})), _ => H.Bool.if(Lx, U32.is_eq(u, 39), _ => lx.char(H.tail(s)), _ => H.Bool.if(Lx, is.punc(u), _ => Lx{TPunc{u}, H.tail(s)}, _ => H.Bool.if(Lx, H.is.digit(u), _ => lx.num(s), _ => H.Bool.if(Lx, is.ident.start(u), _ => Lx{TIdent{String.take(s, run.len(0, s))}, String.drop(s, run.len(0, s))}, _ => H.Bool.if(Lx, is.sym(u), _ => Lx{TSym{String.take(s, run.len(1, s))}, String.drop(s, run.len(1, s))}, _ => Lx{TBad{}, ""})))))))

# Values -----------------------------------------------------------------------------

# a value before its type: a number (its digits, fraction and exponent,
# negated or not), a string, a character, a constructor applied to atoms,
# a record, a list, a tuple
type HV is Data:
  HNum{neg: Bool, int: String, frac: String, exp: String}
  HStr{s: String}
  HChar{c: U32}
  HCon{name: String, args: List<&2, HV>}
  HRec{name: String, fields: List<&2, HF>}
  HList{xs: List<&2, HV>}
  HTup{xs: List<&2, HV>}
  HParen{v: HV}

type HF is Data:
  HF{name: String, v: HV}

type PR is Data:
  PR{v: HV, rest: String}
  PRBad{}

type PL is Data:
  PL{vs: List<&2, HV>, rest: String}
  PLBad{}

type PF is Data:
  PF{fs: List<&2, HF>, rest: String}
  PFBad{}

def is.upper(+s: String) -> Bool:
  H.in(H.char0(s), 65, 90)

def tok.is.punc(t: Tok, +c: U32) -> Bool:
  match t:
    case TPunc{x}:
      U32.is_eq(x, c)
    case _:
      False{}

def lex.t(l: Lx) -> Tok:
  match l:
    case Lx{t, _}:
      t

def next.is(+s: String, +c: U32) -> Bool:
  tok.is.punc(lex.t(lex(s)), c)

def lex.rest(l: Lx) -> String:
  match l:
    case Lx{_, r}:
      r

def atom.starts.t(t: Tok) -> Bool:
  match t:
    case TIdent{x}:
      is.upper(x)
    case TPunc{+c}:
      Bool.or(U32.is_eq(c, 40), U32.is_eq(c, 91))
    case TNum{_, _, _}:
      True{}
    case TStr{_}:
      True{}
    case TChar{_}:
      True{}
    case TSym{x}:
      String.eq(x, "-")
    case TBad{}:
      False{}

# whether an atom can start at s (what a constructor's arguments are)
def atom.starts(+s: String) -> Bool:
  atom.starts.t(lex.t(lex(s)))

# the parser's modes: a value at a precedence (0, or 11 for a
# constructor's argument), the token it starts with, the items of a list
# or tuple (and after each, a comma or the closing bracket), a
# constructor's arguments, a record's fields
type PM is Data:
  PVal{prec: U32}
  PTok{prec: U32, l: Lx}
  PItems{close: U32}
  PMore{close: U32}
  PArgs{}
  PFields{}
  PFMore{}

# its answer by mode: PVal and PTok a value, the lists PL, the fields PF
type PA is Data:
  APR{r: PR}
  APL{r: PL}
  APF{r: PF}

def pa.pr(a: PA) -> PR:
  match a:
    case APR{r}:
      r
    case _:
      PRBad{}

def pa.pl(a: PA) -> PL:
  match a:
    case APL{r}:
      r
    case _:
      PLBad{}

def pa.pf(a: PA) -> PF:
  match a:
    case APF{r}:
      r
    case _:
      PFBad{}

def num.neg(l: Lx) -> PR:
  match l:
    case Lx{TNum{i, f, e}, rest}:
      PR{HNum{True{}, i, f, e}, rest}
    case _:
      PRBad{}

def pr.con(+name: String, l: PL) -> PR:
  match l:
    case PL{vs, rest}:
      PR{HCon{name, vs}, rest}
    case PLBad{}:
      PRBad{}

def pr.rec(+name: String, f: PF) -> PR:
  match f:
    case PF{fs, rest}:
      PR{HRec{name, fs}, rest}
    case PFBad{}:
      PRBad{}

def pl.cons.v(v: HV, l: PL) -> PL:
  match l:
    case PL{vs, rest}:
      PL{v <> vs, rest}
    case PLBad{}:
      PLBad{}

def pl.cons(r: PR, k: String -> PL) -> PL:
  match r:
    case PR{v, rest}:
      pl.cons.v(v, k(rest))
    case PRBad{}:
      PLBad{}

def pf.cons.v(f: HF, l: PF) -> PF:
  match l:
    case PF{fs, rest}:
      PF{f <> fs, rest}
    case PFBad{}:
      PFBad{}

def pf.cons(+name: String, r: PR, k: String -> PF) -> PF:
  match r:
    case PR{v, rest}:
      pf.cons.v(HF{name, v}, k(rest))
    case PRBad{}:
      PFBad{}

# (v) is v kept in HParen (Run reads (v) twice: see Config.bend), () the
# unit, (a, b, ..) a tuple
def paren.of(l: PL) -> PR:
  match l:
    case PL{Con{v, Nil{}}, rest}:
      PR{HParen{v}, rest}
    case PL{Nil{}, rest}:
      PR{HCon{"()", Nil{}}, rest}
    case PL{vs, rest}:
      PR{HTup{vs}, rest}
    case PLBad{}:
      PRBad{}

def list.of(l: PL) -> PR:
  match l:
    case PL{vs, rest}:
      PR{HList{vs}, rest}
    case PLBad{}:
      PRBad{}

def lx.punc(l: Lx, +c: U32) -> Bool:
  match l:
    case Lx{TPunc{x}, _}:
      U32.is_eq(x, c)
    case _:
      False{}

def lx.sym(l: Lx, +s: String) -> Bool:
  match l:
    case Lx{TSym{x}, _}:
      String.eq(x, s)
    case _:
      False{}

# a field: its name, =, its value, then a comma or the closing brace
def parse.field(t: Tok, eq: Bool, +rest: String, v: String -> PR, k: String -> PF) -> PA:
  match t eq:
    case TIdent{n} True{}:
      APF{pf.cons(n, v(rest), k)}
    case _ _:
      APF{PFBad{}}

def parse(fuel: Nat, m: PM, +s: String) -> PA:
  match fuel m:
    case 0n _:
      APR{PRBad{}}
    case 1n+f PVal{prec}:
      parse(f, PTok{prec, lex(s)}, "")
    case 1n+f PTok{+prec, Lx{TPunc{+c}, +r}}:
      +g = f
      H.Bool.if(PA, U32.is_eq(c, 40), _ => APR{paren.of(pa.pl(parse(g, PItems{41}, r)))}, _ => H.Bool.if(PA, U32.is_eq(c, 91), _ => APR{list.of(pa.pl(parse(g, PItems{93}, r)))}, _ => APR{PRBad{}}))
    case 1n+f PTok{_, Lx{TStr{x}, r}}:
      APR{PR{HStr{x}, r}}
    case 1n+f PTok{_, Lx{TChar{x}, r}}:
      APR{PR{HChar{x}, r}}
    case 1n+f PTok{_, Lx{TNum{i, fr, e}, r}}:
      APR{PR{HNum{False{}, i, fr, e}, r}}
    case 1n+f PTok{_, Lx{TSym{+x}, +r}}:
      APR{Bool.pick(PR, String.eq(x, "-"), num.neg(lex(r)), PRBad{})}
    case 1n+f PTok{+prec, Lx{TIdent{+x}, +r}}:
      +g = f
      +nl = lex(r)
      H.Bool.if(PA, Bool.not(is.upper(x)), _ => APR{PRBad{}}, _ => H.Bool.if(PA, lx.punc(nl, 123), _ => APR{pr.rec(x, pa.pf(parse(g, PFields{}, lex.rest(nl))))}, _ => H.Bool.if(PA, U32.is_ge(prec, 11), _ => APR{PR{HCon{x, Nil{}}, r}}, _ => APR{pr.con(x, pa.pl(parse(g, PArgs{}, r)))})))
    case 1n+f PTok{_, Lx{TBad{}, _}}:
      APR{PRBad{}}
    case 1n+f PItems{+close}:
      +g = f
      +l = lex(s)
      H.Bool.if(PA, lx.punc(l, close), _ => APL{PL{Nil{}, lex.rest(l)}}, _ => APL{pl.cons(pa.pr(parse(g, PVal{0}, s)), x => pa.pl(parse(g, PMore{close}, x)))})
    case 1n+f PMore{+close}:
      +g = f
      +l = lex(s)
      H.Bool.if(PA, lx.punc(l, close), _ => APL{PL{Nil{}, lex.rest(l)}}, _ => H.Bool.if(PA, lx.punc(l, 44), _ => APL{pl.cons(pa.pr(parse(g, PVal{0}, lex.rest(l))), x => pa.pl(parse(g, PMore{close}, x)))}, _ => APL{PLBad{}}))
    case 1n+f PArgs{}:
      +g = f
      H.Bool.if(PA, atom.starts(s), _ => APL{pl.cons(pa.pr(parse(g, PVal{11}, s)), x => pa.pl(parse(g, PArgs{}, x)))}, _ => APL{PL{Nil{}, s}})
    case 1n+f PFields{}:
      +g = f
      +l = lex(s)
      +l2 = lex(lex.rest(l))
      parse.field(lex.t(l), lx.sym(l2, "="), lex.rest(l2), x => pa.pr(parse(g, PVal{0}, x)), y => pa.pf(parse(g, PFMore{}, y)))
    case 1n+f PFMore{}:
      +g = f
      +l = lex(s)
      H.Bool.if(PA, lx.punc(l, 125), _ => APF{PF{Nil{}, lex.rest(l)}}, _ => H.Bool.if(PA, lx.punc(l, 44), _ => APF{pa.pf(parse(g, PFields{}, lex.rest(l)))}, _ => APF{PFBad{}}))

# reads at a value: the value and what follows it, or PRBad
def value(+s: String) -> PR:
  pa.pr(parse(Nat.add(Nat.mul(4n, String.length(s)), 8n), PVal{0}, s))

# a value as text (for tests): constructors applied, records, lists,
# tuples, literals as Haskell shows them
type SH is Data:
  SV{v: HV, prec: U32}
  SVs{vs: List<&2, HV>, sep: String}
  SFs{fs: List<&2, HF>}

def show.go(fuel: Nat, m: SH) -> String:
  match fuel m:
    case 0n _:
      "..."
    case 1n+f SV{HNum{neg, i, +fr, e}, +p}:
      +t = i ++ Bool.pick(String, String.is_empty(fr), "", "." ++ fr) ++ e
      Bool.pick(String, neg, H.hs.paren(U32.is_gt(p, 6), "-" ++ t), t)
    case 1n+f SV{HParen{v}, +p}:
      show.go(f, SV{v, p})
    case 1n+f SV{HStr{s}, _}:
      H.show.str(s)
    case 1n+f SV{HChar{c}, _}:
      "'" ++ SCon{Char.from_u32(c), SNil{}} ++ "'"
    case 1n+f SV{HCon{n, Nil{}}, _}:
      n
    case 1n+f SV{HCon{n, args}, +p}:
      H.hs.paren(U32.is_gt(p, 10), n ++ " " ++ show.go(f, SVs{args, " "}))
    case 1n+f SV{HRec{n, fs}, +p}:
      H.hs.paren(U32.is_gt(p, 11), n ++ " {" ++ show.go(f, SFs{fs}) ++ "}")
    case 1n+f SV{HList{xs}, _}:
      "[" ++ show.go(f, SVs{xs, ","}) ++ "]"
    case 1n+f SV{HTup{xs}, _}:
      "(" ++ show.go(f, SVs{xs, ","}) ++ ")"
    case 1n+f SVs{Nil{}, _}:
      ""
    case 1n+f SVs{Con{x, Nil{}}, +sep}:
      show.go(f, SV{x, Bool.pick(U32, String.eq(sep, " "), 11, 0)})
    case 1n+f SVs{Con{x, rest}, +sep}:
      +g = f
      show.go(g, SV{x, Bool.pick(U32, String.eq(sep, " "), 11, 0)}) ++ sep ++ show.go(g, SVs{rest, sep})
    case 1n+f SFs{Nil{}}:
      ""
    case 1n+f SFs{Con{HF{n, v}, Nil{}}}:
      n ++ " = " ++ show.go(f, SV{v, 0})
    case 1n+f SFs{Con{HF{n, v}, rest}}:
      +g = f
      n ++ " = " ++ show.go(g, SV{v, 0}) ++ ", " ++ show.go(g, SFs{rest})

def show(v: HV) -> String:
  show.go(10000n, SV{v, 0})

def up.go(fuel: Nat, v: HV) -> HV:
  match fuel v:
    case 1n+f HParen{x}:
      up.go(f, x)
    case _ x:
      x

# a value without the parentheses around it
def up(v: HV) -> HV:
  up.go(64n, v)

def is.paren(v: HV) -> Bool:
  match v:
    case HParen{_}:
      True{}
    case _:
      False{}
