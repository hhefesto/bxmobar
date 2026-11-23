# System.Console.GetOpt's getOpt Permute, in Bend, as base 4.20 (GHC
# 9.10.3) writes it: the options found, the non-options, and the errors
# (each one's message, then each unrecognized option's), with usageInfo's
# table in an ambiguous option's message. An option descriptor names its
# option by a tag; what the option means is the caller's.

import Base
import ./Hs.bend as H

type ArgD is Data:
  NoArg{}
  ReqArg{name: String}
  OptArg{name: String}

# Option shorts longs arg description
type OptD is Data:
  OptD{tag: U32, shorts: String, longs: List<&2, String>, arg: ArgD, descr: String}

# an option found: its tag and its argument (None: none, NoArg or OptArg)
type Opt is Data:
  Opt{tag: U32, val: Maybe<&2, String>}

type Got is Data:
  Got{opts: List<&2, Opt>, nonopts: List<&2, String>, errs: List<&2, String>}

# what getNext answers about one argument
type OK is Data:
  KOpt{o: Opt}
  KUnreq{u: String}
  KNon{x: String}
  KEnd{}
  KErr{e: String}

type Next is Data:
  Next{k: OK, rest: List<&2, String>}

# usageInfo -------------------------------------------------------------------------

def sep.by(xs: List<&2, String>) -> String:
  match xs:
    case Nil{}:
      ""
    case x <> Nil{}:
      x
    case x <> rest:
      x ++ ", " ++ sep.by(rest)

def fmt.short.one(ad: ArgD, +c: Char) -> String:
  match ad:
    case NoArg{}:
      "-" ++ SCon{c, SNil{}}
    case ReqArg{d}:
      "-" ++ SCon{c, SNil{}} ++ " " ++ d
    case OptArg{d}:
      "-" ++ SCon{c, SNil{}} ++ "[" ++ d ++ "]"


def fmt.short(+ad: ArgD, s: String) -> List<&2, String>:
  match s:
    case SNil{}:
      Nil{}
    case SCon{c, rest}:
      fmt.short.one(ad, c) <> fmt.short(ad, rest)

def fmt.long.one(ad: ArgD, l: String) -> String:
  match ad:
    case NoArg{}:
      "--" ++ l
    case ReqArg{d}:
      "--" ++ l ++ "=" ++ d
    case OptArg{d}:
      "--" ++ l ++ "[=" ++ d ++ "]"

def fmt.long(+ad: ArgD, ls: List<&2, String>) -> List<&2, String>:
  match ls:
    case Nil{}:
      Nil{}
    case l <> rest:
      fmt.long.one(ad, l) <> fmt.long(ad, rest)

type Row is Data:
  Row{s: String, l: String, d: String}

def rows.more(ds: List<&2, String>) -> List<&2, Row>:
  match ds:
    case Nil{}:
      Nil{}
    case d <> rest:
      Row{"", "", d} <> rows.more(rest)

def rows.of(+s: String, +l: String, ds: List<&2, String>) -> List<&2, Row>:
  match ds:
    case Nil{}:
      [Row{s, l, ""}]
    case d <> rest:
      Row{s, l, d} <> rows.more(rest)

def fmt.opt(o: OptD) -> List<&2, Row>:
  match o:
    case OptD{_, ss, ls, +ad, descr}:
      rows.of(sep.by(fmt.short(ad, ss)), sep.by(fmt.long(ad, ls)), String.lines(descr))

def fmt.opts(os: List<&2, OptD>) -> List<&2, Row>:
  match os:
    case Nil{}:
      Nil{}
    case o <> rest:
      List.append(&2, Row, fmt.opt(o), fmt.opts(rest))

def max.s(rs: List<&2, Row>) -> Nat:
  match rs:
    case Nil{}:
      0n
    case Row{s, _, _} <> rest:
      Nat.max(String.length(s), max.s(rest))

def max.l(rs: List<&2, Row>) -> Nat:
  match rs:
    case Nil{}:
      0n
    case Row{_, l, _} <> rest:
      Nat.max(String.length(l), max.l(rest))

# take n (x ++ repeat ' ')
def flush(+n: Nat, +x: String) -> String:
  String.take(x ++ String.repeat(" ", n), n)

def table(rs: List<&2, Row>, +ns: Nat, +nl: Nat) -> String:
  match rs:
    case Nil{}:
      ""
    case Row{s, l, d} <> rest:
      "  " ++ flush(ns, s) ++ "  " ++ flush(nl, l) ++ "  " ++ d ++ "\n" ++ table(rest, ns, nl)

def usage(+header: String, os: List<&2, OptD>) -> String:
  +rs = fmt.opts(os)
  header ++ "\n" ++ table(rs, max.s(rs), max.l(rs))

# getNext ---------------------------------------------------------------------------

def err.ambig(os: List<&2, OptD>, +opt: String) -> OK:
  KErr{usage("option `" ++ opt ++ "' is ambiguous; could be one of:", os)}

def err.req(+d: String, +opt: String) -> OK:
  KErr{"option `" ++ opt ++ "' requires an argument " ++ d ++ "\n"}

def err.noarg(+opt: String) -> OK:
  KErr{"option `" ++ opt ++ "' doesn't allow an argument\n"}

def err.unrec(+opt: String) -> String:
  "unrecognized option `" ++ opt ++ "'\n"

def any.eq(+x: String, ls: List<&2, String>) -> Bool:
  match ls:
    case Nil{}:
      False{}
    case l <> rest:
      Bool.or(String.eq(x, l), any.eq(x, rest))

def any.prefix(+x: String, ls: List<&2, String>) -> Bool:
  match ls:
    case Nil{}:
      False{}
    case l <> rest:
      Bool.or(String.starts_with(l, x), any.prefix(x, rest))

def long.hit(o: OptD, prefix: Bool, +opt: String) -> Bool:
  match o:
    case OptD{_, _, ls, _, _}:
      match prefix:
        case True{}:
          any.prefix(opt, ls)
        case False{}:
          any.eq(opt, ls)

# the descriptors whose long names match opt: exactly (prefix False) or
# as a prefix
def with.long(+prefix: Bool, +opt: String, os: List<&2, OptD>) -> List<&2, OptD>:
  match os:
    case Nil{}:
      Nil{}
    case Con{+o, rest}:
      +r = with.long(prefix, opt, rest)
      Bool.pick(List<&2, OptD>, long.hit(o, prefix, opt), o <> r, r)

# one descriptor for each of its short names that is c
def with.short.n(+c: U32, s: String, +o: OptD) -> List<&2, OptD>:
  match s:
    case SNil{}:
      Nil{}
    case SCon{x, rest}:
      +r = with.short.n(c, rest, o)
      Bool.pick(List<&2, OptD>, H.c.is(x, c), o <> r, r)

def od.shorts(o: OptD) -> String:
  match o:
    case OptD{_, s, _, _, _}:
      s

def with.short(+c: U32, os: List<&2, OptD>) -> List<&2, OptD>:
  match os:
    case Nil{}:
      Nil{}
    case Con{+o, rest}:
      List.append(&2, OptD, with.short.n(c, od.shorts(o), o), with.short(c, rest))

def nonempty(xs: List<&2, OptD>) -> Bool:
  Bool.not(List.is_empty(&2, OptD, xs))

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

# long's cases for one descriptor; arg is "" or starts with =
def long.one.ad(+tag: U32, ad: ArgD, +arg: String, +opt: String, +rs: List<&2, String>) -> Next:
  match ad rs:
    case NoArg{} _:
      Bool.pick(Next, String.is_empty(arg), Next{KOpt{Opt{tag, None{}}}, rs}, Next{err.noarg(opt), rs})
    case ReqArg{+d} Nil{}:
      Bool.pick(Next, String.is_empty(arg), Next{err.req(d, opt), Nil{}}, Next{KOpt{Opt{tag, Some{tail(arg)}}}, Nil{}})
    case ReqArg{_} Con{+r, +rest}:
      Bool.pick(Next, String.is_empty(arg), Next{KOpt{Opt{tag, Some{r}}}, rest}, Next{KOpt{Opt{tag, Some{tail(arg)}}}, r <> rest})
    case OptArg{_} _:
      Bool.pick(Next, String.is_empty(arg), Next{KOpt{Opt{tag, None{}}}, rs}, Next{KOpt{Opt{tag, Some{tail(arg)}}}, rs})


def long.one(o: OptD, +arg: String, +opt: String, +rs: List<&2, String>) -> Next:
  match o:
    case OptD{tag, _, _, ad, _}:
      long.one.ad(tag, ad, arg, opt, rs)

def long.pick(os: List<&2, OptD>, +all: List<&2, OptD>, +arg: String, +opt: String, +ls: String, +rs: List<&2, String>) -> Next:
  match os:
    case Nil{}:
      Next{KUnreq{"--" ++ ls}, rs}
    case o <> Nil{}:
      long.one(o, arg, opt, rs)
    case _ <> _:
      Next{err.ambig(all, opt), rs}

type Brk is Data:
  Brk{a: String, b: String}

def brk.cons(c: Char, b: Brk) -> Brk:
  match b:
    case Brk{x, y}:
      Brk{SCon{c, x}, y}

# break (== '=')
def brk.eq(s: String) -> Brk:
  match s:
    case SNil{}:
      Brk{"", ""}
    case SCon{+c, +r}:
      H.Bool.if(Brk, H.c.is(c, 61), _ => Brk{"", SCon{c, r}}, _ => brk.cons(c, brk.eq(r)))

def long.opt.go(b: Brk, +ls: String, +rs: List<&2, String>, +os: List<&2, OptD>) -> Next:
  match b:
    case Brk{+opt, +arg}:
      +exact = with.long(False{}, opt, os)
      +opts = Bool.pick(List<&2, OptD>, nonempty(exact), exact, with.long(True{}, opt, os))
      long.pick(opts, opts, arg, "--" ++ opt, ls, rs)


def long.opt(+ls: String, +rs: List<&2, String>, +os: List<&2, OptD>) -> Next:
  long.opt.go(brk.eq(ls), ls, rs, os)

def short.one.ad(+tag: U32, ad: ArgD, +ys: String, +opt: String, +rs: List<&2, String>) -> Next:
  match ad rs:
    case NoArg{} _:
      Bool.pick(Next, String.is_empty(ys), Next{KOpt{Opt{tag, None{}}}, rs}, Next{KOpt{Opt{tag, None{}}}, ("-" ++ ys) <> rs})
    case ReqArg{+d} Nil{}:
      Bool.pick(Next, String.is_empty(ys), Next{err.req(d, opt), Nil{}}, Next{KOpt{Opt{tag, Some{ys}}}, Nil{}})
    case ReqArg{_} Con{+r, +rest}:
      Bool.pick(Next, String.is_empty(ys), Next{KOpt{Opt{tag, Some{r}}}, rest}, Next{KOpt{Opt{tag, Some{ys}}}, r <> rest})
    case OptArg{_} _:
      Bool.pick(Next, String.is_empty(ys), Next{KOpt{Opt{tag, None{}}}, rs}, Next{KOpt{Opt{tag, Some{ys}}}, rs})


def short.one(o: OptD, +ys: String, +opt: String, +rs: List<&2, String>) -> Next:
  match o:
    case OptD{tag, _, _, ad, _}:
      short.one.ad(tag, ad, ys, opt, rs)

def short.pick(os: List<&2, OptD>, +all: List<&2, OptD>, +ys: String, +opt: String, +rs: List<&2, String>) -> Next:
  match os:
    case Nil{}:
      Bool.pick(Next, String.is_empty(ys), Next{KUnreq{opt}, rs}, Next{KUnreq{opt}, ("-" ++ ys) <> rs})
    case o <> Nil{}:
      short.one(o, ys, opt, rs)
    case o <> _:
      Next{err.ambig(all, opt), rs}

def short.opt(+c: Char, +ys: String, +rs: List<&2, String>, +os: List<&2, OptD>) -> Next:
  +opts = with.short(Char.to_u32(c), os)
  short.pick(opts, opts, ys, "-" ++ SCon{c, SNil{}}, rs)

# getNext: "--" ends the options, --x is a long one, -x a short one
def short.opt.s(s: String, +rest: List<&2, String>, +os: List<&2, OptD>) -> Next:
  match s:
    case SCon{x, +xs}:
      short.opt(x, xs, rest, os)
    case SNil{}:
      Next{KNon{"-"}, rest}

def next(+a: String, +rest: List<&2, String>, +os: List<&2, OptD>) -> Next:
  H.Bool.if(Next, String.eq(a, "--"), _ => Next{KEnd{}, rest}, _ => H.Bool.if(Next, String.starts_with(a, "--"), _ => long.opt(String.drop(a, 2n), rest, os), _ => H.Bool.if(Next, Bool.and(first.is(a, 45), Nat.is_ge(String.length(a), 2n)), _ => short.opt.s(tail(a), rest, os), _ => Next{KNon{a}, rest})))

# getOpt' ---------------------------------------------------------------------------

type Acc is Data:
  Acc{os: List<&2, Opt>, xs: List<&2, String>, us: List<&2, String>, es: List<&2, String>}

def acc.add.k(k: OK, os: List<&2, Opt>, xs: List<&2, String>, us: List<&2, String>, es: List<&2, String>) -> Acc:
  match k:
    case KOpt{o}:
      Acc{o <> os, xs, us, es}
    case KUnreq{u}:
      Acc{os, xs, u <> us, es}
    case KNon{x}:
      Acc{os, x <> xs, us, es}
    case KErr{e}:
      Acc{os, xs, us, e <> es}
    case KEnd{}:
      Acc{os, xs, us, es}


def acc.add(k: OK, a: Acc) -> Acc:
  match a:
    case Acc{os, xs, us, es}:
      acc.add.k(k, os, xs, us, es)

# each argument in turn; at "--" the rest are non-options, and nothing else
type GM is Data:
  GArgs{args: List<&2, String>}
  GNext{n: Next}

def go(fuel: Nat, m: GM, +os: List<&2, OptD>) -> Acc:
  match fuel m:
    case 0n _:
      Acc{Nil{}, Nil{}, Nil{}, Nil{}}
    case 1n+f GArgs{Nil{}}:
      Acc{Nil{}, Nil{}, Nil{}, Nil{}}
    case 1n+f GArgs{a <> rest}:
      go(f, GNext{next(a, rest, os)}, os)
    case 1n+f GNext{Next{KEnd{}, rest}}:
      Acc{Nil{}, rest, Nil{}, Nil{}}
    case 1n+f GNext{Next{k, rest}}:
      acc.add(k, go(f, GArgs{rest}, os))

def unrecs(us: List<&2, String>) -> List<&2, String>:
  match us:
    case Nil{}:
      Nil{}
    case u <> rest:
      err.unrec(u) <> unrecs(rest)

def count.chars(args: List<&2, String>) -> Nat:
  match args:
    case Nil{}:
      1n
    case a <> rest:
      Nat.add(Nat.add(String.length(a), 2n), count.chars(rest))

def getopt.fin(a: Acc) -> Got:
  match a:
    case Acc{os, xs, us, es}:
      Got{os, xs, List.append(&2, String, es, unrecs(us))}

# getOpt Permute os args
def getopt(+os: List<&2, OptD>, +args: List<&2, String>) -> Got:
  getopt.fin(go(Nat.mul(2n, count.chars(args)), GArgs{args}, os))


def getopt.non(g: Got) -> List<&2, String>:
  match g:
    case Got{_, xs, _}:
      xs
