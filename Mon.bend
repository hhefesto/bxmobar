# xmobar's monitors in Bend, each as a pure step: what it read (a file's
# text, a directory's names, statfs's numbers) and what it kept from its
# last run, to its text and what it keeps for the next. Main.bend does the
# reading; this is Xmobar.Plugins.Monitors.{MultiCpu, Mem, Swap, Net,
# Disk, MultiCoreTemp} and their Linux parts.

import Base
import ./Hs.bend as H
import ./Big.bend as B
import ./Float.bend as F
import ./GetOpt.bend as G
import ./Monitor.bend as M

# Shared ------------------------------------------------------------------------------

# a monitor's own options (after "--"), as parseOptsWith reads them: the
# options found in the order foldr applies them (the last first), or the
# errors
type Own is Data:
  Own{opts: List<&2, G.Opt>}
  OwnErr{text: String}

def own.of(g: G.Got) -> Own:
  match g:
    case G.Got{os, _, Nil{}}:
      Own{List.reverse(&2, G.Opt, os)}
    case G.Got{_, _, es}:
      OwnErr{M.user.error(es)}

def own(+ds: List<&2, G.OptD>, +argv: List<&2, String>) -> Own:
  own.of(G.getopt(ds, argv))

def long(+tag: U32, +name: String) -> G.OptD:
  G.OptD{tag, "", [name], G.ReqArg{""}, ""}

def flag(+tag: U32, +short: String, +name: String) -> G.OptD:
  G.OptD{tag, short, [name], G.NoArg{}, ""}

def val.or(m: Maybe<&2, String>) -> String:
  match m:
    case Some{v}:
      v
    case None{}:
      ""

# Int to Float (an Int32)
def i2f(+n: U32) -> F32:
  M.i2f(n)

def sum.f(xs: List<&2, F32>, +acc: F32) -> F32:
  match xs:
    case Nil{}:
      acc
    case x <> rest:
      sum.f(rest, (acc + x : F32))

# readDec at Int: the leading digits' value (0 if none), modulo 2^32
def read.dec(+s: String) -> U32:
  H.digits.val(10, String.take(s, H.digits.len(1, s)), 0)

def strs.cons(+x: String, xs: List<&2, String>) -> List<&2, String>:
  x <> xs

# MultiCpu ----------------------------------------------------------------------------

def cpu.variables() -> List<&2, String>:
  ["bar", "vbar", "ipat", "total", "user", "nice", "system", "idle"]

# load-icon-pattern, load-icon-patterns, fallback-icon-pattern, contiguous-icons
def multicpu.options() -> List<&2, G.OptD>:
  [long(0, "load-icon-pattern"), long(1, "load-icon-patterns"), long(2, "fallback-icon-pattern"), flag(3, "", "contiguous-icons")]

type CpuOpts is Data:
  CpuOpts{pat: Maybe<&2, String>, pats: List<&2, String>, fallback: Maybe<&2, String>, contiguous: Bool}

def cpu.opt(o: G.Opt, +k: CpuOpts) -> CpuOpts:
  match o k:
    case G.Opt{+t, +v} CpuOpts{p, ps, f, c}:
      H.Bool.if(CpuOpts, U32.is_eq(t, 0), _ => CpuOpts{Some{val.or(v)}, ps, f, c}, _ => H.Bool.if(CpuOpts, U32.is_eq(t, 1), _ => CpuOpts{p, val.or(v) <> ps, f, c}, _ => H.Bool.if(CpuOpts, U32.is_eq(t, 2), _ => CpuOpts{p, ps, Some{val.or(v)}, c}, _ => CpuOpts{p, ps, f, True{}})))

def cpu.opts(os: List<&2, G.Opt>, k: CpuOpts) -> CpuOpts:
  match os:
    case Nil{}:
      k
    case o <> rest:
      cpu.opts(rest, cpu.opt(o, k))

def cpu.line.go(ws: List<&2, String>) -> List<&2, U32>:
  match ws:
    case Nil{}:
      Nil{}
    case w <> rest:
      read.dec(w) <> cpu.line.go(rest)

def first.word(ws: List<&2, String>) -> String:
  match ws:
    case Con{w, _}:
      w
    case Nil{}:
      ""

def after.first(ws: List<&2, String>) -> List<&2, String>:
  match ws:
    case Con{_, rest}:
      rest
    case Nil{}:
      Nil{}

# the lines of /proc/stat that start with cpu (takeWhile), each line's numbers
def cpu.lines(ls: List<&2, String>) -> List<&2, List<&2, U32>>:
  match ls:
    case Nil{}:
      Nil{}
    case Con{+l, rest}:
      +ws = H.words(l)
      H.Bool.if(List<&2, List<&2, U32>>, Bool.and(Bool.not(List.is_empty(&2, String, ws)), String.starts_with(first.word(ws), "cpu")), _ => cpu.line.go(after.first(ws)) <> cpu.lines(rest), _ => Nil{})

# cpuData: /proc/stat's cpu lines
def cpu.data(+text: String) -> List<&2, List<&2, U32>>:
  cpu.lines(H.lines(text))

def difs(b: List<&2, U32>, a: List<&2, U32>) -> List<&2, F32>:
  match b a:
    case Con{x, xs} Con{y, ys}:
      i2f(U32.sub(x, y)) <> difs(xs, ys)
    case _ _:
      Nil{}

def div.all(xs: List<&2, F32>, +t: F32) -> List<&2, F32>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      (x / t : F32) <> div.all(rest, t)

# percent b a: the first four differences over their total
def percent(b: List<&2, U32>, a: List<&2, U32>) -> List<&2, F32>:
  +d = difs(b, a)
  +tot = sum.f(d, 0.0)
  H.Bool.if(List<&2, F32>, F32.is_gt(tot, 0.0), _ => div.all(List.take(&2, F32, d, 4n), tot), _ => [0.0, 0.0, 0.0, 0.0])

def percents(bs: List<&2, List<&2, U32>>, as: List<&2, List<&2, U32>>) -> List<&2, List<&2, F32>>:
  match bs as:
    case Con{b, brest} Con{a, arest}:
      percent(b, a) <> percents(brest, arest)
    case _ _:
      Nil{}

def nth.s(n: Nat, xs: List<&2, String>) -> Maybe<&2, String>:
  match n xs:
    case 0n x <> _:
      Some{x}
    case 1n+m _ <> rest:
      nth.s(m, rest)
    case _ _:
      None{}

def try.pattern(o: CpuOpts, +i: Nat) -> Maybe<&2, String>:
  match o:
    case CpuOpts{p, +ps, f, _}:
      H.Bool.if(Maybe<&2, String>, Nat.is_eq(i, 0n), _ => p, _ => H.Bool.if(Maybe<&2, String>, Nat.is_le(i, List.length(&2, String, ps)), _ => nth.s(Nat.sub(i, 1n), ps), _ => f))

def cpu.format(+c: M.C, +o: CpuOpts, +i: Nat, +xs: List<&2, F32>) -> List<&2, String>:
  +t = sum.f(List.take(&2, F32, xs, 3n), 0.0)
  H.Bool.if(List<&2, String>, Nat.is_lt(List.length(&2, F32, xs), 4n), _ => M.percents.colors(c, [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]), _ => M.bar(c, (100.0 * t : F32), t) <> (M.vbar(c, (100.0 * t : F32), t) <> (M.ipat(try.pattern(o, i), t) <> M.percents.colors(c, t <> xs))))

def cpus.format(xs: List<&2, List<&2, F32>>, +c: M.C, +o: CpuOpts, +i: Nat) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      List.append(&2, String, cpu.format(c, o, i, x), cpus.format(rest, c, o, 1n+i))

def split.every8(fuel: Nat, +xs: List<&2, String>) -> List<&2, List<&2, String>>:
  match fuel:
    case 0n:
      Nil{}
    case 1n+f:
      H.Bool.if(List<&2, List<&2, String>>, List.is_empty(&2, String, xs), _ => Nil{}, _ => List.take(&2, String, xs, 8n) <> split.every8(f, List.drop(&2, String, xs, 8n)))

def heads(xss: List<&2, List<&2, String>>) -> List<&2, String>:
  match xss:
    case Nil{}:
      Nil{}
    case Con{x, _} <> rest:
      x <> heads(rest)
    case Nil{} <> rest:
      heads(rest)

def tails(xss: List<&2, List<&2, String>>) -> List<&2, List<&2, String>>:
  match xss:
    case Nil{}:
      Nil{}
    case Con{_, xs} <> rest:
      xs <> tails(rest)
    case Nil{} <> rest:
      tails(rest)

def all.empty(xss: List<&2, List<&2, String>>) -> Bool:
  match xss:
    case Nil{}:
      True{}
    case Nil{} <> rest:
      all.empty(rest)
    case _ <> _:
      False{}

# Data.List's transpose (rows of any length)
def transpose(fuel: Nat, +xss: List<&2, List<&2, String>>) -> List<&2, List<&2, String>>:
  match fuel:
    case 0n:
      Nil{}
    case 1n+f:
      H.Bool.if(List<&2, List<&2, String>>, all.empty(xss), _ => Nil{}, _ => heads(xss) <> transpose(f, tails(xss)))

def join.with(+contiguous: Bool, xs: List<&2, String>) -> String:
  match contiguous:
    case True{}:
      String.concat(xs)
    case False{}:
      M.join(" ", xs)

def joins(+contiguous: Bool, xss: List<&2, List<&2, String>>) -> List<&2, String>:
  match xss:
    case Nil{}:
      Nil{}
    case xs <> rest:
      join.with(contiguous, xs) <> joins(contiguous, rest)

# formatAutoCpus: each variable over the cpus after the first (all of them)
def cpus.auto(o: CpuOpts, +l: List<&2, String>) -> List<&2, String>:
  match o:
    case CpuOpts{_, _, _, +contiguous}:
      +n = List.length(&2, String, l)
      H.Bool.if(List<&2, String>, Nat.is_eq(n, 0n), _ => ["", "", "", "", "", "", "", ""], _ => joins(contiguous, transpose(Nat.add(n, 1n), List.drop(&2, List<&2, String>, split.every8(Nat.add(n, 1n), l), 1n))))

def prefix.all(+p: String, xs: List<&2, String>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      (p ++ x) <> prefix.all(p, rest)

def suffix.all(xs: List<&2, String>, +s: String) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      (x ++ s) <> suffix.all(rest, s)

def cpu.names.go(n: Nat, +i: Nat) -> List<&2, String>:
  match n:
    case 0n:
      Nil{}
    case 1n+m:
      List.append(&2, String, suffix.all(cpu.variables(), Nat.show(i)), cpu.names.go(m, 1n+i))

# the export list as far as n cpus (the aggregate and n - 1 others)
def cpu.names(+n: Nat) -> List<&2, String>:
  List.append(&2, String, prefix.all("auto", cpu.variables()), List.append(&2, String, cpu.variables(), cpu.names.go(n, 0n)))

def multicpu.config() -> M.MC:
  M.mk("Cpu: <total>%", Nil{})

def multicpu.go(+c: M.C, o: Own, +ps: List<&2, List<&2, F32>>) -> String:
  match o:
    case Own{os}:
      +k = cpu.opts(os, CpuOpts{None{}, Nil{}, None{}, False{}})
      +l = cpus.format(ps, c, k, 0n)
      M.template(M.with.export(c, cpu.names(List.length(&2, List<&2, F32>, ps))), List.append(&2, String, cpus.auto(k, l), l))
    case OwnErr{t}:
      t

# a run: the monitor's config, its arguments after the common options,
# /proc/stat's numbers last time and now
def multicpu.run(+c: M.C, +argv: List<&2, String>, +before: List<&2, List<&2, U32>>, +now: List<&2, List<&2, U32>>) -> String:
  multicpu.go(c, own(multicpu.options(), argv), percents(now, before))

# Memory ------------------------------------------------------------------------------

type MemOpts is Data:
  MemOpts{used: Maybe<&2, String>, free: Maybe<&2, String>, avail: Maybe<&2, String>, scale: Maybe<&2, F32>}

def mem.options() -> List<&2, G.OptD>:
  [long(0, "used-icon-pattern"), long(1, "free-icon-pattern"), long(2, "available-icon-pattern"), long(3, "scale")]

def mem.opt(o: G.Opt, +k: MemOpts) -> MemOpts:
  match o k:
    case G.Opt{+t, +v} MemOpts{u, f, a, s}:
      H.Bool.if(MemOpts, U32.is_eq(t, 0), _ => MemOpts{Some{val.or(v)}, f, a, s}, _ => H.Bool.if(MemOpts, U32.is_eq(t, 1), _ => MemOpts{u, Some{val.or(v)}, a, s}, _ => H.Bool.if(MemOpts, U32.is_eq(t, 2), _ => MemOpts{u, f, Some{val.or(v)}, s}, _ => MemOpts{u, f, a, F.read(val.or(v))})))

def mem.opts(os: List<&2, G.Opt>, k: MemOpts) -> MemOpts:
  match os:
    case Nil{}:
      k
    case o <> rest:
      mem.opts(rest, mem.opt(o, k))

def mem.config() -> M.MC:
  M.mk("Mem: <usedratio>% (<cache>M)", ["usedbar", "usedvbar", "usedipat", "freebar", "freevbar", "freeipat", "availablebar", "availablevbar", "availableipat", "usedratio", "freeratio", "availableratio", "total", "free", "buffer", "cache", "available", "used"])

type KF is Data:
  KF{k: String, v: Maybe<&2, F32>}

def kf.of(ws: List<&2, String>) -> KF:
  match ws:
    case Con{k, Con{+v, _}}:
      KF{k, F.read(v)}
    case Con{k, Nil{}}:
      KF{k, None{}}
    case Nil{}:
      KF{"", None{}}

def kfs(ls: List<&2, String>) -> List<&2, KF>:
  match ls:
    case Nil{}:
      Nil{}
    case l <> rest:
      kf.of(H.words(l)) <> kfs(rest)

# M.! and findWithDefault in Map.fromList (the last of a key); a value
# that did not read is None
type Look is Data:
  Found{v: Maybe<&2, F32>}
  Missing{}

def look(+k: String, kvs: List<&2, KF>, acc: Look) -> Look:
  match kvs:
    case Nil{}:
      acc
    case KF{+key, +v} <> rest:
      look(k, rest, Bool.pick(Look, String.eq(k, key), Found{v}, acc))

def mem.div(m: Maybe<&2, F32>) -> Maybe<&2, F32>:
  match m:
    case Some{x}:
      Some{(x / 1024.0 : F32)}
    case None{}:
      None{}

type Mem is Data:
  Mem{total: F32, free: F32, buffer: F32, cache: F32, avail: F32}
  MemErr{text: String}

def mem.need(l: Look, +k: String) -> Maybe<&2, F32>:
  match l:
    case Found{v}:
      mem.div(v)
    case Missing{}:
      None{}

def mem.avail(+t: F32, +f: F32, +b: F32, +c: F32, a: Look) -> Mem:
  match a:
    case Missing{}:
      Mem{t, f, b, c, (((f + b : F32) + c : F32))}
    case Found{Some{x}}:
      Mem{t, f, b, c, (x / 1024.0 : F32)}
    case Found{None{}}:
      MemErr{"error: Prelude.read: no parse"}

def mem.of4(t: Maybe<&2, F32>, f: Maybe<&2, F32>, b: Maybe<&2, F32>, c: Maybe<&2, F32>, a: Look) -> Mem:
  match t f b c:
    case Some{+t} Some{+f} Some{+b} Some{+c}:
      mem.avail(t, f, b, c, a)
    case _ _ _ _:
      MemErr{"error: Prelude.read: no parse"}

def look.has(l: Look) -> Bool:
  match l:
    case Found{_}:
      True{}
    case Missing{}:
      False{}

def has.all(+kvs: List<&2, KF>, ks: List<&2, String>) -> Bool:
  match ks:
    case Nil{}:
      True{}
    case k <> rest:
      Bool.and(look.has(look(k, kvs, Missing{})), has.all(kvs, rest))

# parseMEM: the first 8 lines of /proc/meminfo, in MB
def mem.parse(+text: String) -> Mem:
  +kvs = kfs(List.take(&2, String, H.lines(text), 8n))
  H.Bool.if(Mem, has.all(kvs, ["MemTotal:", "MemFree:", "Buffers:", "Cached:"]), _ => mem.of4(mem.need(look("MemTotal:", kvs, Missing{}), ""), mem.need(look("MemFree:", kvs, Missing{}), ""), mem.need(look("Buffers:", kvs, Missing{}), ""), mem.need(look("Cached:", kvs, Missing{}), ""), look("MemAvailable:", kvs, Missing{})), _ => MemErr{"error: Map.!: given key is not an element in the map"})

def scale.of(m: Maybe<&2, F32>) -> F32:
  match m:
    case Some{x}:
      x
    case None{}:
      1.0

def mem.values(+c: M.C, +sc: F32, xs: List<&2, F32>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case Con{x, rest}:
      +y = (x / sc : F32)
      M.with.colors(c, F.fixed(U32.to_nat(M.c.dd(c)), y), y) <> mem.values(c, sc, rest)

def mem.bars(+c: M.C, p: Maybe<&2, String>, +x: F32) -> List<&2, String>:
  [M.bar(c, (100.0 * x : F32), x), M.vbar(c, (100.0 * x : F32), x), M.ipat(p, x)]

def mem.format(+c: M.C, k: MemOpts, m: Mem) -> String:
  match k m:
    case MemOpts{u, f, a, s} Mem{+t, +fr, +b, +ca, +av}:
      +used = (t - av : F32)
      +r = (used / t : F32)
      +frr = (fr / t : F32)
      +ar = (av / t : F32)
      M.template(c, List.concat(&2, String, [mem.bars(c, u, r), mem.bars(c, f, frr), mem.bars(c, a, ar), M.percents.colors(c, [r, frr, ar]), mem.values(c, scale.of(s), [t, fr, b, ca, av, used])]))
    case _ MemErr{e}:
      e

def mem.run.go(+c: M.C, o: Own, m: Mem) -> String:
  match o:
    case Own{os}:
      mem.format(c, mem.opts(os, MemOpts{None{}, None{}, None{}, Some{1.0}}), m)
    case OwnErr{t}:
      t

def mem.run(+c: M.C, +argv: List<&2, String>, +text: String) -> String:
  mem.run.go(c, own(mem.options(), argv), mem.parse(text))

# Swap --------------------------------------------------------------------------------

def swap.config() -> M.MC:
  M.mk("Swap: <usedratio>%", ["usedratio", "total", "used", "free"])

def second.or(ws: List<&2, String>) -> String:
  match ws:
    case Con{x, _}:
      x
    case Nil{}:
      ""

# the second word of the first line whose first word is k, in MB
def swap.get(+k: String, ls: List<&2, String>) -> Maybe<&2, F32>:
  match ls:
    case Nil{}:
      None{}
    case Con{+l, rest}:
      +ws = H.words(l)
      H.Bool.if(Maybe<&2, F32>, String.eq(first.word(ws), k), _ => mem.div(F.read(second.or(after.first(ws)))), _ => swap.get(k, rest))

def swap.format(+c: M.C, t: Maybe<&2, F32>, f: Maybe<&2, F32>) -> String:
  match t f:
    case Some{+tot} Some{+free}:
      +used = (tot - free : F32)
      +dd = U32.to_nat(M.c.dd(c))
      M.template(c, M.percent.colors(c, (used / tot : F32)) <> [M.with.colors(c, F.fixed(dd, tot), tot), M.with.colors(c, F.fixed(dd, used), used), M.with.colors(c, F.fixed(dd, free), free)])
    case _ _:
      "error: Prelude.read: no parse"

# runSwap reads no options of its own
def swap.run(+c: M.C, +text: String) -> String:
  +ls = H.lines(text)
  swap.format(c, swap.get("SwapTotal:", ls), swap.get("SwapFree:", ls))

