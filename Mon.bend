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

# Network and DynNetwork ----------------------------------------------------------------

# NetDev: a device's name and its counters (Word64s as Bigs), NI when it
# is down; NA when there is none
type Dev is Data:
  DevUp{name: String, rx: List<&2, U32>, tx: List<&2, U32>}
  DevDown{name: String}
  DevNA{}

# the rates: N d (ND rx tx), N d NI, NA
type Rate is Data:
  RateUp{name: String, rx: F32, tx: F32}
  RateDown{name: String}
  RateNA{}

def net.options() -> List<&2, G.OptD>:
  [long(0, "rx-icon-pattern"), long(1, "tx-icon-pattern"), long(2, "up"), long(3, "devices")]

type NetOpts is Data:
  NetOpts{rxp: Maybe<&2, String>, txp: Maybe<&2, String>, devs: Maybe<&2, List<&2, String>>, up: String}

def split.comma(s: String) -> List<&2, String>:
  String.split(s, ',')

def net.opt(o: G.Opt, +k: NetOpts) -> NetOpts:
  match o k:
    case G.Opt{+t, +v} NetOpts{r, x, d, u}:
      H.Bool.if(NetOpts, U32.is_eq(t, 0), _ => NetOpts{Some{val.or(v)}, x, d, u}, _ => H.Bool.if(NetOpts, U32.is_eq(t, 1), _ => NetOpts{r, Some{val.or(v)}, d, u}, _ => H.Bool.if(NetOpts, U32.is_eq(t, 2), _ => NetOpts{r, x, d, val.or(v)}, _ => NetOpts{r, x, Some{split.comma(val.or(v))}, u})))

def net.opts(os: List<&2, G.Opt>, k: NetOpts) -> NetOpts:
  match os:
    case Nil{}:
      k
    case o <> rest:
      net.opts(rest, net.opt(o, k))

def net.config() -> M.MC:
  M.mk("<dev>: <rx>KB|<tx>KB", ["dev", "rx", "tx", "rxbar", "rxvbar", "rxipat", "txbar", "txvbar", "txipat", "up"])

# a line of /proc/net/dev cut at spaces and colons, the empty pieces gone
def cut.go(s: String, +cur: String, +acc: List<&2, String>) -> List<&2, String>:
  match s:
    case SNil{}:
      List.reverse(&2, String, Bool.pick(List<&2, String>, String.is_empty(cur), acc, String.reverse(cur) <> acc))
    case SCon{+c, +rest}:
      H.Bool.if(List<&2, String>, Bool.or(H.c.is(c, 32), H.c.is(c, 58)), _ => cut.go(rest, "", Bool.pick(List<&2, String>, String.is_empty(cur), acc, String.reverse(cur) <> acc)), _ => cut.go(rest, SCon{c, cur}, acc))

def cut(s: String) -> List<&2, String>:
  cut.go(s, "", Nil{})

# read at Word64 of a column ("" is 0): None where it does not read
def word64(+s: String) -> Maybe<&2, List<&2, U32>>:
  H.Bool.if(Maybe<&2, List<&2, U32>>, String.is_empty(s), _ => Some{Nil{}}, _ => Bool.pick(Maybe<&2, List<&2, U32>>, Bool.and(Nat.is_gt(H.digits.len(1, s), 0n), Nat.is_eq(H.digits.len(1, s), String.length(s))), Some{B.norm(List.take(&2, U32, B.dec.big(s), 4n))}, None{}))

# one device's line: its name and the receive and transmit bytes
type Line is Data:
  Line{name: String, rx: Maybe<&2, List<&2, U32>>, tx: Maybe<&2, List<&2, U32>>}

def line.of(+cols: List<&2, String>) -> Line:
  Line{val.or(nth.s(0n, cols)), word64(val.or(nth.s(1n, cols))), word64(val.or(nth.s(9n, cols)))}

def lines.of(ls: List<&2, String>) -> List<&2, Line>:
  match ls:
    case Nil{}:
      Nil{}
    case l <> rest:
      line.of(cut(l)) <> lines.of(rest)

def dev.of(+up: Bool, +n: String, rx: Maybe<&2, List<&2, U32>>, tx: Maybe<&2, List<&2, U32>>) -> Dev:
  match up rx tx:
    case False{} _ _:
      DevDown{n}
    case True{} Some{r} Some{t}:
      DevUp{n, r, t}
    case True{} _ _:
      DevDown{n}

# findNetDev: the first line of the device; up: its operstate's first
# line is up or unknown
def find.dev(+name: String, ls: List<&2, Line>, +up: Bool) -> Dev:
  match ls:
    case Nil{}:
      DevNA{}
    case Line{+n, rx, tx} <> rest:
      H.Bool.if(Dev, String.eq(n, name), _ => dev.of(up, n, rx, tx), _ => find.dev(name, rest, up))

# /proc/net/dev's devices (its first two lines are headers)
def net.devs(+text: String) -> List<&2, Line>:
  lines.of(List.drop(&2, String, H.lines(text), 2n))

def operstate.up(+text: String) -> Bool:
  +l = first.word(H.lines(text))
  Bool.or(String.eq(l, "up"), String.eq(l, "unknown"))

# rate da db: takeDigits 2 (fromIntegral (db - da) / scx), Word64's
# difference modulo 2^64
def rate(+a: List<&2, U32>, +b: List<&2, U32>, +scx: F32) -> F32:
  +d = Bool.pick(List<&2, U32>, B.is.le(a, b), B.sub(b, a), B.sub(B.add(b, B.two64()), a))
  M.take.digits(2n, (F.of.big(d) / scx : F32))

# diffRate n0 n1, scx the seconds between (1 when not more than 0)
def diff.rate(a: Dev, b: Dev, +scx0: F32) -> Rate:
  match a b:
    case DevUp{n, ra, ta} DevUp{_, rb, tb}:
      +scx = Bool.pick(F32, F32.is_gt(scx0, 0.0), scx0, 1.0)
      RateUp{n, rate(ra, rb, scx), rate(ta, tb, scx)}
    case DevDown{n} _:
      RateDown{n}
    case _ DevDown{n}:
      RateDown{n}
    case _ _:
      RateNA{}

def byte.units(+v: F32) -> String:
  H.Bool.if(String, F32.is_lt(v, 1024.0), _ => "B/s", _ => H.Bool.if(String, F32.is_lt(v, 1048576.0), _ => "KB/s", _ => H.Bool.if(String, F32.is_lt(v, 1073741824.0), _ => "MB/s", _ => "GB/s")))

def byte.value(+v: F32) -> F32:
  H.Bool.if(F32, F32.is_lt(v, 1024.0), _ => v, _ => H.Bool.if(F32, F32.is_lt(v, 1048576.0), _ => (v / 1024.0 : F32), _ => H.Bool.if(F32, F32.is_lt(v, 1073741824.0), _ => (v / 1048576.0 : F32), _ => (v / 1073741824.0 : F32))))

# formatNet: the value (with its unit when -S), its log bar, vbar, ipat
def net.format(+c: M.C, p: Maybe<&2, String>, +d: F32) -> List<&2, String>:
  +dd = U32.to_nat(M.c.dd(c))
  +s = Bool.pick(String, M.c.suffix(c), F.fixed(dd, byte.value(d)) ++ byte.units(d), F.fixed(dd, (d / 1024.0 : F32)))
  [M.with.colors(c, s, d), M.log.bar(c, 0.9, d), M.log.vbar(c, 0.9, d), M.log.ipat(c, p, 0.9, d)]

def nth.or(+n: Nat, xs: List<&2, String>) -> String:
  val.or(nth.s(n, xs))

# printNet
def net.print(+c: M.C, k: NetOpts, r: Rate) -> String:
  match k r:
    case NetOpts{rp, tp, _, up} RateUp{d, rx, tx}:
      +a = net.format(c, rp, rx)
      +b = net.format(c, tp, tx)
      M.template(c, [d, nth.or(0n, a), nth.or(0n, b), nth.or(1n, a), nth.or(2n, a), nth.or(3n, a), nth.or(1n, b), nth.or(2n, b), nth.or(3n, b), up])
    case _ RateDown{_}:
      ""
    case _ RateNA{}:
      M.c.na(c)

def net.defaults() -> NetOpts:
  NetOpts{None{}, None{}, None{}, "+"}

# Ord at NetDev's rates: NA lowest, then down, then up by rx, then tx
# (Float's compare: GT when either is NaN)
def rate.le(a: Rate, b: Rate) -> Bool:
  match a b:
    case RateNA{} _:
      True{}
    case _ RateNA{}:
      False{}
    case RateDown{_} _:
      True{}
    case _ RateDown{_}:
      False{}
    case RateUp{_, +x1, +y1} RateUp{_, +x2, +y2}:
      H.Bool.if(Bool, F32.is_lt(x1, x2), _ => True{}, _ => H.Bool.if(Bool, F32.is_eq(x1, x2), _ => Bool.or(F32.is_lt(y1, y2), F32.is_eq(y1, y2)), _ => False{}))

# maximum: foldl1 max, max x y = if x <= y then y else x (the later of
# equals)
def rates.max(rs: List<&2, Rate>, +acc: Rate) -> Rate:
  match rs:
    case Nil{}:
      acc
    case Con{+r, rest}:
      rates.max(rest, Bool.pick(Rate, rate.le(acc, r), r, acc))

def rates.maximum(rs: List<&2, Rate>) -> Maybe<&2, Rate>:
  match rs:
    case Nil{}:
      None{}
    case Con{r, rest}:
      Some{rates.max(rest, r)}

def in.list(+x: String, xs: List<&2, String>) -> Bool:
  match xs:
    case Nil{}:
      False{}
    case y <> rest:
      Bool.or(String.eq(x, y), in.list(x, rest))

type Named is Data:
  Named{name: String, r: Rate}

def named.name(n: Named) -> String:
  match n:
    case Named{s, _}:
      s

def keep.named(+only: List<&2, String>, xs: List<&2, Named>) -> List<&2, Named>:
  match xs:
    case Nil{}:
      Nil{}
    case Con{+x, rest}:
      +r = keep.named(only, rest)
      Bool.pick(List<&2, Named>, in.list(named.name(x), only), x <> r, r)

def named.rates(xs: List<&2, Named>) -> List<&2, Rate>:
  match xs:
    case Nil{}:
      Nil{}
    case Named{_, r} <> rest:
      r <> named.rates(rest)

# filterRefs: the devices the --devices list names, all if it names none
def filter.refs(d: Maybe<&2, List<&2, String>>, +xs: List<&2, Named>) -> List<&2, Named>:
  match d:
    case None{}:
      xs
    case Some{only}:
      +kept = keep.named(only, xs)
      Bool.pick(List<&2, Named>, List.is_empty(&2, Named, kept), xs, kept)

# What GHC 9.10 (the GHC xmobar 0.50 is built with) shows for maximum []: an
# ErrorCall with its call stack, through Foldable's maximum on lists
def ghc.maximum.empty() -> String:
  "Prelude.maximum: empty list\nCallStack (from HasCallStack):\n  error, called at libraries/ghc-internal/src/GHC/Internal/List.hs:2030:3 in ghc-internal:GHC.Internal.List\n  errorEmptyList, called at libraries/ghc-internal/src/GHC/Internal/List.hs:824:28 in ghc-internal:GHC.Internal.List\n  maximum, called at libraries/ghc-internal/src/GHC/Internal/Data/Foldable.hs:691:15 in ghc-internal:GHC.Internal.Data.Foldable"

def dyn.pick(+c: M.C, +k: NetOpts, m: Maybe<&2, Rate>) -> String:
  match m:
    case Some{r}:
      net.print(c, k, r)
    case None{}:
      "error: " ++ ghc.maximum.empty()

def net.opts.devs(k: NetOpts) -> Maybe<&2, List<&2, String>>:
  match k:
    case NetOpts{_, _, d, _}:
      d

def dyn.go(+c: M.C, o: Own, +xs: List<&2, Named>) -> String:
  match o:
    case Own{os}:
      +k = net.opts(os, net.defaults())
      dyn.pick(c, k, rates.maximum(named.rates(filter.refs(net.opts.devs(k), xs))))
    case OwnErr{t}:
      t

# DynNetwork's run: each device's rate since its last reading
def dyn.run(+c: M.C, +argv: List<&2, String>, +xs: List<&2, Named>) -> String:
  dyn.go(c, own(net.options(), argv), xs)

def net.run.go(+c: M.C, o: Own, r: Rate) -> String:
  match o:
    case Own{os}:
      net.print(c, net.opts(os, net.defaults()), r)
    case OwnErr{t}:
      t

# Network's run: one device's rate
def net.run(+c: M.C, +argv: List<&2, String>, r: Rate) -> String:
  net.run.go(c, own(net.options(), argv), r)

# DiskU -------------------------------------------------------------------------------

def disku.options() -> List<&2, G.OptD>:
  [long(0, "free-icon-pattern"), long(1, "used-icon-pattern"), flag(2, "c", "contiguous")]

type DiskOpts is Data:
  DiskOpts{freep: Maybe<&2, String>, usedp: Maybe<&2, String>, contiguous: Bool}

def disk.opt(o: G.Opt, +k: DiskOpts) -> DiskOpts:
  match o k:
    case G.Opt{+t, +v} DiskOpts{f, u, c}:
      H.Bool.if(DiskOpts, U32.is_eq(t, 0), _ => DiskOpts{Some{val.or(v)}, u, c}, _ => H.Bool.if(DiskOpts, U32.is_eq(t, 1), _ => DiskOpts{f, Some{val.or(v)}, c}, _ => DiskOpts{f, u, True{}}))

def disk.opts(os: List<&2, G.Opt>, k: DiskOpts) -> DiskOpts:
  match os:
    case Nil{}:
      k
    case o <> rest:
      disk.opts(rest, disk.opt(o, k))

def disku.config() -> M.MC:
  M.mk("", ["size", "free", "used", "freep", "usedp", "freebar", "freevbar", "freeipat", "usedbar", "usedvbar", "usedipat"])

type DP is Data:
  DP{dev: String, path: String}

def first.two(ws: List<&2, String>) -> DP:
  match ws:
    case Con{a, Con{b, _}}:
      DP{a, b}
    case _:
      DP{"", ""}

def mtab.keep(d: DP, +rest: List<&2, DP>) -> List<&2, DP>:
  match d:
    case DP{+a, b}:
      Bool.pick(List<&2, DP>, String.starts_with(a, "/dev/"), DP{a, b} <> rest, rest)

def mtab.go(ls: List<&2, String>) -> List<&2, DP>:
  match ls:
    case Nil{}:
      Nil{}
    case Con{l, rest}:
      mtab.keep(first.two(H.words(l)), mtab.go(rest))

# /etc/mtab's mounts of devices (their first two words, the device under
# /dev/), before they are canonicalized
def mtab(+text: String) -> List<&2, DP>:
  mtab.go(H.lines(text))

def is.req(+req: List<&2, String>, d: DP) -> Bool:
  match d:
    case DP{+dev, +path}:
      Bool.or(in.list(path, req), in.list(String.drop(dev, 5n), req))

def undev(d: DP) -> DP:
  match d:
    case DP{dev, p}:
      DP{String.drop(dev, 5n), p}

# the mounts the disks name (by device or by path), each device canonical
# (canonicalizePath's answer, given by the caller for each that exists)
def mounted(+req: List<&2, String>, ds: List<&2, DP>) -> List<&2, DP>:
  match ds:
    case Nil{}:
      Nil{}
    case Con{+d, rest}:
      +r = mounted(req, rest)
      Bool.pick(List<&2, DP>, is.req(req, d), undev(d) <> r, r)

# statfs's bytes: total, available (to users), used
type Stat is Data:
  Stat{total: List<&2, U32>, free: List<&2, U32>, used: List<&2, U32>}

def size.str(+n: List<&2, U32>) -> String:
  M.with.units(10n, 3n, 0, F.of.big(n))

type DT is Data:
  DT{dev: String, tmpl: String}

# findTempl: the template of the first disk named by the device or path
def find.templ(+dev: String, +path: String, ds: List<&2, DT>) -> String:
  match ds:
    case Nil{}:
      ""
    case DT{+d, +t} <> rest:
      Bool.pick(String, Bool.or(String.eq(d, dev), String.eq(d, path)), t, find.templ(dev, path, rest))

def dq.q.big(d: B.DQ) -> List<&2, U32>:
  match d:
    case B.DQ{q, _}:
      q

def disku.one(+c: M.C, k: DiskOpts, s: Stat) -> String:
  match k s:
    case DiskOpts{fp, up, _} Stat{+total, +free, +used}:
      +freep = H.Bool.if(List<&2, U32>, B.is.zero(total), _ => Nil{}, _ => dq.q.big(B.divmod.big(B.mul.small(free, 100), total)))
      +fp100 = F.of.big(freep)
      +up100 = (100.0 - fp100 : F32)
      +fr = (fp100 / 100.0 : F32)
      +ur = (1.0 - fr : F32)
      M.template(c, [size.str(total), M.with.colors(c, size.str(free), fp100), M.with.colors(c, size.str(used), up100), M.percent.colors(c, fr), M.percent.colors(c, ur), M.bar(c, fp100, fr), M.vbar(c, fp100, fr), M.ipat(fp, fr), M.bar(c, up100, ur), M.vbar(c, up100, ur), M.ipat(up, ur)])

type MS is Data:
  MS{d: DP, s: Stat}

def disku.all(+c: M.C, +k: DiskOpts, +disks: List<&2, DT>, ms: List<&2, MS>) -> List<&2, String>:
  match ms:
    case Nil{}:
      Nil{}
    case MS{DP{+dev, +path}, s} <> rest:
      disku.one(M.with.template(c, find.templ(dev, path, disks)), k, s) <> disku.all(c, k, disks, rest)

def disk.contiguous(k: DiskOpts) -> Bool:
  match k:
    case DiskOpts{_, _, c}:
      c

def disku.go(+c: M.C, o: Own, +disks: List<&2, DT>, ms: List<&2, MS>) -> String:
  match o:
    case Own{os}:
      +k = disk.opts(os, DiskOpts{None{}, None{}, False{}})
      join.with(disk.contiguous(k), disku.all(c, k, disks, ms))
    case OwnErr{t}:
      t

# runDiskU: each mount's statfs, each through its disk's template
def disku.run(+c: M.C, +argv: List<&2, String>, +disks: List<&2, DT>, ms: List<&2, MS>) -> String:
  disku.go(c, own(disku.options(), argv), disks, ms)

# MultiCoreTemp ------------------------------------------------------------------------

def coretemp.options() -> List<&2, G.OptD>:
  [long(0, "max-icon-pattern"), long(1, "avg-icon-pattern"), long(2, "mintemp"), long(3, "maxtemp"), long(4, "hwmon-path")]

# mintemp and maxtemp as read reads them (None: no parse)
type CTOpts is Data:
  CTOpts{maxp: Maybe<&2, String>, avgp: Maybe<&2, String>, mint: Maybe<&2, F32>, maxt: Maybe<&2, F32>, path: Maybe<&2, String>}

def ct.opt(o: G.Opt, +k: CTOpts) -> CTOpts:
  match o k:
    case G.Opt{+t, +v} CTOpts{a, b, mn, mx, p}:
      H.Bool.if(CTOpts, U32.is_eq(t, 0), _ => CTOpts{Some{val.or(v)}, b, mn, mx, p}, _ => H.Bool.if(CTOpts, U32.is_eq(t, 1), _ => CTOpts{a, Some{val.or(v)}, mn, mx, p}, _ => H.Bool.if(CTOpts, U32.is_eq(t, 2), _ => CTOpts{a, b, F.read(val.or(v)), mx, p}, _ => H.Bool.if(CTOpts, U32.is_eq(t, 3), _ => CTOpts{a, b, mn, F.read(val.or(v)), p}, _ => CTOpts{a, b, mn, mx, Some{val.or(v)}}))))

def ct.opts(os: List<&2, G.Opt>, k: CTOpts) -> CTOpts:
  match os:
    case Nil{}:
      k
    case o <> rest:
      ct.opts(rest, ct.opt(o, k))

def ct.defaults() -> CTOpts:
  CTOpts{None{}, None{}, Some{0.0}, Some{100.0}, None{}}

def ct.names.go(n: Nat, +i: Nat) -> List<&2, String>:
  match n:
    case 0n:
      Nil{}
    case 1n+m:
      ("core" ++ Nat.show(i)) <> ct.names.go(m, 1n+i)

def coretemp.config() -> M.MC:
  M.mk("Temp: <max>\u{b0}C - <maxpc>%", Nil{})

# numberedPathMatcher prefix suffix: prefix, one or more digits, suffix
def numbered(+prefix: String, +suffix: String, +p: String) -> Bool:
  +a = String.drop(p, String.length(prefix))
  +n = H.digits.len(1, a)
  Bool.and(String.starts_with(p, prefix), Bool.and(Nat.is_gt(n, 0n), String.eq(String.drop(a, n), suffix)))

# isLabelFromCore: a label file that starts with Core, Tdie or Tctl
def core.label(+text: String) -> Bool:
  +t = String.take(text, 4n)
  Bool.or(String.eq(t, "Core"), Bool.or(String.eq(t, "Tdie"), String.eq(t, "Tctl")))

# labelToCore: temp1_label to temp1_input
def label.input(+p: String) -> String:
  String.take(p, Nat.sub(String.length(p), 5n)) ++ "input"

def mem.div.k(m: Maybe<&2, F32>) -> Maybe<&2, F32>:
  match m:
    case Some{x}:
      Some{(x / 1000.0 : F32)}
    case None{}:
      None{}

def first.word.or(ls: List<&2, String>) -> String:
  match ls:
    case Con{l, _}:
      l
    case Nil{}:
      ""

# a core's temperature: read . head . lines, over 1000
def core.temp(+text: String) -> Maybe<&2, F32>:
  mem.div.k(F.read(first.word.or(H.lines(text))))

def max.f(xs: List<&2, F32>, +acc: F32) -> F32:
  match xs:
    case Nil{}:
      acc
    case Con{+x, rest}:
      max.f(rest, Bool.pick(F32, F32.is_le(acc, x), x, acc))

def temps.colors(+c: M.C, xs: List<&2, F32>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case Con{+x, rest}:
      M.with.colors(c, F.show.round(x), x) <> temps.colors(c, rest)

def ct.side(+c: M.C, p: Maybe<&2, String>, +t: F32, +pc: F32) -> List<&2, String>:
  [M.with.colors(c, F.show.round(t), t), M.with.colors(c, F.show.round((100.0 * pc : F32)), t), M.bar(c, t, pc), M.vbar(c, t, pc), M.ipat(p, pc)]

def ct.format(+c: M.C, k: CTOpts, +ts: List<&2, F32>) -> String:
  match k ts:
    case CTOpts{mp, ap, Some{+mn}, Some{+mx}, _} Con{+t0, trest}:
      +dom = (mx - mn : F32)
      +mxc = max.f(trest, t0)
      +avg = (sum.f(ts, 0.0) / U32.to_f32(U32.from_nat(List.length(&2, F32, ts))) : F32)
      M.template(M.with.export(c, List.append(&2, String, ["max", "maxpc", "maxbar", "maxvbar", "maxipat", "avg", "avgpc", "avgbar", "avgvbar", "avgipat"], ct.names.go(List.length(&2, F32, ts), 0n))), List.concat(&2, String, [ct.side(c, mp, mxc, ((mxc - mn : F32) / dom : F32)), ct.side(c, ap, avg, ((avg - mn : F32) / dom : F32)), temps.colors(c, ts)]))
    case _ Nil{}:
      "error: " ++ ghc.maximum.empty()
    case _ _:
      "error: Prelude.read: no parse"

def ct.go(+c: M.C, o: Own, +ts: List<&2, F32>) -> String:
  match o:
    case Own{os}:
      ct.format(c, ct.opts(os, ct.defaults()), ts)
    case OwnErr{t}:
      t

def ct.path.opts(k: CTOpts) -> Maybe<&2, String>:
  match k:
    case CTOpts{_, _, _, _, p}:
      p

def ct.path.of(o: Own) -> Maybe<&2, String>:
  match o:
    case Own{os}:
      ct.path.opts(ct.opts(os, ct.defaults()))
    case OwnErr{_}:
      None{}

# the --hwmon-path option, if any (the reader looks there instead)
def ct.path(+argv: List<&2, String>) -> Maybe<&2, String>:
  ct.path.of(own(coretemp.options(), argv))

# runCT: the cores' temperatures, read in their files' order
def coretemp.run(+c: M.C, +argv: List<&2, String>, +ts: List<&2, F32>) -> String:
  ct.go(c, own(coretemp.options(), argv), ts)


# The cores' temperatures as cTData reads them: a file that does not open
# fails the run (openFile's error, before any is read as a number), then a
# text that does not read as a number fails it (read's error, when formatCT
# forces it)
type Temps is Data:
  TempsOk{ts: List<&2, F32>}
  TempsErr{e: String}

def temps.cons(m: Maybe<&2, F32>, r: Maybe<&2, List<&2, F32>>) -> Maybe<&2, List<&2, F32>>:
  match m r:
    case Some{x} Some{xs}:
      Some{x <> xs}
    case _ _:
      None{}

def core.temps.go(xs: List<&2, String>) -> Maybe<&2, List<&2, F32>>:
  match xs:
    case Nil{}:
      Some{Nil{}}
    case Con{x, rest}:
      temps.cons(core.temp(x), core.temps.go(rest))

def core.temps.of(m: Maybe<&2, List<&2, F32>>) -> Temps:
  match m:
    case Some{ts}:
      TempsOk{ts}
    case None{}:
      TempsErr{"Prelude.read: no parse"}

def core.temps(xs: List<&2, String>) -> Temps:
  core.temps.of(core.temps.go(xs))

def ct.err(o: Own, +e: String) -> String:
  match o:
    case Own{_}:
      "error: " ++ e
    case OwnErr{t}:
      t

# runCT on what was read: its own options' error first, as parseOptsWith
# fails before parseCT reads
def coretemp.run.or(+c: M.C, +argv: List<&2, String>, r: Temps) -> String:
  match r:
    case TempsOk{+ts}:
      coretemp.run(c, argv, ts)
    case TempsErr{+e}:
      ct.err(own(coretemp.options(), argv), e)
