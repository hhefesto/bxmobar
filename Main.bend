# xmobar-bend: the bar. Xmobar.App.Main, Run.Loop, Text.Loop and X11.Loop:
# the configuration found and read, its template cut into its parts, each
# part's command started at its rate (tenths of a second, on the
# multiples of its rate, as Run.Timer's coalesced timers fire), and the
# bar shown again whenever a part's text changes: as a line of text (-T,
# or the config's textOutput), else in its X11 window.
#
# One def is the whole run, loop (text) or x.loop (X11): it runs the parts
# that are due, shows the bar if it changed, then waits for the next part
# due, a line on standard input (when the template has a StdinReader) or
# an X event. Bend asks every loop to end, so it counts down a fuel of
# 10^9 steps (years of a bar); standard input's end ends it sooner, as
# StdinReader's exitImmediately.
#
# Build (x11.c needs X11, Cairo and Pango's flags: the flake's x shell):
#
#   nix build            # result/bin/xmobar-bend
#   nix develop .#x -c bend Main.bend -o xmobar-bend
#   ./xmobar-bend [-T] [config]

import Base
import ./Hs.bend as H
import ./Big.bend as B
import ./Float.bend as F
import ./Template.bend as T
import ./Monitor.bend as M
import ./Mon.bend as N
import ./Read.bend as R
import ./Config.bend as C
import ./Date.bend as D
import ./Sys.bend as S
import ./Win.bend as W
import ./Draw.bend as Dr
import ./X.bend as X
import ./Strip.bend as Sp
import ./Opts.bend as O
import ./GetOpt.bend as G

# Time ---------------------------------------------------------------------------------

# the wall clock: seconds (their low and high words) and nanoseconds
type Now is Data:
  Now{sec: U32, hi: U32, ns: U32}

def now.of(ws: List<&2, U32>) -> Now:
  match ws:
    case lo <> hi <> ns <> _:
      Now{lo, hi, ns}
    case _:
      Now{0, 0, 0}

def clock() -> IO(Now):
  do IO<Now>:
    ws : List<&2, U32> <- S.now()
    IO.pure(Now, now.of(ws))

# tenths of a second since the run began (sec0 its first second)
def tenths(n: Now, +sec0: U32) -> U32:
  match n:
    case Now{s, _, ns}:
      U32.add(U32.mul(U32.sub(s, sec0), 10), U32.div(ns, 100000000))

# the tenths since the epoch modulo r (where a period of r falls; the
# seconds' low word is the whole of them until 2106)
def phase(n: Now, +r: U32) -> U32:
  match n:
    case Now{s, _, ns}:
      U32.mod(U32.add(U32.mul(U32.mod(s, r), 10), U32.div(ns, 100000000)), r)

# the seconds from a to b, as a Float (realToFrac at diffUTCTime)
def seconds(a: Now, b: Now) -> F32:
  match a b:
    case Now{s0, _, n0} Now{s1, _, n1}:
      ((U32.to_f32(U32.sub(s1, s0)) + ((M.i2f(U32.sub(n1, n0))) / 1000000000.0 : F32) : F32))

def never() -> U32:
  4294967295

# The parts -------------------------------------------------------------------------------

# what a monitor keeps between its runs
type Keep is Data:
  KNone{}
  KCpu{prev: List<&2, List<&2, U32>>}
  KNet{refs: List<&2, Ref>}

# a network device's last reading and when
type Ref is Data:
  Ref{name: String, kept: N.Kept, t: Now}

# a part's command: none (constant text), a monitor (its constructor, its
# arguments, its value read), Date, a StdinReader, Com, or not ported
type Job is Data:
  JText{}
  JMon{name: String, v: R.HV, keep: Keep}
  JDate{fmt: String}
  JStdin{unsafe: Bool}
  JCom{prog: String, args: List<&2, String>, msg: String}
  JNo{text: String}

# a part: its command, its rate (tenths; 0: once), the text around it,
# its text now, when it is due (tenths since the start; never: not by time)
type Part is Data:
  Part{job: Job, rate: U32, before: String, after: String, text: String, due: U32}

def lookup(+a: String, rs: List<&2, C.Run>) -> Maybe<&2, C.Run>:
  match rs:
    case Nil{}:
      None{}
    case Con{+r, rest}:
      +m = lookup(a, rest)
      Bool.pick(Maybe<&2, C.Run>, String.eq(C.alias(r), a), Some{r}, m)

def mon.rate.go(xs: List<&2, R.HV>) -> U32:
  match xs:
    case Nil{}:
      10
    case Con{x, Nil{}}:
      C.or0(C.hv.int(x))
    case Con{_, rest}:
      mon.rate.go(rest)

# a monitor's rate: its last argument
def mon.rate(+v: R.HV) -> U32:
  mon.rate.go(C.con.args(v))

def ported(+n: String) -> Bool:
  C.in.list(n, ["MultiCpu", "Memory", "Swap", "DynNetwork", "Network", "DiskU", "MultiCoreTemp"])

def msg.or(m: Maybe<&2, String>, +d: String) -> String:
  match m:
    case Some{x}:
      x
    case None{}:
      d

# UnsafeStdinReader shows each line as it is; StdinReader first takes its
# actions out (stripActions, Strip.bend)
def stdin.part(u: Bool) -> Part:
  Part{JStdin{u}, 0, "", "", "Updating...", never()}

# the job of a part's command (combine's default: Com alias [] [] 10)
def job.of(m: Maybe<&2, C.Run>, +com: String) -> Part:
  match m:
    case None{}:
      Bool.pick(Part, String.is_empty(com), Part{JText{}, 0, "", "", "", never()}, Part{JCom{com, Nil{}, "Could not execute command " ++ com}, 10, "", "", "Updating...", 0})
    case Some{C.RCom{+p, a, msg, _, +r}}:
      Part{JCom{p, a, msg.or(msg, "Could not execute command " ++ p)}, r, "", "", "Updating...", 0}
    case Some{C.RDate{f, _, r}}:
      Part{JDate{f}, r, "", "", "Updating...", 0}
    case Some{C.RStdin{u}}:
      stdin.part(u)
    case Some{C.RMon{+v}}:
      +n = C.con.name(v)
      Bool.pick(Part, ported(n), Part{JMon{n, v, KNone{}}, mon.rate(v), "", "", "Updating...", 0}, Part{JNo{"xmobar-bend: " ++ n ++ " is not ported"}, 0, "", "", "Updating...", 0})
    case Some{C.ROther{+v}}:
      Part{JNo{"xmobar-bend: " ++ C.con.name(v) ++ " is not ported"}, 0, "", "", "Updating...", 0}

def part.around(p: Part, +b: String, +a: String) -> Part:
  match p:
    case Part{j, r, _, _, t, d}:
      Part{j, r, b, a, t, d}

def parts.of(+rs: List<&2, C.Run>, ts: List<&2, T.Part>) -> List<&2, Part>:
  match ts:
    case Nil{}:
      Nil{}
    case T.Part{+com, b, a} <> rest:
      part.around(job.of(lookup(com, rs), com), b, a) <> parts.of(rs, rest)

# the template's parts: its left, center and right, each parsed
def sections(+rs: List<&2, C.Run>, +sep: String, ss: List<&2, String>) -> List<&2, Part>:
  match ss:
    case Nil{}:
      Nil{}
    case s <> rest:
      List.append(&2, Part, parts.of(rs, T.template(sep, s)), sections(rs, sep, rest))

# a part's text as the line shows it
def shown(p: Part) -> String:
  match p:
    case Part{JText{}, _, b, a, _, _}:
      b ++ a
    case Part{_, _, b, a, t, _}:
      b ++ t ++ a

def line(ps: List<&2, Part>) -> String:
  match ps:
    case Nil{}:
      ""
    case p <> rest:
      shown(p) ++ line(rest)

# Monitors' runs -----------------------------------------------------------------------

# where a monitor's [String] arguments are among its own
def mon.args.at(+n: String) -> Nat:
  Bool.pick(Nat, Bool.or(String.eq(n, "Network"), String.eq(n, "DiskU")), 1n, 0n)

def mon.args(+v: R.HV) -> List<&2, String>:
  C.strs.of(C.arg.nth(v, mon.args.at(C.con.name(v))))

def read.cpu() -> IO(List<&2, List<&2, U32>>):
  do IO<List<&2, List<&2, U32>>>:
    t : String <- S.slurp("/proc/stat")
    IO.pure(List<&2, List<&2, U32>>, N.cpu.data(t))

# a device's line now, and whether its operstate says up
type Read is Data:
  Read{raw: N.Raw, up: Bool}

def net.read(+name: String) -> IO(Read):
  do IO<Read>:
    t : String <- S.slurp("/proc/net/dev")
    o : String <- S.slurp("/sys/class/net/" ++ name ++ "/operstate")
    IO.pure(Read, Read{N.find.raw(name, N.net.devs(t)), N.operstate.up(o)})

def ref.of(+name: String, r: Read, +n: Now) -> Ref:
  match r:
    case Read{raw, _}:
      Ref{name, N.net.first(raw), n}

def net.ref(+name: String) -> IO(Ref):
  do IO<Ref>:
    r : Read <- net.read(name)
    n : Now <- clock()
    IO.pure(Ref, ref.of(name, r, n))

# a device is one of /sys/class/net's with an operstate (lo left out)
def net.keep(names: List<&2, String>) -> IO(List<&2, String>):
  match names:
    case Nil{}:
      IO.pure(List<&2, String>, Nil{})
    case Con{+n, rest}:
      do IO<List<&2, String>>:
        k : U32 <- S.kind("/sys/class/net/" ++ n ++ "/operstate")
        +more : List<&2, String> <- net.keep(rest)
        IO.pure(List<&2, String>, Bool.pick(List<&2, String>, Bool.and(U32.is_eq(k, 1), Bool.not(String.eq(n, "lo"))), n <> more, more))

def net.refs(names: List<&2, String>) -> IO(List<&2, Ref>):
  match names:
    case Nil{}:
      IO.pure(List<&2, Ref>, Nil{})
    case Con{+n, rest}:
      do IO<List<&2, Ref>>:
        r : Ref <- net.ref(n)
        more : List<&2, Ref> <- net.refs(rest)
        IO.pure(List<&2, Ref>, r <> more)

# each device's rate since its last reading, and the new readings
type Rates is Data:
  Rates{named: List<&2, N.Named>, refs: List<&2, Ref>}

def rates.cons(+x: N.Named, +r: Ref, rs: Rates) -> Rates:
  match rs:
    case Rates{ns, refs}:
      Rates{x <> ns, r <> refs}

# parseNet at a reading: the rate since the reading kept, and the new one kept
def rates.step(+name: String, k: N.Kept, +t0: Now, r: Read, +t1: Now, more: Rates) -> Rates:
  match r:
    case Read{raw, +up}:
      +s = N.net.step(k, raw, up, seconds(t0, t1))
      rates.cons(N.Named{name, N.step.rate(s)}, Ref{name, N.step.kept(s), t1}, more)

def net.rates(refs: List<&2, Ref>) -> IO(Rates):
  match refs:
    case Nil{}:
      IO.pure(Rates, Rates{Nil{}, Nil{}})
    case Con{Ref{+name, k0, +t0}, rest}:
      do IO<Rates>:
        r1 : Read <- net.read(name)
        +t1 : Now <- clock()
        more : Rates <- net.rates(rest)
        IO.pure(Rates, rates.step(name, k0, t0, r1, t1, more))

def disk.specs.go(xs: List<&2, R.HV>) -> List<&2, N.DT>:
  match xs:
    case Nil{}:
      Nil{}
    case R.HTup{Con{a, Con{b, Nil{}}}} <> rest:
      N.DT{C.hv.str(a), C.hv.str(b)} <> disk.specs.go(rest)
    case _ <> rest:
      disk.specs.go(rest)

# the disks' specs: [(device or path, template)]
def disk.specs(v: R.HV) -> List<&2, N.DT>:
  match v:
    case R.HList{xs}:
      disk.specs.go(xs)
    case _:
      Nil{}

def dt.names(ds: List<&2, N.DT>) -> List<&2, String>:
  match ds:
    case Nil{}:
      Nil{}
    case N.DT{d, _} <> rest:
      d <> dt.names(rest)

# mbcanon: a device that exists, canonicalized
def canon(ds: List<&2, N.DP>) -> IO(List<&2, N.DP>):
  match ds:
    case Nil{}:
      IO.pure(List<&2, N.DP>, Nil{})
    case Con{N.DP{+d, +p}, rest}:
      do IO<List<&2, N.DP>>:
        k : U32 <- S.kind(d)
        c : String <- S.realpath(d)
        +more : List<&2, N.DP> <- canon(rest)
        IO.pure(List<&2, N.DP>, Bool.pick(List<&2, N.DP>, U32.is_eq(k, 1), N.DP{c, p} <> more, more))

def w64(+lo: U32, +hi: U32) -> List<&2, U32>:
  B.add(B.of.u32(lo), B.shl(32n, B.of.u32(hi)))

def stat.of(ws: List<&2, U32>) -> N.Stat:
  match ws:
    case bs0 <> bs1 <> b0 <> b1 <> f0 <> f1 <> a0 <> a1 <> _:
      +bs = w64(bs0, bs1)
      +blocks = w64(b0, b1)
      N.Stat{B.mul.big(blocks, bs), B.mul.big(w64(a0, a1), bs), B.mul.big(B.sub(blocks, w64(f0, f1)), bs)}
    case _:
      N.Stat{Nil{}, Nil{}, Nil{}}

def disk.stats(ds: List<&2, N.DP>) -> IO(List<&2, N.MS>):
  match ds:
    case Nil{}:
      IO.pure(List<&2, N.MS>, Nil{})
    case Con{N.DP{+d, +p}, rest}:
      do IO<List<&2, N.MS>>:
        ws : List<&2, U32> <- S.statfs(p)
        more : List<&2, N.MS> <- disk.stats(rest)
        IO.pure(List<&2, N.MS>, N.MS{N.DP{d, p}, stat.of(ws)} <> more)

def match.names(+dir: String, +prefix: String, +suffix: String, ns: List<&2, String>) -> List<&2, String>:
  match ns:
    case Nil{}:
      Nil{}
    case Con{+n, rest}:
      +more = match.names(dir, prefix, suffix, rest)
      Bool.pick(List<&2, String>, N.numbered(prefix, suffix, n), (dir ++ "/" ++ n) <> more, more)

# getMatchingPathsInDir dir prefix suffix: dir/name for each name matched
def matching(+dir: String, +prefix: String, +suffix: String) -> IO(List<&2, String>):
  do IO<List<&2, String>>:
    k : U32 <- S.kind(dir)
    ns : List<&2, String> <- S.dir(dir)
    IO.pure(List<&2, String>, Bool.pick(List<&2, String>, U32.is_eq(k, 2), match.names(dir, prefix, suffix, ns), Nil{}))

def keep.kind(+want: U32, ps: List<&2, String>) -> IO(List<&2, String>):
  match ps:
    case Nil{}:
      IO.pure(List<&2, String>, Nil{})
    case Con{+p, rest}:
      do IO<List<&2, String>>:
        k : U32 <- S.kind(p)
        +more : List<&2, String> <- keep.kind(want, rest)
        IO.pure(List<&2, String>, Bool.pick(List<&2, String>, U32.is_eq(k, want), p <> more, more))

def first.or(xs: List<&2, String>) -> List<&2, String>:
  match xs:
    case Con{x, _}:
      [x]
    case Nil{}:
      Nil{}

def hwmon.under(xs: List<&2, String>) -> IO(List<&2, String>):
  match xs:
    case Con{x, _}:
      do IO<List<&2, String>>:
        cps : List<&2, String> <- matching(x ++ "/hwmon", "hwmon", "")
        ecps : List<&2, String> <- keep.kind(2, cps)
        IO.pure(List<&2, String>, first.or(ecps))
    case Nil{}:
      do IO<List<&2, String>>:
        cps : List<&2, String> <- matching("/sys/class/hwmon", "hwmon", "")
        keep.kind(2, cps)

# hwmonPaths: coretemp's hwmon (its first), or every /sys/class/hwmon one
def hwmon.paths() -> IO(List<&2, String>):
  do IO<List<&2, String>>:
    ps : List<&2, String> <- matching("/sys/bus/platform/devices", "coretemp.", "")
    xs : List<&2, String> <- keep.kind(2, ps)
    hwmon.under(xs)

def labels(ps: List<&2, String>) -> IO(List<&2, String>):
  match ps:
    case Nil{}:
      IO.pure(List<&2, String>, Nil{})
    case Con{+p, rest}:
      do IO<List<&2, String>>:
        here : List<&2, String> <- matching(p, "temp", "_label")
        more : List<&2, String> <- labels(rest)
        IO.pure(List<&2, String>, List.append(&2, String, here, more))

def core.labels(ps: List<&2, String>) -> IO(List<&2, String>):
  match ps:
    case Nil{}:
      IO.pure(List<&2, String>, Nil{})
    case Con{+p, rest}:
      do IO<List<&2, String>>:
        t : String <- S.slurp(p)
        +more : List<&2, String> <- core.labels(rest)
        IO.pure(List<&2, String>, Bool.pick(List<&2, String>, N.core.label(t), N.label.input(p) <> more, more))

# the files' texts, or the first one's openFile error (GHC's text)
type Texts is Data:
  TxOk{ts: List<&2, String>}
  TxErr{e: String}

def open.err(+k: U32, +p: String) -> Maybe<&2, String>:
  Bool.pick(Maybe<&2, String>, U32.is_eq(k, 0), Some{p ++ ": openFile: does not exist (No such file or directory)"}, Bool.pick(Maybe<&2, String>, U32.is_eq(k, 2), Some{p ++ ": openFile: inappropriate type (is a directory)"}, None{}))

def texts.cons(m: Maybe<&2, String>, t: String, more: Texts) -> Texts:
  match m more:
    case Some{e} _:
      TxErr{e}
    case None{} TxOk{ts}:
      TxOk{t <> ts}
    case None{} TxErr{e}:
      TxErr{e}

def texts(ps: List<&2, String>) -> IO(Texts):
  match ps:
    case Nil{}:
      IO.pure(Texts, TxOk{Nil{}})
    case Con{+p, rest}:
      do IO<Texts>:
        k : U32 <- S.kind(p)
        t : String <- S.slurp(p)
        more : Texts <- texts(rest)
        IO.pure(Texts, texts.cons(open.err(k, p), t, more))

def temps.of(x: Texts) -> N.Temps:
  match x:
    case TxOk{ts}:
      N.core.temps(ts)
    case TxErr{e}:
      N.TempsErr{e}

def core.paths(m: Maybe<&2, String>) -> IO(List<&2, String>):
  match m:
    case Some{p}:
      IO.pure(List<&2, String>, [p])
    case None{}:
      hwmon.paths()

# the non-options a monitor gets (what follows "--"), none on an error
def rest.of(a: M.Args) -> List<&2, String>:
  match a:
    case M.Args{_, r}:
      r
    case M.ArgsErr{_}:
      Nil{}

# a monitor's run: its text and what it keeps
type Ran is Data:
  Ran{text: String, keep: Keep}

def ran(+t: String, k: Keep) -> IO(Ran):
  IO.pure(Ran, Ran{t, k})

def cpu.prev(k: Keep) -> IO(List<&2, List<&2, U32>>):
  match k:
    case KCpu{p}:
      IO.pure(List<&2, List<&2, U32>>, p)
    case _:
      read.cpu()

def run.cpu(+args: List<&2, String>, k: Keep) -> IO(Ran):
  do IO<Ran>:
    before : List<&2, List<&2, U32>> <- cpu.prev(k)
    +now : List<&2, List<&2, U32>> <- read.cpu()
    ran(M.run(args, N.multicpu.config(), c => r => N.multicpu.run(c, r, before, now)), KCpu{now})

def run.mem(+args: List<&2, String>) -> IO(Ran):
  do IO<Ran>:
    t : String <- S.slurp("/proc/meminfo")
    ran(M.run(args, N.mem.config(), c => r => N.mem.run(c, r, t)), KNone{})

def run.swap(+args: List<&2, String>) -> IO(Ran):
  do IO<Ran>:
    t : String <- S.slurp("/proc/meminfo")
    ran(M.run(args, N.swap.config(), c => _ => N.swap.run(c, t)), KNone{})

def ran.rates(+args: List<&2, String>, rs: Rates) -> IO(Ran):
  match rs:
    case Rates{+named, refs}:
      ran(M.run(args, N.net.config(), c => r => N.dyn.run(c, r, named)), KNet{refs})

def net.prev(k: Keep) -> IO(List<&2, Ref>):
  match k:
    case KNet{rs}:
      IO.pure(List<&2, Ref>, rs)
    case _:
      do IO<List<&2, Ref>>:
        ns : List<&2, String> <- S.dir("/sys/class/net")
        ds : List<&2, String> <- net.keep(ns)
        net.refs(ds)

def run.dyn(+args: List<&2, String>, k: Keep) -> IO(Ran):
  do IO<Ran>:
    refs : List<&2, Ref> <- net.prev(k)
    rs : Rates <- net.rates(refs)
    ran.rates(args, rs)

def ran.net(+args: List<&2, String>, rs: Rates) -> IO(Ran):
  match rs:
    case Rates{Con{N.Named{_, +r}, _}, refs}:
      ran(M.run(args, N.net.config(), c => a => N.net.run(c, a, r)), KNet{refs})
    case Rates{_, refs}:
      ran(M.run(args, N.net.config(), c => a => N.net.run(c, a, N.RateNA{})), KNet{refs})

def net.one.prev(+name: String, k: Keep) -> IO(List<&2, Ref>):
  match k:
    case KNet{rs}:
      IO.pure(List<&2, Ref>, rs)
    case _:
      net.refs([name])

def run.net(+name: String, +args: List<&2, String>, k: Keep) -> IO(Ran):
  do IO<Ran>:
    refs : List<&2, Ref> <- net.one.prev(name, k)
    rs : Rates <- net.rates(refs)
    ran.net(args, rs)

def run.disku(+disks: List<&2, N.DT>, +args: List<&2, String>) -> IO(Ran):
  do IO<Ran>:
    t : String <- S.slurp("/etc/mtab")
    ds : List<&2, N.DP> <- canon(N.mtab(t))
    ms : List<&2, N.MS> <- disk.stats(N.mounted(dt.names(disks), ds))
    ran(M.run(args, N.disku.config(), c => r => N.disku.run(c, r, disks, ms)), KNone{})

def ran.ct(+args: List<&2, String>, +ts: N.Temps) -> IO(Ran):
  ran(M.run(args, N.coretemp.config(), c => r => N.coretemp.run.or(c, r, ts)), KNone{})

def run.coretemp(+args: List<&2, String>) -> IO(Ran):
  do IO<Ran>:
    ps : List<&2, String> <- core.paths(N.ct.path(rest.of(M.args(args, N.coretemp.config()))))
    ls : List<&2, String> <- labels(ps)
    fs : List<&2, String> <- keep.kind(1, ls)
    cs : List<&2, String> <- core.labels(fs)
    x : Texts <- texts(cs)
    ran.ct(args, temps.of(x))

def run.mon(+n: String, +v: R.HV, +k: Keep) -> IO(Ran):
  +args = mon.args(v)
  H.Bool.if(IO(Ran), String.eq(n, "MultiCpu"), _ => run.cpu(args, k), _ => H.Bool.if(IO(Ran), String.eq(n, "Memory"), _ => run.mem(args), _ => H.Bool.if(IO(Ran), String.eq(n, "Swap"), _ => run.swap(args), _ => H.Bool.if(IO(Ran), String.eq(n, "DynNetwork"), _ => run.dyn(args, k), _ => H.Bool.if(IO(Ran), String.eq(n, "Network"), _ => run.net(C.hv.str(C.arg.nth(v, 0n)), args, k), _ => H.Bool.if(IO(Ran), String.eq(n, "DiskU"), _ => run.disku(disk.specs(R.up(C.arg.nth(v, 0n))), args), _ => run.coretemp(args)))))))


# Running the parts ----------------------------------------------------------------------

# Com: the first line of the output when the program exits 0 (hGetLine:
# none is ""), else the message
def lines.first(xs: List<&2, String>) -> String:
  match xs:
    case Con{x, _}:
      x
    case Nil{}:
      ""

def com.text(r: Result<&1, &1, U32 & String, U32 & (String & String)>, +msg: String) -> String:
  match r:
    case Done{Tuple{c, Tuple{o, _}}}:
      Bool.pick(String, U32.is_eq(c, 0), lines.first(H.lines(o)), msg)
    case _:
      msg

def strs.io(xs: List<&2, String>) -> List<String>:
  match xs:
    case Nil{}:
      Nil{}
    case Con{x, rest}:
      Con{x, strs.io(rest)}

def strs.of.io(xs: List<String>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case Con{x, rest}:
      Con{x, strs.of.io(rest)}

# a command that has not ended in a minute is taken as failed
def run.com(+p: String, +args: List<&2, String>, +msg: String) -> IO(String):
  do IO<String>:
    r : Result<&1, &1, U32 & String, U32 & (String & String)> <- Process.run(p, strs.io(args), "", 1048576, 60000)
    IO.pure(String, com.text(r, msg))

# Date: the local time now, as localtime breaks it down, then formatTime
def tm.of(ws: List<&2, U32>, +ns: U32, +zone: String, +ep: U32) -> D.Tm:
  match ws:
    case y <> mo <> d <> h <> mi <> s <> wd <> yd <> off <> _:
      D.Tm{y, mo, d, h, mi, s, ns, wd, yd, off, zone, ep}
    case _:
      D.Tm{1970, 1, 1, 0, 0, 0, ns, 4, 0, 0, zone, ep}

def date.at(+f: String, n: Now) -> IO(String):
  match n:
    case Now{+s, +hi, +ns}:
      do IO<String>:
        ws : List<&2, U32> <- S.local(s, hi)
        z : String <- S.zone(s, hi)
        IO.pure(String, D.format(f, tm.of(ws, ns, z, s)))

def run.date(+f: String) -> IO(String):
  do IO<String>:
    n : Now <- clock()
    date.at(f, n)

# a part's job after a run, and its text
type JR is Data:
  JR{job: Job, text: String}

def jr.mon(+n: String, +v: R.HV, r: Ran) -> JR:
  match r:
    case Ran{t, k}:
      JR{JMon{n, v, k}, t}

def run.job(j: Job, +old: String) -> IO(JR):
  match j:
    case JMon{+n, +v, k}:
      do IO<JR>:
        r : Ran <- run.mon(n, v, k)
        IO.pure(JR, jr.mon(n, v, r))
    case JDate{+f}:
      do IO<JR>:
        t : String <- run.date(f)
        IO.pure(JR, JR{JDate{f}, t})
    case JCom{+p, +a, +m}:
      do IO<JR>:
        t : String <- run.com(p, a, m)
        IO.pure(JR, JR{JCom{p, a, m}, t})
    case JNo{+t}:
      IO.pure(JR, JR{JNo{t}, t})
    case JText{}:
      IO.pure(JR, JR{JText{}, old})
    case JStdin{u}:
      IO.pure(JR, JR{JStdin{u}, old})

# when a part runs next: Run.Timer's next multiple of its rate after t
# (a rate of 0 or less: once, as Com does it)
def due.at(timed: Bool, +t: U32, n: Now, +r: U32) -> U32:
  match timed:
    case True{}:
      U32.add(U32.sub(t, phase(n, r)), r)
    case False{}:
      never()

def due.after(+t: U32, n: Now, +r: U32) -> U32:
  due.at(Bool.not(Bool.or(U32.is_eq(r, 0), H.is.neg(r))), t, n, r)

def part.of(x: JR, +r: U32, +b: String, +a: String, +d: U32) -> Part:
  match x:
    case JR{j, t}:
      Part{j, r, b, a, t, d}

def run.part.at(due: Bool, j: Job, +r: U32, +b: String, +a: String, +old: String, +d: U32, +t: U32, n: Now) -> IO(Part):
  match due:
    case False{}:
      IO.pure(Part, Part{j, r, b, a, old, d})
    case True{}:
      do IO<Part>:
        x : JR <- run.job(j, old)
        IO.pure(Part, part.of(x, r, b, a, due.after(t, n, r)))

def run.part(p: Part, +t: U32, n: Now) -> IO(Part):
  match p:
    case Part{j, +r, +b, +a, +old, +d}:
      run.part.at(U32.is_le(d, t), j, r, b, a, old, d, t, n)

# the parts, each one due at t (tenths since the start) run, in order
def run.due(ps: List<&2, Part>, +t: U32, +n: Now) -> IO(List<&2, Part>):
  match ps:
    case Nil{}:
      IO.pure(List<&2, Part>, Nil{})
    case Con{p, rest}:
      do IO<List<&2, Part>>:
        q : Part <- run.part(p, t, n)
        qs : List<&2, Part> <- run.due(rest, t, n)
        IO.pure(List<&2, Part>, q <> qs)

# the soonest a part is due (m: the soonest so far)
def soonest(ps: List<&2, Part>, +m: U32) -> U32:
  match ps:
    case Nil{}:
      m
    case Con{Part{_, _, _, _, _, +d}, rest}:
      soonest(rest, Bool.pick(U32, U32.is_lt(d, m), d, m))

# the milliseconds until tenth `due` begins (at most an hour)
def wait.ms(+due: U32, n: Now, +sec0: U32) -> U32:
  match n:
    case Now{+s, +hi, +ns}:
      +cur = tenths(Now{s, hi, ns}, sec0)
      +dt = Bool.pick(U32, U32.is_gt(due, cur), U32.sub(due, cur), 0)
      +dt1 = Bool.pick(U32, U32.is_gt(dt, 36000), 36000, dt)
      +into = U32.mod(U32.div(ns, 1000000), 100)
      Bool.pick(U32, U32.is_eq(dt1, 0), 0, U32.sub(U32.mul(dt1, 100), into))

def has.stdin(ps: List<&2, Part>) -> Bool:
  match ps:
    case Nil{}:
      False{}
    case Con{Part{JStdin{_}, _, _, _, _, _}, _}:
      True{}
    case Con{_, rest}:
      has.stdin(rest)

# a line read: every StdinReader part's text
def feed(ps: List<&2, Part>, +l: String) -> List<&2, Part>:
  match ps:
    case Nil{}:
      Nil{}
    case Con{Part{JStdin{+u}, +r, +b, +a, _, +d}, rest}:
      Part{JStdin{u}, r, b, a, Bool.pick(String, u, l, Sp.strip(l)), d} <> feed(rest, l)
    case Con{p, rest}:
      p <> feed(rest, l)

def last.of(xs: List<&2, String>, acc: Maybe<&2, String>) -> Maybe<&2, String>:
  match xs:
    case Nil{}:
      acc
    case Con{x, rest}:
      last.of(rest, Some{x})

# the lines read since the last wait: the last one is shown (xmobar's
# checker may show the others for an instant, or not)
def feed.last(m: Maybe<&2, String>, ps: List<&2, Part>) -> List<&2, Part>:
  match m:
    case Some{l}:
      feed(ps, l)
    case None{}:
      ps

# The loop ---------------------------------------------------------------------------------

# a wait's end: the lines read, and whether standard input is still open
type Wake is Data:
  Wake{lines: List<&2, String>, go: Bool}

def wake.go(w: Wake) -> Bool:
  match w:
    case Wake{_, g}:
      g

def wake.feed(w: Wake, ps: List<&2, Part>) -> List<&2, Part>:
  match w:
    case Wake{ls, _}:
      feed.last(last.of(ls, None{}), ps)

def wait(stdin: Bool, +ms: U32) -> IO(Wake):
  match stdin:
    case False{}:
      do IO<Wake>:
        Unit <- IO.sleep(ms)
        IO.pure(Wake, Wake{Nil{}, True{}})
    case True{}:
      do IO<Wake>:
        r : U32 <- S.in_wait(ms)
        ls : List<&2, String> <- S.in_lines()
        e : U32 <- S.in_eof()
        IO.pure(Wake, Wake{ls, Bool.not(U32.is_eq(e, 1))})

def same(m: Maybe<&2, String>, +l: String) -> Bool:
  match m:
    case Some{x}:
      String.eq(x, l)
    case None{}:
      False{}

def say(same: Bool, +l: String) -> IO(String):
  match same:
    case True{}:
      IO.pure(String, l)
    case False{}:
      do IO<String>:
        Unit <- IO.print(l)
        Unit <- S.flush()
        IO.pure(String, l)

# the line (format at Plain), written when it is not the last one written
def show.line(+fg: String, ps: List<&2, Part>, last: Maybe<&2, String>) -> IO(String):
  +l = T.plain(fg, line(ps))
  say(same(last, l), l)

# the bar: the parts due run, the line written if it changed, then a wait
# for the next part due or a line on standard input; its end ends the bar
# (go False), as StdinReader's exitImmediately
def loop(fuel: Nat, go: Bool, +sec0: U32, +fg: String, +stdin: Bool, ps: List<&2, Part>, last: Maybe<&2, String>) -> IO(Unit):
  match fuel go:
    case 1n+f True{}:
      do IO<Unit>:
        +n : Now <- clock()
        +ps1 : List<&2, Part> <- run.due(ps, tenths(n, sec0), n)
        l : String <- show.line(fg, ps1, last)
        n2 : Now <- clock()
        +w : Wake <- wait(stdin, wait.ms(soonest(ps1, never()), n2, sec0))
        loop(f, wake.go(w), sec0, fg, stdin, wake.feed(w, ps1), Some{l})
    case _ _:
      do IO<Unit>:
        l : String <- show.line(fg, ps, last)
        IO.pure(Unit, Unit{})

def fuel() -> Nat:
  1000000000n

# Startup ------------------------------------------------------------------------------------

def env.of(r: Result<&1, &1, U32 & String, String>) -> Maybe<&2, String>:
  match r:
    case Done{s}:
      Some{s}
    case Fail{_}:
      None{}

def env(+name: String) -> IO(Maybe<&2, String>):
  do IO<Maybe<&2, String>>:
    r : Result<&1, &1, U32 & String, String> <- IO.get_env(name)
    IO.pure(Maybe<&2, String>, env.of(r))

def or.str(m: Maybe<&2, String>, +d: String) -> String:
  match m:
    case Some{x}:
      x
    case None{}:
      d

def dirs.of(e: Maybe<&2, String>, +app: String, +xdg: String, +home: String) -> List<&2, String>:
  match e:
    case Some{p}:
      [p, app, xdg, home]
    case None{}:
      [app, xdg, home]

# d </> fn (an empty d: fn itself)
def join(+d: String, +fn: String) -> String:
  Bool.pick(String, String.is_empty(d), fn, d ++ "/" ++ fn)

# getXdgDirectory XdgConfig: $XDG_CONFIG_HOME unless unset or empty
def xdg.or(m: Maybe<&2, String>, +d: String) -> String:
  match m:
    case Some{+x}:
      Bool.pick(String, String.is_empty(x), d, x)
    case None{}:
      d

# the first dir holding fn
def in.dirs(ds: List<&2, String>, +fn: String) -> IO(Maybe<&2, String>):
  match ds:
    case Nil{}:
      IO.pure(Maybe<&2, String>, None{})
    case Con{+d, rest}:
      do IO<Maybe<&2, String>>:
        k : U32 <- S.kind(join(d, fn))
        +more : Maybe<&2, String> <- in.dirs(rest, fn)
        IO.pure(Maybe<&2, String>, Bool.pick(Maybe<&2, String>, U32.is_eq(k, 0), more, Some{join(d, fn)}))

def maybe.or(m: Maybe<&2, String>, n: Maybe<&2, String>) -> Maybe<&2, String>:
  match m:
    case Some{x}:
      Some{x}
    case None{}:
      n

def first.some(fns: List<&2, String>, +ds: List<&2, String>) -> IO(Maybe<&2, String>):
  match fns:
    case Nil{}:
      IO.pure(Maybe<&2, String>, None{})
    case Con{+fn, rest}:
      do IO<Maybe<&2, String>>:
        +m : Maybe<&2, String> <- in.dirs(ds, fn)
        +more : Maybe<&2, String> <- first.some(rest, ds)
        IO.pure(Maybe<&2, String>, maybe.or(m, more))

def config.in(e: Maybe<&2, String>, +home: String, x: Maybe<&2, String>) -> IO(Maybe<&2, String>):
  first.some(["xmobar.hs", ".xmobarrc", "xmobarrc"], dirs.of(e, home ++ "/.xmobar", xdg.or(x, home ++ "/.config") ++ "/xmobar", home))

# xmobarConfigFile: xmobar.hs, .xmobarrc, xmobarrc, each in
# $XMOBAR_CONFIG_DIR, ~/.xmobar, $XDG_CONFIG_HOME/xmobar, ~
def config.file() -> IO(Maybe<&2, String>):
  do IO<Maybe<&2, String>>:
    e : Maybe<&2, String> <- env("XMOBAR_CONFIG_DIR")
    h : Maybe<&2, String> <- env("HOME")
    x : Maybe<&2, String> <- env("XDG_CONFIG_HOME")
    config.in(e, or.str(h, ""), x)

def tof.plain(t: C.TOF) -> Bool:
  match t:
    case C.Plain{}:
      True{}
    case _:
      False{}

def now.sec(n: Now) -> U32:
  match n:
    case Now{s, _, _}:
      s

# The X11 bar (Xmobar.X11.Loop, Window) ----------------------------------------------------

# the bar's fixed facts: its window, its config's position, pickBroadest
# and the font's height, how it draws, standard input's use, the run's
# first second, each section's count of parts
type XC is Data:
  XC{win: U32, pos: C.Pos, broadest: Bool, ht: U32, paint: Dr.Paint, fg: String, bg: String, alpha: U32, dpi: String, stdin: Bool, sec0: U32, counts: List<&2, Nat>, root: String}

# what changes: the window's rectangle, where the actions are, the
# sections' texts last drawn (None: draw again)
type XS is Data:
  XS{r: W.Rect, acts: List<&2, Dr.ActPos>, last: Maybe<&2, List<&2, String>>}

def x.rects(ws: List<&2, U32>) -> List<&2, W.Rect>:
  match ws:
    case Con{+x, Con{+y, Con{+w, Con{+h, rest}}}}:
      W.Rect{x, y, w, h} <> x.rects(rest)
    case _:
      Nil{}

def x.screens() -> IO(List<&2, W.Rect>):
  do IO<List<&2, W.Rect>>:
    ws : List<&2, U32> <- X.screens()
    IO.pure(List<&2, W.Rect>, x.rects(ws))

def nums.str(xs: List<&2, U32>) -> String:
  match xs:
    case Nil{}:
      ""
    case Con{+x, Nil{}}:
      U32.show(x)
    case Con{+x, rest}:
      U32.show(x) ++ " " ++ nums.str(rest)

# setStruts' d w svs
def x.struts.of(+win: U32, +v: List<&2, U32>) -> IO(U32):
  do IO<U32>:
    _ : U32 <- X.prop32(win, "_NET_WM_STRUT_PARTIAL", "CARDINAL", nums.str(v))
    X.prop32(win, "_NET_WM_STRUT", "CARDINAL", nums.str(List.take(&2, U32, v, 4n)))

# setStruts r c d w srs
def x.struts(+win: U32, r: W.Rect, p: C.Pos, rs: List<&2, W.Rect>) -> IO(U32):
  x.struts.of(win, W.struts(r, p, rs))

def x.when(c: Bool, act: Unit -> IO(U32)) -> IO(U32):
  H.Bool.if(IO(U32), c, act, _ => IO.pure(U32, 0))

# showWindow r c d w: mapped, its struts again from the screens now
def x.show(+win: U32, +r: W.Rect, p: C.Pos) -> IO(U32):
  do IO<U32>:
    _ : U32 <- X.map(win)
    rs : List<&2, W.Rect> <- x.screens()
    _ : U32 <- x.struts(win, r, p, rs)
    X.sync(0)

# the sections' texts: each section's parts' texts, concatenated
def sec.texts(counts: List<&2, Nat>, +ps: List<&2, Part>) -> List<&2, String>:
  match counts:
    case Nil{}:
      Nil{}
    case Con{+n, rest}:
      line(List.take(&2, Part, ps, n)) <> sec.texts(rest, List.drop(&2, Part, ps, n))

def strs.eq(a: List<&2, String>, b: List<&2, String>) -> Bool:
  match a b:
    case Nil{} Nil{}:
      True{}
    case Con{x, xr} Con{y, yr}:
      Bool.and(String.eq(x, y), strs.eq(xr, yr))
    case _ _:
      False{}

def drawn.same(m: Maybe<&2, List<&2, String>>, ts: List<&2, String>) -> Bool:
  match m:
    case Some{l}:
      strs.eq(l, ts)
    case None{}:
      False{}

def ext.of(ws: List<&2, U32>) -> Dr.Ext:
  match ws:
    case Con{+o, Con{+u, Con{+w, Con{+h, _}}}}:
      Dr.Ext{o, u, w, h}
    case _:
      Dr.Ext{0, 0, 0, 0}

# each markup laid out in the frame's context, in order
def x.measure(ms: List<&2, String>) -> IO(List<&2, Dr.Ext>):
  match ms:
    case Nil{}:
      IO.pure(List<&2, Dr.Ext>, Nil{})
    case Con{m, rest}:
      do IO<List<&2, Dr.Ext>>:
        ws : List<&2, U32> <- X.layout(m)
        more : List<&2, Dr.Ext> <- x.measure(rest)
        IO.pure(List<&2, Dr.Ext>, ext.of(ws) <> more)

# parseSegments: each section's text parsed (take 3 $ s ++ repeat "")
def parse.secs(+fg: String, ts: List<&2, String>) -> List<&2, List<&2, T.Seg>>:
  match ts:
    case Nil{}:
      Nil{}
    case Con{+t, rest}:
      T.parse(fg, t) <> parse.secs(fg, rest)

def x.put(+win: U32, r: W.Rect, +bg: String, +alpha: U32, f: Dr.Frame) -> IO(List<&2, Dr.ActPos>):
  match r f:
    case W.Rect{+x, +y, +w, +h} Dr.Frame{ops, acts}:
      do IO<List<&2, Dr.ActPos>>:
        _ : U32 <- X.draw(win, x, y, w, h, bg, alpha, ops)
        IO.pure(List<&2, Dr.ActPos>, acts)

def rect.wh(r: W.Rect) -> List<&2, U32>:
  match r:
    case W.Rect{_, _, w, h}:
      [w, h]

def frame.at(+p: Dr.Paint, wh: List<&2, U32>, +segs: List<&2, List<&2, T.Seg>>, exts: List<&2, Dr.Ext>, +icons: List<&2, Dr.Icon>) -> Dr.Frame:
  match wh:
    case Con{+w, Con{+h, _}}:
      Dr.frame(p, w, h, segs, exts, icons)
    case _:
      Dr.frame(p, 0, 0, segs, exts, icons)

# FilePath's </> (Posix): b when it is absolute or a empty, a when b is
# empty, else joined by one /
def fp.join(+a: String, +b: String) -> String:
  H.Bool.if(String, Bool.or(H.first.is(b, 47), String.is_empty(a)), _ => b, _ => H.Bool.if(String, String.is_empty(b), _ => a, _ => H.Bool.if(String, String.ends_with(a, "/"), _ => a ++ b, _ => a ++ "/" ++ b)))

# updateCache's expandPath: a path from / or ./ or ../ as it is, else
# under iconRoot
def icon.full(+root: String, +p: String) -> String:
  H.Bool.if(String, Bool.or(H.first.is(p, 47), Bool.or(String.starts_with(p, "./"), String.starts_with(p, "../"))), _ => p, _ => fp.join(root, p))

def icon.cons(+p: String, ws: List<&2, U32>, more: List<&2, Dr.Icon>) -> List<&2, Dr.Icon>:
  match ws:
    case Con{+i, Con{+w, Con{+h, _}}}:
      Dr.Icon{p, i, w, h} <> more
    case _:
      more

# updateIconCache: each icon the segments name, loaded if not yet; the
# ones in the cache
def x.icons(+win: U32, +root: String, ps: List<&2, String>) -> IO(List<&2, Dr.Icon>):
  match ps:
    case Nil{}:
      IO.pure(List<&2, Dr.Icon>, Nil{})
    case Con{+p, rest}:
      do IO<List<&2, Dr.Icon>>:
        ws : List<&2, U32> <- X.icon(win, p, icon.full(root, p))
        more : List<&2, Dr.Icon> <- x.icons(win, root, rest)
        IO.pure(List<&2, Dr.Icon>, icon.cons(p, ws, more))

# Draw.draw: the sections parsed, their icons loaded, their texts
# measured, the frame drawn
def x.frame(xc: XC, +r: W.Rect, +ts: List<&2, String>) -> IO(List<&2, Dr.ActPos>):
  match xc:
    case XC{+win, _, _, _, +p, +fg, +bg, +alpha, +dpi, _, _, _, +root}:
      +segs = parse.secs(fg, ts)
      do IO<List<&2, Dr.ActPos>>:
        icons : List<&2, Dr.Icon> <- x.icons(win, root, Dr.icon.paths(segs, Nil{}))
        _ : U32 <- X.begin(dpi)
        exts : List<&2, Dr.Ext> <- x.measure(Dr.markups(p, segs))
        x.put(win, r, bg, alpha, frame.at(p, rect.wh(r), segs, exts, icons))

def x.redrawn(+xc: XC, +r: W.Rect, +ts: List<&2, String>) -> IO(XS):
  do IO<XS>:
    acts : List<&2, Dr.ActPos> <- x.frame(xc, r, ts)
    IO.pure(XS, XS{r, acts, Some{ts}})

# a Wakeup: drawn when the texts changed (or an Expose asked)
def x.redraw(+xc: XC, s: XS, +ts: List<&2, String>) -> IO(XS):
  match s:
    case XS{+r, +acts, +last}:
      H.Bool.if(IO(XS), drawn.same(last, ts), _ => IO.pure(XS, XS{r, acts, last}), _ => x.redrawn(xc, r, ts))

# runActions: each command spawned
def x.spawns(cs: List<&2, String>) -> IO(U32):
  match cs:
    case Nil{}:
      IO.pure(U32, 0)
    case Con{c, rest}:
      do IO<U32>:
        _ : U32 <- X.spawn(c)
        x.spawns(rest)

def rect.of.at(+broadest: Bool, p: C.Pos, +rs: List<&2, W.Rect>, +ht: U32) -> W.Rect:
  W.set.position(broadest, p, rs, ht)

def rect.parts(r: W.Rect) -> List<&2, U32>:
  match r:
    case W.Rect{x, y, w, h}:
      [x, y, w, h]

def x.move(+win: U32, r: List<&2, U32>) -> IO(U32):
  match r:
    case Con{+x, Con{+y, Con{+w, Con{+h, _}}}}:
      X.move_resize(win, x, y, w, h)
    case _:
      IO.pure(U32, 0)

# repositionWin: the position again on the screens now, the window moved
# there, its struts set
def x.reposition(xc: XC) -> IO(W.Rect):
  match xc:
    case XC{+win, +p, +broadest, +ht, _, _, _, _, _, _, _, _, _}:
      do IO<W.Rect>:
        +rs : List<&2, W.Rect> <- x.screens()
        +r : W.Rect <- IO.pure(W.Rect, rect.of.at(broadest, p, rs, ht))
        _ : U32 <- x.move(win, rect.parts(r))
        _ : U32 <- x.struts(win, r, p, rs)
        _ : U32 <- X.sync(0)
        IO.pure(W.Rect, r)

def xs.acts(s: XS) -> List<&2, Dr.ActPos>:
  match s:
    case XS{_, a, _}:
      a

def xs.forget(s: XS) -> XS:
  match s:
    case XS{r, a, _}:
      XS{r, a, None{}}

def xs.at(s: XS, +r: W.Rect) -> XS:
  match s:
    case XS{_, a, l}:
      XS{r, a, l}

# the events: an Expose draws again, a ButtonPress runs its actions, a
# ConfigureNotify or a screen change repositions the window
def x.events(evs: List<&2, U32>, +xc: XC, +s: XS) -> IO(XS):
  match evs:
    case Con{+k, Con{+a, Con{+b, +rest}}}:
      H.Bool.if(IO(XS), U32.is_eq(k, 1), _ => x.events(rest, xc, xs.forget(s)), _ => H.Bool.if(IO(XS), U32.is_eq(k, 2), _ => IO.bind(U32, XS, x.spawns(Dr.clicked(xs.acts(s), a, b)), _ => x.events(rest, xc, s)), _ => IO.bind(W.Rect, XS, x.reposition(xc), r => x.events(rest, xc, xs.at(s, r)))))
    case _:
      IO.pure(XS, s)

# a wait's end in the X bar: whether the bar goes on, its parts (a line
# read fed to them), its state
type XW is Data:
  XW{go: Bool, ps: List<&2, Part>, s: XS}

def xw.go(w: XW) -> Bool:
  match w:
    case XW{g, _, _}:
      g

def xw.ps(w: XW) -> List<&2, Part>:
  match w:
    case XW{_, ps, _}:
      ps

def xw.s(w: XW) -> XS:
  match w:
    case XW{_, _, s}:
      s

def x.stdin(+ps: List<&2, Part>, +s: XS) -> IO(XW):
  do IO<XW>:
    ls : List<&2, String> <- S.in_lines()
    e : U32 <- S.in_eof()
    IO.pure(XW, XW{Bool.not(U32.is_eq(e, 1)), wake.feed(Wake{ls, True{}}, ps), s})

# what woke the bar: X's events (2), standard input (1) or the time (0)
def x.woke(+k: U32, +xc: XC, +ps: List<&2, Part>, +s: XS) -> IO(XW):
  H.Bool.if(IO(XW), U32.is_eq(k, 2), _ => IO.bind(List<&2, U32>, XW, X.events(), evs => IO.bind(XS, XW, x.events(evs, xc, s), s2 => IO.pure(XW, XW{True{}, ps, s2}))), _ => H.Bool.if(IO(XW), U32.is_eq(k, 1), _ => x.stdin(ps, s), _ => IO.pure(XW, XW{True{}, ps, s})))

def xc.sec0(xc: XC) -> U32:
  match xc:
    case XC{_, _, _, _, _, _, _, _, _, _, s, _, _}:
      s

def xc.counts(xc: XC) -> List<&2, Nat>:
  match xc:
    case XC{_, _, _, _, _, _, _, _, _, _, _, c, _}:
      c

def xc.stdin(xc: XC) -> U32:
  match xc:
    case XC{_, _, _, _, _, _, _, _, _, i, _, _, _}:
      Bool.pick(U32, i, 1, 0)

# the X bar's loop: the parts due run, the frame drawn when the sections'
# texts changed, then a wait for the next part due, an X event or a line
def x.loop(fuel: Nat, go: Bool, +xc: XC, ps: List<&2, Part>, s: XS) -> IO(Unit):
  match fuel go:
    case 1n+f True{}:
      do IO<Unit>:
        +n : Now <- clock()
        +ps1 : List<&2, Part> <- run.due(ps, tenths(n, xc.sec0(xc)), n)
        s1 : XS <- x.redraw(xc, s, sec.texts(xc.counts(xc), ps1))
        n2 : Now <- clock()
        k : U32 <- X.wait(wait.ms(soonest(ps1, never()), n2, xc.sec0(xc)), xc.stdin(xc))
        +w : XW <- x.woke(k, xc, ps1, s1)
        x.loop(f, xw.go(w), xc, xw.ps(w), xw.s(w))
    case _ _:
      IO.pure(Unit, Unit{})

def x.bool(b: Bool) -> U32:
  Bool.pick(U32, b, 1, 0)

def x.create.at(r: List<&2, U32>, +o: U32) -> IO(U32):
  match r:
    case Con{+x, Con{+y, Con{+w, Con{+h, _}}}}:
      X.create(x, y, w, h, o)
    case _:
      X.create(0, 0, 1, 1, o)

# createWin: the window where setPosition puts it, its properties
# (setProperties) and struts, lowered (lowerOnStart), shown unless
# hideOnStart; then the event loop's input selected, and the loop
def x.win.at(+c: C.Config, +ps: List<&2, Part>, +counts: List<&2, Nat>, +rs: List<&2, W.Rect>, +ht: U32, +sec0: U32) -> IO(Unit):
  match c:
    case C.Config{_, _, +cls, +name, +bg, +fg, +pos, _, _, _, _, _, _, _, _, +alpha, +hide, +alld, +over, +broad, +lower, _, +root, _, _, _, _, _, +dpi}:
      +r = W.set.position(broad, pos, rs, ht)
      do IO<Unit>:
        +win : U32 <- x.create.at(rect.parts(r), x.bool(over))
        _ : U32 <- X.set_text(win, 0, cls)
        _ : U32 <- X.set_text(win, 1, name)
        dock : U32 <- X.atom("_NET_WM_WINDOW_TYPE_DOCK")
        _ : U32 <- X.prop32(win, "_NET_WM_WINDOW_TYPE", "ATOM", U32.show(dock))
        _ : U32 <- x.when(alld, _ => X.prop32(win, "_NET_WM_DESKTOP", "CARDINAL", "4294967295"))
        pid : U32 <- X.pid()
        _ : U32 <- X.prop32(win, "_NET_WM_PID", "CARDINAL", U32.show(pid))
        _ : U32 <- x.struts(win, r, pos, rs)
        _ : U32 <- x.when(lower, _ => X.lower(win))
        _ : U32 <- x.when(Bool.not(hide), _ => x.show(win, r, pos))
        _ : U32 <- X.select(win)
        x.loop(fuel(), True{}, XC{win, pos, broad, ht, Dr.look(c), fg, bg, alpha, dpi, has.stdin(ps), sec0, counts, root}, ps, XS{r, Nil{}, None{}})

def x.font.ht(fe: List<&2, U32>) -> Maybe<&2, U32>:
  match fe:
    case Con{+a, Con{+d, _}}:
      Some{W.height(a, d)}
    case _:
      None{}

def x.started(m: Maybe<&2, U32>, +c: C.Config, +ps: List<&2, Part>, +counts: List<&2, Nat>, +rs: List<&2, W.Rect>, +sec0: U32) -> IO(Unit):
  match m:
    case Some{+ht}:
      x.win.at(c, ps, counts, rs, ht, sec0)
    case None{}:
      IO.die(Unit, 1, "xmobar-bend: createFontSet: no font set loads")

def config.font(c: C.Config) -> String:
  match c:
    case C.Config{f, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _}:
      f

def x.start(+c: C.Config, +ps: List<&2, Part>, +counts: List<&2, Nat>) -> IO(Unit):
  do IO<Unit>:
    rs : List<&2, W.Rect> <- x.screens()
    fe : List<&2, U32> <- X.font_extents(config.font(c))
    n : Now <- clock()
    x.started(x.font.ht(fe), c, ps, counts, rs, now.sec(n))

# x11Loop: the display, the window, the loop
def x.bar(+c: C.Config, +ps: List<&2, Part>, +counts: List<&2, Nat>) -> IO(Unit):
  do IO<Unit>:
    ok : U32 <- X.open()
    H.Bool.if(IO(Unit), U32.is_eq(ok, 0), _ => IO.die(Unit, 1, "xmobar-bend: cannot open display"), _ => x.start(c, ps, counts))

def bar.go(plain: Bool, +fg: String, +ps: List<&2, Part>) -> IO(Unit):
  match plain:
    case True{}:
      do IO<Unit>:
        n : Now <- clock()
        loop(fuel(), True{}, now.sec(n), fg, has.stdin(ps), ps, None{})
    case False{}:
      IO.die(Unit, 1, "xmobar-bend: only Plain text output is ported")

# how many parts each section has
def counts.of(+rs: List<&2, C.Run>, +sep: String, ss: List<&2, String>) -> List<&2, Nat>:
  match ss:
    case Nil{}:
      Nil{}
    case s <> rest:
      List.length(&2, Part, parts.of(rs, T.template(sep, s))) <> counts.of(rs, sep, rest)

# a config's parts, and how many each section has
def bar.parts(c: C.Config) -> List<&2, Part>:
  match c:
    case C.Config{_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, +cmds, +sep, +align, +tpl, _, _}:
      sections(cmds, sep, T.split(align, tpl))

def bar.counts(c: C.Config) -> List<&2, Nat>:
  match c:
    case C.Config{_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, +cmds, +sep, +align, +tpl, _, _}:
      counts.of(cmds, sep, T.split(align, tpl))

def bar.fg(c: C.Config) -> String:
  match c:
    case C.Config{_, _, _, _, _, fg, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _}:
      fg

def bar.text(c: C.Config) -> Bool:
  match c:
    case C.Config{_, _, _, _, _, _, _, t, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _}:
      t

def bar.plain(c: C.Config) -> Bool:
  match c:
    case C.Config{_, _, _, _, _, _, _, _, tof, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _}:
      tof.plain(tof)

# the bar from a config: text (the config's textOutput, -T) at its format,
# or the X11 window
def bar(+c: C.Config) -> IO(Unit):
  H.Bool.if(IO(Unit), bar.text(c), _ => bar.go(bar.plain(c), bar.fg(c), bar.parts(c)), _ => x.bar(c, bar.parts(c), bar.counts(c)))

# doOpts' end: the config it made (after what it said), or the run's end
def bar.done(d: O.Done) -> IO(Unit):
  match d:
    case O.DOk{c, said}:
      do IO<Unit>:
        Unit <- IO.write(said)
        bar(c)
    case O.DExit{+code, out, +err}:
      do IO<Unit>:
        Unit <- IO.write(out)
        H.Bool.if(IO(Unit), String.is_empty(err), _ => IO.die(Unit, code, ""), _ => IO.die(Unit, code, err))

def bar.of(r: C.CR, +path: String, +os: List<&2, G.Opt>) -> IO(Unit):
  match r:
    case C.CROk{c, _}:
      bar.done(O.do.opts(c, os))
    case C.CRErr{at}:
      IO.die(Unit, 1, "xmobar-bend: " ++ path ++ ": invalid configuration file, at: " ++ String.take(at, 60n))

def from.file(+path: String, +os: List<&2, G.Opt>) -> IO(Unit):
  do IO<Unit>:
    k : U32 <- S.kind(path)
    t : String <- S.slurp(path)
    H.Bool.if(IO(Unit), U32.is_eq(k, 0), _ => IO.die(Unit, 1, path ++ ": file not found"), _ => bar.of(C.parse(t), path, os))

def from.found(m: Maybe<&2, String>, +os: List<&2, G.Opt>) -> IO(Unit):
  match m:
    case None{}:
      bar.done(O.do.opts(C.default(), os))
    case Some{+p}:
      H.Bool.if(IO(Unit), String.ends_with(p, "xmobar.hs"), _ => IO.die(Unit, 1, "xmobar-bend: " ++ p ++ " is a Haskell configuration, not supported"), _ => from.file(p, os))

def shown.strs(xs: List<&2, String>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case Con{x, rest}:
      H.show.str(x) <> shown.strs(rest)

# xmobarMain: the options; the configuration file (the one argument left,
# else xmobarConfigFile's), read, then doOpts on it
def start.files(+os: List<&2, G.Opt>, +files: List<&2, String>) -> IO(Unit):
  match files:
    case Nil{}:
      do IO<Unit>:
        m : Maybe<&2, String> <- config.file()
        from.found(m, os)
    case Con{p, Nil{}}:
      from.file(p, os)
    case _:
      IO.die(Unit, 1, "xmobar-bend: Too many arguments: " ++ H.show.list(shown.strs(files)))

# getOpts: an error ends the run with the usage; -h writes the usage, -V
# the version, and ends it
def start.got(g: G.Got) -> IO(Unit):
  match g:
    case G.Got{+os, +files, +errs}:
      H.Bool.if(IO(Unit), Bool.not(List.is_empty(&2, String, errs)), _ => IO.die(Unit, 1, "xmobar-bend: " ++ String.concat(errs) ++ O.usage()), _ => H.Bool.if(IO(Unit), O.has.tag(0, os), _ => IO.write(O.usage()), _ => H.Bool.if(IO(Unit), O.has.tag(3, os), _ => IO.write(O.info()), _ => start.files(os, files))))

def start.of(+xs: List<&2, String>) -> IO(Unit):
  start.got(G.getopt(O.options(), xs))

def main() -> IO(Unit):
  do IO<Unit>:
    as : List<String> <- IO.args()
    start.of(List.tail(&2, String, strs.of.io(as)))

