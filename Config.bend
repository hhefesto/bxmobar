# xmobar's configuration in Bend: Xmobar.Config.Types' Config,
# Xmobar.App.Config's defaultConfig, and Xmobar.Config.Parse's parseConfig
# (the comments stripped, "Config {", then the fields in any order, each
# at most once: strings between quotes with \-newline continuations, Read
# for the rest, the commands a Haskell list of Runs read by Read.bend).
#
# The commands are read as Read reads [Runnable]: each Run's value must be
# one of the runnable types xmobar 0.50 is built with (nixpkgs' flags) at
# its constructor's argument types, else the whole list does not read.
# What a Run is kept as: Com/ComX, Date, StdinReader/UnsafeStdinReader,
# a monitor (its constructor and arguments), or another plugin (its value).
#
# An invalid file's message is not parsec's (which names what it expected
# where): it says only that the file did not parse, and where.

import Base
import ./Hs.bend as H
import ./Read.bend as R
import ./Template.bend as T
import ./Float.bend as F

# Config ------------------------------------------------------------------------------

type Pos is Data:
  Top{}
  TopH{h: U32}
  TopHM{h: U32, l: U32, r: U32, t: U32, b: U32}
  TopW{a: T.Align, w: U32}
  TopSize{a: T.Align, w: U32, h: U32}
  TopP{l: U32, r: U32}
  Bottom{}
  BottomH{h: U32}
  BottomHM{h: U32, l: U32, r: U32, t: U32, b: U32}
  BottomP{l: U32, b: U32}
  BottomW{a: T.Align, w: U32}
  BottomSize{a: T.Align, w: U32, h: U32}
  Static{x: U32, y: U32, w: U32, h: U32}
  OnScreen{n: U32, p: Pos}

type Border is Data:
  NoBorder{}
  TopB{}
  BottomB{}
  FullB{}
  TopBM{n: U32}
  BottomBM{n: U32}
  FullBM{n: U32}

type TOF is Data:
  Plain{}
  Ansi{}
  Pango{}
  Swaybar{}

type Run is Data:
  RCom{prog: String, args: List<&2, String>, msg: Maybe<&2, String>, alias: String, rate: U32}
  RDate{fmt: String, alias: String, rate: U32}
  RStdin{unsafe: Bool}
  RMon{v: R.HV}
  ROther{v: R.HV}

# Config's fields in its declaration's order (dpi as its text: a Double)
type Config is Data:
  Config{font: String, fonts: List<&2, String>, wmClass: String, wmName: String, bg: String, fg: String, position: Pos, textOutput: Bool, tof: TOF, textOffset: U32, textOffsets: List<&2, U32>, iconOffset: U32, border: Border, borderColor: String, borderWidth: U32, alpha: U32, hideOnStart: Bool, allDesktops: Bool, overrideRedirect: Bool, pickBroadest: Bool, lowerOnStart: Bool, persistent: Bool, iconRoot: String, commands: List<&2, Run>, sepChar: String, alignSep: String, template: String, verbose: Bool, dpi: String}

def default.commands() -> List<&2, Run>:
  [RDate{"%a %b %_d %Y * %H:%M:%S", "theDate", 10}, RStdin{False{}}]

# defaultConfig
def default() -> Config:
  Config{"-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*", Nil{}, "xmobar", "xmobar", "#000000", "#BFBFBF", Top{}, False{}, Plain{}, 4294967295, Nil{}, 4294967295, NoBorder{}, "#BFBFBF", 1, 255, False{}, True{}, True{}, False{}, True{}, False{}, ".", default.commands(), "%", "}{", "%StdinReader% }{ <fc=#00FF00>%uname%</fc> * <fc=#FF0000>%theDate%</fc>", False{}, "96.0"}

# A Run's alias (Exec's alias) -------------------------------------------------------

type KS is Data:
  KS{k: String, v: String}


def con.name.m(v: R.HV) -> String:
  match v:
    case R.HCon{n, _}:
      n
    case R.HRec{n, _}:
      n
    case _:
      ""

def con.name(v: R.HV) -> String:
  con.name.m(R.up(v))

def hv.str.m(v: R.HV) -> String:
  match v:
    case R.HStr{s}:
      s
    case _:
      ""

def hv.str(v: R.HV) -> String:
  hv.str.m(R.up(v))

def hv.int.text.m(v: R.HV) -> String:
  match v:
    case R.HNum{neg, +i, _, _}:
      Bool.pick(String, neg, "-" ++ i, i)
    case _:
      ""

def hv.int.text(v: R.HV) -> String:
  hv.int.text.m(R.up(v))

def arg.nth.go(xs: List<&2, R.HV>, n: Nat) -> R.HV:
  match xs n:
    case Con{x, _} 0n:
      x
    case Con{_, rest} 1n+m:
      arg.nth.go(rest, m)
    case _ _:
      R.HStr{""}

def arg.nth.m(v: R.HV, n: Nat) -> R.HV:
  match v:
    case R.HCon{_, args}:
      arg.nth.go(args, n)
    case _:
      R.HStr{""}

def arg.nth(v: R.HV, n: Nat) -> R.HV:
  arg.nth.m(R.up(v), n)

def mon.lookup(+n: String, xs: List<&2, KS>) -> String:
  match xs:
    case Nil{}:
      n
    case Con{KS{+k, +v}, rest}:
      Bool.pick(String, String.eq(k, n), v, mon.lookup(n, rest))

def mon.alias.table(+n: String) -> String:
  mon.lookup(n, [KS{"DynNetwork", "dynnetwork"}, KS{"Load", "load"}, KS{"Memory", "memory"}, KS{"Swap", "swap"}, KS{"Cpu", "cpu"}, KS{"MultiCpu", "multicpu"}, KS{"Battery", "battery"}, KS{"BatteryP", "battery"}, KS{"Brightness", "bright"}, KS{"CpuFreq", "cpufreq"}, KS{"TopProc", "top"}, KS{"TopMem", "topmem"}, KS{"CoreTemp", "coretemp"}, KS{"MultiCoreTemp", "multicoretemp"}, KS{"K10Temp", "k10temp"}, KS{"DiskU", "disku"}, KS{"DiskIO", "diskio"}, KS{"Uptime", "uptime"}, KS{"MPD", "mpd"}, KS{"MPDX", "mpd"}, KS{"AutoMPD", "autompd"}, KS{"Volume", "volume"}, KS{"Alsa", "alsa"}, KS{"Mpris1", "mpris1"}, KS{"Mpris2", "mpris2"}])

# Plugins.Monitors' alias
def mon.alias(+v: R.HV) -> String:
  +n = con.name(v)
  H.Bool.if(String, String.eq(n, "Network"), _ => hv.str(arg.nth(v, 0n)), _ => H.Bool.if(String, String.eq(n, "Thermal"), _ => hv.str(arg.nth(v, 0n)), _ => H.Bool.if(String, String.eq(n, "ThermalZone"), _ => "thermal" ++ hv.int.text(arg.nth(v, 0n)), _ => H.Bool.if(String, String.eq(n, "BatteryN"), _ => hv.str(arg.nth(v, 3n)), _ => H.Bool.if(String, String.eq(n, "CatInt"), _ => "cat" ++ hv.int.text(arg.nth(v, 0n)), _ => H.Bool.if(String, Bool.or(String.eq(n, "Weather"), Bool.or(String.eq(n, "WeatherX"), String.eq(n, "UVMeter"))), _ => hv.str(arg.nth(v, 0n)), _ => H.Bool.if(String, String.eq(n, "Wireless"), _ => hv.str(arg.nth(v, 0n)) ++ "wi", _ => mon.alias.table(n))))))))

# another plugin's alias: its alias field, or its constructor's name
def other.alias(+v: R.HV) -> String:
  +n = con.name(v)
  H.Bool.if(String, Bool.or(String.eq(n, "PipeReader"), Bool.or(String.eq(n, "CommandReader"), String.eq(n, "NamedXPropertyLog"))), _ => hv.str(arg.nth(v, 1n)), _ => H.Bool.if(String, String.eq(n, "UnsafeNamedXPropertyLog"), _ => hv.str(arg.nth(v, 1n)), _ => H.Bool.if(String, Bool.or(String.eq(n, "XPropertyLog"), Bool.or(String.eq(n, "UnsafeXPropertyLog"), String.eq(n, "BufferedPipeReader"))), _ => hv.str(arg.nth(v, 0n)), _ => H.Bool.if(String, String.eq(n, "MarqueePipeReader"), _ => hv.str(arg.nth(v, 2n)), _ => H.Bool.if(String, String.eq(n, "Kbd"), _ => "kbd", _ => H.Bool.if(String, Bool.or(String.eq(n, "Locks"), String.eq(n, "Locks'")), _ => "locks", _ => H.Bool.if(String, String.eq(n, "DateZone"), _ => hv.str(arg.nth(v, 1n)), _ => H.Bool.if(String, String.eq(n, "Mail"), _ => hv.str(arg.nth(v, 1n)), _ => H.Bool.if(String, String.eq(n, "MBox"), _ => hv.str(arg.nth(v, 2n)), _ => n)))))))))

def alias(r: Run) -> String:
  match r:
    case RCom{+p, _, _, +a, _}:
      Bool.pick(String, String.is_empty(p), "", Bool.pick(String, String.is_empty(a), p, a))
    case RDate{_, a, _}:
      a
    case RStdin{True{}}:
      "UnsafeStdinReader"
    case RStdin{False{}}:
      "StdinReader"
    case RMon{+v}:
      mon.alias(v)
    case ROther{+v}:
      other.alias(v)

# Reading a value at a type --------------------------------------------------------------

# the value of a number with no fraction or exponent, modulo 2^32
def hv.int.m(v: R.HV) -> Maybe<&2, U32>:
  match v:
    case R.HNum{neg, +i, f, e}:
      Bool.pick(Maybe<&2, U32>, Bool.and(String.is_empty(f), String.is_empty(e)), Some{Bool.pick(U32, neg, H.neg(H.digits.val(10, i, 0)), H.digits.val(10, i, 0))}, None{})
    case _:
      None{}

def hv.int(v: R.HV) -> Maybe<&2, U32>:
  hv.int.m(R.up(v))

def hv.is.str.m(v: R.HV) -> Bool:
  match v:
    case R.HStr{_}:
      True{}
    case _:
      False{}

def hv.is.str(v: R.HV) -> Bool:
  hv.is.str.m(R.up(v))

def hv.is.int(+v: R.HV) -> Bool:
  Bool.not(Maybe.is_none(&2, U32, hv.int(v)))

def all.str(xs: List<&2, R.HV>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case x <> rest:
      Bool.and(hv.is.str(x), all.str(rest))

def hv.is.strs.m(v: R.HV) -> Bool:
  match v:
    case R.HList{xs}:
      all.str(xs)
    case _:
      False{}

def hv.is.strs(v: R.HV) -> Bool:
  hv.is.strs.m(R.up(v))

def is.pair.m(v: R.HV) -> Bool:
  match v:
    case R.HTup{Con{a, Con{b, Nil{}}}}:
      Bool.and(hv.is.str(a), hv.is.str(b))
    case _:
      False{}

def is.pair(v: R.HV) -> Bool:
  is.pair.m(R.up(v))

def all.pair(xs: List<&2, R.HV>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case x <> rest:
      Bool.and(is.pair(x), all.pair(rest))

def hv.is.pairs.m(v: R.HV) -> Bool:
  match v:
    case R.HList{xs}:
      all.pair(xs)
    case _:
      False{}

def hv.is.pairs(v: R.HV) -> Bool:
  hv.is.pairs.m(R.up(v))

def hv.is.bool.m(v: R.HV) -> Bool:
  match v:
    case R.HCon{+n, Nil{}}:
      Bool.or(String.eq(n, "True"), String.eq(n, "False"))
    case _:
      False{}

def hv.is.bool(v: R.HV) -> Bool:
  hv.is.bool.m(R.up(v))

# an argument's type: S String, I Int, L [String], P [(String, String)],
# X anything (a type kept, not checked)
def arg.ok(+code: U32, +v: R.HV) -> Bool:
  H.Bool.if(Bool, U32.is_eq(code, 83), _ => hv.is.str(v), _ => H.Bool.if(Bool, U32.is_eq(code, 73), _ => hv.is.int(v), _ => H.Bool.if(Bool, U32.is_eq(code, 76), _ => hv.is.strs(v), _ => H.Bool.if(Bool, U32.is_eq(code, 80), _ => hv.is.pairs(v), _ => True{}))))

def args.ok(sig: String, args: List<&2, R.HV>) -> Bool:
  match sig args:
    case SNil{} Nil{}:
      True{}
    case SCon{c, srest} Con{+a, arest}:
      Bool.and(arg.ok(Char.to_u32(c), a), args.ok(srest, arest))
    case _ _:
      False{}

# the runnable types and their constructors' argument types (nixpkgs'
# xmobar 0.50: weather, uvmeter, iwlib, mpd, alsa, mpris, datezone, mail)
def signatures() -> List<&2, KS>:
  [KS{"Com", "SLSI"}, KS{"ComX", "SLSSI"}, KS{"Network", "SLI"}, KS{"DynNetwork", "LI"}, KS{"BatteryP", "LLI"}, KS{"BatteryN", "LLIS"}, KS{"Battery", "LI"}, KS{"DiskU", "PLI"}, KS{"DiskIO", "PLI"}, KS{"Load", "LI"}, KS{"Thermal", "SLI"}, KS{"ThermalZone", "ILI"}, KS{"Memory", "LI"}, KS{"Swap", "LI"}, KS{"Cpu", "LI"}, KS{"MultiCpu", "LI"}, KS{"Brightness", "LI"}, KS{"CpuFreq", "LI"}, KS{"CoreTemp", "LI"}, KS{"MultiCoreTemp", "LI"}, KS{"K10Temp", "SLI"}, KS{"TopProc", "LI"}, KS{"TopMem", "LI"}, KS{"Uptime", "LI"}, KS{"CatInt", "ISLI"}, KS{"Weather", "SLI"}, KS{"WeatherX", "SPLI"}, KS{"UVMeter", "SLI"}, KS{"Wireless", "SLI"}, KS{"MPD", "LI"}, KS{"MPDX", "LIS"}, KS{"AutoMPD", "L"}, KS{"Volume", "SSLI"}, KS{"Alsa", "SSL"}, KS{"Mpris1", "SLI"}, KS{"Mpris2", "SLI"}, KS{"Date", "SSI"}, KS{"DateZone", "SSSSI"}, KS{"PipeReader", "SS"}, KS{"BufferedPipeReader", "SX"}, KS{"CommandReader", "SS"}, KS{"StdinReader", ""}, KS{"UnsafeStdinReader", ""}, KS{"XMonadLog", ""}, KS{"UnsafeXMonadLog", ""}, KS{"XPropertyLog", "S"}, KS{"UnsafeXPropertyLog", "S"}, KS{"NamedXPropertyLog", "SS"}, KS{"UnsafeNamedXPropertyLog", "SS"}, KS{"EWMH", ""}, KS{"EWMHFMT", "X"}, KS{"Kbd", "P"}, KS{"Locks", ""}, KS{"Locks'", "X"}, KS{"ArchUpdates", "XI"}, KS{"Mail", "XS"}, KS{"MBox", "XLS"}, KS{"MarqueePipeReader", "SXS"}]

def sig.of(+n: String, xs: List<&2, KS>) -> Maybe<&2, String>:
  match xs:
    case Nil{}:
      None{}
    case Con{KS{+k, +v}, rest}:
      Bool.pick(Maybe<&2, String>, String.eq(k, n), Some{v}, sig.of(n, rest))

def is.monitor(+n: String) -> Bool:
  Bool.and(Bool.not(String.eq(n, "Com")), Bool.and(Bool.not(String.eq(n, "ComX")), Bool.not(Maybe.is_none(&2, String, sig.of(n, List.take(&2, KS, signatures(), 36n))))))

def strs.of.go(xs: List<&2, R.HV>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      hv.str(x) <> strs.of.go(rest)

def strs.of.m(v: R.HV) -> List<&2, String>:
  match v:
    case R.HList{xs}:
      strs.of.go(xs)
    case _:
      Nil{}

def strs.of(v: R.HV) -> List<&2, String>:
  strs.of.m(R.up(v))

def or0(m: Maybe<&2, U32>) -> U32:
  match m:
    case Some{x}:
      x
    case None{}:
      0

def run.make(+n: String, +v: R.HV) -> Run:
  H.Bool.if(Run, String.eq(n, "Com"), _ => RCom{hv.str(arg.nth(v, 0n)), strs.of(arg.nth(v, 1n)), None{}, hv.str(arg.nth(v, 2n)), or0(hv.int(arg.nth(v, 3n)))}, _ => H.Bool.if(Run, String.eq(n, "ComX"), _ => RCom{hv.str(arg.nth(v, 0n)), strs.of(arg.nth(v, 1n)), Some{hv.str(arg.nth(v, 2n))}, hv.str(arg.nth(v, 3n)), or0(hv.int(arg.nth(v, 4n)))}, _ => H.Bool.if(Run, String.eq(n, "Date"), _ => RDate{hv.str(arg.nth(v, 0n)), hv.str(arg.nth(v, 1n)), or0(hv.int(arg.nth(v, 2n)))}, _ => H.Bool.if(Run, String.eq(n, "StdinReader"), _ => RStdin{False{}}, _ => H.Bool.if(Run, String.eq(n, "UnsafeStdinReader"), _ => RStdin{True{}}, _ => Bool.pick(Run, is.monitor(n), RMon{v}, ROther{v}))))))

def con.args.m(v: R.HV) -> List<&2, R.HV>:
  match v:
    case R.HCon{_, args}:
      args
    case _:
      Nil{}

def con.args(v: R.HV) -> List<&2, R.HV>:
  con.args.m(R.up(v))

def run.of.sig(m: Maybe<&2, String>, +n: String, +x: R.HV) -> Maybe<&2, Run>:
  match m:
    case Some{sig}:
      Bool.pick(Maybe<&2, Run>, args.ok(sig, con.args(x)), Some{run.make(n, x)}, None{})
    case None{}:
      None{}

def run.of.x(+x: R.HV) -> Maybe<&2, Run>:
  +n = con.name(x)
  run.of.sig(sig.of(n, signatures()), n, x)

# Run v, v of a runnable type
# Run v, v of a runnable type: Run (v) reads two ways (Run's parens and
# v's own), so read' takes neither; (Run v) does not read
# (Run reads its value at its own precedence, so the value's arguments
# follow it: Run Memory [] 10 is Run applied, to Memory [] 10)
def run.of(v: R.HV) -> Maybe<&2, Run>:
  match v:
    case R.HCon{+run, Con{R.HCon{+n, Nil{}}, +rest}}:
      Bool.pick(Maybe<&2, Run>, String.eq(run, "Run"), run.of.x(R.HCon{n, rest}), None{})
    case R.HCon{+run, Con{R.HRec{+n, +fs}, Nil{}}}:
      Bool.pick(Maybe<&2, Run>, String.eq(run, "Run"), run.of.x(R.HRec{n, fs}), None{})
    case _:
      None{}

def runs.cons(m: Maybe<&2, Run>, r: Maybe<&2, List<&2, Run>>) -> Maybe<&2, List<&2, Run>>:
  match m r:
    case Some{x} Some{xs}:
      Some{x <> xs}
    case _ _:
      None{}

def runs.of(xs: List<&2, R.HV>) -> Maybe<&2, List<&2, Run>>:
  match xs:
    case Nil{}:
      Some{Nil{}}
    case x <> rest:
      runs.cons(run.of(x), runs.of(rest))

def read.runs.pr(p: R.PR) -> Maybe<&2, List<&2, Run>>:
  match p:
    case R.PR{R.HList{xs}, _}:
      runs.of(xs)
    case _:
      None{}

# reads at [Runnable]
def read.runs(+s: String) -> Maybe<&2, List<&2, Run>>:
  read.runs.pr(R.value(s))

# Values at Config's types ------------------------------------------------------------

def hv.bool.m(v: R.HV) -> Maybe<&2, Bool>:
  match v:
    case R.HCon{+n, _}:
      Bool.pick(Maybe<&2, Bool>, String.eq(n, "True"), Some{True{}}, Bool.pick(Maybe<&2, Bool>, String.eq(n, "False"), Some{False{}}, None{}))
    case _:
      None{}

def hv.bool(v: R.HV) -> Maybe<&2, Bool>:
  hv.bool.m(R.up(v))

def hv.align.m(v: R.HV) -> Maybe<&2, T.Align>:
  match v:
    case R.HCon{+n, Nil{}}:
      Bool.pick(Maybe<&2, T.Align>, String.eq(n, "L"), Some{T.AL{}}, Bool.pick(Maybe<&2, T.Align>, String.eq(n, "R"), Some{T.AR{}}, Bool.pick(Maybe<&2, T.Align>, String.eq(n, "C"), Some{T.AC{}}, None{})))
    case _:
      None{}

def hv.align(v: R.HV) -> Maybe<&2, T.Align>:
  hv.align.m(R.up(v))

def int.arg(+v: R.HV, n: Nat) -> Maybe<&2, U32>:
  hv.int(arg.nth(v, n))

def nargs(v: R.HV) -> Nat:
  List.length(&2, R.HV, con.args(v))

def mi(m: Maybe<&2, U32>) -> Bool:
  Bool.not(Maybe.is_none(&2, U32, m))

def ma(m: Maybe<&2, T.Align>) -> Bool:
  Bool.not(Maybe.is_none(&2, T.Align, m))

def align.or(m: Maybe<&2, T.Align>) -> T.Align:
  match m:
    case Some{a}:
      a
    case None{}:
      T.AC{}

def field.int(fs: List<&2, R.HF>, +name: String) -> Maybe<&2, U32>:
  match fs:
    case Con{R.HF{+n, v}, _}:
      Bool.pick(Maybe<&2, U32>, String.eq(n, name), hv.int(v), None{})
    case Nil{}:
      None{}

def fields.drop1(fs: List<&2, R.HF>) -> List<&2, R.HF>:
  match fs:
    case Con{_, rest}:
      rest
    case Nil{}:
      Nil{}

# Static {xpos, ypos, width, height}: the fields in their order, all there
def hv.static(+fs: List<&2, R.HF>) -> Maybe<&2, Pos>:
  +f1 = fields.drop1(fs)
  +f2 = fields.drop1(f1)
  +f3 = fields.drop1(f2)
  +a = field.int(fs, "xpos")
  +b = field.int(f1, "ypos")
  +c = field.int(f2, "width")
  +d = field.int(f3, "height")
  Bool.pick(Maybe<&2, Pos>, Bool.and(List.is_empty(&2, R.HF, fields.drop1(f3)), Bool.and(mi(a), Bool.and(mi(b), Bool.and(mi(c), mi(d))))), Some{Static{or0(a), or0(b), or0(c), or0(d)}}, None{})

# a constructor and its first k arguments (the rest is what reads leaves)
def pos.args(+v: R.HV, +k: Nat) -> Bool:
  Nat.is_ge(nargs(v), k)

def pos.ints.go(k: Nat, +v: R.HV) -> Bool:
  match k:
    case 0n:
      True{}
    case 1n+k2:
      +j = k2
      Bool.and(mi(int.arg(v, j)), pos.ints.go(j, v))

def pos.ints(+v: R.HV, +k: Nat) -> Bool:
  Bool.and(pos.args(v, k), pos.ints.go(k, v))

def i(+v: R.HV, +n: Nat) -> U32:
  or0(int.arg(v, n))

def static.of.m(v: R.HV) -> Maybe<&2, Pos>:
  match v:
    case R.HRec{_, fs}:
      hv.static(fs)
    case _:
      None{}

def static.of(v: R.HV) -> Maybe<&2, Pos>:
  static.of.m(R.up(v))

def onscreen(+n: U32, m: Maybe<&2, Pos>) -> Maybe<&2, Pos>:
  match m:
    case Some{p}:
      Some{OnScreen{n, p}}
    case None{}:
      None{}

# XPosition at a value (fuel: OnScreen's nesting)
def hv.pos(fuel: Nat, +v: R.HV) -> Maybe<&2, Pos>:
  match fuel:
    case 0n:
      None{}
    case 1n+f:
      +n = con.name(v)
      H.Bool.if(Maybe<&2, Pos>, String.eq(n, "Top"), _ => Some{Top{}}, _ => H.Bool.if(Maybe<&2, Pos>, String.eq(n, "Bottom"), _ => Some{Bottom{}}, _ => H.Bool.if(Maybe<&2, Pos>, Bool.and(String.eq(n, "TopH"), pos.ints(v, 1n)), _ => Some{TopH{i(v, 0n)}}, _ => H.Bool.if(Maybe<&2, Pos>, Bool.and(String.eq(n, "BottomH"), pos.ints(v, 1n)), _ => Some{BottomH{i(v, 0n)}}, _ => H.Bool.if(Maybe<&2, Pos>, Bool.and(String.eq(n, "TopHM"), pos.ints(v, 5n)), _ => Some{TopHM{i(v, 0n), i(v, 1n), i(v, 2n), i(v, 3n), i(v, 4n)}}, _ => H.Bool.if(Maybe<&2, Pos>, Bool.and(String.eq(n, "BottomHM"), pos.ints(v, 5n)), _ => Some{BottomHM{i(v, 0n), i(v, 1n), i(v, 2n), i(v, 3n), i(v, 4n)}}, _ => H.Bool.if(Maybe<&2, Pos>, Bool.and(String.eq(n, "TopP"), pos.ints(v, 2n)), _ => Some{TopP{i(v, 0n), i(v, 1n)}}, _ => H.Bool.if(Maybe<&2, Pos>, Bool.and(String.eq(n, "BottomP"), pos.ints(v, 2n)), _ => Some{BottomP{i(v, 0n), i(v, 1n)}}, _ => H.Bool.if(Maybe<&2, Pos>, Bool.and(String.eq(n, "TopW"), Bool.and(pos.args(v, 2n), Bool.and(ma(hv.align(arg.nth(v, 0n))), mi(int.arg(v, 1n))))), _ => Some{TopW{align.or(hv.align(arg.nth(v, 0n))), i(v, 1n)}}, _ => H.Bool.if(Maybe<&2, Pos>, Bool.and(String.eq(n, "BottomW"), Bool.and(pos.args(v, 2n), Bool.and(ma(hv.align(arg.nth(v, 0n))), mi(int.arg(v, 1n))))), _ => Some{BottomW{align.or(hv.align(arg.nth(v, 0n))), i(v, 1n)}}, _ => H.Bool.if(Maybe<&2, Pos>, Bool.and(String.eq(n, "TopSize"), Bool.and(pos.args(v, 3n), Bool.and(ma(hv.align(arg.nth(v, 0n))), Bool.and(mi(int.arg(v, 1n)), mi(int.arg(v, 2n)))))), _ => Some{TopSize{align.or(hv.align(arg.nth(v, 0n))), i(v, 1n), i(v, 2n)}}, _ => H.Bool.if(Maybe<&2, Pos>, Bool.and(String.eq(n, "BottomSize"), Bool.and(pos.args(v, 3n), Bool.and(ma(hv.align(arg.nth(v, 0n))), Bool.and(mi(int.arg(v, 1n)), mi(int.arg(v, 2n)))))), _ => Some{BottomSize{align.or(hv.align(arg.nth(v, 0n))), i(v, 1n), i(v, 2n)}}, _ => H.Bool.if(Maybe<&2, Pos>, String.eq(n, "Static"), _ => static.of(v), _ => H.Bool.if(Maybe<&2, Pos>, Bool.and(String.eq(n, "OnScreen"), Bool.and(pos.args(v, 2n), mi(int.arg(v, 0n)))), _ => onscreen(i(v, 0n), hv.pos(f, arg.nth(v, 1n))), _ => None{}))))))))))))))

def hv.border(+v: R.HV) -> Maybe<&2, Border>:
  +n = con.name(v)
  H.Bool.if(Maybe<&2, Border>, String.eq(n, "NoBorder"), _ => Some{NoBorder{}}, _ => H.Bool.if(Maybe<&2, Border>, String.eq(n, "TopB"), _ => Some{TopB{}}, _ => H.Bool.if(Maybe<&2, Border>, String.eq(n, "BottomB"), _ => Some{BottomB{}}, _ => H.Bool.if(Maybe<&2, Border>, String.eq(n, "FullB"), _ => Some{FullB{}}, _ => H.Bool.if(Maybe<&2, Border>, Bool.and(String.eq(n, "TopBM"), pos.ints(v, 1n)), _ => Some{TopBM{i(v, 0n)}}, _ => H.Bool.if(Maybe<&2, Border>, Bool.and(String.eq(n, "BottomBM"), pos.ints(v, 1n)), _ => Some{BottomBM{i(v, 0n)}}, _ => H.Bool.if(Maybe<&2, Border>, Bool.and(String.eq(n, "FullBM"), pos.ints(v, 1n)), _ => Some{FullBM{i(v, 0n)}}, _ => None{})))))))

def hv.tof(+v: R.HV) -> Maybe<&2, TOF>:
  +n = con.name(v)
  H.Bool.if(Maybe<&2, TOF>, String.eq(n, "Plain"), _ => Some{Plain{}}, _ => H.Bool.if(Maybe<&2, TOF>, String.eq(n, "Ansi"), _ => Some{Ansi{}}, _ => H.Bool.if(Maybe<&2, TOF>, String.eq(n, "Pango"), _ => Some{Pango{}}, _ => H.Bool.if(Maybe<&2, TOF>, String.eq(n, "Swaybar"), _ => Some{Swaybar{}}, _ => None{}))))

def hv.dbl.m(v: R.HV) -> Maybe<&2, String>:
  match v:
    case R.HNum{neg, i, +f, e}:
      Some{Bool.pick(String, neg, "-", "") ++ i ++ Bool.pick(String, String.is_empty(f), "", "." ++ f) ++ e}
    case _:
      None{}

def hv.dbl(v: R.HV) -> Maybe<&2, String>:
  hv.dbl.m(R.up(v))


# Parsing the file --------------------------------------------------------------------

type Val is Data:
  VStr{s: String}
  VStrs{ss: List<&2, String>}
  VBool{b: Bool}
  VInt{n: U32}
  VInts{ns: List<&2, U32>}
  VPos{p: Pos}
  VBorder{b: Border}
  VTof{t: TOF}
  VRuns{rs: List<&2, Run>}
  VDbl{s: String}

# A field: which, its name, and how its value is read (S a string, L a
# list of strings, R Read at its type, I a list of Ints, C the commands)
type FK is Data:
  FFont{}
  FAdditionalFonts{}
  FWmClass{}
  FWmName{}
  FBgColor{}
  FFgColor{}
  FPosition{}
  FTextOutput{}
  FTextOutputFormat{}
  FTextOffset{}
  FTextOffsets{}
  FIconOffset{}
  FBorder{}
  FBorderColor{}
  FBorderWidth{}
  FAlpha{}
  FHideOnStart{}
  FAllDesktops{}
  FOverrideRedirect{}
  FPickBroadest{}
  FLowerOnStart{}
  FPersistent{}
  FIconRoot{}
  FCommands{}
  FSepChar{}
  FAlignSep{}
  FTemplate{}
  FVerbose{}
  FDpi{}
  FSignal{}

type FD is Data:
  FD{k: FK, name: String, how: U32}

# the fields in the order parsec's permute tries them
def perm() -> List<&2, FD>:
  [FD{FDpi{}, "dpi", 82}, FD{FSignal{}, "signal", 82}, FD{FVerbose{}, "verbose", 82}, FD{FTemplate{}, "template", 83}, FD{FAlignSep{}, "alignSep", 83}, FD{FSepChar{}, "sepChar", 83}, FD{FCommands{}, "commands", 67}, FD{FIconRoot{}, "iconRoot", 82}, FD{FPersistent{}, "persistent", 82}, FD{FLowerOnStart{}, "lowerOnStart", 82}, FD{FPickBroadest{}, "pickBroadest", 82}, FD{FOverrideRedirect{}, "overrideRedirect", 82}, FD{FAllDesktops{}, "allDesktops", 82}, FD{FHideOnStart{}, "hideOnStart", 82}, FD{FAlpha{}, "alpha", 82}, FD{FBorderWidth{}, "borderWidth", 82}, FD{FBorderColor{}, "borderColor", 83}, FD{FBorder{}, "border", 82}, FD{FIconOffset{}, "iconOffset", 82}, FD{FTextOffsets{}, "textOffsets", 73}, FD{FTextOffset{}, "textOffset", 82}, FD{FTextOutputFormat{}, "textOutputFormat", 82}, FD{FTextOutput{}, "textOutput", 82}, FD{FPosition{}, "position", 82}, FD{FFgColor{}, "fgColor", 83}, FD{FBgColor{}, "bgColor", 83}, FD{FWmName{}, "wmName", 83}, FD{FWmClass{}, "wmClass", 83}, FD{FAdditionalFonts{}, "additionalFonts", 76}, FD{FFont{}, "font", 83}]

# one field of a Config set to its value
def set.field(k: FK, v: Val, c: Config) -> Config:
  match k v c:
    case FFont{} VStr{x} Config{_, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{x, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FAdditionalFonts{} VStrs{x} Config{font, _, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, x, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FWmClass{} VStr{x} Config{font, fonts, _, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, x, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FWmName{} VStr{x} Config{font, fonts, wmClass, _, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, x, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FBgColor{} VStr{x} Config{font, fonts, wmClass, wmName, _, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, x, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FFgColor{} VStr{x} Config{font, fonts, wmClass, wmName, bg, _, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, x, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FPosition{} VPos{x} Config{font, fonts, wmClass, wmName, bg, fg, _, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, x, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FTextOutput{} VBool{x} Config{font, fonts, wmClass, wmName, bg, fg, position, _, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, x, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FTextOutputFormat{} VTof{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, _, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, x, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FTextOffset{} VInt{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, _, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, x, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FTextOffsets{} VInts{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, _, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, x, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FIconOffset{} VInt{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, _, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, x, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FBorder{} VBorder{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, _, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, x, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FBorderColor{} VStr{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, _, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, x, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FBorderWidth{} VInt{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, _, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, x, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FAlpha{} VInt{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, _, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, x, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FHideOnStart{} VBool{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, _, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, x, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FAllDesktops{} VBool{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, _, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, x, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FOverrideRedirect{} VBool{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, _, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, x, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FPickBroadest{} VBool{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, _, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, x, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FLowerOnStart{} VBool{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, _, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, x, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FPersistent{} VBool{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, _, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, x, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}
    case FIconRoot{} VStr{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, _, commands, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, x, commands, sepChar, alignSep, template, verbose, dpi}
    case FCommands{} VRuns{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, _, sepChar, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, x, sepChar, alignSep, template, verbose, dpi}
    case FSepChar{} VStr{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, _, alignSep, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, x, alignSep, template, verbose, dpi}
    case FAlignSep{} VStr{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, _, template, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, x, template, verbose, dpi}
    case FTemplate{} VStr{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, _, verbose, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, x, verbose, dpi}
    case FVerbose{} VBool{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, _, dpi}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, x, dpi}
    case FDpi{} VDbl{x} Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, _}:
      Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, x}
    case _ _ cfg:
      cfg

# stripComments: each line to its first -- outside a string (the quotes
# counted from the line's start)
def strip.line(fuel: Nat, +m: Bool, +s: String) -> String:
  match fuel:
    case 0n:
      ""
    case 1n+f:
      +g = f
      H.Bool.if(String, String.is_empty(s), _ => "", _ => H.Bool.if(String, String.starts_with(s, "--"), _ => Bool.pick(String, m, "--" ++ strip.line(g, m, String.drop(s, 2n)), ""), _ => H.Bool.if(String, H.first.is(s, 34), _ => "\"" ++ strip.line(g, Bool.not(m), H.tail(s)), _ => String.take(s, 1n) ++ strip.line(g, m, H.tail(s)))))

def strip.lines(ls: List<&2, String>) -> String:
  match ls:
    case Nil{}:
      ""
    case Con{+l, rest}:
      strip.line(Nat.add(String.length(l), 1n), False{}, l) ++ "\n" ++ strip.lines(rest)

def strip(+text: String) -> String:
  strip.lines(H.lines(text))

type VR is Data:
  VR{v: Val, rest: String}
  VRErr{at: String}

def sp(+s: String) -> String:
  H.drop.space(s)

# strMulti's text, from after its opening quote (or a continuation's \):
# up to a quote; \ and a newline go on at the next line's \
def str.body(fuel: Nat, +s: String, +acc: String) -> VR:
  match fuel:
    case 0n:
      VRErr{s}
    case 1n+f:
      +g = f
      +t = String.drop(s, 2n)
      H.Bool.if(VR, String.is_empty(s), _ => VRErr{s}, _ => H.Bool.if(VR, String.starts_with(s, "\\\n"), _ => H.Bool.if(VR, H.first.is(t, 34), _ => VR{VStr{String.reverse(acc)}, H.tail(t)}, _ => Bool.pick(VR, H.first.is(sp(t), 92), str.body(g, H.tail(sp(t)), acc), VRErr{t})), _ => H.Bool.if(VR, H.first.is(s, 34), _ => VR{VStr{String.reverse(acc)}, H.tail(s)}, _ => str.body(g, H.tail(s), SCon{Char.from_u32(H.char0(s)), acc}))))

def str.multi(+s: String) -> VR:
  +t = sp(s)
  Bool.pick(VR, H.first.is(t, 34), str.body(Nat.add(String.length(t), 1n), H.tail(t), ""), VRErr{t})

def vr.str(r: VR) -> Maybe<&2, String>:
  match r:
    case VR{VStr{s}, _}:
      Some{s}
    case _:
      None{}

def vr.rest(r: VR) -> String:
  match r:
    case VR{_, rest}:
      rest
    case VRErr{at}:
      at

def vr.ok(r: VR) -> Bool:
  match r:
    case VR{_, _}:
      True{}
    case VRErr{_}:
      False{}

def str.or(m: Maybe<&2, String>) -> String:
  match m:
    case Some{s}:
      s
    case None{}:
      ""

def str.list.end(+s: String, xs: List<&2, String>) -> VR:
  +t = sp(s)
  Bool.pick(VR, H.first.is(t, 93), VR{VStrs{xs}, H.tail(t)}, VRErr{t})

# strList: [ strings, each followed by spaces, between commas ]
def str.items(fuel: Nat, +s: String, +acc: List<&2, String>) -> VR:
  match fuel:
    case 0n:
      VRErr{s}
    case 1n+f:
      +g = f
      +r = str.multi(s)
      +after = sp(vr.rest(r))
      H.Bool.if(VR, vr.ok(r), _ => H.Bool.if(VR, H.first.is(after, 44), _ => str.items(g, H.tail(after), (str.or(vr.str(r)) <> acc)), _ => str.list.end(after, List.reverse(&2, String, (str.or(vr.str(r)) <> acc)))), _ => VRErr{vr.rest(r)})

def str.list(+s: String) -> VR:
  +t = sp(s)
  +u = H.tail(t)
  H.Bool.if(VR, Bool.not(H.first.is(t, 91)), _ => VRErr{t}, _ => H.Bool.if(VR, H.first.is(sp(u), 34), _ => str.items(Nat.add(String.length(u), 1n), u, Nil{}), _ => H.Bool.if(VR, H.is.space(H.char0(u)), _ => VRErr{sp(u)}, _ => str.list.end(u, Nil{}))))

# tillFieldEnd: Static {..}, (the comma after it too), else the text to a
# comma, a brace or the line's end
type TF is Data:
  TF{text: String, rest: String}
  TFErr{at: String}

def not.end.len(s: String) -> Nat:
  match s:
    case SNil{}:
      0n
    case SCon{c, rest}:
      +u = Char.to_u32(c)
      H.Bool.if(Nat, Bool.or(U32.is_eq(u, 44), Bool.or(U32.is_eq(u, 125), Bool.or(U32.is_eq(u, 10), U32.is_eq(u, 13)))), _ => 0n, _ => 1n+not.end.len(rest))

def not.brace.len(s: String) -> Nat:
  match s:
    case SNil{}:
      0n
    case SCon{c, rest}:
      H.Bool.if(Nat, H.c.is(c, 125), _ => 0n, _ => 1n+not.brace.len(rest))

def static.pos(+s: String) -> TF:
  +a = sp(String.drop(s, 6n))
  +b = sp(H.tail(a))
  +n = not.brace.len(b)
  +c = String.drop(b, n)
  +d = sp(H.tail(c))
  H.Bool.if(TF, Bool.not(String.starts_with(s, "Static")), _ => TFErr{s}, _ => H.Bool.if(TF, Bool.not(H.first.is(a, 123)), _ => TFErr{a}, _ => H.Bool.if(TF, Bool.not(H.first.is(c, 125)), _ => TFErr{c}, _ => Bool.pick(TF, H.first.is(d, 44), TF{"Static {" ++ String.take(b, n) ++ "}", H.tail(d)}, TFErr{d}))))

def till.end(+s: String) -> TF:
  +n = not.end.len(s)
  H.Bool.if(TF, H.first.is(s, 83), _ => static.pos(s), _ => TF{String.take(s, n), String.drop(s, n)})

def m.map.pos(m: Maybe<&2, Pos>) -> Maybe<&2, Val>:
  match m:
    case Some{x}:
      Some{VPos{x}}
    case None{}:
      None{}

def m.map.border(m: Maybe<&2, Border>) -> Maybe<&2, Val>:
  match m:
    case Some{x}:
      Some{VBorder{x}}
    case None{}:
      None{}

def m.map.tof(m: Maybe<&2, TOF>) -> Maybe<&2, Val>:
  match m:
    case Some{x}:
      Some{VTof{x}}
    case None{}:
      None{}

def m.map.dbl(m: Maybe<&2, String>) -> Maybe<&2, Val>:
  match m:
    case Some{x}:
      Some{VDbl{x}}
    case None{}:
      None{}

def m.map.bool(m: Maybe<&2, Bool>) -> Maybe<&2, Val>:
  match m:
    case Some{x}:
      Some{VBool{x}}
    case None{}:
      None{}

def m.map.int(m: Maybe<&2, U32>) -> Maybe<&2, Val>:
  match m:
    case Some{x}:
      Some{VInt{x}}
    case None{}:
      None{}

# reads at the field's type: the one parse, what follows it left
def m.map.str(+v: R.HV) -> Maybe<&2, Val>:
  Bool.pick(Maybe<&2, Val>, hv.is.str(v), Some{VStr{hv.str(v)}}, None{})

def read.as(k: FK, +v: R.HV) -> Maybe<&2, Val>:
  match k:
    case FIconRoot{}:
      m.map.str(v)
    case FPosition{}:
      m.map.pos(hv.pos(8n, v))
    case FBorder{}:
      m.map.border(hv.border(v))
    case FTextOutputFormat{}:
      m.map.tof(hv.tof(v))
    case FDpi{}:
      m.map.dbl(hv.dbl(v))
    case FTextOutput{}:
      m.map.bool(hv.bool(v))
    case FHideOnStart{}:
      m.map.bool(hv.bool(v))
    case FAllDesktops{}:
      m.map.bool(hv.bool(v))
    case FOverrideRedirect{}:
      m.map.bool(hv.bool(v))
    case FPickBroadest{}:
      m.map.bool(hv.bool(v))
    case FLowerOnStart{}:
      m.map.bool(hv.bool(v))
    case FPersistent{}:
      m.map.bool(hv.bool(v))
    case FVerbose{}:
      m.map.bool(hv.bool(v))
    case _:
      m.map.int(hv.int(v))

def vr.of(m: Maybe<&2, Val>, +rest: String, +at: String) -> VR:
  match m:
    case Some{v}:
      VR{v, rest}
    case None{}:
      VRErr{at}

def read.pr(k: FK, p: R.PR, +rest: String, +at: String) -> VR:
  match p:
    case R.PR{v, _}:
      vr.of(read.as(k, v), rest, at)
    case R.PRBad{}:
      VRErr{at}

def read.field.tf(k: FK, t: TF, +at: String) -> VR:
  match t:
    case TF{+text, +rest}:
      read.pr(k, R.value(text), rest, at)
    case TFErr{a}:
      VRErr{a}

def read.field(k: FK, +s: String) -> VR:
  read.field.tf(k, till.end(s), s)

# readIntList: [ Ints between commas ], each as parsec-numbers' int reads
# it (a sign, then 0x.., 0o.., or decimal digits)
type NI is Data:
  NI{n: U32, rest: String}
  NIErr{at: String}

def nat.of(+s: String) -> NI:
  +t = H.tail(s)
  +x = U32.or(H.char0(t), 32)
  +h = H.digits.len(2, H.tail(t))
  +o = H.digits.len(0, H.tail(t))
  +d = H.digits.len(1, s)
  H.Bool.if(NI, Bool.and(H.first.is(s, 48), U32.is_eq(x, 120)), _ => Bool.pick(NI, Nat.is_gt(h, 0n), NI{H.digits.val(16, String.take(H.tail(t), h), 0), String.drop(H.tail(t), h)}, NIErr{H.tail(t)}), _ => H.Bool.if(NI, Bool.and(H.first.is(s, 48), U32.is_eq(x, 111)), _ => Bool.pick(NI, Nat.is_gt(o, 0n), NI{H.digits.val(8, String.take(H.tail(t), o), 0), String.drop(H.tail(t), o)}, NIErr{H.tail(t)}), _ => Bool.pick(NI, Nat.is_gt(d, 0n), NI{H.digits.val(10, String.take(s, d), 0), String.drop(s, d)}, NIErr{s})))

def ni.neg(r: NI) -> NI:
  match r:
    case NI{n, rest}:
      NI{H.neg(n), rest}
    case NIErr{a}:
      NIErr{a}

def int.of(+s: String) -> NI:
  H.Bool.if(NI, H.first.is(s, 45), _ => ni.neg(nat.of(H.tail(s))), _ => nat.of(Bool.pick(String, H.first.is(s, 43), H.tail(s), s)))

def ints.go(fuel: Nat, r: NI, +acc: List<&2, U32>) -> VR:
  match fuel r:
    case 0n _:
      VRErr{""}
    case 1n+f NI{+n, rest}:
      +t = sp(rest)
      H.Bool.if(VR, H.first.is(t, 44), _ => ints.go(f, int.of(sp(H.tail(t))), n <> acc), _ => Bool.pick(VR, H.first.is(t, 93), VR{VInts{List.reverse(&2, U32, n <> acc)}, H.tail(t)}, VRErr{t}))
    case 1n+f NIErr{a}:
      VRErr{a}

def int.list(+s: String) -> VR:
  +t = sp(s)
  +u = H.tail(t)
  +v = sp(u)
  H.Bool.if(VR, Bool.not(H.first.is(t, 91)), _ => VRErr{t}, _ => H.Bool.if(VR, Bool.or(H.is.digit(H.char0(v)), Bool.or(H.first.is(v, 45), H.first.is(v, 43))), _ => ints.go(Nat.add(String.length(v), 1n), int.of(v), Nil{}), _ => H.Bool.if(VR, H.is.space(H.char0(u)), _ => VRErr{v}, _ => Bool.pick(VR, H.first.is(u, 93), VR{VInts{Nil{}}, H.tail(u)}, VRErr{u}))))

# readCommands: the text to a ] that a } follows, or a comma not followed
# by Run; the text with ] read as [Runnable]
def cmd.end(+s: String) -> Maybe<&2, String>:
  +t = sp(s)
  +u = sp(H.tail(t))
  +w = H.tail(u)
  H.Bool.if(Maybe<&2, String>, Bool.not(H.first.is(t, 93)), _ => None{}, _ => H.Bool.if(Maybe<&2, String>, H.first.is(u, 125), _ => Some{w}, _ => Bool.pick(Maybe<&2, String>, Bool.and(H.first.is(u, 44), Bool.not(String.starts_with(sp(w), "Run"))), Some{w}, None{})))

def cmd.scan(fuel: Nat, m: Maybe<&2, String>, +s: String, +acc: String) -> TF:
  match fuel m:
    case 0n _:
      TFErr{s}
    case _ Some{rest}:
      TF{String.reverse(acc), rest}
    case 1n+f None{}:
      H.Bool.if(TF, String.is_empty(s), _ => TFErr{s}, _ => cmd.scan(f, cmd.end(H.tail(s)), H.tail(s), SCon{Char.from_u32(H.char0(s)), acc}))

def commands.runs(m: Maybe<&2, List<&2, Run>>, +rest: String, +at: String) -> VR:
  match m:
    case Some{rs}:
      VR{VRuns{rs}, rest}
    case None{}:
      VRErr{at}

def commands.tf(t: TF, +at: String) -> VR:
  match t:
    case TF{+text, rest}:
      commands.runs(read.runs(text ++ "]"), rest, at)
    case TFErr{a}:
      VRErr{a}

def commands(+s: String) -> VR:
  commands.tf(cmd.scan(Nat.add(String.length(s), 1n), cmd.end(s), s, ""), s)

# The fields' loop (parseConf's perms, then eof) ------------------------------------------

def read.or.signal(k: FK, +s: String) -> VR:
  match k:
    case FSignal{}:
      VRErr{s}
    case k2:
      read.field(k2, s)

def field.value(+how: U32, k: FK, +s: String) -> VR:
  H.Bool.if(VR, U32.is_eq(how, 83), _ => str.multi(s), _ => H.Bool.if(VR, U32.is_eq(how, 76), _ => str.list(s), _ => H.Bool.if(VR, U32.is_eq(how, 73), _ => int.list(s), _ => H.Bool.if(VR, U32.is_eq(how, 67), _ => commands(s), _ => read.or.signal(k, s)))))

def fd.name(d: FD) -> String:
  match d:
    case FD{_, n, _}:
      n

def find.field(+t: String, ds: List<&2, FD>) -> Maybe<&2, FD>:
  match ds:
    case Nil{}:
      None{}
    case Con{+d, rest}:
      H.Bool.if(Maybe<&2, FD>, String.starts_with(t, fd.name(d)), _ => Some{d}, _ => find.field(t, rest))

def remove.field(+name: String, ds: List<&2, FD>) -> List<&2, FD>:
  match ds:
    case Nil{}:
      Nil{}
    case Con{+d, +rest}:
      Bool.pick(List<&2, FD>, String.eq(fd.name(d), name), rest, d <> remove.field(name, rest))

# fieldEnd: spaces, commas and closing braces
def field.end(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{+c, +rest}:
      +u = Char.to_u32(c)
      Bool.pick(String, Bool.or(H.is.space(u), Bool.or(U32.is_eq(u, 44), U32.is_eq(u, 125))), field.end(rest), SCon{c, rest})

# the state's field names: those not given are defaulted
def state.fields() -> List<&2, String>:
  ["font", "additionalFonts", "bgColor", "fgColor", "wmClass", "wmName", "sepChar", "alignSep", "border", "borderColor", "template", "position", "textOffset", "textOffsets", "iconOffset", "allDesktops", "overrideRedirect", "pickBroadest", "hideOnStart", "lowerOnStart", "persistent", "iconRoot", "alpha", "commands", "verbose", "signal", "textOutput", "textOutputFormat", "dpi"]

def in.list(+x: String, xs: List<&2, String>) -> Bool:
  match xs:
    case Nil{}:
      False{}
    case y <> rest:
      Bool.or(String.eq(x, y), in.list(x, rest))

def missing(+parsed: List<&2, String>, fs: List<&2, String>) -> List<&2, String>:
  match fs:
    case Nil{}:
      Nil{}
    case Con{+f, rest}:
      +r = missing(parsed, rest)
      Bool.pick(List<&2, String>, in.list(f, parsed), r, f <> r)

type CR is Data:
  CROk{c: Config, missing: List<&2, String>}
  CRErr{at: String}

type LM is Data:
  LFind{}
  LGot{d: Maybe<&2, FD>}
  LVal{d: FD, r: VR}

def loop(fuel: Nat, m: LM, +t: String, +left: List<&2, FD>, +parsed: List<&2, String>, +c: Config) -> CR:
  match fuel m:
    case 0n _:
      CRErr{t}
    case 1n+f LFind{}:
      loop(f, LGot{find.field(t, left)}, t, left, parsed, c)
    case 1n+f LGot{None{}}:
      Bool.pick(CR, String.is_empty(t), CROk{c, missing(parsed, state.fields())}, CRErr{t})
    case 1n+f LGot{Some{FD{+k, +name, +how}}}:
      +t1 = sp(String.drop(t, String.length(name)))
      H.Bool.if(CR, H.first.is(t1, 61), _ => loop(f, LVal{FD{k, name, how}, field.value(how, k, sp(H.tail(t1)))}, t, left, parsed, c), _ => CRErr{t1})
    case 1n+f LVal{FD{k, +name, _}, VR{v, rest}}:
      loop(f, LFind{}, field.end(sp(rest)), remove.field(name, left), name <> parsed, set.field(k, v, c))
    case 1n+f LVal{_, VRErr{a}}:
      CRErr{a}

# parseConfig defaultConfig: the config and the fields left at their
# defaults, or where the text stopped reading
def parse(+text: String) -> CR:
  +t = sp(strip(text))
  +t1 = sp(String.drop(t, 6n))
  +t2 = sp(H.tail(t1))
  H.Bool.if(CR, Bool.and(String.starts_with(t, "Config"), H.first.is(t1, 123)), _ => loop(Nat.add(Nat.mul(3n, 31n), 8n), LFind{}, t2, perm(), Nil{}, default()), _ => CRErr{t})

# Show (Config's derived Show, signal = SignalChan Nothing) -------------------------------

def show.align(a: T.Align) -> String:
  T.show.align(a)

def show.pos(fuel: Nat, +p: U32, q: Pos) -> String:
  match fuel q:
    case 0n _:
      ""
    case _ Top{}:
      "Top"
    case _ Bottom{}:
      "Bottom"
    case _ TopH{h}:
      H.hs.paren(U32.is_gt(p, 10), "TopH " ++ H.show.i32(11n, h))
    case _ BottomH{h}:
      H.hs.paren(U32.is_gt(p, 10), "BottomH " ++ H.show.i32(11n, h))
    case _ TopHM{h, l, r, t, b}:
      H.hs.paren(U32.is_gt(p, 10), "TopHM " ++ H.show.i32(11n, h) ++ " " ++ H.show.i32(11n, l) ++ " " ++ H.show.i32(11n, r) ++ " " ++ H.show.i32(11n, t) ++ " " ++ H.show.i32(11n, b))
    case _ BottomHM{h, l, r, t, b}:
      H.hs.paren(U32.is_gt(p, 10), "BottomHM " ++ H.show.i32(11n, h) ++ " " ++ H.show.i32(11n, l) ++ " " ++ H.show.i32(11n, r) ++ " " ++ H.show.i32(11n, t) ++ " " ++ H.show.i32(11n, b))
    case _ TopP{l, r}:
      H.hs.paren(U32.is_gt(p, 10), "TopP " ++ H.show.i32(11n, l) ++ " " ++ H.show.i32(11n, r))
    case _ BottomP{l, b}:
      H.hs.paren(U32.is_gt(p, 10), "BottomP " ++ H.show.i32(11n, l) ++ " " ++ H.show.i32(11n, b))
    case _ TopW{a, w}:
      H.hs.paren(U32.is_gt(p, 10), "TopW " ++ show.align(a) ++ " " ++ H.show.i32(11n, w))
    case _ BottomW{a, w}:
      H.hs.paren(U32.is_gt(p, 10), "BottomW " ++ show.align(a) ++ " " ++ H.show.i32(11n, w))
    case _ TopSize{a, w, h}:
      H.hs.paren(U32.is_gt(p, 10), "TopSize " ++ show.align(a) ++ " " ++ H.show.i32(11n, w) ++ " " ++ H.show.i32(11n, h))
    case _ BottomSize{a, w, h}:
      H.hs.paren(U32.is_gt(p, 10), "BottomSize " ++ show.align(a) ++ " " ++ H.show.i32(11n, w) ++ " " ++ H.show.i32(11n, h))
    case _ Static{x, y, w, h}:
      H.hs.paren(U32.is_gt(p, 10), "Static {xpos = " ++ H.show.i32(0n, x) ++ ", ypos = " ++ H.show.i32(0n, y) ++ ", width = " ++ H.show.i32(0n, w) ++ ", height = " ++ H.show.i32(0n, h) ++ "}")
    case 1n+f OnScreen{n, q2}:
      H.hs.paren(U32.is_gt(p, 10), "OnScreen " ++ H.show.i32(11n, n) ++ " " ++ show.pos(f, 11, q2))

def show.border(b: Border) -> String:
  match b:
    case NoBorder{}:
      "NoBorder"
    case TopB{}:
      "TopB"
    case BottomB{}:
      "BottomB"
    case FullB{}:
      "FullB"
    case TopBM{n}:
      "TopBM " ++ H.show.i32(11n, n)
    case BottomBM{n}:
      "BottomBM " ++ H.show.i32(11n, n)
    case FullBM{n}:
      "FullBM " ++ H.show.i32(11n, n)

def show.tof(t: TOF) -> String:
  match t:
    case Plain{}:
      "Plain"
    case Ansi{}:
      "Ansi"
    case Pango{}:
      "Pango"
    case Swaybar{}:
      "Swaybar"

def show.bool(b: Bool) -> String:
  match b:
    case True{}:
      "True"
    case False{}:
      "False"

def show.strs(xs: List<&2, String>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      H.show.str(x) <> show.strs(rest)

def show.ints(xs: List<&2, U32>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      H.show.i32(0n, x) <> show.ints(rest)

def show.run(r: Run) -> String:
  match r:
    case RCom{p, a, None{}, al, rt}:
      "Run Com " ++ H.show.str(p) ++ " " ++ H.show.list(show.strs(a)) ++ " " ++ H.show.str(al) ++ " " ++ H.show.i32(11n, rt)
    case RCom{p, a, Some{m}, al, rt}:
      "Run ComX " ++ H.show.str(p) ++ " " ++ H.show.list(show.strs(a)) ++ " " ++ H.show.str(m) ++ " " ++ H.show.str(al) ++ " " ++ H.show.i32(11n, rt)
    case RDate{f, a, rt}:
      "Run Date " ++ H.show.str(f) ++ " " ++ H.show.str(a) ++ " " ++ H.show.i32(11n, rt)
    case RStdin{True{}}:
      "Run UnsafeStdinReader"
    case RStdin{False{}}:
      "Run StdinReader"
    case RMon{v}:
      "Run " ++ R.show(v)
    case ROther{v}:
      "Run " ++ R.show(v)

def show.runs(rs: List<&2, Run>) -> List<&2, String>:
  match rs:
    case Nil{}:
      Nil{}
    case r <> rest:
      show.run(r) <> show.runs(rest)

def show.dpi.of(m: Maybe<&2, F32>) -> String:
  match m:
    case Some{x}:
      F.show(0n, x)
    case None{}:
      "?"

def show.dpi(+s: String) -> String:
  show.dpi.of(F.read(s))

def show(c: Config) -> String:
  match c:
    case Config{font, fonts, wmClass, wmName, bg, fg, position, textOutput, tof, textOffset, textOffsets, iconOffset, border, borderColor, borderWidth, alpha, hideOnStart, allDesktops, overrideRedirect, pickBroadest, lowerOnStart, persistent, iconRoot, commands, sepChar, alignSep, template, verbose, dpi}:
      "Config {font = " ++ H.show.str(font) ++ ", additionalFonts = " ++ H.show.list(show.strs(fonts)) ++ ", wmClass = " ++ H.show.str(wmClass) ++ ", wmName = " ++ H.show.str(wmName) ++ ", bgColor = " ++ H.show.str(bg) ++ ", fgColor = " ++ H.show.str(fg) ++ ", position = " ++ show.pos(16n, 0, position) ++ ", textOutput = " ++ show.bool(textOutput) ++ ", textOutputFormat = " ++ show.tof(tof) ++ ", textOffset = " ++ H.show.i32(0n, textOffset) ++ ", textOffsets = " ++ H.show.list(show.ints(textOffsets)) ++ ", iconOffset = " ++ H.show.i32(0n, iconOffset) ++ ", border = " ++ show.border(border) ++ ", borderColor = " ++ H.show.str(borderColor) ++ ", borderWidth = " ++ H.show.i32(0n, borderWidth) ++ ", alpha = " ++ H.show.i32(0n, alpha) ++ ", hideOnStart = " ++ show.bool(hideOnStart) ++ ", allDesktops = " ++ show.bool(allDesktops) ++ ", overrideRedirect = " ++ show.bool(overrideRedirect) ++ ", pickBroadest = " ++ show.bool(pickBroadest) ++ ", lowerOnStart = " ++ show.bool(lowerOnStart) ++ ", persistent = " ++ show.bool(persistent) ++ ", iconRoot = " ++ H.show.str(iconRoot) ++ ", commands = " ++ H.show.list(show.runs(commands)) ++ ", sepChar = " ++ H.show.str(sepChar) ++ ", alignSep = " ++ H.show.str(alignSep) ++ ", template = " ++ H.show.str(template) ++ ", verbose = " ++ show.bool(verbose) ++ ", signal = SignalChan Nothing, dpi = " ++ show.dpi(dpi) ++ "}"

