# What every xmobar monitor shares, in Bend (Xmobar.Plugins.Monitors.Common):
# its configuration (MConfig, mkMConfig's defaults), the options every
# monitor reads (pluginOptions, doConfigOptions, through GetOpt.bend), the
# output helpers (Common.Output: padString, the colors, percentages, bars,
# icon patterns, log scaling, units), and the monitor's own template
# (Common.Parsers: <name> fields among text, <fc=..> and <icon=../>, each
# field's value parsed again as a template, and the total width's trim).
#
# Haskell reads the numeric options with read, lazily: a number that does
# not read fails the monitor when its value is first used, which every run
# does (the bar, the colors, the paddings, the total width), so here it
# fails the run: the monitor shows "error: Prelude.read: no parse". Ints
# are Int32s (U32s in two's complement).

import Base
import ./Hs.bend as H
import ./Big.bend as B
import ./Float.bend as F
import ./GetOpt.bend as G

# MConfig ----------------------------------------------------------------------------

# a number option: Some value, or None when it did not read
type MC is Data:
  MC{nc: Maybe<&2, String>, low: Maybe<&2, U32>, lc: Maybe<&2, String>, high: Maybe<&2, U32>, hc: Maybe<&2, String>, tmpl: String, export: List<&2, String>, ppad: Maybe<&2, U32>, dd: Maybe<&2, U32>, minw: Maybe<&2, U32>, maxw: Maybe<&2, U32>, ell: String, padc: String, padr: Bool, bback: String, bfore: String, bw: Maybe<&2, U32>, suffix: Bool, na: String, maxtw: Maybe<&2, U32>, maxtell: String}

# mkMConfig template exports
def mk(tmpl: String, export: List<&2, String>) -> MC:
  MC{None{}, Some{33}, None{}, Some{66}, None{}, tmpl, export, Some{0}, Some{0}, Some{0}, Some{0}, "", " ", False{}, ":", "#", Some{10}, False{}, "N/A", Some{0}, ""}

# the same MConfig with every number read: what a run uses
type C is Data:
  C{nc: Maybe<&2, String>, low: U32, lc: Maybe<&2, String>, high: U32, hc: Maybe<&2, String>, tmpl: String, export: List<&2, String>, ppad: U32, dd: U32, minw: U32, maxw: U32, ell: String, padc: String, padr: Bool, bback: String, bfore: String, bw: U32, suffix: Bool, na: String, maxtw: U32, maxtell: String}

def all.read(ms: List<&2, Maybe<&2, U32>>) -> Bool:
  match ms:
    case Nil{}:
      True{}
    case Some{_} <> rest:
      all.read(rest)
    case None{} <> _:
      False{}

def or0(m: Maybe<&2, U32>) -> U32:
  match m:
    case Some{x}:
      x
    case None{}:
      0

# Some config, or None when a number did not read
def ready(m: MC) -> Maybe<&2, C>:
  match m:
    case MC{nc, +low, lc, +high, hc, t, e, +pp, +dd, +mn, +mx, ell, pc, pr, bb, bf, +bw, su, na, +mt, mtell}:
      Bool.pick(Maybe<&2, C>, all.read([low, high, pp, dd, mn, mx, bw, mt]), Some{C{nc, or0(low), lc, or0(high), hc, t, e, or0(pp), or0(dd), or0(mn), or0(mx), ell, pc, pr, bb, bf, or0(bw), su, na, or0(mt), mtell}}, None{})

# The options -----------------------------------------------------------------------

def plugin.options() -> List<&2, G.OptD>:
  [G.OptD{0, "H", ["High"], G.ReqArg{"number"}, "The high threshold"},
   G.OptD{1, "L", ["Low"], G.ReqArg{"number"}, "The low threshold"},
   G.OptD{2, "h", ["high"], G.ReqArg{"color number"}, "Color for the high threshold: ex \"#FF0000\""},
   G.OptD{3, "n", ["normal"], G.ReqArg{"color number"}, "Color for the normal threshold: ex \"#00FF00\""},
   G.OptD{4, "l", ["low"], G.ReqArg{"color number"}, "Color for the low threshold: ex \"#0000FF\""},
   G.OptD{5, "t", ["template"], G.ReqArg{"output template"}, "Output template."},
   G.OptD{6, "S", ["suffix"], G.ReqArg{"True/False"}, "Use % to display percents or other suffixes."},
   G.OptD{7, "d", ["ddigits"], G.ReqArg{"decimal digits"}, "Number of decimal digits to display."},
   G.OptD{8, "p", ["ppad"], G.ReqArg{"percent padding"}, "Minimum percentage width."},
   G.OptD{9, "m", ["minwidth"], G.ReqArg{"minimum width"}, "Minimum field width"},
   G.OptD{10, "M", ["maxwidth"], G.ReqArg{"maximum width"}, "Maximum field width"},
   G.OptD{11, "w", ["width"], G.ReqArg{"fixed width"}, "Fixed field width"},
   G.OptD{12, "e", ["maxwidthellipsis"], G.ReqArg{"Maximum width ellipsis"}, "Ellipsis to be added to the field when it has reached its max width."},
   G.OptD{13, "c", ["padchars"], G.ReqArg{"padding chars"}, "Characters to use for padding"},
   G.OptD{14, "a", ["align"], G.ReqArg{"padding alignment"}, "'l' for left padding, 'r' for right"},
   G.OptD{15, "b", ["bback"], G.ReqArg{"bar background"}, "Characters used to draw bar backgrounds"},
   G.OptD{16, "f", ["bfore"], G.ReqArg{"bar foreground"}, "Characters used to draw bar foregrounds"},
   G.OptD{17, "W", ["bwidth"], G.ReqArg{"bar width"}, "Bar width"},
   G.OptD{18, "x", ["nastring"], G.ReqArg{"N/A string"}, "String used when the monitor is not available"},
   G.OptD{19, "T", ["maxtwidth"], G.ReqArg{"Maximum total width"}, "Maximum total width"},
   G.OptD{20, "E", ["maxtwidthellipsis"], G.ReqArg{"Maximum total width ellipsis"}, "Ellipsis to be added to the total text when it has reached its max width."}]

def nz.of(m: Maybe<&2, U32>) -> Maybe<&2, U32>:
  match m:
    case Some{+x}:
      Some{Bool.pick(U32, F.i.is.neg(x), 0, x)}
    case None{}:
      None{}

# nz: max 0 (read s)
def nz(+s: String) -> Maybe<&2, U32>:
  nz.of(H.read.int(s))

def is.true(+s: String) -> Bool:
  Bool.or(String.eq(s, "True"), Bool.or(String.eq(s, "true"), Bool.or(String.eq(s, "Yes"), Bool.or(String.eq(s, "yes"), Bool.or(String.eq(s, "On"), String.eq(s, "on"))))))

def set.more(+tag: U32, +v: String, +m: MC) -> MC:
  match m:
    case MC{nc, low, lc, high, hc, t, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, bw, su, na, mt, mtell}:
      H.Bool.if(MC, U32.is_eq(tag, 9), _ => MC{nc, low, lc, high, hc, t, e, pp, dd, nz(v), mx, ell, pc, pr, bb, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 10), _ => MC{nc, low, lc, high, hc, t, e, pp, dd, mn, nz(v), ell, pc, pr, bb, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 11), _ => MC{nc, low, lc, high, hc, t, e, pp, dd, nz(v), nz(v), ell, pc, pr, bb, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 12), _ => MC{nc, low, lc, high, hc, t, e, pp, dd, mn, mx, v, pc, pr, bb, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 13), _ => MC{nc, low, lc, high, hc, t, e, pp, dd, mn, mx, ell, v, pr, bb, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 14), _ => MC{nc, low, lc, high, hc, t, e, pp, dd, mn, mx, ell, pc, String.starts_with(v, "r"), bb, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 15), _ => MC{nc, low, lc, high, hc, t, e, pp, dd, mn, mx, ell, pc, pr, v, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 16), _ => MC{nc, low, lc, high, hc, t, e, pp, dd, mn, mx, ell, pc, pr, bb, v, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 17), _ => MC{nc, low, lc, high, hc, t, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, nz(v), su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 18), _ => MC{nc, low, lc, high, hc, t, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, bw, su, v, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 19), _ => MC{nc, low, lc, high, hc, t, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, bw, su, na, nz(v), mtell}, _ => MC{nc, low, lc, high, hc, t, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, bw, su, na, mt, v})))))))))))

# one option's effect (doConfigOptions)
def set(+tag: U32, +v: String, +m: MC) -> MC:
  match m:
    case MC{nc, low, lc, high, hc, t, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, bw, su, na, mt, mtell}:
      H.Bool.if(MC, U32.is_eq(tag, 0), _ => MC{nc, low, lc, H.read.int(v), hc, t, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 1), _ => MC{nc, H.read.int(v), lc, high, hc, t, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 2), _ => MC{nc, low, lc, high, Some{v}, t, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 3), _ => MC{Some{v}, low, lc, high, hc, t, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 4), _ => MC{nc, low, Some{v}, high, hc, t, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 5), _ => MC{nc, low, lc, high, hc, v, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 6), _ => MC{nc, low, lc, high, hc, t, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, bw, is.true(v), na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 7), _ => MC{nc, low, lc, high, hc, t, e, pp, nz(v), mn, mx, ell, pc, pr, bb, bf, bw, su, na, mt, mtell}, _ => H.Bool.if(MC, U32.is_eq(tag, 8), _ => MC{nc, low, lc, high, hc, t, e, nz(v), dd, mn, mx, ell, pc, pr, bb, bf, bw, su, na, mt, mtell}, _ => set.more(tag, v, MC{nc, low, lc, high, hc, t, e, pp, dd, mn, mx, ell, pc, pr, bb, bf, bw, su, na, mt, mtell}))))))))))

def val.or(m: Maybe<&2, String>) -> String:
  match m:
    case Some{v}:
      v
    case None{}:
      ""

def set.all(os: List<&2, G.Opt>, m: MC) -> MC:
  match os:
    case Nil{}:
      m
    case G.Opt{tag, v} <> rest:
      set.all(rest, set(tag, val.or(v), m))

# doArgs before the run: the options set, the non-options for the monitor
# (what follows "--"), or the errors' text
type Args is Data:
  Args{m: MC, rest: List<&2, String>}
  ArgsErr{text: String}

def args.of(g: G.Got, m: MC) -> Args:
  match g:
    case G.Got{os, xs, Nil{}}:
      Args{set.all(os, m), xs}
    case G.Got{_, _, es}:
      ArgsErr{String.concat(es)}

def args(+argv: List<&2, String>, m: MC) -> Args:
  args.of(G.getopt(plugin.options(), argv), m)

# Output (Common.Output) --------------------------------------------------------------

def cycle(+pad: String, +n: Nat) -> String:
  String.take(String.repeat(pad, n), n)

# padString mnw mxw pad pr ellipsis s (Ints as Nats: mnw and mxw are
# never negative, nz keeps them so)
def pad(+mnw: U32, +mxw: U32, +padc: String, +pr: Bool, +ell: String, +s: String) -> String:
  +len = String.length(s)
  +rmin = U32.to_nat(mnw)
  +rmax = Bool.pick(Nat, U32.is_eq(mxw, 0), Nat.max(len, rmin), U32.to_nat(mxw))
  +rmn = Nat.min(rmin, rmax)
  +rmx = Nat.max(rmin, rmax)
  +rlen = Nat.min(Nat.max(rmn, len), rmx)
  +ps = cycle(padc, Nat.sub(rlen, len))
  H.Bool.if(String, Nat.is_lt(rlen, len), _ => String.take(s, rlen) ++ ell, _ => Bool.pick(String, pr, s ++ ps, ps ++ s))

def with.padding(c: C, +s: String) -> String:
  match c:
    case C{_, _, _, _, _, _, _, _, _, mn, mx, ell, pc, pr, _, _, _, _, _, _, _}:
      pad(mn, mx, pc, pr, ell, s)

def fc(m: Maybe<&2, String>, +s: String) -> String:
  match m:
    case None{}:
      s
    case Some{c}:
      "<fc=" ++ c ++ ">" ++ s ++ "</fc>"

def i2f(+n: U32) -> F32:
  H.Bool.if(F32, F.i.is.neg(n), _ => F32.neg(U32.to_f32(F.i.neg(n))), _ => U32.to_f32(n))

# colorizeString x s: the high color above the higher threshold, the
# normal one above the lower, else the low one
def colorize(c: C, +x: F32, +s: String) -> String:
  match c:
    case C{nc, +low, lc, +high, hc, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _}:
      +a = i2f(low)
      +b = i2f(high)
      +lo = Bool.pick(F32, F.i.lt(high, low), b, a)
      +hi = Bool.pick(F32, F.i.lt(high, low), a, b)
      H.Bool.if(String, F32.is_gt(x, hi), _ => fc(hc, s), _ => H.Bool.if(String, F32.is_gt(x, lo), _ => fc(nc, s), _ => fc(lc, s)))

# showWithColors f x, the text f x given
def with.colors(+c: C, +str: String, +x: F32) -> String:
  colorize(c, x, with.padding(c, str))

# floatToPercent
def percent(c: C, +x: F32) -> String:
  match c:
    case C{_, _, _, _, _, _, _, +pp, _, _, _, _, pc, pr, _, _, _, su, _, _, _}:
      pad(pp, pp, pc, pr, "", F.fixed(0n, (x * 100.0 : F32))) ++ Bool.pick(String, su, "%", "")

# showPercentWithColors
def percent.colors(+c: C, +x: F32) -> String:
  with.colors(c, percent(c, x), (x * 100.0 : F32))

def percents.colors(+c: C, xs: List<&2, F32>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      percent.colors(c, x) <> percents.colors(c, rest)

# round at Float to Int, as an Int32: NaN, the infinities and what is past
# Int's 64 bits are minBound (what GHC's float2Int answers), kept as the
# Int32 minBound; what is past 2^31 is the Int32 bound of its sign, so it
# compares as Haskell's does with every small Int
def round.i(+x: F32) -> U32:
  +r = F.round.f(x)
  H.Bool.if(U32, Bool.or(F.is.nan(r), Bool.not(F32.is_lt(F32.abs(r), 9223372036854775808.0))), _ => 2147483648, _ => H.Bool.if(U32, F32.is_ge(r, 2147483648.0), _ => 2147483647, _ => H.Bool.if(U32, F32.is_le(r, F32.neg(2147483648.0)), _ => 2147483649, _ => H.Bool.if(U32, F.is.neg(r), _ => F.i.neg(F32.to_u32(F32.neg(r))), _ => F32.to_u32(r)))))

def ceiling.i(+x: F32) -> U32:
  +r = F32.ceil(x)
  H.Bool.if(U32, Bool.or(F.is.nan(r), Bool.not(F32.is_lt(F32.abs(r), 9223372036854775808.0))), _ => 2147483648, _ => H.Bool.if(U32, F32.is_ge(r, 2147483648.0), _ => 2147483647, _ => H.Bool.if(U32, F32.is_le(r, F32.neg(2147483648.0)), _ => 2147483649, _ => H.Bool.if(U32, F.is.neg(r), _ => F.i.neg(F32.to_u32(F32.neg(r))), _ => F32.to_u32(r)))))

def nat.of(+n: U32) -> Nat:
  Bool.pick(Nat, F.i.is.neg(n), 0n, U32.to_nat(n))

# xs !! n (the last when n is past the end: Haskell would fail)
def char.nth(s: String, n: Nat) -> String:
  match s n:
    case SCon{c, SNil{}} _:
      SCon{c, SNil{}}
    case SCon{c, _} 0n:
      SCon{c, SNil{}}
    case SCon{_, rest} 1n+m:
      char.nth(rest, m)
    case SNil{} _:
      ""

def bar.cycle(+c: C, +v: F32, +bb: String, +bf: String, +bw: U32, +r: U32) -> String:
  +len = Bool.pick(U32, F.i.lt(bw, r), bw, r)
  colorize(c, v, cycle(bf, nat.of(len))) ++ cycle(bb, nat.of(U32.sub(bw, len)))

def bar.char(+c: C, +v: F32, +bf: String, +w: Nat, +x: F32) -> String:
  +len = Nat.min(w, nat.of(ceiling.i((U32.to_f32(U32.from_nat(w)) * x : F32))))
  colorize(c, v, char.nth(bf, Nat.sub(len, 1n)))

# showPercentBar v x
def bar(+c: C, +v: F32, +x: F32) -> String:
  match c:
    case C{_, _, _, _, _, _, _, _, _, _, _, _, _, _, +bb, +bf, +bw, _, _, _, _}:
      H.Bool.if(String, U32.is_lt(bw, 1), _ => bar.char(c, v, bf, String.length(bf), x), _ => bar.cycle(c, v, bb, bf, bw, round.i((U32.to_f32(bw) * x : F32))))

# Haskell's div at Int32 by 12 (rounding down)
def div12(+n: U32) -> U32:
  H.Bool.if(U32, F.i.is.neg(n), _ => F.i.neg(U32.div(U32.add(F.i.neg(n), 11), 12)), _ => U32.div(n, 12))

# showVerticalBar v x
def vbar(+c: C, +v: F32, +x: F32) -> String:
  +t = U32.add(9600, div12(round.i((100.0 * x : F32))))
  colorize(c, v, H.Bool.if(String, F.i.lt(t, 9601), _ => " ", _ => SCon{Char.from_u32(Bool.pick(U32, F.i.lt(9608, t), 9608, t)), SNil{}}))

# parseIconPattern: the text split at %%, joined again with the number
def icon.split.cons(c: Char, xs: List<&2, String>) -> List<&2, String>:
  match xs:
    case x <> rest:
      SCon{c, x} <> rest
    case Nil{}:
      [SCon{c, SNil{}}]

def icon.split(fuel: Nat, +s: String) -> List<&2, String>:
  match fuel s:
    case 0n _:
      [""]
    case _ SNil{}:
      [""]
    case 1n+f SCon{+c, +rest}:
      +g = f
      H.Bool.if(List<&2, String>, Bool.and(H.c.is(c, 37), H.first.is(rest, 37)), _ => "" <> icon.split(g, H.tail(rest)), _ => icon.split.cons(c, icon.split(g, rest)))

def join(+sep: String, xs: List<&2, String>) -> String:
  match xs:
    case Nil{}:
      ""
    case x <> Nil{}:
      x
    case x <> rest:
      x ++ sep ++ join(sep, rest)

# showIconPattern p x
def ipat(p: Maybe<&2, String>, +x: F32) -> String:
  match p:
    case None{}:
      ""
    case Some{+path}:
      +t = div12(round.i((100.0 * x : F32)))
      join(F.i.show(Bool.pick(U32, F.i.lt(t, 1), 0, Bool.pick(U32, F.i.lt(8, t), 8, t))), icon.split(Nat.add(String.length(path), 1n), path))

# logScaling f v
def log.scale(c: C, +f: F32, +v: F32) -> F32:
  match c:
    case C{_, +low, _, +high, _, _, _, _, _, _, _, _, _, _, _, _, +bw, _, _, _, _}:
      +a = i2f(low)
      +b = i2f(high)
      +lo = Bool.pick(F32, F.i.lt(high, low), b, a)
      +hi = Bool.pick(F32, F.i.lt(high, low), a, b)
      +bw2 = Bool.pick(F32, U32.is_gt(bw, 0), U32.to_f32(bw), 10.0)
      H.Bool.if(F32, F32.is_eq(v, 0.0), _ => 0.0, _ => H.Bool.if(F32, F32.is_le(v, lo), _ => (1.0 / bw2 : F32), _ => (f + ((F32.log((v / hi : F32)) / F32.log(2.0) : F32) / bw2 : F32) : F32)))

def log.bar(+c: C, +f: F32, +v: F32) -> String:
  bar(c, v, log.scale(c, f, v))

def log.vbar(+c: C, +f: F32, +v: F32) -> String:
  vbar(c, v, log.scale(c, f, v))

def log.ipat(+c: C, p: Maybe<&2, String>, +f: F32, +v: F32) -> String:
  ipat(p, log.scale(c, f, v))

def units(+n: U32) -> String:
  H.Bool.if(String, U32.is_eq(n, 0), _ => "B", _ => H.Bool.if(String, U32.is_eq(n, 1), _ => "K", _ => H.Bool.if(String, U32.is_eq(n, 2), _ => "M", _ => H.Bool.if(String, U32.is_eq(n, 3), _ => "G", _ => "T"))))

def pow10.f(n: Nat) -> F32:
  match n:
    case 0n:
      1.0
    case 1n+m:
      (pow10.f(m) * 10.0 : F32)

# showWithUnits d n x
def with.units(fuel: Nat, +d: Nat, +n: U32, +x: F32) -> String:
  match fuel:
    case 0n:
      ""
    case 1n+f:
      +g = f
      H.Bool.if(String, F32.is_lt(x, 0.0), _ => "-" ++ with.units(g, d, n, F32.neg(x)), _ => H.Bool.if(String, Bool.or(U32.is_gt(n, 3), F32.is_lt(x, pow10.f(1n+d))), _ => F.show.round(x) ++ units(n), _ => H.Bool.if(String, F32.is_le(x, 1024.0), _ => F.fixed(d, (x / 1024.0 : F32)) ++ units(U32.add(n, 1)), _ => with.units(g, d, U32.add(n, 1), (x / 1024.0 : F32)))))

# takeDigits d x
def take.digits(+d: Nat, +x: F32) -> F32:
  +fact = pow10.f(d)
  (F.round.f((x * fact : F32)) / fact : F32)

# The monitor's template (Common.Parsers) -------------------------------------------

type Tri is Data:
  Tri{s: String, com: String, ss: String}

def first.is(s: String, +u: U32) -> Bool:
  H.first.is(s, u)

def not.lt.len(s: String) -> Nat:
  match s:
    case SNil{}:
      0n
    case SCon{c, rest}:
      H.Bool.if(Nat, H.c.is(c, 60), _ => 0n, _ => 1n+not.lt.len(rest))

def fcspec.len(s: String) -> Nat:
  match s:
    case SNil{}:
      0n
    case SCon{c, rest}:
      +u = Char.to_u32(c)
      H.Bool.if(Nat, Bool.or(H.is.alnum(u), Bool.or(U32.is_eq(u, 44), U32.is_eq(u, 35))), _ => 1n+fcspec.len(rest), _ => 0n)

def icon.len.inc(m: Maybe<&2, Nat>) -> Maybe<&2, Nat>:
  match m:
    case Some{n}:
      Some{1n+n}
    case None{}:
      None{}

# "<icon=" chars not > up to "/>": how many characters, or None
def icon.len(+s: String) -> Maybe<&2, Nat>:
  match s:
    case SNil{}:
      None{}
    case SCon{c, r}:
      H.Bool.if(Maybe<&2, Nat>, String.starts_with(s, "/>"), _ => Some{2n}, _ => H.Bool.if(Maybe<&2, Nat>, H.c.is(c, 62), _ => None{}, _ => icon.len.inc(icon.len(r))))

def maybe.or0(m: Maybe<&2, Nat>) -> Nat:
  match m:
    case Some{n}:
      n
    case None{}:
      0n

# an icon spec's length, or 0 when it does not parse (try: nothing read)
def icon.piece(m: Maybe<&2, Nat>) -> Nat:
  match m:
    case Some{n}:
      Nat.add(6n, n)
    case None{}:
      0n

# how long the piece of nonPlaceHolder at s is: text without <, </fc>,
# <fc=..>, <icon=../>; 0: none
def piece.len(+s: String) -> Nat:
  +t = not.lt.len(s)
  +f = fcspec.len(String.drop(s, 4n))
  H.Bool.if(Nat, Nat.is_gt(t, 0n), _ => t, _ => H.Bool.if(Nat, String.starts_with(s, "</fc>"), _ => 5n, _ => H.Bool.if(Nat, Bool.and(String.starts_with(s, "<fc="), Bool.and(Nat.is_gt(f, 0n), first.is(String.drop(s, Nat.add(4n, f)), 62))), _ => Nat.add(5n, f), _ => H.Bool.if(Nat, String.starts_with(s, "<icon="), _ => icon.piece(icon.len(String.drop(s, 6n))), _ => 0n))))

# nonPlaceHolder's length
def nph.len(fuel: Nat, +s: String) -> Nat:
  match fuel:
    case 0n:
      0n
    case 1n+g:
      +n = piece.len(s)
      H.Bool.if(Nat, Nat.is_eq(n, 0n), _ => 0n, _ => Nat.add(n, nph.len(g, String.drop(s, n))))

def nph(+s: String) -> Nat:
  nph.len(Nat.add(String.length(s), 1n), s)

def tris.cons(t: Tri, m: Maybe<&2, List<&2, Tri>>) -> Maybe<&2, List<&2, Tri>>:
  match m:
    case Some{ts}:
      Some{t <> ts}
    case None{}:
      None{}

def gt.len(s: String) -> Nat:
  match s:
    case SNil{}:
      0n
    case SCon{c, rest}:
      H.Bool.if(Nat, H.c.is(c, 62), _ => 0n, _ => 1n+gt.len(rest))

# many templateStringParser; None: it failed after reading (runP: [])
def tris(fuel: Nat, +s: String) -> Maybe<&2, List<&2, Tri>>:
  match fuel:
    case 0n:
      None{}
    case 1n+g:
      +a = nph(s)
      +r = String.drop(s, a)
      +c = gt.len(H.tail(r))
      +r2 = String.drop(H.tail(r), c)
      +r3 = H.tail(r2)
      +b = nph(r3)
      H.Bool.if(Maybe<&2, List<&2, Tri>>, first.is(r, 60), _ => H.Bool.if(Maybe<&2, List<&2, Tri>>, first.is(r2, 62), _ => tris.cons(Tri{String.take(s, a), String.take(H.tail(r), c), String.take(r3, b)}, tris(g, String.drop(r3, b))), _ => None{}), _ => Bool.pick(Maybe<&2, List<&2, Tri>>, Nat.is_eq(a, 0n), Some{Nil{}}, None{}))

def tris.or(m: Maybe<&2, List<&2, Tri>>) -> List<&2, Tri>:
  match m:
    case Some{ts}:
      ts
    case None{}:
      Nil{}

# runP templateParser t
def run.p(+t: String) -> List<&2, Tri>:
  tris.or(tris(Nat.add(String.length(t), 1n), t))

type KV is Data:
  KV{k: String, v: String}

def zip.kv(ks: List<&2, String>, vs: List<&2, String>) -> List<&2, KV>:
  match ks vs:
    case Con{k, krest} Con{v, vrest}:
      KV{k, v} <> zip.kv(krest, vrest)
    case _ _:
      Nil{}

# Map.lookup in Map.fromList: the last pair with the key
def lookup(+k: String, kvs: List<&2, KV>, acc: Maybe<&2, String>) -> Maybe<&2, String>:
  match kvs:
    case Nil{}:
      acc
    case KV{+key, +v} <> rest:
      lookup(k, rest, Bool.pick(Maybe<&2, String>, String.eq(k, key), Some{v}, acc))

# combine: each field's value, itself parsed as a template (f "" = r): one
# def, its mode the list of a template's parts or one field's value
type CM is Data:
  CList{ts: List<&2, Tri>}
  CField{com: String, v: Maybe<&2, String>}

def combine(fuel: Nat, mode: CM, +m: List<&2, KV>) -> String:
  match fuel mode:
    case 0n _:
      ""
    case 1n+g CList{Nil{}}:
      ""
    case 1n+g CList{Tri{s, +com, ss} <> rest}:
      +h = g
      s ++ combine(h, CField{com, lookup(com, m, None{})}, m) ++ ss ++ combine(h, CList{rest}, m)
    case 1n+g CField{com, None{}}:
      "<" ++ com ++ ">"
    case 1n+g CField{_, Some{+r}}:
      +x = combine(g, CList{run.p(r)}, m)
      Bool.pick(String, String.is_empty(x), r, x)

def kv.len(kvs: List<&2, KV>) -> Nat:
  match kvs:
    case Nil{}:
      0n
    case KV{_, v} <> rest:
      Nat.add(String.length(v), kv.len(rest))

# parseTemplate' t m
def expand(+t: String, +m: List<&2, KV>) -> String:
  combine(Nat.add(Nat.mul(4n, Nat.add(String.length(t), kv.len(m))), 16n), CList{run.p(t)}, m)

def until.gt(s: String) -> Nat:
  gt.len(s)

type TR is Data:
  TR{n: U32, p: String}

# trimTo n p s: n characters of text kept, every <..> tag kept whole
def trim.to(fuel: Nat, +n: U32, +p: String, +s: String) -> TR:
  match fuel:
    case 0n:
      TR{n, p}
    case 1n+f:
      +g = f
      +cs = H.tail(s)
      +k = gt.len(cs)
      +t = not.lt.len(s)
      +pt = String.take(s, t)
      H.Bool.if(TR, String.is_empty(s), _ => TR{n, p}, _ => H.Bool.if(TR, first.is(s, 60), _ => trim.to(g, n, p ++ "<" ++ String.take(cs, k) ++ ">", String.drop(cs, 1n+k)), _ => H.Bool.if(TR, U32.is_eq(n, 0), _ => trim.to(g, 0, p, String.drop(s, t)), _ => H.Bool.if(TR, Bool.not(F.i.lt(n, U32.from_nat(t))), _ => trim.to(g, U32.sub(n, U32.from_nat(t)), p ++ pt, String.drop(s, t)), _ => trim.to(g, 0, p ++ String.take(pt, nat.of(n)), String.drop(s, t))))))

def trim.fin(r: TR, +ell: String) -> String:
  match r:
    case TR{+n, +p}:
      Bool.pick(String, F.i.lt(0, n), p, p ++ ell)

# parseTemplate values: the monitor's template with its fields' values,
# trimmed to the total width (with its ellipsis) when it is longer
def template(c: C, +values: List<&2, String>) -> String:
  match c:
    case C{_, _, _, _, _, +t, +e, _, _, _, _, _, _, _, _, _, _, _, _, +w, +ell}:
      +s = expand(t, zip.kv(e, values))
      +len = String.length(s)
      H.Bool.if(String, Bool.and(U32.is_gt(w, 0), Bool.and(Bool.not(F.i.is.neg(w)), Nat.is_gt(len, U32.to_nat(w)))), _ => trim.fin(trim.to(Nat.add(Nat.mul(2n, len), 2n), U32.sub(w, U32.from_nat(String.length(ell))), "", s), ell), _ => s)

# a run: the options (or their errors' text), the numbers read (or the
# read's error), then the monitor's own run on the config and the
# non-options
def run.ready(m: Maybe<&2, C>, +rest: List<&2, String>, k: C -> List<&2, String> -> String) -> String:
  match m:
    case Some{c}:
      k(c, rest)
    case None{}:
      "error: Prelude.read: no parse"

def run.args(a: Args, k: C -> List<&2, String> -> String) -> String:
  match a:
    case Args{m, rest}:
      run.ready(ready(m), rest, k)
    case ArgsErr{t}:
      t

def run(+argv: List<&2, String>, m: MC, k: C -> List<&2, String> -> String) -> String:
  run.args(args(argv, m), k)

