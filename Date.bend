# xmobar's Date plugin's text in Bend: time 1.12.2's formatTime with
# defaultTimeLocale at a ZonedTime (Data.Time.Format.Format.Class and
# .Instances), as the Date plugin calls it every rate tenths of a second.
# The time comes broken down, as localtime gives it (the C side), with
# the zone's offset and name.
#
# formatTime: after a %, one of _ - 0 ^ # (padding with spaces, none,
# zeros; upper or lower case), a width, an E, then the specifier; a %
# with nothing after it is itself. Each specifier is a string or a number
# with its own default padding (getPadOption); %c %R %T %X %r %D %F %x
# format their own formats, then pad the whole as a string; a specifier
# the locale does not know is nothing.

import Base
import ./Hs.bend as H

# a broken-down local time: the year, the month (1-12), the day (1-31),
# hours, minutes, seconds (0-60), nanoseconds, the weekday (0 Sunday), the
# day of the year (0-365), the zone's offset in minutes (an Int32) and
# name, and the seconds since the epoch
type Tm is Data:
  Tm{year: U32, mon: U32, mday: U32, hour: U32, min: U32, sec: U32, nsec: U32, wday: U32, yday: U32, off: U32, zone: String, epoch: U32}

def nth(+n: U32, xs: List<&2, String>) -> String:
  H.nth(U32.to_nat(n), xs)

def days.long() -> List<&2, String>:
  ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]

def days.short() -> List<&2, String>:
  ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

def months.long() -> List<&2, String>:
  ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

def months.short() -> List<&2, String>:
  ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

def neg(+n: U32) -> U32:
  H.neg(n)

def is.neg(+n: U32) -> Bool:
  H.is.neg(n)

# Haskell's div and mod at Int32 by a positive d
def div(+n: U32, +d: U32) -> U32:
  H.Bool.if(U32, is.neg(n), _ => neg(U32.div(U32.add(neg(n), U32.sub(d, 1)), d)), _ => U32.div(n, d))

def mod(+n: U32, +d: U32) -> U32:
  U32.sub(n, U32.mul(div(n, d), d))

def digits(+n: U32) -> String:
  Nat.show(U32.to_nat(n))

# Padding (Data.Time.Calendar.Private's PadOption, Class's getPadOption) ---------------

type PO is Data:
  NoPad{}
  Pad{i: Nat, c: U32}

# a specifier's modifiers: the padding asked (None: none asked; Some{None}:
# -, no padding; Some{Some{c}}: _ or 0), the width asked
type Mod is Data:
  Mod{pad: Maybe<&2, Maybe<&2, U32>>, width: Maybe<&2, Nat>}

def po.c(p: Maybe<&2, Maybe<&2, U32>>, +cdef: U32) -> U32:
  match p:
    case Some{Some{c}}:
      c
    case Some{None{}}:
      32
    case None{}:
      cdef

def po.f(m: Mod, +fdef: Bool) -> Bool:
  match m:
    case Mod{_, Some{_}}:
      True{}
    case Mod{None{}, None{}}:
      fdef
    case Mod{Some{None{}}, None{}}:
      False{}
    case Mod{Some{Some{_}}, None{}}:
      True{}

def po.i(m: Mod, +trunc: Bool, +idef: Nat) -> Nat:
  match m:
    case Mod{Some{None{}}, Some{+w}}:
      w
    case Mod{_, Some{+w}}:
      Bool.pick(Nat, trunc, w, Nat.max(w, idef))
    case Mod{_, None{}}:
      idef

def mod.pad(m: Mod) -> Maybe<&2, Maybe<&2, U32>>:
  match m:
    case Mod{p, _}:
      p

# getPadOption trunc fdef idef cdef
def pad.opt(+trunc: Bool, +fdef: Bool, +idef: Nat, +cdef: U32, +m: Mod) -> PO:
  Bool.pick(PO, po.f(m, fdef), Pad{po.i(m, trunc, idef), po.c(mod.pad(m), cdef)}, NoPad{})

def show.padded(p: PO, +s: String) -> String:
  match p:
    case NoPad{}:
      s
    case Pad{+i, +c}:
      String.repeat(SCon{Char.from_u32(c), SNil{}}, Nat.sub(i, String.length(s))) ++ s

# showPaddedNum: a negative number's sign before its padding
def show.num(+p: PO, +n: U32) -> String:
  H.Bool.if(String, is.neg(n), _ => "-" ++ show.padded(p, digits(neg(n))), _ => show.padded(p, digits(n)))

# formatString and formatNumber fdef idef cdef
def fstr(+m: Mod, +s: String) -> String:
  show.padded(pad.opt(False{}, False{}, 1n, 32, m), s)

def fnum(+m: Mod, +fdef: Bool, +idef: Nat, +cdef: U32, +n: U32) -> String:
  show.num(pad.opt(False{}, fdef, idef, cdef, m), n)

# The calendar -----------------------------------------------------------------------

def hour12(+h: U32) -> U32:
  U32.add(U32.mod(U32.add(h, 11), 12), 1)

def leap(+y: U32) -> Bool:
  Bool.and(U32.is_eq(mod(y, 4), 0), Bool.or(U32.is_ne(mod(y, 100), 0), U32.is_eq(mod(y, 400), 0)))

def year.days(+y: U32) -> U32:
  Bool.pick(U32, leap(y), 366, 365)

# the ISO weekday (1 Monday .. 7 Sunday)
def iso.day(+wday: U32) -> U32:
  Bool.pick(U32, U32.is_eq(wday, 0), 7, wday)

# how many ISO weeks a year has, its 1 January's weekday (0 Sunday) given:
# 53 when it starts on a Thursday, or on a Wednesday in a leap year
def weeks.in(+y: U32, +jan1: U32) -> U32:
  Bool.pick(U32, Bool.or(U32.is_eq(jan1, 4), Bool.and(leap(y), U32.is_eq(jan1, 3))), 53, 52)

# the ISO week date: the year and the week (1-53)
type Week is Data:
  Week{year: U32, week: U32}

def iso.week(+y: U32, +wd: U32, +yd: U32) -> Week:
  +w = div(U32.sub(U32.add(yd, 11), iso.day(wd)), 7)
  +jan1 = mod(U32.sub(wd, yd), 7)
  +py = U32.sub(y, 1)
  H.Bool.if(Week, Bool.or(is.neg(w), U32.is_eq(w, 0)), _ => Week{py, weeks.in(py, mod(U32.sub(jan1, year.days(py)), 7))}, _ => H.Bool.if(Week, U32.is_gt(w, weeks.in(y, jan1)), _ => Week{U32.add(y, 1), 1}, _ => Week{y, w}))

def wk.year(w: Week) -> U32:
  match w:
    case Week{y, _}:
      y

def wk.week(w: Week) -> U32:
  match w:
    case Week{_, x}:
      x

# sundayStartWeek's and mondayStartWeek's week numbers
def week.sun(+yd: U32, +wd: U32) -> U32:
  div(U32.sub(U32.add(yd, 7), wd), 7)

def week.mon(+yd: U32, +wd: U32) -> U32:
  div(U32.sub(U32.add(yd, 7), mod(U32.add(wd, 6), 7)), 7)

def colon.po(p: PO) -> PO:
  match p:
    case NoPad{}:
      NoPad{}
    case Pad{+i, c}:
      Pad{Nat.sub(i, 3n), c}

# timeZoneOffsetString'' colon opt (minutes)
def zone.offset(+colon: Bool, +p: PO, +off: U32) -> String:
  +t = Bool.pick(U32, is.neg(off), neg(off), off)
  +sign = Bool.pick(String, is.neg(off), "-", "+")
  H.Bool.if(String, colon, _ => sign ++ show.num(colon.po(p), div(t, 60)) ++ ":" ++ show.padded(Pad{2n, 48}, digits(mod(t, 60))), _ => sign ++ show.num(p, U32.add(U32.mul(div(t, 60), 100), mod(t, 60))))

def drop.zeros(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{+c, +rest}:
      Bool.pick(String, H.c.is(c, 48), drop.zeros(rest), SCon{c, rest})

# the seconds' fraction's digits as showFixed True shows a Pico (twelve,
# the trailing zeros gone)
def frac.digits(+ns: U32) -> String:
  String.reverse(drop.zeros(String.reverse(show.padded(Pad{9n, 48}, digits(ns)) ++ "000")))

# showPaddedFixedFraction
def frac.padded(p: PO, +d: String) -> String:
  match p:
    case NoPad{}:
      d
    case Pad{+i, +c}:
      +n = String.length(d)
      Bool.pick(String, Nat.is_lt(i, n), String.take(d, i), d ++ String.repeat(SCon{Char.from_u32(c), SNil{}}, Nat.sub(i, n)))

def dot.nonempty(+s: String) -> String:
  Bool.pick(String, String.is_empty(s), "", "." ++ s)

# One specifier -----------------------------------------------------------------------

def ampm(+h: U32) -> String:
  Bool.pick(String, U32.is_lt(h, 12), "AM", "PM")

def spec.month(+c: U32, +m: Mod, +y: U32, +mo: U32) -> String:
  H.Bool.if(String, U32.is_eq(c, 89), _ => fnum(m, False{}, 4n, 48, y), _ => H.Bool.if(String, U32.is_eq(c, 121), _ => fnum(m, True{}, 2n, 48, mod(y, 100)), _ => H.Bool.if(String, U32.is_eq(c, 67), _ => fnum(m, False{}, 2n, 48, div(y, 100)), _ => H.Bool.if(String, U32.is_eq(c, 66), _ => fstr(m, nth(U32.sub(mo, 1), months.long())), _ => H.Bool.if(String, Bool.or(U32.is_eq(c, 98), U32.is_eq(c, 104)), _ => fstr(m, nth(U32.sub(mo, 1), months.short())), _ => H.Bool.if(String, U32.is_eq(c, 109), _ => fnum(m, True{}, 2n, 48, mo), _ => ""))))))

def spec.day(+c: U32, +e: Bool, +m: Mod, +y: U32, +mo: U32, +md: U32, +wd: U32, +yd: U32, +off: U32, +zone: String, +iw: Week) -> String:
  H.Bool.if(String, U32.is_eq(c, 122), _ => zone.offset(e, pad.opt(False{}, True{}, Bool.pick(Nat, e, 5n, 4n), 48, m), off), _ => H.Bool.if(String, U32.is_eq(c, 90), _ => Bool.pick(String, String.is_empty(zone), zone.offset(e, pad.opt(False{}, True{}, Bool.pick(Nat, e, 5n, 4n), 48, m), off), fstr(m, zone)), _ => H.Bool.if(String, U32.is_eq(c, 100), _ => fnum(m, True{}, 2n, 48, md), _ => H.Bool.if(String, U32.is_eq(c, 101), _ => fnum(m, True{}, 2n, 32, md), _ => H.Bool.if(String, U32.is_eq(c, 106), _ => fnum(m, True{}, 3n, 48, U32.add(yd, 1)), _ => H.Bool.if(String, U32.is_eq(c, 71), _ => fnum(m, False{}, 4n, 48, wk.year(iw)), _ => H.Bool.if(String, U32.is_eq(c, 103), _ => fnum(m, True{}, 2n, 48, mod(wk.year(iw), 100)), _ => H.Bool.if(String, U32.is_eq(c, 102), _ => fnum(m, False{}, 2n, 48, div(wk.year(iw), 100)), _ => H.Bool.if(String, U32.is_eq(c, 86), _ => fnum(m, True{}, 2n, 48, wk.week(iw)), _ => H.Bool.if(String, U32.is_eq(c, 117), _ => fnum(m, True{}, 1n, 48, iso.day(wd)), _ => H.Bool.if(String, U32.is_eq(c, 97), _ => fstr(m, nth(wd, days.short())), _ => H.Bool.if(String, U32.is_eq(c, 65), _ => fstr(m, nth(wd, days.long())), _ => H.Bool.if(String, U32.is_eq(c, 85), _ => fnum(m, True{}, 2n, 48, week.sun(yd, wd)), _ => H.Bool.if(String, U32.is_eq(c, 119), _ => fnum(m, True{}, 1n, 48, wd), _ => H.Bool.if(String, U32.is_eq(c, 87), _ => fnum(m, True{}, 2n, 48, week.mon(yd, wd)), _ => spec.month(c, m, y, mo))))))))))))))))

# a specifier that is not a format of its own: c its character, e an E
def spec(+c: U32, +e: Bool, +m: Mod, t: Tm) -> String:
  match t:
    case Tm{+y, +mo, +md, +h, +mi, +s, +ns, +wd, +yd, +off, +zone, +ep}:
      +iw = iso.week(y, wd, yd)
      H.Bool.if(String, U32.is_eq(c, 37), _ => fstr(m, "%"), _ => H.Bool.if(String, U32.is_eq(c, 116), _ => fstr(m, "\t"), _ => H.Bool.if(String, U32.is_eq(c, 110), _ => fstr(m, "\n"), _ => H.Bool.if(String, U32.is_eq(c, 115), _ => fnum(m, True{}, 1n, 48, ep), _ => H.Bool.if(String, U32.is_eq(c, 80), _ => fstr(m, String.to_lower(ampm(h))), _ => H.Bool.if(String, U32.is_eq(c, 112), _ => fstr(m, ampm(h)), _ => H.Bool.if(String, U32.is_eq(c, 72), _ => fnum(m, True{}, 2n, 48, h), _ => H.Bool.if(String, U32.is_eq(c, 73), _ => fnum(m, True{}, 2n, 48, hour12(h)), _ => H.Bool.if(String, U32.is_eq(c, 107), _ => fnum(m, True{}, 2n, 32, h), _ => H.Bool.if(String, U32.is_eq(c, 108), _ => fnum(m, True{}, 2n, 32, hour12(h)), _ => H.Bool.if(String, U32.is_eq(c, 77), _ => fnum(m, True{}, 2n, 48, mi), _ => H.Bool.if(String, U32.is_eq(c, 83), _ => fnum(m, True{}, 2n, 48, s), _ => H.Bool.if(String, U32.is_eq(c, 113), _ => frac.padded(pad.opt(True{}, True{}, 12n, 48, m), frac.digits(ns)), _ => H.Bool.if(String, U32.is_eq(c, 81), _ => dot.nonempty(frac.padded(pad.opt(True{}, False{}, 12n, 48, m), frac.digits(ns))), _ => spec.day(c, e, m, y, mo, md, wd, yd, off, zone, iw)))))))))))))))

# the format of a specifier that is one (%c %R %T %X %r %D %F %x), or ""
def composite(+c: U32) -> String:
  H.Bool.if(String, U32.is_eq(c, 99), _ => "%a %b %e %H:%M:%S %Z %Y", _ => H.Bool.if(String, U32.is_eq(c, 82), _ => "%H:%M", _ => H.Bool.if(String, Bool.or(U32.is_eq(c, 84), U32.is_eq(c, 88)), _ => "%H:%M:%S", _ => H.Bool.if(String, U32.is_eq(c, 114), _ => "%I:%M:%S %p", _ => H.Bool.if(String, Bool.or(U32.is_eq(c, 68), U32.is_eq(c, 120)), _ => "%m/%d/%y", _ => H.Bool.if(String, U32.is_eq(c, 70), _ => "%Y-%m-%d", _ => ""))))))

# formatTime -------------------------------------------------------------------------

# after %: the case (0 as is, 1 upper, 2 lower), the padding asked
type M1 is Data:
  M1{kase: U32, pad: Maybe<&2, Maybe<&2, U32>>, rest: String}

def m1(+s: String) -> M1:
  +u = H.char0(s)
  +r = H.tail(s)
  H.Bool.if(M1, String.is_empty(s), _ => M1{0, None{}, s}, _ => H.Bool.if(M1, U32.is_eq(u, 95), _ => M1{0, Some{Some{32}}, r}, _ => H.Bool.if(M1, U32.is_eq(u, 45), _ => M1{0, Some{None{}}, r}, _ => H.Bool.if(M1, U32.is_eq(u, 48), _ => M1{0, Some{Some{48}}, r}, _ => H.Bool.if(M1, U32.is_eq(u, 94), _ => M1{1, None{}, r}, _ => H.Bool.if(M1, U32.is_eq(u, 35), _ => M1{2, None{}, r}, _ => M1{0, None{}, s}))))))

def recase(+k: U32, +s: String) -> String:
  H.Bool.if(String, U32.is_eq(k, 1), _ => String.to_upper(s), _ => H.Bool.if(String, U32.is_eq(k, 2), _ => String.to_lower(s), _ => s))

# formatTime's steps: FText reads on, FPct takes a % and its modifiers
# read (after: the text after the %); full: %c and the like format theirs
type FM is Data:
  FText{}
  FPct{m: M1}

def fmt(fuel: Nat, mode: FM, +full: Bool, +s: String, +t: Tm) -> String:
  match fuel mode:
    case 0n _:
      ""
    case 1n+f FText{}:
      +g = f
      H.Bool.if(String, String.is_empty(s), _ => "", _ => H.Bool.if(String, Bool.not(H.first.is(s, 37)), _ => String.take(s, 1n) ++ fmt(g, FText{}, full, H.tail(s), t), _ => fmt(g, FPct{m1(H.tail(s))}, full, H.tail(s), t)))
    case 1n+f FPct{M1{+k, +p, +r}}:
      +after = s
      +g = f
      +n = H.digits.len(1, r)
      +w = Bool.pick(Maybe<&2, Nat>, Nat.is_gt(n, 0n), Some{U32.to_nat(H.digits.val(10, String.take(r, n), 0))}, None{})
      +r2 = String.drop(r, n)
      +e = H.first.is(r2, 69)
      +r3 = Bool.pick(String, e, H.tail(r2), r2)
      +c = H.char0(r3)
      +md = {Mod{p, w} : Mod}
      +comp = composite(c)
      H.Bool.if(String, String.is_empty(r3), _ => "%" ++ fmt(g, FText{}, full, after, t), _ => recase(k, Bool.pick(String, Bool.and(full, Bool.not(String.is_empty(comp))), fstr(md, fmt(g, FText{}, False{}, comp, t)), spec(c, e, md, t))) ++ fmt(g, FText{}, full, H.tail(r3), t))

# formatTime defaultTimeLocale f t
def format(+f: String, +t: Tm) -> String:
  fmt(Nat.add(Nat.mul(4n, String.length(f)), 64n), FText{}, True{}, f, t)

