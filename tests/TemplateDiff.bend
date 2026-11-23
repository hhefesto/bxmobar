# The markup's and templates' differential test: every case of
# tests/TemplateCases.bend (written by tests/gen/GenTemplate.hs with
# xmobar 0.50's own answers) through Template.bend, shown as Haskell shows
# them; it prints how many answers are the same and each that differs.
#
#   tests/template-diff.sh SEED COUNT

import Base
import ../Hs.bend as H
import ../Template.bend as T
import ./TemplateCases.bend as C

def strs(xs: List<&2, String>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      H.show.str(x) <> strs(rest)

def part(p: T.Part) -> String:
  match p:
    case T.Part{c, a, b}:
      "(" ++ H.show.str(c) ++ "," ++ H.show.str(a) ++ "," ++ H.show.str(b) ++ ")"

def parts(ps: List<&2, T.Part>) -> List<&2, String>:
  match ps:
    case Nil{}:
      Nil{}
    case p <> rest:
      part(p) <> parts(rest)

type Got is Data:
  Got{input: String, want: String, got: String}

def got(c: C.Case) -> Got:
  match c:
    case C.PCase{+fg, +s, w}:
      Got{"fg " ++ fg ++ ": " ++ s, w, T.show.segs(T.parse(fg, s))}
    case C.SCase{+sep, +t, w}:
      Got{"split " ++ sep ++ ": " ++ t, w, H.show.list(strs(T.split(sep, t)))}
    case C.TCase{+sep, +t, w}:
      Got{"template " ++ sep ++ ": " ++ t, w, H.show.list(parts(T.template(sep, t)))}

# where two texts first differ (the length of their common start)
def first.diff(a: String, b: String, +i: Nat) -> Nat:
  match a b:
    case SCon{x, ra} SCon{y, rb}:
      Bool.pick(Nat, U32.is_eq(Char.to_u32(x), Char.to_u32(y)), first.diff(ra, rb, 1n+i), i)
    case _ _:
      i

# 160 characters of s from 60 before the place p
def around(+s: String, +p: Nat) -> String:
  String.take(String.drop(s, Nat.sub(p, 60n)), 160n)

def check.one(g: Got, +i: Nat, +same: Nat, +differ: Nat, +report: String, k: Nat -> Nat -> String -> String) -> String:
  match g:
    case Got{+input, +want, +got}:
      +ok = String.eq(want, got)
      +p = first.diff(want, got, 0n)
      k(Bool.pick(Nat, ok, 1n+same, same), Bool.pick(Nat, ok, differ, 1n+differ), Bool.pick(String, ok, report, report ++ "case " ++ Nat.show(i) ++ ", from character " ++ Nat.show(p) ++ "\n  input:  " ++ H.show.str(input) ++ "\n  xmobar: " ++ around(want, p) ++ "\n  bend:   " ++ around(got, p) ++ "\n"))

def check(cs: List<&2, C.Case>, +i: Nat, +same: Nat, +differ: Nat, +report: String) -> String:
  match cs:
    case Nil{}:
      "same " ++ Nat.show(same) ++ ", differ " ++ Nat.show(differ) ++ "\n" ++ report
    case c <> rest:
      check.one(got(c), i, same, differ, report, s => d => r => check(rest, 1n+i, s, d, r))

def main() -> IO(Unit):
  IO.print(check(C.cases(), 1n, 0n, 0n, ""))
