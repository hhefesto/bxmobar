# The configuration's differential test: every case of
# tests/ConfigCases.bend (xmobar 0.50's parseConfig answers) through
# Config.bend; it prints how many are the same and each that differs.
import Base
import ../Hs.bend as H
import ../Config.bend as C
import ./ConfigCases.bend as K

def strs(xs: List<&2, String>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      H.show.str(x) <> strs(rest)

def got(r: C.CR) -> String:
  match r:
    case C.CROk{c, m}:
      C.show(c) ++ "\n" ++ H.show.list(strs(m))
    case C.CRErr{_}:
      "ERROR"

# where two texts first differ (the length of their common start)
def first.diff(a: String, b: String, +i: Nat) -> Nat:
  match a b:
    case SCon{x, ra} SCon{y, rb}:
      Bool.pick(Nat, U32.is_eq(Char.to_u32(x), Char.to_u32(y)), first.diff(ra, rb, 1n+i), i)
    case _ _:
      i

def around(+s: String, +p: Nat) -> String:
  String.take(String.drop(s, Nat.sub(p, 80n)), 200n)

type T is Data:
  T{same: Nat, differ: Nat, report: String}

def one(k: K.Case, t: T) -> T:
  match k t:
    case K.Case{+text, +want} T{+s, +d, +r}:
      +g = got(C.parse(text))
      +p = first.diff(want, g, 0n)
      Bool.pick(T, String.eq(want, g), T{1n+s, d, r}, T{s, 1n+d, r ++ "text: " ++ H.show.str(text) ++ "\n  xmobar: " ++ H.show.str(around(want, p)) ++ "\n  bend:   " ++ H.show.str(around(g, p)) ++ "\n"})

def all(ks: List<&2, K.Case>, t: T) -> T:
  match ks:
    case Nil{}:
      t
    case k <> rest:
      all(rest, one(k, t))

def fin(t: T) -> String:
  match t:
    case T{s, d, r}:
      "same " ++ Nat.show(s) ++ ", differ " ++ Nat.show(d) ++ "\n" ++ r

def main() -> IO(Unit):
  IO.print(fin(all(K.cases(), T{0n, 0n, ""})))
