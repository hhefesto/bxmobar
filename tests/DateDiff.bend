# The Date plugin's differential test: every case of tests/DateCases.bend
# (time's formatTime answers) through Date.bend's format.
import Base
import ../Hs.bend as H
import ../Date.bend as D
import ./DateCases.bend as C

type T is Data:
  T{same: Nat, differ: Nat, report: String}

def one(c: C.Case, t: T) -> T:
  match c t:
    case C.Case{+f, +tm, +w} T{+s, +d, +r}:
      +g = D.format(f, tm)
      Bool.pick(T, String.eq(g, w), T{1n+s, d, r}, T{s, 1n+d, r ++ "format " ++ H.show.str(f) ++ "\n  time:   " ++ H.show.str(w) ++ "\n  bend:   " ++ H.show.str(g) ++ "\n"})

def all(cs: List<&2, C.Case>, t: T) -> T:
  match cs:
    case Nil{}:
      t
    case c <> rest:
      all(rest, one(c, t))

def fin(t: T) -> String:
  match t:
    case T{s, d, r}:
      "same " ++ Nat.show(s) ++ ", differ " ++ Nat.show(d) ++ "\n" ++ r

def main() -> IO(Unit):
  IO.print(fin(all(C.cases(), T{0n, 0n, ""})))
