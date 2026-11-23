# StdinReader's stripActions against xmobar 0.50's: every case of
# tests/StripCases.bend through Strip.bend; how many are the same, and
# each that differs.
import Base
import ../Hs.bend as H
import ../Strip.bend as S
import ./StripCases.bend as C

type Tally is Data:
  Tally{same: Nat, differ: Nat, report: String}

def one(c: C.Case, t: Tally) -> Tally:
  match c t:
    case C.Case{+l, +w} Tally{+s, +d, +r}:
      +g = S.strip(l)
      Bool.pick(Tally, String.eq(g, w), Tally{1n+s, d, r}, Tally{s, 1n+d, r ++ H.show.str(l) ++ "\n  xmobar: " ++ H.show.str(w) ++ "\n  bend:   " ++ H.show.str(g) ++ "\n"})

def all(cs: List<&2, C.Case>, t: Tally) -> Tally:
  match cs:
    case Nil{}:
      t
    case c <> rest:
      all(rest, one(c, t))

def fin(t: Tally) -> String:
  match t:
    case Tally{s, d, r}:
      "same " ++ Nat.show(s) ++ ", differ " ++ Nat.show(d) ++ "\n" ++ r

def main() -> IO(Unit):
  IO.print(fin(all(C.cases(), Tally{0n, 0n, ""})))
