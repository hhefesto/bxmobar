# Float's differential test: every case of tests/FloatCases.bend (GHC's
# answers) through Float.bend; it prints how many are the same and each
# that differs.
import Base
import ../Hs.bend as H
import ../Big.bend as B
import ../Float.bend as F
import ./FloatCases.bend as C

def fixed.all(+x: F32, n: Nat, +d: Nat) -> List<&2, String>:
  match n:
    case 0n:
      Nil{}
    case 1n+m:
      F.fixed(d, x) <> fixed.all(x, m, 1n+d)

def same.list(a: List<&2, String>, b: List<&2, String>) -> Bool:
  match a b:
    case Nil{} Nil{}:
      True{}
    case Con{x, xs} Con{y, ys}:
      Bool.and(String.eq(x, y), same.list(xs, ys))
    case _ _:
      False{}

def strs(xs: List<&2, String>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      H.show.str(x) <> strs(rest)

type Got is Data:
  Got{ok: Bool, report: String}

def got.f(+b: U32, +fx: List<&2, String>, +sh: String, +rd: String) -> Got:
  +x = F.of.bits(b)
  +gfx = fixed.all(x, 5n, 0n)
  +gsh = F.show(0n, x)
  +grd = Bool.pick(String, String.is_empty(rd), "", F.show.round(x))
  +ok = Bool.and(same.list(fx, gfx), Bool.and(String.eq(sh, gsh), String.eq(rd, grd)))
  Got{ok, "bits " ++ H.show.u32(b) ++ "\n  ghc:  " ++ H.show.list(strs(fx)) ++ " " ++ sh ++ " " ++ rd ++ "\n  bend: " ++ H.show.list(strs(gfx)) ++ " " ++ gsh ++ " " ++ grd ++ "\n"}

def got.i(+n: String, +b: U32) -> Got:
  +g = F.bits(F.of.big(B.dec.big(n)))
  Got{U32.is_eq(g, b), "integer " ++ n ++ "\n  ghc:  " ++ H.show.u32(b) ++ "\n  bend: " ++ H.show.u32(g) ++ "\n"}

def got(c: C.Case) -> Got:
  match c:
    case C.FCase{b, fx, sh, rd}:
      got.f(b, fx, sh, rd)
    case C.ICase{n, b}:
      got.i(n, b)

def tally(g: Got, +same: Nat, +differ: Nat, +report: String, k: Nat -> Nat -> String -> String) -> String:
  match g:
    case Got{+ok, r}:
      k(Bool.pick(Nat, ok, 1n+same, same), Bool.pick(Nat, ok, differ, 1n+differ), Bool.pick(String, ok, report, report ++ r))

def check(cs: List<&2, C.Case>, +same: Nat, +differ: Nat, +report: String) -> String:
  match cs:
    case Nil{}:
      "same " ++ Nat.show(same) ++ ", differ " ++ Nat.show(differ) ++ "\n" ++ report
    case c <> rest:
      tally(got(c), same, differ, report, s => d => r => check(rest, s, d, r))

def main() -> IO(Unit):
  IO.print(check(C.cases(), 0n, 0n, ""))
