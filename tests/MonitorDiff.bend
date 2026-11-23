# The monitors' common part's differential test: every case of
# tests/MonitorCases.bend (xmobar 0.50's answers) run through Monitor.bend
# as a monitor runs it; it prints how many are the same and each that
# differs.
import Base
import ../Hs.bend as H
import ../Float.bend as F
import ../Monitor.bend as M
import ./MonitorCases.bend as C

def exports() -> List<&2, String>:
  ["a", "b", "c", "total", "x1"]

def act(+k: U32, +a: F32, +b: F32, +s: String, +vs: List<&2, String>, +c: M.C) -> String:
  H.Bool.if(String, U32.is_eq(k, 0), _ => M.percent.colors(c, a), _ => H.Bool.if(String, U32.is_eq(k, 1), _ => M.bar(c, a, b), _ => H.Bool.if(String, U32.is_eq(k, 2), _ => M.vbar(c, a, b), _ => H.Bool.if(String, U32.is_eq(k, 3), _ => M.with.colors(c, s, a), _ => H.Bool.if(String, U32.is_eq(k, 4), _ => M.log.bar(c, a, b), _ => H.Bool.if(String, U32.is_eq(k, 5), _ => M.percent(c, a), _ => H.Bool.if(String, U32.is_eq(k, 6), _ => M.template(c, vs), _ => H.Bool.if(String, U32.is_eq(k, 7), _ => M.ipat(Some{s}, a), _ => H.Bool.if(String, U32.is_eq(k, 8), _ => M.with.units(10n, 2n, 1, a), _ => H.Bool.if(String, U32.is_eq(k, 9), _ => M.with.units(10n, 3n, 0, a), _ => H.Bool.if(String, U32.is_eq(k, 10), _ => F.show(0n, M.take.digits(2n, a)), _ => H.Bool.if(String, U32.is_eq(k, 11), _ => M.log.vbar(c, a, b), _ => M.log.ipat(c, Some{s}, a, b)))))))))))))

def strs(xs: List<&2, String>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      H.show.str(x) <> strs(rest)

type Got is Data:
  Got{ok: Bool, report: String}

def got(c: C.Case) -> Got:
  match c:
    case C.MCase{+args, +k, +a, +b, +s, +vs, +w}:
      +g = M.run(args, M.mk("<a>|<b>", exports()), cfg => _ => act(k, F.of.bits(a), F.of.bits(b), s, vs, cfg))
      Got{String.eq(g, w), "args " ++ H.show.list(strs(args)) ++ " kind " ++ H.show.u32(k) ++ " a " ++ F.show(0n, F.of.bits(a)) ++ " b " ++ F.show(0n, F.of.bits(b)) ++ " s " ++ H.show.str(s) ++ " values " ++ H.show.list(strs(vs)) ++ "\n  xmobar: " ++ H.show.str(w) ++ "\n  bend:   " ++ H.show.str(g) ++ "\n"}

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

def got.report(g: Got) -> String:
  match g:
    case Got{_, r}:
      r
