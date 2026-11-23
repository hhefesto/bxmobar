# The monitors' differential test: every case of tests/MonCases.bend (the
# /proc files of a few steps, a monitor's arguments, and what xmobar
# 0.50's own monitor answered at each step) through Mon.bend; it prints
# how many steps are the same and each that differs.
import Base
import ../Hs.bend as H
import ../Monitor.bend as M
import ../Mon.bend as N
import ./MonCases.bend as C

def cpu.outs(files: List<&2, String>, +args: List<&2, String>, +before: List<&2, List<&2, U32>>) -> List<&2, String>:
  match files:
    case Nil{}:
      Nil{}
    case Con{f, rest}:
      +now = N.cpu.data(f)
      M.run(args, N.multicpu.config(), c => r => N.multicpu.run(c, r, before, now)) <> cpu.outs(rest, args, now)

def cpu.first(files: List<&2, String>) -> List<&2, List<&2, U32>>:
  match files:
    case Con{f, _}:
      N.cpu.data(f)
    case Nil{}:
      Nil{}

def mem.outs(+args: List<&2, String>, files: List<&2, String>) -> List<&2, String>:
  match files:
    case Nil{}:
      Nil{}
    case Con{+f, rest}:
      M.run(args, N.mem.config(), c => r => N.mem.run(c, r, f)) <> mem.outs(args, rest)

def swap.outs(+args: List<&2, String>, files: List<&2, String>) -> List<&2, String>:
  match files:
    case Nil{}:
      Nil{}
    case Con{+f, rest}:
      M.run(args, N.swap.config(), c => _ => N.swap.run(c, f)) <> swap.outs(args, rest)

type Tally is Data:
  Tally{same: Nat, differ: Nat, report: String}

def strs(xs: List<&2, String>) -> List<&2, String>:
  match xs:
    case Nil{}:
      Nil{}
    case x <> rest:
      H.show.str(x) <> strs(rest)

def compare(ws: List<&2, String>, gs: List<&2, String>, t: Tally, +name: String, +args: List<&2, String>, +i: Nat) -> Tally:
  match ws gs t:
    case Con{+w, wr} Con{+g, gr} Tally{+s, +d, +r}:
      +ok = String.eq(w, g)
      compare(wr, gr, Bool.pick(Tally, ok, Tally{1n+s, d, r}, Tally{s, 1n+d, r ++ name ++ " " ++ H.show.list(strs(args)) ++ " step " ++ Nat.show(i) ++ "\n  xmobar: " ++ H.show.str(w) ++ "\n  bend:   " ++ H.show.str(g) ++ "\n"}), name, args, 1n+i)
    case _ _ Tally{s, d, r}:
      Tally{s, d, r}

def one(c: C.Case, t: Tally) -> Tally:
  match c:
    case C.CpuCase{+args, +files, w}:
      compare(w, cpu.outs(files, args, cpu.first(files)), t, "multicpu", args, 0n)
    case C.MemCase{+args, +files, w}:
      compare(w, mem.outs(args, files), t, "memory", args, 0n)
    case C.SwapCase{+args, +files, w}:
      compare(w, swap.outs(args, files), t, "swap", args, 0n)

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
