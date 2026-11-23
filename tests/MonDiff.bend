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

# Network at each step: parseNet from the reading kept (its IORef's) to the
# step's, at 0 seconds between them, as the reference's stopped clock has it
def net.outs(+dev: String, +args: List<&2, String>, procs: List<&2, String>, opers: List<&2, String>, k: N.Kept) -> List<&2, String>:
  match procs opers:
    case Con{+p, pr} Con{+o, orr}:
      +s = N.net.step(k, N.find.raw(dev, N.net.devs(p)), N.operstate.up(o), 0.0)
      +r = N.step.rate(s)
      M.run(args, N.net.config(), c => a => N.net.run(c, a, r)) <> net.outs(dev, args, pr, orr, N.step.kept(s))
    case _ _:
      Nil{}

def net.first.of(+dev: String, procs: List<&2, String>) -> N.Kept:
  match procs:
    case Con{+p, _}:
      N.net.first(N.find.raw(dev, N.net.devs(p)))
    case Nil{}:
      N.net.first(N.RawNA{})

# DynNetwork's step: each device's parseNet, the readings kept next
type DynSt is Data:
  DynSt{named: List<&2, N.Named>, kept: List<&2, N.Kept>}

def dyn.cons(+x: N.Named, k: N.Kept, st: DynSt) -> DynSt:
  match st:
    case DynSt{ns, ks}:
      DynSt{x <> ns, k <> ks}

def dyn.step(devs: List<&2, String>, ks: List<&2, N.Kept>, +p: String, os: List<&2, String>) -> DynSt:
  match devs ks os:
    case Con{+d, dr} Con{k, kr} Con{+o, orr}:
      +s = N.net.step(k, N.find.raw(d, N.net.devs(p)), N.operstate.up(o), 0.0)
      dyn.cons(N.Named{d, N.step.rate(s)}, N.step.kept(s), dyn.step(dr, kr, p, orr))
    case _ _ _:
      DynSt{Nil{}, Nil{}}

def dyn.kept(st: DynSt) -> List<&2, N.Kept>:
  match st:
    case DynSt{_, ks}:
      ks

def dyn.out(+args: List<&2, String>, st: DynSt) -> String:
  match st:
    case DynSt{+ns, _}:
      M.run(args, N.net.config(), c => a => N.dyn.run(c, a, ns))

def dyn.outs(+devs: List<&2, String>, +args: List<&2, String>, procs: List<&2, String>, opers: List<&2, List<&2, String>>, ks: List<&2, N.Kept>) -> List<&2, String>:
  match procs opers:
    case Con{+p, pr} Con{os, orr}:
      +st = dyn.step(devs, ks, p, os)
      dyn.out(args, st) <> dyn.outs(devs, args, pr, orr, dyn.kept(st))
    case _ _:
      Nil{}

def dyn.firsts(devs: List<&2, String>, +p: String) -> List<&2, N.Kept>:
  match devs:
    case Nil{}:
      Nil{}
    case Con{+d, rest}:
      N.net.first(N.find.raw(d, N.net.devs(p))) <> dyn.firsts(rest, p)

def head.or(xs: List<&2, String>) -> String:
  match xs:
    case Con{x, _}:
      x
    case Nil{}:
      ""

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
    case C.NetCase{+dev, +args, +procs, opers, w}:
      compare(w, net.outs(dev, args, procs, opers, net.first.of(dev, procs)), t, "network " ++ dev, args, 0n)
    case C.DynCase{+devs, +args, +procs, opers, w}:
      compare(w, dyn.outs(devs, args, procs, opers, dyn.firsts(devs, head.or(procs))), t, "dynnetwork " ++ H.show.list(strs(devs)), args, 0n)

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
