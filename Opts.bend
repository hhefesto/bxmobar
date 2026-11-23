# xmobar's command line (Xmobar.App.Opts): getOpt Permute over its
# options, -h and -V answered at once, then doOpts: each option, in order,
# changing the config read; last, lowerOnStart only with overrideRedirect.
#
# A value read with Haskell's read that does not read is read's error;
# xmobar meets it when it uses the value, here at once.

import Base
import ./Hs.bend as H
import ./GetOpt.bend as G
import ./Read.bend as R
import ./Config.bend as C

def options() -> List<&2, G.OptD>:
  [G.OptD{0, "h?", ["help"], G.NoArg{}, "This help"}, G.OptD{1, "v", ["verbose"], G.NoArg{}, "Emit verbose debugging messages"}, G.OptD{2, "r", ["recompile"], G.NoArg{}, "Force recompilation"}, G.OptD{3, "V", ["version"], G.NoArg{}, "Show version information"}, G.OptD{4, "T", ["text"], G.OptArg{"color"}, "Write text-only output to stdout. Plain/Ansi/Pango/Swaybar"}, G.OptD{5, "f", ["font"], G.ReqArg{"font name"}, "Font name"}, G.OptD{6, "N", ["add-font"], G.ReqArg{"font name"}, "Add to the list of additional fonts"}, G.OptD{7, "w", ["wmclass"], G.ReqArg{"class"}, "X11 WM_CLASS property"}, G.OptD{8, "n", ["wmname"], G.ReqArg{"name"}, "X11 WM_NAME property"}, G.OptD{9, "B", ["bgcolor"], G.ReqArg{"bg color"}, "The background color. Default black"}, G.OptD{10, "F", ["fgcolor"], G.ReqArg{"fg color"}, "The foreground color. Default grey"}, G.OptD{11, "i", ["iconroot"], G.ReqArg{"path"}, "Root directory for icon pattern paths. Default '.'"}, G.OptD{12, "A", ["alpha"], G.ReqArg{"alpha"}, "Transparency: 0 is transparent, 255 is opaque. Default: 255"}, G.OptD{13, "o", ["top"], G.NoArg{}, "Place xmobar at the top of the screen"}, G.OptD{14, "b", ["bottom"], G.NoArg{}, "Place xmobar at the bottom of the screen"}, G.OptD{15, "d", ["dock"], G.NoArg{}, "Don't override redirect from WM and function as a dock"}, G.OptD{16, "a", ["alignsep"], G.ReqArg{"alignsep"}, "Separators for left, center and right text\nalignment. Default: '}{'"}, G.OptD{17, "s", ["sepchar"], G.ReqArg{"char"}, "Character used to separate commands in\nthe output template. Default '%'"}, G.OptD{18, "t", ["template"], G.ReqArg{"template"}, "Output template"}, G.OptD{19, "c", ["commands"], G.ReqArg{"commands"}, "List of commands to be executed"}, G.OptD{20, "C", ["add-command"], G.ReqArg{"command"}, "Add to the list of commands to be executed"}, G.OptD{21, "x", ["screen"], G.ReqArg{"screen"}, "On which X screen number to start"}, G.OptD{22, "p", ["position"], G.ReqArg{"position"}, "Specify position of xmobar. Same syntax as in config file"}, G.OptD{23, "D", ["dpi"], G.ReqArg{"dpi"}, "The DPI scaling factor. Default 96.0"}]

# usage
def usage() -> String:
  G.usage("Usage: xmobar [OPTION...] [FILE]\nOptions:", options()) ++ "\nMail bug reports and suggestions to <mail@jao.io>\n"

def info() -> String:
  "xmobar-bend 0.50 (xmobar 0.50 in Bend)\n (xmobar: (C) 2010 - 2025 Jose A Ortega Ruiz\n (C) 2007 - 2010 Andrea Rossato)\n <mail@jao.io>\n\nThis program is distributed in the hope that it will be useful,\nbut WITHOUT ANY WARRANTY; without even the implied warranty of\nMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\nSee the License for more details.\n"

def has.tag(+t: U32, os: List<&2, G.Opt>) -> Bool:
  match os:
    case Nil{}:
      False{}
    case Con{G.Opt{+u, _}, rest}:
      Bool.or(U32.is_eq(t, u), has.tag(t, rest))

# What doOpts ends with: the config (and what it wrote: a position that
# did not read is ignored, said on standard output), or the run's end
type Done is Data:
  DOk{c: C.Config, said: String}
  DExit{code: U32, out: String, err: String}

# read v :: t of a whole text (spaces around it): the value
def whole(p: R.PR) -> Maybe<&2, R.HV>:
  match p:
    case R.PR{v, rest}:
      Bool.pick(Maybe<&2, R.HV>, String.is_empty(H.drop.space(rest)), Some{v}, None{})
    case R.PRBad{}:
      None{}

def read.as.m(k: C.FK, m: Maybe<&2, R.HV>) -> Maybe<&2, C.Val>:
  match m:
    case Some{+v}:
      C.read.as(k, v)
    case None{}:
      None{}

def read.at(k: C.FK, +s: String) -> Maybe<&2, C.Val>:
  read.as.m(k, whole(R.value(s)))

def runs.m(m: Maybe<&2, R.HV>) -> Maybe<&2, List<&2, C.Run>>:
  match m:
    case Some{v}:
      C.read.runs.pr(R.PR{v, ""})
    case None{}:
      None{}

def runs.at(+s: String) -> Maybe<&2, List<&2, C.Run>>:
  runs.m(whole(R.value(s)))

def bad.read() -> Done:
  DExit{1, "", "xmobar-bend: Prelude.read: no parse"}

def val.or(m: Maybe<&2, String>) -> String:
  match m:
    case Some{x}:
      x
    case None{}:
      ""

def config.fonts(c: C.Config) -> List<&2, String>:
  match c:
    case C.Config{_, fs, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _}:
      fs

def config.runs(c: C.Config) -> List<&2, C.Run>:
  match c:
    case C.Config{_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, rs, _, _, _, _, _}:
      rs

def config.pos(c: C.Config) -> C.Pos:
  match c:
    case C.Config{_, _, _, _, _, _, p, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _}:
      p

# a value that read, set; read's error else
def set.or(m: Maybe<&2, C.Val>, k: C.FK, +c: C.Config, +said: String) -> Done:
  match m:
    case Some{v}:
      DOk{C.set.field(k, v, c), said}
    case None{}:
      bad.read()

def set.int(m: Maybe<&2, U32>, k: C.FK, +c: C.Config, +said: String) -> Done:
  match m:
    case Some{n}:
      DOk{C.set.field(k, C.VInt{n}, c), said}
    case None{}:
      bad.read()

def screen.or(m: Maybe<&2, U32>, +c: C.Config, +said: String) -> Done:
  match m:
    case Some{n}:
      DOk{C.set.field(C.FPosition{}, C.VPos{C.OnScreen{n, config.pos(c)}}, c), said}
    case None{}:
      bad.read()

def runs.or(m: Maybe<&2, List<&2, C.Run>>, +add: Bool, +flag: String, +c: C.Config, +said: String) -> Done:
  match m:
    case Some{+rs}:
      DOk{C.set.field(C.FCommands{}, C.VRuns{Bool.pick(List<&2, C.Run>, add, List.append(&2, C.Run, config.runs(c), rs), rs)}, c), said}
    case None{}:
      DExit{1, "xmobar: cannot read list of commands specified with the -" ++ flag ++ " option\n" ++ usage(), ""}

def pos.or(m: Maybe<&2, C.Val>, +c: C.Config, +said: String) -> Done:
  match m:
    case Some{v}:
      DOk{C.set.field(C.FPosition{}, v, c), said}
    case None{}:
      DOk{c, said ++ "Can't parse position option, ignoring\n"}

def opt.more(+t: U32, +v: String, +c: C.Config, +said: String) -> Done:
  H.Bool.if(Done, U32.is_eq(t, 12), _ => set.int(H.read.int(v), C.FAlpha{}, c, said), _ => H.Bool.if(Done, U32.is_eq(t, 13), _ => DOk{C.set.field(C.FPosition{}, C.VPos{C.Top{}}, c), said}, _ => H.Bool.if(Done, U32.is_eq(t, 14), _ => DOk{C.set.field(C.FPosition{}, C.VPos{C.Bottom{}}, c), said}, _ => H.Bool.if(Done, U32.is_eq(t, 15), _ => DOk{C.set.field(C.FOverrideRedirect{}, C.VBool{False{}}, c), said}, _ => H.Bool.if(Done, U32.is_eq(t, 16), _ => DOk{C.set.field(C.FAlignSep{}, C.VStr{v}, c), said}, _ => H.Bool.if(Done, U32.is_eq(t, 17), _ => DOk{C.set.field(C.FSepChar{}, C.VStr{v}, c), said}, _ => H.Bool.if(Done, U32.is_eq(t, 18), _ => DOk{C.set.field(C.FTemplate{}, C.VStr{v}, c), said}, _ => H.Bool.if(Done, U32.is_eq(t, 19), _ => runs.or(runs.at(v), False{}, "c", c, said), _ => H.Bool.if(Done, U32.is_eq(t, 20), _ => runs.or(runs.at(v), True{}, "C", c, said), _ => H.Bool.if(Done, U32.is_eq(t, 21), _ => screen.or(H.read.int(v), c, said), _ => H.Bool.if(Done, U32.is_eq(t, 22), _ => pos.or(read.at(C.FPosition{}, v), c, said), _ => set.or(read.at(C.FDpi{}, v), C.FDpi{}, c, said))))))))))))

# one option of doOpts
def opt(+t: U32, +v: String, +c: C.Config, +said: String) -> Done:
  H.Bool.if(Done, U32.is_lt(t, 4), _ => H.Bool.if(Done, U32.is_eq(t, 1), _ => DOk{C.set.field(C.FVerbose{}, C.VBool{True{}}, c), said}, _ => DOk{c, said}), _ => H.Bool.if(Done, U32.is_eq(t, 24), _ => DOk{C.set.field(C.FTextOutput{}, C.VBool{True{}}, c), said}, _ => H.Bool.if(Done, U32.is_eq(t, 4), _ => set.or(read.at(C.FTextOutputFormat{}, v), C.FTextOutputFormat{}, C.set.field(C.FTextOutput{}, C.VBool{True{}}, c), said), _ => H.Bool.if(Done, U32.is_eq(t, 5), _ => DOk{C.set.field(C.FFont{}, C.VStr{v}, c), said}, _ => H.Bool.if(Done, U32.is_eq(t, 6), _ => DOk{C.set.field(C.FAdditionalFonts{}, C.VStrs{List.append(&2, String, config.fonts(c), [v])}, c), said}, _ => H.Bool.if(Done, U32.is_eq(t, 7), _ => DOk{C.set.field(C.FWmClass{}, C.VStr{v}, c), said}, _ => H.Bool.if(Done, U32.is_eq(t, 8), _ => DOk{C.set.field(C.FWmName{}, C.VStr{v}, c), said}, _ => H.Bool.if(Done, U32.is_eq(t, 9), _ => DOk{C.set.field(C.FBgColor{}, C.VStr{v}, c), said}, _ => H.Bool.if(Done, U32.is_eq(t, 10), _ => DOk{C.set.field(C.FFgColor{}, C.VStr{v}, c), said}, _ => H.Bool.if(Done, U32.is_eq(t, 11), _ => DOk{C.set.field(C.FIconRoot{}, C.VStr{v}, c), said}, _ => opt.more(t, v, c, said)))))))))))

def config.lower(c: C.Config) -> Bool:
  match c:
    case C.Config{_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, o, _, l, _, _, _, _, _, _, _, _}:
      Bool.and(l, o)

# -T with no format: tag 24
def tag.of(+t: U32, m: Maybe<&2, String>) -> U32:
  match m:
    case None{}:
      Bool.pick(U32, U32.is_eq(t, 4), 24, t)
    case Some{_}:
      t

def done.next(fuel: Nat, d: Done, os: List<&2, G.Opt>) -> Done:
  match fuel d os:
    case 1n+f DOk{+c, +said} Con{G.Opt{+t, +v}, rest}:
      done.next(f, opt(tag.of(t, v), val.or(v), c, said), rest)
    case _ DOk{+c, +said} Nil{}:
      DOk{C.set.field(C.FLowerOnStart{}, C.VBool{config.lower(c)}, c), said}
    case _ d _:
      d

# doOpts conf opts
def do.opts(+c: C.Config, +os: List<&2, G.Opt>) -> Done:
  done.next(1n+List.length(&2, G.Opt, os), DOk{c, ""}, os)
