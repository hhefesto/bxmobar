# X11, as xmobar-bend reaches it: each def one Xlib, Cairo or Pango call
# or a fixed few, done by x11.c (the C side). Nothing here decides
# anything: Win.bend says where the window goes, Draw.bend what a frame
# draws, Main.bend when.
#
# Words are U32s (a Position in two's complement); a list of numbers
# going to C is a String of decimal numbers. These are foreign effects.

import Base

# 1 when the display opened, 0 else
def open() -> IO(U32):
  import "./x11.c"

# getScreenInfo: x y w h of each screen
def screens() -> IO(List<&2, U32>):
  import "./x11.c"

# initFont's font set (misc-fixed when the name does not load): the ascent
# and descent of "0"; [] when neither loads
def font_extents(font: String) -> IO(List<&2, U32>):
  import "./x11.c"

# newWindow: the window at x y w h, override_redirect when o is 1
def create(x: U32, y: U32, w: U32, h: U32, o: U32) -> IO(U32):
  import "./x11.c"

# setTextProperty: WM_CLASS (which 0) or WM_NAME (1)
def set_text(win: U32, which: U32, text: String) -> IO(U32):
  import "./x11.c"

# changeProperty32 name type (PropModeReplace) values
def prop32(win: U32, name: String, ty: String, values: String) -> IO(U32):
  import "./x11.c"

# internAtom name False
def atom(name: String) -> IO(U32):
  import "./x11.c"

# getProcessID
def pid() -> IO(U32):
  import "./x11.c"

def lower(win: U32) -> IO(U32):
  import "./x11.c"

def map(win: U32) -> IO(U32):
  import "./x11.c"

def unmap(win: U32) -> IO(U32):
  import "./x11.c"

def move_resize(win: U32, x: U32, y: U32, w: U32, h: U32) -> IO(U32):
  import "./x11.c"

# XSync, discarding the events queued when discard is 1
def sync(discard: U32) -> IO(U32):
  import "./x11.c"

# the event loop's input: screen changes, exposures, structure, buttons
def select(win: U32) -> IO(U32):
  import "./x11.c"

# 2 when X has events, 1 when standard input can be read or ended (when
# stdin is 1), 0 after ms milliseconds
def wait(ms: U32, stdin: U32) -> IO(U32):
  import "./x11.c"

# the events queued, three words each: 1 0 0 Expose, 2 button x
# ButtonPress, 3 0 0 ConfigureNotify or a screen change
def events() -> IO(List<&2, U32>):
  import "./x11.c"

# a frame's Pango context at a dpi (a Double's text)
def begin(dpi: String) -> IO(U32):
  import "./x11.c"

# a markup laid out in the frame's context: its logical extents x y w h
# (Pango units)
def layout(markup: String) -> IO(List<&2, U32>):
  import "./x11.c"

# the frame drawn: the window at x y w h, its background colour and alpha,
# Draw.bend's ops
def draw(win: U32, x: U32, y: U32, w: U32, h: U32, bg: String, alpha: U32, ops: String) -> IO(U32):
  import "./x11.c"

# runAction: sh -c (cmd ++ " &"), waited for
def spawn(cmd: String) -> IO(U32):
  import "./x11.c"
