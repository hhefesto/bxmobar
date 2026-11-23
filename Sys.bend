# The system, as xmobar-bend reaches it: each def one system call or a few,
# done by sys.c (the C side). Nothing here decides anything; Mon.bend says
# what a file's text means and Main.bend when each is read.
#
# Numbers are U32s, a 64-bit one two words (low first). These are foreign
# effects, so a check names every def that uses them; the pure modules use
# none.

import Base

# a file's bytes ("" when it does not open)
def slurp(path: String) -> IO(String):
  import "./sys.c"

# 0 nothing, 1 a file (not a directory), 2 a directory (links followed)
def kind(path: String) -> IO(U32):
  import "./sys.c"

# a directory's names in readdir's order, without . and ..
def dir(path: String) -> IO(List<&2, String>):
  import "./sys.c"

# canonicalizePath ("" when it fails)
def realpath(path: String) -> IO(String):
  import "./sys.c"

# [bsize, blocks, bfree, bavail] as pairs of words, or []
def statfs(path: String) -> IO(List<&2, U32>):
  import "./sys.c"

# the wall clock: [seconds low, seconds high, nanoseconds]
def now() -> IO(List<&2, U32>):
  import "./sys.c"

# localtime: [year, month, day, hour, minute, second, weekday, yearday,
# offset in minutes]
def local(lo: U32, hi: U32) -> IO(List<&2, U32>):
  import "./sys.c"

# the zone's name at those seconds
def zone(lo: U32, hi: U32) -> IO(String):
  import "./sys.c"

# 1 once standard input can be read or has ended, 0 after ms milliseconds
def in_wait(ms: U32) -> IO(U32):
  import "./sys.c"

# the whole lines read so far (at the end, what is left too)
def in_lines() -> IO(List<&2, String>):
  import "./sys.c"

# 1 once standard input has ended and every line was taken
def in_eof() -> IO(U32):
  import "./sys.c"

def flush() -> IO(Unit):
  import "./sys.c"
