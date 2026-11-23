// The system for xmobar-bend
// ==========================
//
// What the monitors read and the loop waits on, each effect one system
// call or a few, with no decisions in them: Sys.bend declares them, the
// monitors (Mon.bend) and the loop (Main.bend) decide. This file is
// spliced into the program's C after Bend's runtime.
//
// Numbers cross as U32s; a 64-bit one as two, its low word first.

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/vfs.h>
#include <time.h>
#include <unistd.h>

static Term sys_words(Env e, const u32* p, u64 n) {
  Term xs = term_pak(CID(Nil), 0);
  for (u64 i = n; i > 0; i -= 1) {
    xs = io_node(e, CID(Con), (Term)p[i - 1], xs);
  }
  return xs;
}

// Sys.slurp(path): the file's bytes as a String ("" when it does not open)
#ifdef CID(slurp)
Term sys_slurp_run(Env e, Term* f, IoWork* w) {
  u64 n = 0;
  char* path = io_cstr(e, f[0], &n);
  int fd = open(path, O_RDONLY | O_CLOEXEC);
  free(path);
  if (fd < 0) {
    return io_str(e, "", 0);
  }
  size_t cap = 8192, len = 0;
  char* buf = malloc(cap);
  for (;;) {
    ssize_t r = read(fd, buf + len, cap - len);
    if (r < 0 && errno == EINTR) {
      continue;
    }
    if (r <= 0) {
      break;
    }
    len += (size_t)r;
    if (len == cap) {
      cap *= 2;
      buf = realloc(buf, cap);
    }
  }
  close(fd);
  Term t = io_str(e, buf, len);
  free(buf);
  return t;
}
static void __attribute__((constructor)) sys_slurp_use(void) {
  io_eff(CID(slurp), sys_slurp_run);
}
#endif

// Sys.kind(path): 0 nothing there, 1 a file (anything but a directory),
// 2 a directory; symbolic links followed (doesFileExist, doesDirectoryExist)
#ifdef CID(kind)
Term sys_kind_run(Env e, Term* f, IoWork* w) {
  u64 n = 0;
  char* path = io_cstr(e, f[0], &n);
  struct stat st;
  u32 k = 0;
  if (stat(path, &st) == 0) {
    k = S_ISDIR(st.st_mode) ? 2 : 1;
  }
  free(path);
  return (Term)k;
}
static void __attribute__((constructor)) sys_kind_use(void) {
  io_eff(CID(kind), sys_kind_run);
}
#endif

// Sys.dir(path): the directory's names in readdir's order, . and .. left
// out (listDirectory); [] when it does not open
#ifdef CID(dir)
Term sys_dir_run(Env e, Term* f, IoWork* w) {
  u64 n = 0;
  char* path = io_cstr(e, f[0], &n);
  DIR* d = opendir(path);
  free(path);
  Term xs = term_pak(CID(Nil), 0);
  if (!d) {
    return xs;
  }
  // collected first, then consed from the last, so the list keeps the order
  size_t cap = 64, len = 0;
  char** names = malloc(cap * sizeof(char*));
  struct dirent* de;
  while ((de = readdir(d)) != NULL) {
    if (strcmp(de->d_name, ".") == 0 || strcmp(de->d_name, "..") == 0) {
      continue;
    }
    if (len == cap) {
      cap *= 2;
      names = realloc(names, cap * sizeof(char*));
    }
    names[len++] = strdup(de->d_name);
  }
  closedir(d);
  for (size_t i = len; i > 0; i -= 1) {
    xs = io_node(e, CID(Con), io_str(e, names[i - 1], strlen(names[i - 1])), xs);
    free(names[i - 1]);
  }
  free(names);
  return xs;
}
static void __attribute__((constructor)) sys_dir_use(void) {
  io_eff(CID(dir), sys_dir_run);
}
#endif

// Sys.realpath(path): canonicalizePath's answer ("" when it fails)
#ifdef CID(realpath)
Term sys_realpath_run(Env e, Term* f, IoWork* w) {
  u64 n = 0;
  char* path = io_cstr(e, f[0], &n);
  char out[PATH_MAX];
  Term t = realpath(path, out) ? io_str(e, out, strlen(out)) : io_str(e, "", 0);
  free(path);
  return t;
}
static void __attribute__((constructor)) sys_realpath_use(void) {
  io_eff(CID(realpath), sys_realpath_run);
}
#endif

// Sys.statfs(path): [bsize, blocks, bfree, bavail], each 64-bit as two
// words (low first): [] when statfs fails
#ifdef CID(statfs)
Term sys_statfs_run(Env e, Term* f, IoWork* w) {
  u64 n = 0;
  char* path = io_cstr(e, f[0], &n);
  struct statfs s;
  int r = statfs(path, &s);
  free(path);
  if (r != 0) {
    return term_pak(CID(Nil), 0);
  }
  u64 v[4] = { (u64)s.f_bsize, (u64)s.f_blocks, (u64)s.f_bfree, (u64)s.f_bavail };
  u32 ws[8];
  for (int i = 0; i < 4; i += 1) {
    ws[2 * i] = (u32)v[i];
    ws[2 * i + 1] = (u32)(v[i] >> 32);
  }
  return sys_words(e, ws, 8);
}
static void __attribute__((constructor)) sys_statfs_use(void) {
  io_eff(CID(statfs), sys_statfs_run);
}
#endif

// Sys.now(): the wall clock: [seconds (low), seconds (high), nanoseconds]
#ifdef CID(now)
Term sys_now_run(Env e, Term* f, IoWork* w) {
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  u64 s = (u64)ts.tv_sec;
  u32 ws[3] = { (u32)s, (u32)(s >> 32), (u32)ts.tv_nsec };
  return sys_words(e, ws, 3);
}
static void __attribute__((constructor)) sys_now_use(void) {
  io_eff(CID(now), sys_now_run);
}
#endif

// Sys.local(lo, hi): localtime of those seconds: [year, month (1-12),
// day, hour, minute, second, weekday (0 Sunday), day of the year (0-365),
// the zone's offset in minutes (two's complement)]
#ifdef CID(local)
Term sys_local_run(Env e, Term* f, IoWork* w) {
  time_t t = (time_t)(((u64)(u32)f[1] << 32) | (u64)(u32)f[0]);
  struct tm tm;
  tzset();
  localtime_r(&t, &tm);
  u32 ws[9] = { (u32)(tm.tm_year + 1900), (u32)(tm.tm_mon + 1), (u32)tm.tm_mday,
    (u32)tm.tm_hour, (u32)tm.tm_min, (u32)tm.tm_sec, (u32)tm.tm_wday,
    (u32)tm.tm_yday, (u32)(int32_t)(tm.tm_gmtoff / 60) };
  return sys_words(e, ws, 9);
}
static void __attribute__((constructor)) sys_local_use(void) {
  io_eff(CID(local), sys_local_run);
}
#endif

// Sys.zone(lo, hi): the zone's name at those seconds (tm_zone)
#ifdef CID(zone)
Term sys_zone_run(Env e, Term* f, IoWork* w) {
  time_t t = (time_t)(((u64)(u32)f[1] << 32) | (u64)(u32)f[0]);
  struct tm tm;
  tzset();
  localtime_r(&t, &tm);
  const char* z = tm.tm_zone ? tm.tm_zone : "";
  return io_str(e, z, strlen(z));
}
static void __attribute__((constructor)) sys_zone_use(void) {
  io_eff(CID(zone), sys_zone_run);
}
#endif

// Standard input, a line at a time: what has been read and not yet
// answered waits here; at end of file the rest is a last line
static char*  sys_in_buf;
static size_t sys_in_len, sys_in_cap;
static int    sys_in_eof;

static void sys_in_fill(void) {
  if (sys_in_eof) {
    return;
  }
  int fl = fcntl(0, F_GETFL);
  fcntl(0, F_SETFL, fl | O_NONBLOCK);
  for (;;) {
    if (sys_in_cap - sys_in_len < 4096) {
      sys_in_cap = sys_in_cap ? sys_in_cap * 2 : 8192;
      sys_in_buf = realloc(sys_in_buf, sys_in_cap);
    }
    ssize_t r = read(0, sys_in_buf + sys_in_len, sys_in_cap - sys_in_len);
    if (r > 0) {
      sys_in_len += (size_t)r;
      continue;
    }
    if (r == 0) {
      sys_in_eof = 1;
    }
    break;
  }
  fcntl(0, F_SETFL, fl);
}

static Term sys_in_wait_more(Env e, IoWork* w) {
  struct pollfd p = { 0, POLLIN, 0 };
  return (Term)(u32)(poll(&p, 1, 0) > 0 ? 1 : 0);
}

// Sys.in_wait(ms): 1 once standard input can be read (or is at its end),
// 0 when ms milliseconds pass first
#ifdef CID(in_wait)
Term sys_in_wait_run(Env e, Term* f, IoWork* w) {
  if (sys_in_eof || memchr(sys_in_buf ? sys_in_buf : "", '\n', sys_in_len)) {
    return (Term)(u32)1;
  }
  return io_wait_on(w, 0, POLLIN, io_tick() + (u64)(u32)f[0] * 1000000ull, sys_in_wait_more);
}
static void __attribute__((constructor)) sys_in_wait_use(void) {
  io_eff(CID(in_wait), sys_in_wait_run);
}
#endif

// Sys.in_lines(): the whole lines read so far (their newlines dropped);
// at end of file, what is left as one more
#ifdef CID(in_lines)
Term sys_in_lines_run(Env e, Term* f, IoWork* w) {
  sys_in_fill();
  Term xs = term_pak(CID(Nil), 0);
  // the lines, last first, then reversed by consing from the end
  size_t count = 0, start = 0;
  size_t* starts = NULL;
  size_t* ends = NULL;
  for (size_t i = 0; i < sys_in_len; i += 1) {
    if (sys_in_buf[i] == '\n') {
      starts = realloc(starts, (count + 1) * sizeof(size_t));
      ends = realloc(ends, (count + 1) * sizeof(size_t));
      starts[count] = start;
      ends[count] = i;
      count += 1;
      start = i + 1;
    }
  }
  if (sys_in_eof && start < sys_in_len) {
    starts = realloc(starts, (count + 1) * sizeof(size_t));
    ends = realloc(ends, (count + 1) * sizeof(size_t));
    starts[count] = start;
    ends[count] = sys_in_len;
    count += 1;
    start = sys_in_len;
  }
  for (size_t i = count; i > 0; i -= 1) {
    xs = io_node(e, CID(Con), io_str(e, sys_in_buf + starts[i - 1], ends[i - 1] - starts[i - 1]), xs);
  }
  free(starts);
  free(ends);
  memmove(sys_in_buf, sys_in_buf + start, sys_in_len - start);
  sys_in_len -= start;
  return xs;
}
static void __attribute__((constructor)) sys_in_lines_use(void) {
  io_eff(CID(in_lines), sys_in_lines_run);
}
#endif

// Sys.in_eof(): 1 once standard input has ended and every line was answered
#ifdef CID(in_eof)
Term sys_in_eof_run(Env e, Term* f, IoWork* w) {
  return (Term)(u32)(sys_in_eof && sys_in_len == 0 ? 1 : 0);
}
static void __attribute__((constructor)) sys_in_eof_use(void) {
  io_eff(CID(in_eof), sys_in_eof_run);
}
#endif

// Sys.flush(): stdout's buffer written out (a line at a time, as xmobar's
// LineBuffering)
#ifdef CID(flush)
Term sys_flush_run(Env e, Term* f, IoWork* w) {
  fflush(stdout);
  return term_pak(CID(Unit), 0);
}
static void __attribute__((constructor)) sys_flush_use(void) {
  io_eff(CID(flush), sys_flush_run);
}
#endif
