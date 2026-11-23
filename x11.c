// X11 for xmobar-bend
// ===================
//
// The bar's window and its drawing, each effect one Xlib, Cairo or Pango
// call or a fixed few, with no decisions in them: where the window goes,
// its struts, where each text, background, box and the border is drawn,
// and what an event means are decided in Bend (Win.bend, Draw.bend,
// Main.bend). X.bend declares each effect; this file is spliced into the
// program's C after Bend's runtime, each entry present only when its
// effect is reachable.
//
// One connection, opened by X.open and kept here. Words cross as U32s
// (a Position in two's complement); a list of numbers as a String of
// decimal numbers, a space between; drawing coordinates are whole numbers
// of 1/2048 pixel (q), which every coordinate xmobar draws at is.
//
// A frame is drawn as xmobar 0.50 draws one (Xmobar.X11.Draw,
// Xmobar.Draw.Cairo, Xmobar.X11.XRender): X.begin makes the frame's Pango
// context (Cairo's default font map, the config's dpi), X.layout lays out
// each segment's markup in it and answers its logical extents, and X.draw
// draws on a new pixmap: the background by XRender (the bg colour over the
// root's _XROOTPMAP_ID at the config's alpha), then each op with a new
// cairo_t on an Xlib surface of the pixmap, then copies it to the window
// and syncs, discarding the events queued (X.sync d True, as xmobar).

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/extensions/Xrender.h>
#include <X11/extensions/Xrandr.h>
#include <X11/extensions/Xinerama.h>
#include <cairo/cairo.h>
#include <cairo/cairo-xlib.h>
#include <pango/pangocairo.h>
#include <locale.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <wchar.h>

static Display* xb_dpy;
static Window   xb_root;
static int      xb_rr_base;

static PangoContext* xb_ctx;
static PangoLayout** xb_lays;
static u32           xb_nlays;
static u32           xb_cap;

static Term xb_words(Env e, const u32* p, u64 n) {
  Term xs = term_pak(CID(Nil), 0);
  for (u64 i = n; i > 0; i -= 1) {
    xs = io_node(e, CID(Con), (Term)p[i - 1], xs);
  }
  return xs;
}

#define xb_ok(r) ((Term)(u32)(r))

// X errors are printed and ignored, as Xlib's default handler would not
// (it exits): xmobar's requests on its own window do not fail.
static int xb_error(Display* d, XErrorEvent* ev) {
  char msg[256];
  XGetErrorText(d, ev->error_code, msg, sizeof msg);
  fprintf(stderr, "xmobar-bend: X error: %s (request %d)\n", msg, ev->request_code);
  return 0;
}

// X.open(): 1 when the display (DISPLAY) opened, 0 else. LC_CTYPE is the
// environment's first, as GHC's runtime sets it, for the font sets.
#ifdef CID(open)
Term xb_open_run(Env e, Term* f, IoWork* w) {
  setlocale(LC_CTYPE, "");
  XInitThreads();
  xb_dpy = XOpenDisplay(NULL);
  if (xb_dpy == NULL) {
    return xb_ok(0);
  }
  XSetErrorHandler(xb_error);
  xb_root = DefaultRootWindow(xb_dpy);
  int err;
  if (!XRRQueryExtension(xb_dpy, &xb_rr_base, &err)) {
    xb_rr_base = -1;
  }
  return xb_ok(1);
}
static void __attribute__((constructor)) xb_open_use(void) {
  io_eff(CID(open), xb_open_run);
}
#endif

// X.screens(): getScreenInfo: Xinerama's screens, else the root window's
// rectangle; x y w h each
#ifdef CID(screens)
Term xb_screens_run(Env e, Term* f, IoWork* w) {
  int n = 0;
  XineramaScreenInfo* s = XineramaQueryScreens(xb_dpy, &n);
  if (s != NULL) {
    u32* v = malloc(sizeof(u32) * 4 * (n > 0 ? n : 1));
    for (int i = 0; i < n; i += 1) {
      v[4 * i + 0] = (u32)(int)s[i].x_org;
      v[4 * i + 1] = (u32)(int)s[i].y_org;
      v[4 * i + 2] = (u32)(int)s[i].width;
      v[4 * i + 3] = (u32)(int)s[i].height;
    }
    XFree(s);
    Term t = xb_words(e, v, 4 * (u64)n);
    free(v);
    return t;
  }
  XWindowAttributes wa;
  XGetWindowAttributes(xb_dpy, xb_root, &wa);
  u32 v[4] = { (u32)wa.x, (u32)wa.y, (u32)wa.width, (u32)wa.height };
  return xb_words(e, v, 4);
}
static void __attribute__((constructor)) xb_screens_use(void) {
  io_eff(CID(screens), xb_screens_run);
}
#endif

// X.font_extents(font): initFont then textExtents fs "0": the font set of
// that name, else misc-fixed's; its ascent and descent of "0" ([] when
// neither loads)
#ifdef CID(font_extents)
static XFontSet xb_fontset(const char* name) {
  char** missing = NULL;
  int    n       = 0;
  char*  def     = NULL;
  XFontSet fs = XCreateFontSet(xb_dpy, name, &missing, &n, &def);
  if (missing != NULL) {
    XFreeStringList(missing);
  }
  return fs;
}
Term xb_font_extents_run(Env e, Term* f, IoWork* w) {
  u64 n = 0;
  char* name = io_cstr(e, f[0], &n);
  XFontSet fs = xb_fontset(name);
  free(name);
  if (fs == NULL) {
    fs = xb_fontset("-misc-fixed-*-*-*-*-*-*-*-*-*-*-*-*");
  }
  if (fs == NULL) {
    return term_pak(CID(Nil), 0);
  }
  XRectangle ink, log;
  wchar_t zero[1] = { L'0' };
  XwcTextExtents(fs, zero, 1, &ink, &log);
  u32 v[2] = { (u32)(int)(-log.y), (u32)(int)(log.height + log.y) };
  XFreeFontSet(xb_dpy, fs);
  return xb_words(e, v, 2);
}
static void __attribute__((constructor)) xb_font_extents_use(void) {
  io_eff(CID(font_extents), xb_font_extents_run);
}
#endif

// X.create(x, y, w, h, override): newWindow: an InputOutput child of the
// root at the default depth and visual, override_redirect its only
// attribute (set when override is 1)
#ifdef CID(create)
Term xb_create_run(Env e, Term* f, IoWork* w) {
  int scr = DefaultScreen(xb_dpy);
  XSetWindowAttributes a;
  memset(&a, 0, sizeof a);
  a.override_redirect = (u32)f[4] != 0;
  Window win = XCreateWindow(xb_dpy, xb_root, (int)(u32)f[0], (int)(u32)f[1],
    (unsigned)(u32)f[2], (unsigned)(u32)f[3], 0, DefaultDepth(xb_dpy, scr),
    InputOutput, DefaultVisual(xb_dpy, scr),
    (u32)f[4] != 0 ? CWOverrideRedirect : 0, &a);
  return xb_ok(win);
}
static void __attribute__((constructor)) xb_create_use(void) {
  io_eff(CID(create), xb_create_run);
}
#endif

// X.set_text(win, which, text): setTextProperty: WM_CLASS (which 0) or
// WM_NAME (1), the text's bytes as one STRING
#ifdef CID(set_text)
Term xb_set_text_run(Env e, Term* f, IoWork* w) {
  u64 n = 0;
  char* s = io_cstr(e, f[2], &n);
  XTextProperty tp;
  tp.value    = (unsigned char*)s;
  tp.encoding = XA_STRING;
  tp.format   = 8;
  tp.nitems   = (unsigned long)n;
  XSetTextProperty(xb_dpy, (Window)(u32)f[0], &tp, (u32)f[1] == 0 ? XA_WM_CLASS : XA_WM_NAME);
  free(s);
  return xb_ok(0);
}
static void __attribute__((constructor)) xb_set_text_use(void) {
  io_eff(CID(set_text), xb_set_text_run);
}
#endif

// X.prop32(win, name, type, values): changeProperty32 at PropModeReplace:
// the atoms of name and type interned, values its decimal numbers
#ifdef CID(prop32)
Term xb_prop32_run(Env e, Term* f, IoWork* w) {
  u64 n = 0;
  char* name = io_cstr(e, f[1], &n);
  char* type = io_cstr(e, f[2], &n);
  char* vals = io_cstr(e, f[3], &n);
  long  v[64];
  int   k = 0;
  char* p = vals;
  while (*p != 0 && k < 64) {
    char* q;
    unsigned long x = strtoul(p, &q, 10);
    if (q == p) {
      break;
    }
    v[k++] = (long)(u32)x;
    p = q;
  }
  Atom an = XInternAtom(xb_dpy, name, False);
  Atom at = XInternAtom(xb_dpy, type, False);
  XChangeProperty(xb_dpy, (Window)(u32)f[0], an, at, 32, PropModeReplace, (unsigned char*)v, k);
  free(name);
  free(type);
  free(vals);
  return xb_ok(0);
}
static void __attribute__((constructor)) xb_prop32_use(void) {
  io_eff(CID(prop32), xb_prop32_run);
}
#endif

// X.atom(name): the interned atom (an ATOM property's value)
#ifdef CID(atom)
Term xb_atom_run(Env e, Term* f, IoWork* w) {
  u64 n = 0;
  char* name = io_cstr(e, f[0], &n);
  Atom a = XInternAtom(xb_dpy, name, False);
  free(name);
  return xb_ok(a);
}
static void __attribute__((constructor)) xb_atom_use(void) {
  io_eff(CID(atom), xb_atom_run);
}
#endif

// X.pid(): getProcessID
#ifdef CID(pid)
Term xb_pid_run(Env e, Term* f, IoWork* w) {
  return xb_ok(getpid());
}
static void __attribute__((constructor)) xb_pid_use(void) {
  io_eff(CID(pid), xb_pid_run);
}
#endif

// X.lower(win), X.map(win), X.unmap(win)
#ifdef CID(lower)
Term xb_lower_run(Env e, Term* f, IoWork* w) {
  XLowerWindow(xb_dpy, (Window)(u32)f[0]);
  return xb_ok(0);
}
static void __attribute__((constructor)) xb_lower_use(void) {
  io_eff(CID(lower), xb_lower_run);
}
#endif

#ifdef CID(map)
Term xb_map_run(Env e, Term* f, IoWork* w) {
  XMapWindow(xb_dpy, (Window)(u32)f[0]);
  return xb_ok(0);
}
static void __attribute__((constructor)) xb_map_use(void) {
  io_eff(CID(map), xb_map_run);
}
#endif

#ifdef CID(unmap)
Term xb_unmap_run(Env e, Term* f, IoWork* w) {
  XUnmapWindow(xb_dpy, (Window)(u32)f[0]);
  return xb_ok(0);
}
static void __attribute__((constructor)) xb_unmap_use(void) {
  io_eff(CID(unmap), xb_unmap_run);
}
#endif

// X.move_resize(win, x, y, w, h)
#ifdef CID(move_resize)
Term xb_move_resize_run(Env e, Term* f, IoWork* w) {
  XMoveResizeWindow(xb_dpy, (Window)(u32)f[0], (int)(u32)f[1], (int)(u32)f[2],
    (unsigned)(u32)f[3], (unsigned)(u32)f[4]);
  return xb_ok(0);
}
static void __attribute__((constructor)) xb_move_resize_use(void) {
  io_eff(CID(move_resize), xb_move_resize_run);
}
#endif

// X.sync(discard): XSync
#ifdef CID(sync)
Term xb_sync_run(Env e, Term* f, IoWork* w) {
  XSync(xb_dpy, (u32)f[0] != 0);
  return xb_ok(0);
}
static void __attribute__((constructor)) xb_sync_use(void) {
  io_eff(CID(sync), xb_sync_run);
}
#endif

// X.select(win): the event loop's selection: the root's screen changes
// (Xrandr), the window's exposures, structure and button presses
#ifdef CID(select)
Term xb_select_run(Env e, Term* f, IoWork* w) {
  if (xb_rr_base >= 0) {
    XRRSelectInput(xb_dpy, xb_root, RRScreenChangeNotifyMask);
  }
  XSelectInput(xb_dpy, (Window)(u32)f[0], ExposureMask | StructureNotifyMask | ButtonPressMask);
  XFlush(xb_dpy);
  return xb_ok(0);
}
static void __attribute__((constructor)) xb_select_use(void) {
  io_eff(CID(select), xb_select_run);
}
#endif

// X.wait(ms, stdin): 2 when X has events, else 1 when standard input (if
// stdin is 1) can be read or has ended, 0 when ms milliseconds pass first
#ifdef CID(wait)
Term xb_wait_run(Env e, Term* f, IoWork* w) {
  XFlush(xb_dpy);
  if (XPending(xb_dpy) > 0) {
    return xb_ok(2);
  }
  struct pollfd p[2] = {
    { ConnectionNumber(xb_dpy), POLLIN, 0 },
    { 0, POLLIN, 0 },
  };
  int n = (u32)f[1] != 0 ? 2 : 1;
  int r = poll(p, n, (int)(u32)f[0]);
  if (r <= 0) {
    return xb_ok(0);
  }
  if (p[0].revents != 0 && XPending(xb_dpy) > 0) {
    return xb_ok(2);
  }
  return xb_ok(n == 2 && p[1].revents != 0 ? 1 : 0);
}
static void __attribute__((constructor)) xb_wait_use(void) {
  io_eff(CID(wait), xb_wait_run);
}
#endif

// X.events(): the events queued, three words each: 1 0 0 an Expose, 2
// button x a ButtonPress, 3 0 0 a ConfigureNotify or an Xrandr screen
// change (the others dropped)
#ifdef CID(events)
Term xb_events_run(Env e, Term* f, IoWork* w) {
  u32  cap = 48, n = 0;
  u32* v   = malloc(sizeof(u32) * cap);
  while (XPending(xb_dpy) > 0) {
    XEvent ev;
    XNextEvent(xb_dpy, &ev);
    u32 k = 0, a = 0, b = 0;
    if (ev.type == Expose) {
      k = 1;
    } else if (ev.type == ButtonPress) {
      k = 2;
      a = ev.xbutton.button;
      b = (u32)ev.xbutton.x;
    } else if (ev.type == ConfigureNotify
               || (xb_rr_base >= 0 && ev.type == xb_rr_base + RRScreenChangeNotify)) {
      if (xb_rr_base >= 0 && ev.type == xb_rr_base + RRScreenChangeNotify) {
        XRRUpdateConfiguration(&ev);
      }
      k = 3;
    }
    if (k != 0) {
      if (n + 3 > cap) {
        cap *= 2;
        v = realloc(v, sizeof(u32) * cap);
      }
      v[n++] = k;
      v[n++] = a;
      v[n++] = b;
    }
  }
  Term t = xb_words(e, v, n);
  free(v);
  return t;
}
static void __attribute__((constructor)) xb_events_use(void) {
  io_eff(CID(events), xb_events_run);
}
#endif

// Drawing --------------------------------------------------------------------------

static void xb_free_frame(void) {
  for (u32 i = 0; i < xb_nlays; i += 1) {
    g_object_unref(xb_lays[i]);
  }
  xb_nlays = 0;
  if (xb_ctx != NULL) {
    g_object_unref(xb_ctx);
    xb_ctx = NULL;
  }
}

// X.begin(dpi): a frame's Pango context: cairoCreateContext Nothing (the
// default Cairo font map's), its resolution the dpi (a Double's text)
#ifdef CID(begin)
Term xb_begin_run(Env e, Term* f, IoWork* w) {
  u64 n = 0;
  char* dpi = io_cstr(e, f[0], &n);
  xb_free_frame();
  xb_ctx = pango_font_map_create_context(pango_cairo_font_map_get_default());
  pango_cairo_context_set_resolution(xb_ctx, strtod(dpi, NULL));
  free(dpi);
  return xb_ok(0);
}
static void __attribute__((constructor)) xb_begin_use(void) {
  io_eff(CID(begin), xb_begin_run);
}
#endif

// X.layout(markup): layoutEmpty, layoutSetMarkup, layoutGetExtents: the
// logical rectangle in Pango units (x y w h); the layout kept for X.draw
#ifdef CID(layout)
Term xb_layout_run(Env e, Term* f, IoWork* w) {
  u64 n = 0;
  char* m = io_cstr(e, f[0], &n);
  PangoLayout* l = pango_layout_new(xb_ctx);
  pango_layout_set_markup(l, m, (int)n);
  free(m);
  PangoRectangle ink, log;
  pango_layout_get_extents(l, &ink, &log);
  if (xb_nlays == xb_cap) {
    xb_cap  = xb_cap == 0 ? 16 : xb_cap * 2;
    xb_lays = realloc(xb_lays, sizeof(PangoLayout*) * xb_cap);
  }
  xb_lays[xb_nlays++] = l;
  u32 v[4] = { (u32)log.x, (u32)log.y, (u32)log.width, (u32)log.height };
  return xb_words(e, v, 4);
}
static void __attribute__((constructor)) xb_layout_use(void) {
  io_eff(CID(layout), xb_layout_run);
}
#endif

#ifdef CID(draw)

static double xb_q(long v) {
  return (double)(int32_t)(u32)v / 2048.0;
}

// setSourceColor: r g b (0-255), alpha 0 opaque, 1 at alpha 0; 2 is read's
// error in Haskell, here the bar's end
static void xb_source(cairo_t* cr, long r, long g, long b, long a) {
  if (a == 2) {
    fprintf(stderr, "xmobar-bend: Prelude.read: no parse\n");
    exit(1);
  }
  if (a == 1) {
    cairo_set_source_rgba(cr, r / 255.0, g / 255.0, b / 255.0, 0.0);
  } else {
    cairo_set_source_rgb(cr, r / 255.0, g / 255.0, b / 255.0);
  }
}

// XRender.drawBackground d p bgc alpha (Rectangle x y w h)
static void xb_background(Drawable p, const char* bg, int alpha, int x, int y, unsigned w, unsigned h) {
  XRenderPictFormat* fmt = XRenderFindStandardFormat(xb_dpy, PictStandardRGB24);
  XRenderPictureAttributes attr;
  memset(&attr, 0, sizeof attr);
  Picture pic = XRenderCreatePicture(xb_dpy, p, fmt, 0, &attr);
  XColor c;
  memset(&c, 0, sizeof c);
  if (!XParseColor(xb_dpy, DefaultColormap(xb_dpy, DefaultScreen(xb_dpy)), bg, &c)) {
    fprintf(stderr, "xmobar-bend: parseColor: bad color %s\n", bg);
    exit(1);
  }
  XRenderColor bc = { c.red, c.green, c.blue, 0xFFFF };
  Picture bgfill = XRenderCreateSolidFill(xb_dpy, &bc);
  XRenderColor mc = { 0, 0, 0, (unsigned short)(257 * alpha) };
  Picture mask = XRenderCreateSolidFill(xb_dpy, &mc);
  XRenderComposite(xb_dpy, PictOpSrc, bgfill, mask, pic, x, y, 0, 0, 0, 0, w, h);
  XRenderFreePicture(xb_dpy, mask);
  XRenderFreePicture(xb_dpy, bgfill);
  Atom xid = XInternAtom(xb_dpy, "_XROOTPMAP_ID", False);
  Atom type;
  int format;
  unsigned long items, after;
  unsigned char* prop = NULL;
  XGetWindowProperty(xb_dpy, xb_root, xid, 0, 1, False, 20, &type, &format, &items, &after, &prop);
  if (prop != NULL) {
    Pixmap rootbg = *(Pixmap*)prop;
    XFree(prop);
    Picture bgpic = XRenderCreatePicture(xb_dpy, rootbg, fmt, 0, &attr);
    XRenderColor ac = { 0, 0, 0, (unsigned short)(0xFFFF - 257 * alpha) };
    Picture amask = XRenderCreateSolidFill(xb_dpy, &ac);
    XRenderComposite(xb_dpy, PictOpAdd, bgpic, amask, pic, x, y, 0, 0, 0, 0, w, h);
    XRenderFreePicture(xb_dpy, amask);
    XRenderFreePicture(xb_dpy, bgpic);
  }
  XRenderFreePicture(xb_dpy, pic);
}

// one op of Draw.bend's frame, each with a new cairo_t (renderWith):
//   T layout x y width   the layout at x y, ellipsized at width (-1 none)
//   R colour x y w h     a rectangle filled (fillPreserve)
//   L colour lw n x0 y0 x1 y1 ...  n lines stroked at line width lw
//   S colour lw x y w h  a rectangle stroked
static void xb_op(cairo_surface_t* s, char* line) {
  long v[1 + 4 + 2 + 4 * 16];
  int  k = 0;
  char op = line[0];
  char* p = line + 1;
  while (*p != 0 && k < (int)(sizeof v / sizeof v[0])) {
    char* q;
    long x = strtol(p, &q, 10);
    if (q == p) {
      break;
    }
    v[k++] = x;
    p = q;
  }
  cairo_t* cr = cairo_create(s);
  if (op == 'T' && k >= 4 && (u32)v[0] < xb_nlays) {
    PangoLayout* l = xb_lays[v[0]];
    if (v[3] >= 0) {
      pango_layout_set_ellipsize(l, PANGO_ELLIPSIZE_END);
      pango_layout_set_width(l, (int)v[3]);
    }
    cairo_move_to(cr, xb_q(v[1]), xb_q(v[2]));
    pango_cairo_show_layout(cr, l);
  } else if (op == 'R' && k >= 8) {
    xb_source(cr, v[0], v[1], v[2], v[3]);
    cairo_rectangle(cr, xb_q(v[4]), xb_q(v[5]), xb_q(v[6]), xb_q(v[7]));
    cairo_fill_preserve(cr);
  } else if (op == 'L' && k >= 6) {
    xb_source(cr, v[0], v[1], v[2], v[3]);
    cairo_set_line_width(cr, xb_q(v[4]));
    for (long i = 0; i < v[5] && 6 + 4 * i + 3 < k; i += 1) {
      cairo_move_to(cr, xb_q(v[6 + 4 * i]), xb_q(v[7 + 4 * i]));
      cairo_line_to(cr, xb_q(v[8 + 4 * i]), xb_q(v[9 + 4 * i]));
      cairo_stroke(cr);
    }
  } else if (op == 'S' && k >= 9) {
    xb_source(cr, v[0], v[1], v[2], v[3]);
    cairo_set_line_width(cr, xb_q(v[4]));
    cairo_rectangle(cr, xb_q(v[5]), xb_q(v[6]), xb_q(v[7]), xb_q(v[8]));
    cairo_stroke(cr);
  }
  cairo_destroy(cr);
}

// X.draw(win, x, y, w, h, bg, alpha, ops): Xmobar.X11.Draw's draw: a
// pixmap of the window's size, its background, the ops, copied to the
// window; the frame's layouts and context then freed
Term xb_draw_run(Env e, Term* f, IoWork* w) {
  u64 n = 0;
  Window   win = (Window)(u32)f[0];
  int      x   = (int)(u32)f[1];
  int      y   = (int)(u32)f[2];
  unsigned wd  = (unsigned)(u32)f[3];
  unsigned ht  = (unsigned)(u32)f[4];
  char*    bg  = io_cstr(e, f[5], &n);
  int      al  = (int)(u32)f[6];
  char*    ops = io_cstr(e, f[7], &n);
  int      scr = DefaultScreen(xb_dpy);
  Pixmap   p   = XCreatePixmap(xb_dpy, win, wd, ht, DefaultDepth(xb_dpy, scr));
  GC       gc  = XCreateGC(xb_dpy, win, 0, NULL);
  XSetGraphicsExposures(xb_dpy, gc, False);
  xb_background(p, bg, al, x, y, wd, ht);
  cairo_surface_t* s = cairo_xlib_surface_create(xb_dpy, p, DefaultVisual(xb_dpy, scr), (int)wd, (int)ht);
  char* line = ops;
  while (*line != 0) {
    char* end = strchr(line, '\n');
    if (end != NULL) {
      *end = 0;
    }
    xb_op(s, line);
    if (end == NULL) {
      break;
    }
    line = end + 1;
  }
  cairo_surface_destroy(s);
  XCopyArea(xb_dpy, p, win, gc, 0, 0, wd, ht, 0, 0);
  XFreeGC(xb_dpy, gc);
  XFreePixmap(xb_dpy, p);
  XSync(xb_dpy, True);
  xb_free_frame();
  free(bg);
  free(ops);
  return xb_ok(0);
}
static void __attribute__((constructor)) xb_draw_use(void) {
  io_eff(CID(draw), xb_draw_run);
}
#endif

// X.spawn(cmd): runAction: spawnCommand (cmd ++ " &") and its wait
#ifdef CID(spawn)
#include <sys/wait.h>
Term xb_spawn_run(Env e, Term* f, IoWork* w) {
  u64 n = 0;
  char* cmd = io_cstr(e, f[0], &n);
  char* full = malloc(n + 3);
  memcpy(full, cmd, n);
  memcpy(full + n, " &", 3);
  pid_t pid = fork();
  if (pid == 0) {
    execl("/bin/sh", "sh", "-c", full, (char*)NULL);
    _exit(127);
  }
  int st = 0;
  if (pid > 0) {
    waitpid(pid, &st, 0);
  }
  free(cmd);
  free(full);
  return xb_ok(0);
}
static void __attribute__((constructor)) xb_spawn_use(void) {
  io_eff(CID(spawn), xb_spawn_run);
}
#endif
