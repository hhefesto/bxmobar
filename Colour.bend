# Colours as xmobar 0.50 draws them with cairo (Xmobar.Draw.Cairo's
# readColourName): a name of the colour package's Data.Colour.Names (the
# SVG 1.1 names, lower case only), else sRGB24reads ("#rrggbb" or
# "rrggbb", two hex digits each), else white. What sRGB24reads leaves after
# the six digits is an alpha, read ("0x" ++ rest) :: Double: below 1 (that
# is, 0) the colour is set with that alpha, else opaque; a rest that does
# not read is read's error, when the colour is used.
#
# The table is colour-2.3.7's Names.hs, by tests/gen/colour-names.awk.

import Base
import ./Hs.bend as H

type RGB is Data:
  RGB{r: U32, g: U32, b: U32}

type Alpha is Data:
  Opaque{}
  Clear{}
  AlphaBad{}

type Colour is Data:
  Colour{rgb: RGB, a: Alpha}

type CName is Data:
  CName{name: String, rgb: RGB}

def names() -> List<&2, CName>:
  [
    CName{"aliceblue", RGB{240, 248, 255}},
    CName{"antiquewhite", RGB{250, 235, 215}},
    CName{"aqua", RGB{0, 255, 255}},
    CName{"aquamarine", RGB{127, 255, 212}},
    CName{"azure", RGB{240, 255, 255}},
    CName{"beige", RGB{245, 245, 220}},
    CName{"bisque", RGB{255, 228, 196}},
    CName{"black", RGB{0, 0, 0}},
    CName{"blanchedalmond", RGB{255, 235, 205}},
    CName{"blue", RGB{0, 0, 255}},
    CName{"blueviolet", RGB{138, 43, 226}},
    CName{"brown", RGB{165, 42, 42}},
    CName{"burlywood", RGB{222, 184, 135}},
    CName{"cadetblue", RGB{95, 158, 160}},
    CName{"chartreuse", RGB{127, 255, 0}},
    CName{"chocolate", RGB{210, 105, 30}},
    CName{"coral", RGB{255, 127, 80}},
    CName{"cornflowerblue", RGB{100, 149, 237}},
    CName{"cornsilk", RGB{255, 248, 220}},
    CName{"crimson", RGB{220, 20, 60}},
    CName{"cyan", RGB{0, 255, 255}},
    CName{"darkblue", RGB{0, 0, 139}},
    CName{"darkcyan", RGB{0, 139, 139}},
    CName{"darkgoldenrod", RGB{184, 134, 11}},
    CName{"darkgray", RGB{169, 169, 169}},
    CName{"darkgreen", RGB{0, 100, 0}},
    CName{"darkgrey", RGB{169, 169, 169}},
    CName{"darkkhaki", RGB{189, 183, 107}},
    CName{"darkmagenta", RGB{139, 0, 139}},
    CName{"darkolivegreen", RGB{85, 107, 47}},
    CName{"darkorange", RGB{255, 140, 0}},
    CName{"darkorchid", RGB{153, 50, 204}},
    CName{"darkred", RGB{139, 0, 0}},
    CName{"darksalmon", RGB{233, 150, 122}},
    CName{"darkseagreen", RGB{143, 188, 143}},
    CName{"darkslateblue", RGB{72, 61, 139}},
    CName{"darkslategray", RGB{47, 79, 79}},
    CName{"darkslategrey", RGB{47, 79, 79}},
    CName{"darkturquoise", RGB{0, 206, 209}},
    CName{"darkviolet", RGB{148, 0, 211}},
    CName{"deeppink", RGB{255, 20, 147}},
    CName{"deepskyblue", RGB{0, 191, 255}},
    CName{"dimgray", RGB{105, 105, 105}},
    CName{"dimgrey", RGB{105, 105, 105}},
    CName{"dodgerblue", RGB{30, 144, 255}},
    CName{"firebrick", RGB{178, 34, 34}},
    CName{"floralwhite", RGB{255, 250, 240}},
    CName{"forestgreen", RGB{34, 139, 34}},
    CName{"fuchsia", RGB{255, 0, 255}},
    CName{"gainsboro", RGB{220, 220, 220}},
    CName{"ghostwhite", RGB{248, 248, 255}},
    CName{"gold", RGB{255, 215, 0}},
    CName{"goldenrod", RGB{218, 165, 32}},
    CName{"gray", RGB{128, 128, 128}},
    CName{"grey", RGB{128, 128, 128}},
    CName{"green", RGB{0, 128, 0}},
    CName{"greenyellow", RGB{173, 255, 47}},
    CName{"honeydew", RGB{240, 255, 240}},
    CName{"hotpink", RGB{255, 105, 180}},
    CName{"indianred", RGB{205, 92, 92}},
    CName{"indigo", RGB{75, 0, 130}},
    CName{"ivory", RGB{255, 255, 240}},
    CName{"khaki", RGB{240, 230, 140}},
    CName{"lavender", RGB{230, 230, 250}},
    CName{"lavenderblush", RGB{255, 240, 245}},
    CName{"lawngreen", RGB{124, 252, 0}},
    CName{"lemonchiffon", RGB{255, 250, 205}},
    CName{"lightblue", RGB{173, 216, 230}},
    CName{"lightcoral", RGB{240, 128, 128}},
    CName{"lightcyan", RGB{224, 255, 255}},
    CName{"lightgoldenrodyellow", RGB{250, 250, 210}},
    CName{"lightgray", RGB{211, 211, 211}},
    CName{"lightgreen", RGB{144, 238, 144}},
    CName{"lightgrey", RGB{211, 211, 211}},
    CName{"lightpink", RGB{255, 182, 193}},
    CName{"lightsalmon", RGB{255, 160, 122}},
    CName{"lightseagreen", RGB{32, 178, 170}},
    CName{"lightskyblue", RGB{135, 206, 250}},
    CName{"lightslategray", RGB{119, 136, 153}},
    CName{"lightslategrey", RGB{119, 136, 153}},
    CName{"lightsteelblue", RGB{176, 196, 222}},
    CName{"lightyellow", RGB{255, 255, 224}},
    CName{"lime", RGB{0, 255, 0}},
    CName{"limegreen", RGB{50, 205, 50}},
    CName{"linen", RGB{250, 240, 230}},
    CName{"magenta", RGB{255, 0, 255}},
    CName{"maroon", RGB{128, 0, 0}},
    CName{"mediumaquamarine", RGB{102, 205, 170}},
    CName{"mediumblue", RGB{0, 0, 205}},
    CName{"mediumorchid", RGB{186, 85, 211}},
    CName{"mediumpurple", RGB{147, 112, 219}},
    CName{"mediumseagreen", RGB{60, 179, 113}},
    CName{"mediumslateblue", RGB{123, 104, 238}},
    CName{"mediumspringgreen", RGB{0, 250, 154}},
    CName{"mediumturquoise", RGB{72, 209, 204}},
    CName{"mediumvioletred", RGB{199, 21, 133}},
    CName{"midnightblue", RGB{25, 25, 112}},
    CName{"mintcream", RGB{245, 255, 250}},
    CName{"mistyrose", RGB{255, 228, 225}},
    CName{"moccasin", RGB{255, 228, 181}},
    CName{"navajowhite", RGB{255, 222, 173}},
    CName{"navy", RGB{0, 0, 128}},
    CName{"oldlace", RGB{253, 245, 230}},
    CName{"olive", RGB{128, 128, 0}},
    CName{"olivedrab", RGB{107, 142, 35}},
    CName{"orange", RGB{255, 165, 0}},
    CName{"orangered", RGB{255, 69, 0}},
    CName{"orchid", RGB{218, 112, 214}},
    CName{"palegoldenrod", RGB{238, 232, 170}},
    CName{"palegreen", RGB{152, 251, 152}},
    CName{"paleturquoise", RGB{175, 238, 238}},
    CName{"palevioletred", RGB{219, 112, 147}},
    CName{"papayawhip", RGB{255, 239, 213}},
    CName{"peachpuff", RGB{255, 218, 185}},
    CName{"peru", RGB{205, 133, 63}},
    CName{"pink", RGB{255, 192, 203}},
    CName{"plum", RGB{221, 160, 221}},
    CName{"powderblue", RGB{176, 224, 230}},
    CName{"purple", RGB{128, 0, 128}},
    CName{"red", RGB{255, 0, 0}},
    CName{"rosybrown", RGB{188, 143, 143}},
    CName{"royalblue", RGB{65, 105, 225}},
    CName{"saddlebrown", RGB{139, 69, 19}},
    CName{"salmon", RGB{250, 128, 114}},
    CName{"sandybrown", RGB{244, 164, 96}},
    CName{"seagreen", RGB{46, 139, 87}},
    CName{"seashell", RGB{255, 245, 238}},
    CName{"sienna", RGB{160, 82, 45}},
    CName{"silver", RGB{192, 192, 192}},
    CName{"skyblue", RGB{135, 206, 235}},
    CName{"slateblue", RGB{106, 90, 205}},
    CName{"slategray", RGB{112, 128, 144}},
    CName{"slategrey", RGB{112, 128, 144}},
    CName{"snow", RGB{255, 250, 250}},
    CName{"springgreen", RGB{0, 255, 127}},
    CName{"steelblue", RGB{70, 130, 180}},
    CName{"tan", RGB{210, 180, 140}},
    CName{"teal", RGB{0, 128, 128}},
    CName{"thistle", RGB{216, 191, 216}},
    CName{"tomato", RGB{255, 99, 71}},
    CName{"turquoise", RGB{64, 224, 208}},
    CName{"violet", RGB{238, 130, 238}},
    CName{"wheat", RGB{245, 222, 179}},
    CName{"white", RGB{255, 255, 255}},
    CName{"whitesmoke", RGB{245, 245, 245}},
    CName{"yellow", RGB{255, 255, 0}},
    CName{"yellowgreen", RGB{154, 205, 50}}
  ]

def white() -> RGB:
  RGB{255, 255, 255}

def name.find(+s: String, ns: List<&2, CName>) -> Maybe<&2, RGB>:
  match ns:
    case Nil{}:
      None{}
    case Con{CName{+n, c}, rest}:
      H.Bool.if(Maybe<&2, RGB>, String.eq(n, s), _ => Some{c}, _ => name.find(s, rest))

# two hex digits, and what follows them
type HexPair is Data:
  HexPair{v: U32, rest: String}

# readPair: the first two characters, both hex digits
def read.pair(s: String) -> Maybe<&2, HexPair>:
  match s:
    case SCon{+a, SCon{+b, rest}}:
      +ua = Char.to_u32(a)
      +ub = Char.to_u32(b)
      Bool.pick(Maybe<&2, HexPair>, Bool.and(H.is.hex(ua), H.is.hex(ub)), Some{HexPair{U32.add(U32.mul(H.digit.val(ua), 16), H.digit.val(ub)), rest}}, None{})
    case _:
      None{}

# the colour of three pairs, and what is left
type Read3 is Data:
  Read3{rgb: RGB, rest: String}

def read.b(+r: U32, +g: U32, m: Maybe<&2, HexPair>) -> Maybe<&2, Read3>:
  match m:
    case Some{HexPair{b, rest}}:
      Some{Read3{RGB{r, g, b}, rest}}
    case None{}:
      None{}

def read.g(+r: U32, m: Maybe<&2, HexPair>) -> Maybe<&2, Read3>:
  match m:
    case Some{HexPair{+g, rest}}:
      read.b(r, g, read.pair(rest))
    case None{}:
      None{}

def read.r(m: Maybe<&2, HexPair>) -> Maybe<&2, Read3>:
  match m:
    case Some{HexPair{+r, rest}}:
      read.g(r, read.pair(rest))
    case None{}:
      None{}

# sRGB24reads: a leading '#' dropped, then three pairs
def srgb24.reads(+s: String) -> Maybe<&2, Read3>:
  match s:
    case SNil{}:
      None{}
    case SCon{+c, +rest}:
      read.r(read.pair(Bool.pick(String, H.c.is(c, 35), rest, s)))

def all.space(s: String) -> Bool:
  match s:
    case SNil{}:
      True{}
    case SCon{c, rest}:
      Bool.and(H.c.is.space(c), all.space(rest))

def zeros.then.space(s: String) -> Bool:
  match s:
    case SNil{}:
      True{}
    case SCon{+c, +rest}:
      Bool.pick(Bool, H.c.is(c, 48), zeros.then.space(rest), all.space(SCon{c, rest}))

def hex.then.space(s: String) -> Bool:
  match s:
    case SNil{}:
      True{}
    case SCon{+c, +rest}:
      Bool.pick(Bool, H.is.hex(Char.to_u32(c)), hex.then.space(rest), all.space(SCon{c, rest}))

# read ("0x" ++ rest) :: Double, for a rest that is not empty: hex digits
# (at least one) then only spaces; 0 when they are all 0
def alpha.of(+rest: String) -> Alpha:
  match rest:
    case SNil{}:
      Opaque{}
    case SCon{+c, _}:
      H.Bool.if(Alpha, Bool.and(H.is.hex(Char.to_u32(c)), hex.then.space(rest)), _ => Bool.pick(Alpha, zeros.then.space(rest), Clear{}, Opaque{}), _ => AlphaBad{})

def read.hex.colour(m: Maybe<&2, Read3>) -> Colour:
  match m:
    case Some{Read3{c, rest}}:
      Colour{c, alpha.of(rest)}
    case None{}:
      Colour{white(), Opaque{}}

def read.named(m: Maybe<&2, RGB>, +s: String) -> Colour:
  match m:
    case Some{c}:
      Colour{c, Opaque{}}
    case None{}:
      read.hex.colour(srgb24.reads(s))

# readColourName
def read(+s: String) -> Colour:
  read.named(name.find(s, names()), s)
