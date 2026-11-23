# colour-names.awk: Colour.bend's names() from the colour package's
# Data/Colour/Names.hs (colour-2.3.7): each readColourName "NAME" in its
# order, at the NAME = sRGB24 R G B its value is (black is
# Data.Colour.Internal's, 0 0 0).
#   awk -f tests/gen/colour-names.awk colour-2.3.7/Data/Colour/Names.hs
/^readColourName "/ { gsub(/"/, "", $2); order[++c] = $2; n[$2] = $5 }
/^[a-z]+ = sRGB24 / { v[$1] = $4 ", " $5 ", " $6 }
END {
  v["black"] = "0, 0, 0"
  for (i = 1; i <= c; i++) {
    k = order[i]
    printf "    Named{\"%s\", RGB{%s}}%s\n", k, v[n[k]], (i < c ? "," : "")
  }
}
