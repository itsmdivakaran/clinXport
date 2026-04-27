# Render clinXport hex sticker to high-res PNG
# Requires: rsvg (install with install.packages("rsvg"))

svg_path <- here::here("man/figures/logo.svg")
png_path <- here::here("man/figures/logo.png")

if (!requireNamespace("rsvg", quietly = TRUE)) {
  install.packages("rsvg")
}

# Standard hex sticker: 181 x 209 px at 72 dpi → render at 4x = 724 x 836
rsvg::rsvg_png(
  svg  = svg_path,
  file = png_path,
  width  = 724,
  height = 836
)

message("Hex sticker written to: ", png_path)
