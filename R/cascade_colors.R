## Vendored from cascade (https://github.com/adafede/cascade)
## Maintainer: Adriano Rutz <adafede@gmail.com>
##
## Color palettes used by prepare_plot() for phytochemical classification
## visualization. Vendored from cascade to avoid pulling in the full
## cascade dependency tree.
##
## Original file: R/colors.R in adafede/cascade

cvd_green <- c("#4E7705", "#6D9F06", "#97CE2F", "#BDEC6F", "#DDFFA0")
cvd_orange <- c("#9D654C", "#C17754", "#F09163", "#FCB076", "#FFD5AF")
cvd_blue <- c("#098BD9", "#56B4E9", "#7DCCFF", "#BCE1FF", "#E7F4FF")
cvd_turquoise <- c("#148F77", "#009E73", "#43BA8F", "#48C9B0", "#A3E4D7")
cvd_purple <- c("#7D3560", "#A1527F", "#CC79A7", "#E794C1", "#EFB6D6")
cvd_gray <- c("#616161", "#8B8B8B", "#B7B7B7", "#D6D6D6", "#F5F5F5")
purple <- c("#6a51a3", "#807dba", "#9e9ac8", "#bcbddc", "#dadaeb")
orange <- c("#ff7f00", "#fe9929", "#fdae6b", "#fec44f", "#feeda0")
blue <- c("#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#eff3ff")
green <- c("#238b45", "#41ab5d", "#74c476", "#a1d99b", "#c7e9c0")
brown <- c("#7D3200", "#9E5C00", "#B78560", "#CAA995", "#D8C7BE")
gray <- c("#525252", "#737373", "#969696", "#bdbdbd", "#d9d9d9")

microshades <- list(
  cvd_green,
  cvd_orange,
  cvd_blue,
  cvd_turquoise,
  cvd_purple,
  purple,
  orange,
  blue,
  green,
  brown
)
microshades_grey <- list(cvd_gray, gray)
