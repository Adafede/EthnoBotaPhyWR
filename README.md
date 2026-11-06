<!-- README.md is generated from README.qmd. Please edit that file -->

# EthnoBotaPhyWR <img src='https://raw.githubusercontent.com/adafede/EthnoBotaPhyWR/main/man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![No Maintenance
Intended](http://unmaintained.tech/badge.svg)](http://unmaintained.tech/)
[![R-CMD-check](https://github.com/adafede/EthnoBotaPhyWR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adafede/EthnoBotaPhyWR/actions/workflows/R-CMD-check.yaml)
[![r-universe
badge](https://adafede.r-universe.dev/EthnoBotaPhyWR/badges/version.png)](https://adafede.r-universe.dev/EthnoBotaPhyWR)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.TODO.svg)](https://doi.org/10.5281/zenodo.TODO)
<!-- badges: end -->

**EthnoBotaPhyWR** stands for *ethnobotany*, *phytochemistry*, *Wikidata* and
*R*
<span class="smallcaps">(maybe you can also find the
*bot* of *robot*)</span> It
should be pronounced "Ethnobota Fever".

## Requirements

Here is what you *minimally* need:

- The scientific name of the taxonomic family you are interested in

Additionally, you can provide:

- Your own list of chemical compounds found in subtaxa of the family
- Your own list of ethnobotanical uses of subtaxa of the family

Or simply use the included orchids datasets for analysis.

## Features

**EthnoBotaPhyWR** provides:

- **Built-in datasets** for *Orchidaceae* (3,055 compounds + 419 ethnobotanical
  uses + pre-computed Open Tree of Life `phylo` caches)
- **Bundled Newick & iTOL v6 files** (`inst/extdata/itol/`) for immediate
  interactive exploration on [iTOL](https://itol.embl.de)
- **Publication & interactive tree bundles**: every run exports vector `.svg`,
  journal-ready vector `.pdf`, and self-contained interactive `.html` (`ggiraph`
  with pan/zoom, hover tooltips, and a live taxon search bar), including both a
  clade-collapsed rectangular overview (`▲` for unstudied sister clades) and an
  uncollapsed circular (`fan`) supplementary overview
- **Log-scaled compound bars & hierarchical legend**: bar widths scale by
  $\log(1 + n)$ with exact compound counts printed at each bar tip and a
  6-column hierarchical pathway/superclass legend (`[Pathway]` + 5 `microshades`
  superclasses per row)
- **Phylogenetic integration** via Open Tree of Life (`rotl`)
- **Phytochemical analysis** with NPClassifier taxonomy
- **Data federation** from Wikidata (QLever), LOTUS, and local tables

## Installation

As the package is not (yet) available on CRAN, you will need to install with:

```r
install.packages(
  "EthnoBotaPhyWR",
  repos = c(
    "https://adafede.r-universe.dev",
    "https://davidgohel.r-universe.dev", # required for ggiraph, required for ggtree
    "https://cran.r-universe.dev",
    "https://bioc.r-universe.dev"
  )
)
```

Once installed, you are ready to go through our
[documentation](https://adafede.github.io/EthnoBotaPhyWR/articles), with the
major steps detailed.

## Quick Start

### Load included datasets

```r
library(EthnoBotaPhyWR)

# Phytochemical data with SMILES and classification
data(orchids_compounds)
head(orchids_compounds)

# NPClassifier taxonomy lookup
data(orchids_compound_classes)
head(orchids_compound_classes)

# Ethnobotanical uses linked to phylogenetics
data(orchids_ethnobotanical)
head(orchids_ethnobotanical)

# Bundled Newick (.tre) and iTOL v6 annotation (.txt) files
itol_dir <- system.file("extdata", "itol", package = "EthnoBotaPhyWR")
list.files(itol_dir)
```

### Interactive Exploration on iTOL (Interactive Tree Of Life)

Both the pre-bundled *Orchidaceae* files
(`system.file("extdata", "itol", package = "EthnoBotaPhyWR")`) and the files
generated in `<dirname(export_table_counts)>/itol/` when running
`ethnobotafever_tree()` on any plant family can be explored on
[iTOL](https://itol.embl.de):

1. Open <https://itol.embl.de/upload.cgi> and upload one of the `.tre` Newick
   files:
   - `<Taxon>_full_genera_tree.tre` (all genera in the family)
   - `<Taxon>_phyto_genera_tree.tre` (genera with reported compounds or uses)
   - `<Taxon>_phyto_species_tree.tre` (species-level tree for major genera)
2. Once the tree opens in the iTOL viewer, **drag and drop** the three companion
   `.txt` annotation files directly onto the browser canvas:
   - `*_itol_ethnobotany.txt` (`DATASET_GRADIENT`: $\log_{10}$ ethnobotanical
     species count)
   - `*_itol_species_richness.txt` (`DATASET_SIMPLEBAR`: known species count per
     taxon)
   - `*_itol_compounds_multibar.txt` (`DATASET_MULTIBAR`: stacked compound
     superclass counts colored with the exact `cascade`/`microshades` palette)
3. Use iTOL's built-in controls to search taxa, dynamically collapse or expand
   clades, re-root, and switch between rectangular, circular, and unrooted
   layouts.

### Analyze your own family

```r
# Get data for any plant family (requires Wikidata)
result <- ethnobotafever_tree(
  taxon_name = "Orchidaceae",
  n_multiple_genera = 1,
  n_ethnobotanical_count_min = 0,
  n_compounds_count_min = 0,
  example_family_names = c("Solanaceae", "Fabaceae"),
  path_molecules = NULL,  # or path to your own
  path_classes = NULL,
  path_ethnobotanical = NULL,
  export_tree_full = "~/orchids_tree_full.pdf",
  export_tree_phyto = "~/orchids_tree_phyto.pdf",
  export_tree_phyto_sub = "~/orchids_tree_phyto_sub.pdf",
  export_table_classified = "~/orchids_classified.tsv",
  export_table_counts = "~/orchids_counts.tsv",
  export_table_counts_sub = "~/orchids_counts_sub.tsv",
  local_cache = "~/.ethnobotaphy_cache",
  log_dir = "~/"
)
```

## Main Citations

### LOTUS

<https://doi.org/10.7554/eLife.70780>

### Open Tree Taxonomy

<https://doi.org/10.3897/BDJ.5.e12581>

### NPClassifier

<https://doi.org/10.1021/acs.jnatprod.1c00399>

## Additional software credits

  | Package        | Version    | Citation                                                                    |
  | :---           | :---       | :---                                                                        |
  | ape            | 5.8.1      | Paradis and Schliep (2019)                                                  |
  | base           | 4.6.2      | R Core Team (2026a)                                                         |
  | BiocManager    | 1.30.27    | Morgan and Ramos (2025)                                                     |
  | BiocVersion    | 3.23.1     | Morgan (2025)                                                               |
  | curl           | 8.1.0      | Ooms (2026)                                                                 |
  | data.table     | 1.18.6.1   | Barrett et al. (2026)                                                       |
  | EthnoBotaPhyWR | 0.0.0.9001 | Rutz (2026)                                                                 |
  | ggiraph        | 0.9.6      | Gohel and Skintzos (2026)                                                   |
  | ggtree         | 4.2.0      | Yu et al. (2017); Yu et al. (2018); Yu (2020); Yu (2022a); Xu et al. (2022) |
  | ggtreeExtra    | 1.22.1     | Xu et al. (2021); Yu (2022b)                                                |
  | grid           | 4.6.2      | R Core Team (2026b)                                                         |
  | gt             | 1.4.0      | Iannone et al. (2026)                                                       |
  | htmlwidgets    | 1.6.4      | Vaidyanathan et al. (2023)                                                  |
  | khroma         | 1.17.0     | Frerebeau (2025)                                                            |
  | knitr          | 1.52       | Xie (2014); Xie (2015); Xie (2026)                                          |
  | logger         | 0.4.3      | Daróczi and Wickham (2026)                                                  |
  | rmarkdown      | 2.32       | Xie et al. (2018); Xie et al. (2020); Allaire et al. (2026)                 |
  | rotl           | 3.1.1      | Michonneau et al. (2016); OpenTreeOfLife et al. (2019)                      |
  | spelling       | 2.3.2      | Ooms and Hester (2025)                                                      |
  | testthat       | 3.3.2      | Wickham (2011)                                                              |
  | tidytable      | 0.11.2     | Fairbanks (2024)                                                            |
  | tidyverse      | 2.0.0      | Wickham et al. (2019)                                                       |
  | tools          | 4.6.2      | R Core Team (2026c)                                                         |

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-rmarkdown2026" class="csl-entry">

Allaire, JJ, Yihui Xie, Christophe Dervieux, et al. 2026.
*<span class="nocase">rmarkdown</span>: Dynamic Documents for r*.
<https://github.com/rstudio/rmarkdown>.

</div>

<div id="ref-datatable" class="csl-entry">

Barrett, Tyson, Matt Dowle, Arun Srinivasan, et al. 2026.
*<span class="nocase">data.table</span>: Extension of
“<span class="nocase">data.frame</span>”*.
<https://doi.org/10.32614/CRAN.package.data.table>.

</div>

<div id="ref-logger" class="csl-entry">

Daróczi, Gergely, and Hadley Wickham. 2026.
*<span class="nocase">logger</span>: A Lightweight, Modern and Flexible
Logging Utility*. <https://doi.org/10.32614/CRAN.package.logger>.

</div>

<div id="ref-tidytable" class="csl-entry">

Fairbanks, Mark. 2024. *<span class="nocase">tidytable</span>: Tidy
Interface to “<span class="nocase">data.table</span>”*.
<https://doi.org/10.32614/CRAN.package.tidytable>.

</div>

<div id="ref-khroma" class="csl-entry">

Frerebeau, Nicolas. 2025. *<span class="nocase">khroma: Colour Schemes
for Scientific Data Visualization</span>*. Université Bordeaux
Montaigne. <https://doi.org/10.5281/zenodo.1472077>.

</div>

<div id="ref-ggiraph" class="csl-entry">

Gohel, David, and Panagiotis Skintzos. 2026.
*<span class="nocase">ggiraph</span>: Make
“<span class="nocase">ggplot2</span>” Graphics Interactive*.
<https://doi.org/10.32614/CRAN.package.ggiraph>.

</div>

<div id="ref-gt" class="csl-entry">

Iannone, Richard, Joe Cheng, Barret Schloerke, et al. 2026.
*<span class="nocase">gt</span>: Easily Create Presentation-Ready
Display Tables*. <https://doi.org/10.32614/CRAN.package.gt>.

</div>

<div id="ref-rotl2016" class="csl-entry">

Michonneau, Francois, Joseph W. Brown, and David J. Winter. 2016.
“<span class="nocase">rotl</span>: An r Package to Interact with the
Open Tree of Life Data.” *Methods in Ecology and Evolution* 7 (12):
1476–81. <https://doi.org/10.1111/2041-210X.12593>.

</div>

<div id="ref-BiocVersion" class="csl-entry">

Morgan, Martin. 2025. *BiocVersion: Set the Appropriate Version of
Bioconductor Packages*.

</div>

<div id="ref-BiocManager" class="csl-entry">

Morgan, Martin, and Marcel Ramos. 2025. *BiocManager: Access the
Bioconductor Project Package Repository*.
<https://doi.org/10.32614/CRAN.package.BiocManager>.

</div>

<div id="ref-curl" class="csl-entry">

Ooms, Jeroen. 2026. *<span class="nocase">curl</span>: A Modern and
Flexible Web Client for r*.
<https://doi.org/10.32614/CRAN.package.curl>.

</div>

<div id="ref-spelling" class="csl-entry">

Ooms, Jeroen, and Jim Hester. 2025.
*<span class="nocase">spelling</span>: Tools for Spell Checking in r*.
<https://doi.org/10.32614/CRAN.package.spelling>.

</div>

<div id="ref-rotl2019" class="csl-entry">

OpenTreeOfLife, Benjamin Redelings, Luna Luisa Sanchez Reyes, et al.
2019. *Open Tree of Life Synthetic Tree*. Version 12.3. Zenodo.
<https://doi.org/10.5281/zenodo.3937741>.

</div>

<div id="ref-ape" class="csl-entry">

Paradis, Emmanuel, and Klaus Schliep. 2019. “Ape 5.0: An Environment for
Modern Phylogenetics and Evolutionary Analyses in R.” *Bioinformatics*
35: 526–28. <https://doi.org/10.1093/bioinformatics/bty633>.

</div>

<div id="ref-base" class="csl-entry">

R Core Team. 2026a. *R: A Language and Environment for Statistical
Computing*. R Foundation for Statistical Computing.
<https://doi.org/10.32614/R.manuals>.

</div>

<div id="ref-grid" class="csl-entry">

R Core Team. 2026b. *R: A Language and Environment for Statistical
Computing*. R Foundation for Statistical Computing.
<https://doi.org/10.32614/R.manuals>.

</div>

<div id="ref-tools" class="csl-entry">

R Core Team. 2026c. *R: A Language and Environment for Statistical
Computing*. R Foundation for Statistical Computing.
<https://doi.org/10.32614/R.manuals>.

</div>

<div id="ref-EthnoBotaPhyWR" class="csl-entry">

Rutz, Adriano. 2026. *EthnoBotaPhyWR: Infrastructure for Ethnobotanical
and Phytochemical Data Integration with Wikidata*.
<https://doi.org/10.5281/zenodo.TODO>.

</div>

<div id="ref-htmlwidgets" class="csl-entry">

Vaidyanathan, Ramnath, Yihui Xie, JJ Allaire, Joe Cheng, Carson Sievert,
and Kenton Russell. 2023. *<span class="nocase">htmlwidgets</span>: HTML
Widgets for r*. <https://doi.org/10.32614/CRAN.package.htmlwidgets>.

</div>

<div id="ref-testthat" class="csl-entry">

Wickham, Hadley. 2011. “<span class="nocase">testthat</span>: Get
Started with Testing.” *The R Journal* 3: 5–10.
<https://journal.r-project.org/articles/RJ-2011-002/>.

</div>

<div id="ref-tidyverse" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, et al. 2019. “Welcome to
the <span class="nocase">tidyverse</span>.” *Journal of Open Source
Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

</div>

<div id="ref-knitr2014" class="csl-entry">

Xie, Yihui. 2014. “<span class="nocase">knitr</span>: A Comprehensive
Tool for Reproducible Research in R.” In *Implementing Reproducible
Computational Research*, edited by Victoria Stodden, Friedrich Leisch,
and Roger D. Peng. Chapman; Hall/CRC.

</div>

<div id="ref-knitr2015" class="csl-entry">

Xie, Yihui. 2015. *Dynamic Documents with R and Knitr*. 2nd ed. Chapman;
Hall/CRC. <https://yihui.org/knitr/>.

</div>

<div id="ref-knitr2026" class="csl-entry">

Xie, Yihui. 2026. *<span class="nocase">knitr</span>: A General-Purpose
Package for Dynamic Report Generation in R*. <https://yihui.org/knitr/>.

</div>

<div id="ref-rmarkdown2018" class="csl-entry">

Xie, Yihui, J. J. Allaire, and Garrett Grolemund. 2018. *R Markdown: The
Definitive Guide*. Chapman; Hall/CRC.
<https://pkg.yihui.org/rmarkdown-book/>.

</div>

<div id="ref-rmarkdown2020" class="csl-entry">

Xie, Yihui, Christophe Dervieux, and Emily Riederer. 2020. *R Markdown
Cookbook*. Chapman; Hall/CRC.
<https://pkg.yihui.org/rmarkdown-cookbook/>.

</div>

<div id="ref-ggtreeExtra2021" class="csl-entry">

Xu, Shuangbin, Zehan Dai, Pingfan Guo, et al. 2021.
“<span class="nocase">ggtreeExtra</span>: Compact Visualization of
Richly Annotated Phylogenetic Data.” *Molecular Biology and Evolution*
38 (June): 4039–42. <https://doi.org/10.1093/molbev/msab166>.

</div>

<div id="ref-ggtree2022b" class="csl-entry">

Xu, Shuangbin, Lin Li, Xiao Luo, et al. 2022. “Ggtree: A Serialized Data
Object for Visualization of a Phylogenetic Tree and Annotation Data.”
*iMeta* 1 (4): e56. <https://doi.org/10.1002/imt2.56>.

</div>

<div id="ref-ggtree2020c" class="csl-entry">

Yu, Guangchuang. 2020. “Using Ggtree to Visualize Data on Tree-Like
Structures.” *Current Protocols in Bioinformatics* 69 (1): e96.
<https://doi.org/10.1002/cpbi.96>.

</div>

<div id="ref-ggtree2022a" class="csl-entry">

Yu, Guangchuang. 2022a. *Data Integration, Manipulation and
Visualization of Phylogenetic Treess*. 1st edition. Chapman; Hall/CRC.
<https://doi.org/10.1201/9781003279242>.

</div>

<div id="ref-ggtreeExtra2022" class="csl-entry">

Yu, Guangchuang. 2022b. *Data Integration, Manipulation and
Visualization of Phylogenetic Treess*. 1st edition. Chapman; Hall/CRC.
<https://www.amazon.com/Integration-Manipulation-Visualization-Phylogenetic-Computational-ebook/dp/B0B5NLZR1Z/>.

</div>

<div id="ref-ggtree2018d" class="csl-entry">

Yu, Guangchuang, Tommy Tsan-Yuk Lam, Huachen Zhu, and Yi Guan. 2018.
“Two Methods for Mapping and Visualizing Associated Data on Phylogeny
Using Ggtree.” *Molecular Biology and Evolution* 35: 3041–43.
<https://doi.org/10.1093/molbev/msy194>.

</div>

<div id="ref-ggtree2017e" class="csl-entry">

Yu, Guangchuang, David Smith, Huachen Zhu, Yi Guan, and Tommy Tsan-Yuk
Lam. 2017. “<span class="nocase">ggtree</span>: An r Package for
Visualization and Annotation of Phylogenetic Trees with Their Covariates
and Other Associated Data.” *Methods in Ecology and Evolution* 8: 28–36.
<https://doi.org/10.1111/2041-210X.12628>.

</div>

</div>
