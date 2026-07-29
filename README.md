<!-- README.md is generated from README.qmd. Please edit that file -->

# EthnoBotaPhyWR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![No Maintenance
Intended](http://unmaintained.tech/badge.svg)](http://unmaintained.tech/)
[![CRAN
status](https://www.r-pkg.org/badges/version/tima.png)](https://CRAN.R-project.org/package=EthnoBotaPhyWR)
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
  uses)
- **Phylogenetic integration** via Open Tree of Life
- **Phytochemical analysis** with NPClassifier taxonomy
- **Data federation** from Wikidata, LOTUS, and external sources

## Installation

As the package is not (yet) available on CRAN, you will need to install with:

```r
install.packages(
  "EthnoBotaPhyWR",
  repos = c(
    "https://adafede.r-universe.dev",
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
```

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
  | :------------- | :--------- | :-------------------------------------------------------------------------- |
  | ape            | 5.8.1      | Paradis and Schliep (2019)                                                  |
  | base           | 4.6.1      | R Core Team (2026a)                                                         |
  | BiocManager    | 1.30.27    | Morgan and Ramos (2025)                                                     |
  | BiocVersion    | 3.23.1     | Morgan (2025)                                                               |
  | cascade        | 0.0.0.9002 | Rutz and Wolfender (2023); Rutz (2026a)                                     |
  | EthnoBotaPhyWR | 0.0.0.9001 | Rutz (2026b)                                                                |
  | fastmatch      | 1.1.8      | Urbanek (2026)                                                              |
  | ggpubr         | 1.0.0      | Kassambara (2026)                                                           |
  | ggtext         | 0.1.2      | Wilke and Wiernik (2022)                                                    |
  | ggtree         | 4.2.0      | Yu et al. (2017); Yu et al. (2018); Yu (2020); Yu (2022a); Xu et al. (2022) |
  | ggtreeExtra    | 1.22.1     | Xu et al. (2021); Yu (2022b)                                                |
  | grid           | 4.6.1      | R Core Team (2026b)                                                         |
  | gt             | 1.3.0      | Iannone et al. (2026)                                                       |
  | httr2          | 1.3.0      | Wickham (2026)                                                              |
  | khroma         | 1.17.0     | Frerebeau (2025)                                                            |
  | knitr          | 1.51       | Xie (2014); Xie (2015); Xie (2025)                                          |
  | logger         | 0.4.2      | Daróczi and Wickham (2026)                                                  |
  | openssl        | 2.4.2      | Ooms (2026)                                                                 |
  | phangorn       | 2.12.1     | Schliep (2011); Schliep et al. (2017)                                       |
  | quarto         | 1.5.1      | Allaire and Dervieux (2025)                                                 |
  | R.utils        | 2.13.0     | Bengtsson (2025)                                                            |
  | rmarkdown      | 2.31       | Xie et al. (2018); Xie et al. (2020); Allaire et al. (2026)                 |
  | rotl           | 3.1.1      | Michonneau et al. (2016); OpenTreeOfLife et al. (2019)                      |
  | scales         | 1.4.0      | Wickham et al. (2025)                                                       |
  | spelling       | 2.3.2      | Ooms and Hester (2025)                                                      |
  | stringi        | 1.8.7      | Gagolewski (2022)                                                           |
  | tidytable      | 0.11.2     | Fairbanks (2024)                                                            |
  | tidyverse      | 2.0.0      | Wickham et al. (2019)                                                       |
  | tools          | 4.6.1      | R Core Team (2026c)                                                         |

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-quarto" class="csl-entry">

Allaire, JJ, and Christophe Dervieux. 2025.
*<span class="nocase">quarto</span>: R Interface to “Quarto” Markdown
Publishing System*. <https://doi.org/10.32614/CRAN.package.quarto>.

</div>

<div id="ref-rmarkdown2026" class="csl-entry">

Allaire, JJ, Yihui Xie, Christophe Dervieux, et al. 2026.
*<span class="nocase">rmarkdown</span>: Dynamic Documents for r*.
<https://github.com/rstudio/rmarkdown>.

</div>

<div id="ref-Rutils" class="csl-entry">

Bengtsson, Henrik. 2025. *<span class="nocase">R.utils</span>: Various
Programming Utilities*. <https://doi.org/10.32614/CRAN.package.R.utils>.

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

<div id="ref-stringi" class="csl-entry">

Gagolewski, Marek. 2022. “<span class="nocase">stringi</span>: Fast and
Portable Character String Processing in R.” *Journal of Statistical
Software* 103 (2): 1–59. <https://doi.org/10.18637/jss.v103.i02>.

</div>

<div id="ref-gt" class="csl-entry">

Iannone, Richard, Joe Cheng, Barret Schloerke, et al. 2026.
*<span class="nocase">gt</span>: Easily Create Presentation-Ready
Display Tables*. <https://doi.org/10.32614/CRAN.package.gt>.

</div>

<div id="ref-ggpubr" class="csl-entry">

Kassambara, Alboukadel. 2026. *<span class="nocase">ggpubr</span>:
“<span class="nocase">ggplot2</span>” Based Publication Ready Plots*.
<https://CRAN.R-project.org/package=ggpubr>.

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

<div id="ref-openssl" class="csl-entry">

Ooms, Jeroen. 2026. *<span class="nocase">openssl</span>: Toolkit for
Encryption, Signatures and Certificates Based on OpenSSL*.
<https://doi.org/10.32614/CRAN.package.openssl>.

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

<div id="ref-cascade2026" class="csl-entry">

Rutz, Adriano. 2026a. *<span class="nocase">cascade</span>:
Contextualizing Untargeted Annotation with Semi-Quantitative Charged
Aerosol Detection for Pertinent Characterization of Natural Extracts*.

</div>

<div id="ref-EthnoBotaPhyWR" class="csl-entry">

Rutz, Adriano. 2026b. *EthnoBotaPhyWR: \*\*EthnoBotaPhyWR\*\* Stands for
\*Ethnobotany\*, \*Phytochemistry\*, \*Wikidata\* and \*r\*. It Should
Be Pronounced “Ethnobota Fever.”*
<https://github.com/adafede/EthnoBotaPhyWR>.

</div>

<div id="ref-cascade2023" class="csl-entry">

Rutz, Adriano, and Jean-Luc Wolfender. 2023. “Automated Composition
Assessment of Natural Extracts: Untargeted Mass Spectrometry-Based
Metabolite Profiling Integrating Semiquantitative Detection.” *Journal
of Agricultural and Food Chemistry* 71 (46).
<https://doi.org/10.1021/acs.jafc.3c03099>.

</div>

<div id="ref-phangorn2011" class="csl-entry">

Schliep, Klaus. 2011. “<span class="nocase">phangorn</span>:
Phylogenetic Analysis in r.” *Bioinformatics* 27 (4): 592–93.
<https://doi.org/10.1093/bioinformatics/btq706>.

</div>

<div id="ref-phangorn2017" class="csl-entry">

Schliep, Klaus, Alastair J. Potts, David A. Morrison, and Guido W.
Grimm. 2017. “Intertwining Phylogenetic Trees and Networks.” *Methods in
Ecology and Evolution* 8 (10): 1212–20.

</div>

<div id="ref-fastmatch" class="csl-entry">

Urbanek, Simon. 2026. *<span class="nocase">fastmatch</span>: Fast
“<span class="nocase">match()</span>” Function*.
<https://doi.org/10.32614/CRAN.package.fastmatch>.

</div>

<div id="ref-httr2" class="csl-entry">

Wickham, Hadley. 2026. *Httr2: Perform HTTP Requests and Process the
Responses*. <https://doi.org/10.32614/CRAN.package.httr2>.

</div>

<div id="ref-tidyverse" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, et al. 2019. “Welcome to
the <span class="nocase">tidyverse</span>.” *Journal of Open Source
Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

</div>

<div id="ref-scales" class="csl-entry">

Wickham, Hadley, Thomas Lin Pedersen, and Dana Seidel. 2025.
*<span class="nocase">scales</span>: Scale Functions for Visualization*.
<https://doi.org/10.32614/CRAN.package.scales>.

</div>

<div id="ref-ggtext" class="csl-entry">

Wilke, Claus O., and Brenton M. Wiernik. 2022.
*<span class="nocase">ggtext</span>: Improved Text Rendering Support for
“<span class="nocase">ggplot2</span>”*.
<https://doi.org/10.32614/CRAN.package.ggtext>.

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

<div id="ref-knitr2025" class="csl-entry">

Xie, Yihui. 2025. *<span class="nocase">knitr</span>: A General-Purpose
Package for Dynamic Report Generation in R*. <https://yihui.org/knitr/>.

</div>

<div id="ref-rmarkdown2018" class="csl-entry">

Xie, Yihui, J. J. Allaire, and Garrett Grolemund. 2018. *R Markdown: The
Definitive Guide*. Chapman; Hall/CRC. <https://yihui.org/rmarkdown/>.

</div>

<div id="ref-rmarkdown2020" class="csl-entry">

Xie, Yihui, Christophe Dervieux, and Emily Riederer. 2020. *R Markdown
Cookbook*. Chapman; Hall/CRC. <https://yihui.org/rmarkdown-cookbook>.

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
