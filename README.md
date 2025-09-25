

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

**EthnoBotaPhyWR** stands for *ethnobotany*, *phytochemistry*,
*Wikidata* and *R* <span class="smallcaps">(maybe you can also find the
*bot* of *robot*)</span> It should be pronounced “Ethnobota Fever”.

## Requirements

Here is what you *minimally* need:

- The scientific name of the taxonomic family you are interested in

Additionally, you can provide:

- Your own list of chemical compounds found in subtaxa of the family
- Your own list of ethnobotanical uses of subtaxa of the family

## Installation

As the package is not (yet) available on CRAN, you will need to install
with:

``` r
install.packages(
  "EthnoBotaPhyWR",
  repos = c(
    "https://adafede.r-universe.dev",
    "https://bioc.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

Once installed, you are ready to go through our
[documentation](https://adafede.github.io/EthnoBotaPhyWR/articles), with
the major steps detailed.

## Main Citations

### LOTUS

<https://doi.org/10.7554/eLife.70780>

### Open Tree Taxonomy

<https://doi.org/10.3897/BDJ.5.e12581>

### NPClassifier

<https://doi.org/10.1021/acs.jnatprod.1c00399>

## Additional software credits

| Package | Version | Citation |
|:---|:---|:---|
| ape | 5.8.1 | Paradis and Schliep (2019) |
| base | 4.5.1 | R Core Team (2025) |
| BiocManager | 1.30.26 | Morgan and Ramos (2025) |
| BiocVersion | 3.22.0 | Morgan (2025) |
| cachem | 1.1.0 | Chang (2024) |
| cascade | 0.0.0.9001 | Rutz and Wolfender (2023); Rutz (2025a) |
| EthnoBotaPhyWR | 0.0.0.9000 | Rutz (2025b) |
| ggpubr | 0.6.1 | Kassambara (2025) |
| ggtext | 0.1.2 | Wilke and Wiernik (2022) |
| ggtree | 3.17.1 | Yu et al. (2017); Yu et al. (2018); Yu (2020); Yu (2022a); Xu et al. (2022) |
| ggtreeExtra | 1.19.0 | Xu et al. (2021); Yu (2022b) |
| gt | 1.1.0 | Iannone et al. (2025) |
| httr2 | 1.2.1 | Wickham (2025) |
| khroma | 1.16.0 | Frerebeau (2025) |
| knitr | 1.50 | Xie (2014); Xie (2015); Xie (2025) |
| logger | 0.4.1 | Daróczi and Wickham (2025) |
| memoise | 2.0.1 | Wickham et al. (2021) |
| phangorn | 2.12.1 | Schliep (2011); Schliep et al. (2017) |
| R.utils | 2.13.0 | Bengtsson (2025) |
| rmarkdown | 2.29 | Xie, Allaire, and Grolemund (2018); Xie, Dervieux, and Riederer (2020); Allaire et al. (2024) |
| rotl | 3.1.0 | Michonneau, Brown, and Winter (2016); OpenTreeOfLife et al. (2019) |
| scales | 1.4.0 | Wickham, Pedersen, and Seidel (2025) |
| stringi | 1.8.7 | Gagolewski (2022) |
| tidytable | 0.11.2 | Fairbanks (2024) |
| tidyverse | 2.0.0 | Wickham et al. (2019) |
| WikidataQueryServiceR | 1.0.0 | Popov (2020) |

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-rmarkdown2024" class="csl-entry">

Allaire, JJ, Yihui Xie, Christophe Dervieux, Jonathan McPherson, Javier
Luraschi, Kevin Ushey, Aron Atkins, et al. 2024.
*<span class="nocase">rmarkdown</span>: Dynamic Documents for r*.
<https://github.com/rstudio/rmarkdown>.

</div>

<div id="ref-Rutils" class="csl-entry">

Bengtsson, Henrik. 2025. *<span class="nocase">R.utils</span>: Various
Programming Utilities*. <https://doi.org/10.32614/CRAN.package.R.utils>.

</div>

<div id="ref-cachem" class="csl-entry">

Chang, Winston. 2024. *<span class="nocase">cachem</span>: Cache r
Objects with Automatic Pruning*.
<https://doi.org/10.32614/CRAN.package.cachem>.

</div>

<div id="ref-logger" class="csl-entry">

Daróczi, Gergely, and Hadley Wickham. 2025.
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
for Scientific Data Visualization</span>*. Pessac, France: Université
Bordeaux Montaigne. <https://doi.org/10.5281/zenodo.1472077>.

</div>

<div id="ref-stringi" class="csl-entry">

Gagolewski, Marek. 2022. “<span class="nocase">stringi</span>: Fast and
Portable Character String Processing in R.” *Journal of Statistical
Software* 103 (2): 1–59. <https://doi.org/10.18637/jss.v103.i02>.

</div>

<div id="ref-gt" class="csl-entry">

Iannone, Richard, Joe Cheng, Barret Schloerke, Ellis Hughes, Alexandra
Lauer, JooYoung Seo, Ken Brevoort, and Olivier Roy. 2025.
*<span class="nocase">gt</span>: Easily Create Presentation-Ready
Display Tables*. <https://doi.org/10.32614/CRAN.package.gt>.

</div>

<div id="ref-ggpubr" class="csl-entry">

Kassambara, Alboukadel. 2025. *<span class="nocase">ggpubr</span>:
“<span class="nocase">ggplot2</span>” Based Publication Ready Plots*.
<https://doi.org/10.32614/CRAN.package.ggpubr>.

</div>

<div id="ref-rotl2016" class="csl-entry">

Michonneau, Francois, Joseph W. Brown, and David J. Winter. 2016.
“<span class="nocase">rotl</span>: An r Package to Interact with the
Open Tree of Life Data.” *Methods in Ecology and Evolution* 7 (12):
1476–81. <https://doi.org/10.1111/2041-210X.12593>.

</div>

<div id="ref-BiocVersion" class="csl-entry">

Morgan, Martin. 2025. *BiocVersion: Set the Appropriate Version of
Bioconductor Packages*. <https://doi.org/10.18129/B9.bioc.BiocVersion>.

</div>

<div id="ref-BiocManager" class="csl-entry">

Morgan, Martin, and Marcel Ramos. 2025. *BiocManager: Access the
Bioconductor Project Package Repository*.
<https://doi.org/10.32614/CRAN.package.BiocManager>.

</div>

<div id="ref-rotl2019" class="csl-entry">

OpenTreeOfLife, Benjamin Redelings, Luna Luisa Sanchez Reyes, Karen A.
Cranston, Jim Allman, Mark T. Holder, and Emily Jane McTavish. 2019.
“Open Tree of Life Synthetic Tree.” Zenodo.
<https://doi.org/10.5281/zenodo.3937741>.

</div>

<div id="ref-ape" class="csl-entry">

Paradis, Emmanuel, and Klaus Schliep. 2019. “Ape 5.0: An Environment for
Modern Phylogenetics and Evolutionary Analyses in R.” *Bioinformatics*
35: 526–28. <https://doi.org/10.1093/bioinformatics/bty633>.

</div>

<div id="ref-WikidataQueryServiceR" class="csl-entry">

Popov, Mikhail. 2020. *WikidataQueryServiceR: API Client Library for
“Wikidata Query Service”*.
<https://doi.org/10.32614/CRAN.package.WikidataQueryServiceR>.

</div>

<div id="ref-base" class="csl-entry">

R Core Team. 2025. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-cascade2025" class="csl-entry">

Rutz, Adriano. 2025a. *<span class="nocase">cascade</span>:
Contextualizing Untargeted Annotation with Semi-Quantitative Charged
Aerosol Detection for Pertinent Characterization of Natural Extracts*.

</div>

<div id="ref-EthnoBotaPhyWR" class="csl-entry">

———. 2025b. *EthnoBotaPhyWR: \*\*EthnoBotaPhyWR\*\* Stands for
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

<div id="ref-httr2" class="csl-entry">

Wickham, Hadley. 2025. *Httr2: Perform HTTP Requests and Process the
Responses*. <https://doi.org/10.32614/CRAN.package.httr2>.

</div>

<div id="ref-tidyverse" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the <span class="nocase">tidyverse</span>.” *Journal of Open
Source Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

</div>

<div id="ref-memoise" class="csl-entry">

Wickham, Hadley, Jim Hester, Winston Chang, Kirill Müller, and Daniel
Cook. 2021. *<span class="nocase">memoise</span>: “Memoisation” of
Functions*. <https://doi.org/10.32614/CRAN.package.memoise>.

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

———. 2015. *Dynamic Documents with R and Knitr*. 2nd ed. Boca Raton,
Florida: Chapman; Hall/CRC. <https://yihui.org/knitr/>.

</div>

<div id="ref-knitr2025" class="csl-entry">

———. 2025. *<span class="nocase">knitr</span>: A General-Purpose Package
for Dynamic Report Generation in R*. <https://yihui.org/knitr/>.

</div>

<div id="ref-rmarkdown2018" class="csl-entry">

Xie, Yihui, J. J. Allaire, and Garrett Grolemund. 2018. *R Markdown: The
Definitive Guide*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown>.

</div>

<div id="ref-rmarkdown2020" class="csl-entry">

Xie, Yihui, Christophe Dervieux, and Emily Riederer. 2020. *R Markdown
Cookbook*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown-cookbook>.

</div>

<div id="ref-ggtreeExtra2021" class="csl-entry">

Xu, Shuangbin, Zehan Dai, Pingfan Guo, Xiaocong Fu, Shanshan Liu, Lang
Zhou, Wenli Tang, et al. 2021. “<span class="nocase">ggtreeExtra</span>:
Compact Visualization of Richly Annotated Phylogenetic Data.” *Molecular
Biology and Evolution* 38 (June): 4039–42.
<https://doi.org/10.1093/molbev/msab166>.

</div>

<div id="ref-ggtree2022b" class="csl-entry">

Xu, Shuangbin, Lin Li, Xiao Luo, Meijun Chen, Wenli Tang, Li Zhan, Zehan
Dai, Tommy T. Lam, Yi Guan, and Guangchuang Yu. 2022. “Ggtree: A
Serialized Data Object for Visualization of a Phylogenetic Tree and
Annotation Data.” *iMeta* 1 (4): e56. <https://doi.org/10.1002/imt2.56>.

</div>

<div id="ref-ggtree2020c" class="csl-entry">

Yu, Guangchuang. 2020. “Using Ggtree to Visualize Data on Tree-Like
Structures.” *Current Protocols in Bioinformatics* 69 (1): e96.
<https://doi.org/10.1002/cpbi.96>.

</div>

<div id="ref-ggtree2022a" class="csl-entry">

———. 2022a. *Data Integration, Manipulation and Visualization of
Phylogenetic Treess*. 1st edition. Chapman; Hall/CRC.
<https://doi.org/10.1201/9781003279242>.

</div>

<div id="ref-ggtreeExtra2022" class="csl-entry">

———. 2022b. *Data Integration, Manipulation and Visualization of
Phylogenetic Treess*. 1st edition. Chapman; Hall/CRC.
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
