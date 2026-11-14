# Contributing to EthnoBotaPhyWR

Contributions, bug reports, and curated botanical/phytochemical data fixes are welcome via [GitHub issues](https://github.com/adafede/EthnoBotaPhyWR/issues) and pull requests.

## Development requirements

- R (>= 4.5.0) with package imports (`ape`, `curl`, `data.table`, `ggplot2`, `ggtree`, `ggtreeExtra`, `jsonlite`, `khroma`, `logger`, `rotl`, `tidytable`)
- Development packages: `roxygen2`, `testthat`, `pkgload`, `lintr`, `spelling`, `covr`, `grateful`, `knitr`, `cffr`, `codemetar`
- CLI tools: `quarto`, `air`, `jarl`, and `prek` (optional pre-commit runner)

## Local verification commands

Run these checks from the repository root before opening a pull request:

```bash
air format .
jarl check .
Rscript -e 'stopifnot(length(lintr::lint_package()) == 0L)'
Rscript -e 'stopifnot(nrow(spelling::spell_check_package()) == 0L)'
Rscript -e 'testthat::test_local()'
```

To run a full build and check from the command line:

```bash
R CMD build --no-manual .
R CMD check --no-manual --as-cran EthnoBotaPhyWR_*.tar.gz
```

## Updating documentation and datasets

- Edit `R/*.R` roxygen comments and run `Rscript -e 'roxygen2::roxygenise()'` to regenerate `man/*.Rd` and `NAMESPACE`.
- Edit `README.qmd` (never `README.md` directly) and render with `quarto render README.qmd --to gfm`.
- Regenerate bundled `data/orchids_*.rda` datasets from `data-raw/*.tsv` by running `data-raw/orchids_compounds.R`, `data-raw/orchids_compound_classes.R`, and `data-raw/orchids_ethnobotanical.R`. Ensure exported datasets remain plain base `data.frame` objects.
- Keep `DESCRIPTION`, `inst/CITATION`, `CITATION.cff`, `codemeta.json`, and `inst/WORDLIST` consistent when dependencies or domain terminology change.
