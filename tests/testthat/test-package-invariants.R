test_that("bundled orchid datasets are plain non-empty data.frames", {
  e <- new.env(parent = emptyenv())
  utils::data(
    list = c(
      "orchids_compounds",
      "orchids_compound_classes",
      "orchids_ethnobotanical"
    ),
    package = "EthnoBotaPhyWR",
    envir = e
  )

  expect_identical(class(e$orchids_compounds), "data.frame")
  expect_identical(class(e$orchids_compound_classes), "data.frame")
  expect_identical(class(e$orchids_ethnobotanical), "data.frame")

  expect_gt(nrow(e$orchids_compounds), 0L)
  expect_gt(nrow(e$orchids_compound_classes), 0L)
  expect_gt(nrow(e$orchids_ethnobotanical), 0L)

  expect_true(all(
    c(
      "taxon",
      "taxon_name",
      "compound",
      "compound_smiles_canonical",
      "compound_inchikey",
      "connectivity_layer",
      "pathway",
      "superclass",
      "class",
      "glycoside"
    ) %in%
      names(e$orchids_compounds)
  ))
  expect_true(all(
    c("query", "pathway", "superclass", "class", "glycoside") %in%
      names(e$orchids_compound_classes)
  ))
  expect_true(all(
    c("taxon_name", "taxon", "taxon_id_ott") %in%
      names(e$orchids_ethnobotanical)
  ))
  expect_false(anyNA(e$orchids_compound_classes$glycoside))
})

test_that("Orchidaceae iTOL files exist, match species tree tips, and cap richness at 1", {
  itol_dir <- system.file("extdata", "itol", package = "EthnoBotaPhyWR")
  expect_true(nzchar(itol_dir))

  expected_files <- c(
    "Orchidaceae_full_genera_itol_compounds_multibar.txt",
    "Orchidaceae_full_genera_itol_ethnobotany.txt",
    "Orchidaceae_full_genera_itol_species_richness.txt",
    "Orchidaceae_full_genera_tree.tre",
    "Orchidaceae_phyto_genera_itol_compounds_multibar.txt",
    "Orchidaceae_phyto_genera_itol_ethnobotany.txt",
    "Orchidaceae_phyto_genera_itol_species_richness.txt",
    "Orchidaceae_phyto_genera_tree.tre",
    "Orchidaceae_phyto_species_itol_compounds_multibar.txt",
    "Orchidaceae_phyto_species_itol_ethnobotany.txt",
    "Orchidaceae_phyto_species_itol_species_richness.txt",
    "Orchidaceae_phyto_species_tree.tre"
  )
  paths <- file.path(itol_dir, expected_files)
  expect_true(all(file.exists(paths)))
  expect_true(all(file.info(paths)$size > 0))

  parse_itol_rows <- function(path) {
    lines <- readLines(path, warn = FALSE)
    data_idx <- match("DATA", lines)
    expect_false(is.na(data_idx))
    rows <- lines[(data_idx + 1L):length(lines)]
    rows[nzchar(trimws(rows))]
  }

  sp_tree <- ape::read.tree(
    file.path(itol_dir, "Orchidaceae_phyto_species_tree.tre")
  )
  richness_rows <- parse_itol_rows(
    file.path(itol_dir, "Orchidaceae_phyto_species_itol_species_richness.txt")
  )
  richness_parts <- strsplit(richness_rows, "\t", fixed = TRUE)
  richness_vals <- as.numeric(vapply(richness_parts, `[[`, character(1), 2L))
  expect_true(all(richness_vals <= 1))

  for (fname in c(
    "Orchidaceae_phyto_species_itol_species_richness.txt",
    "Orchidaceae_phyto_species_itol_ethnobotany.txt",
    "Orchidaceae_phyto_species_itol_compounds_multibar.txt"
  )) {
    rows <- parse_itol_rows(file.path(itol_dir, fname))
    ids <- vapply(strsplit(rows, "\t", fixed = TRUE), `[[`, character(1), 1L)
    expect_true(all(ids %in% sp_tree$tip.label))
  }
})

test_that("CITATION is valid and human docs are clean", {
  cit_path <- system.file("CITATION", package = "EthnoBotaPhyWR")
  expect_true(nzchar(cit_path))
  cit_lines <- readLines(cit_path, warn = FALSE)
  expect_false(any(grepl("TODO", cit_lines, fixed = TRUE)))
  expect_s3_class(utils::readCitationFile(cit_path), "bibentry")

  pkg_root <- testthat::test_path("../..")
  if (file.exists(file.path(pkg_root, "DESCRIPTION"))) {
    for (rel in c(
      "README.md",
      "CONTRIBUTING.md",
      "NEWS.md",
      "DESCRIPTION",
      "inst/CITATION"
    )) {
      p <- file.path(pkg_root, rel)
      if (file.exists(p)) {
        txt <- readLines(p, warn = FALSE)
        expect_false(
          any(grepl("zenodo\\.TODO|AGENTS\\.md|\\.agents", txt)),
          info = rel
        )
      }
    }
  }
})
