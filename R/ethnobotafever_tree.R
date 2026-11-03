#' @title Ethnobota fever
#'
#' @description
#' Collects and plots ethnobotanical and phytochemical data for a given taxon.
#' This function performs data retrieval, merging, and logging, and produces
#' summary statistics and outputs for downstream analysis and visualization.
#'
#' @include calculate_branches_values.R
#' @include get_npclassifier_taxonomy.R
#' @include get_results_wd.R
#' @include get_trees.R
#' @include normalize_ott_ids.R
#' @include plot_trees.R
#'
#' @param taxon_name Character. Name of the taxon of interest.
#' @param n_multiple_genera Integer. Minimum number of genera for filtering.
#' @param n_ethnobotanical_count_min Integer. Minimum ethnobotanical count for
#' filtering.
#' @param n_compounds_count_min Integer. Minimum compound count for filtering.
#' @param example_family_names Character vector. Example family names for
#' comparison.
#' @param path_molecules Character, data.frame, or NULL. Local molecules file or
#' table.
#' @param path_classes Character, data.frame, or NULL. Local classes file or
#' seed data.
#' @param path_ethnobotanical Character, data.frame, or NULL. Local
#' ethnobotanical file or table.
#' @param export_tree_full Character. Path for the main full-tree PDF figure.
#' @param export_tree_phyto Character. Path for the phytochemical overview PDF
#' figure.
#' @param export_tree_phyto_sub Character. Path for the species-level
#' phytochemical PDF figure.
#' @param export_table_classified Character. Path for the classified compounds
#' export.
#' @param export_table_counts Character. Path for the genus-level summary table
#' export.
#' @param export_table_counts_sub Character or NULL. Path for the filtered
#' species-level counts export.
#' @param local_cache Character. Directory containing `.rda` tree cache datasets
#' (for example `data-raw`).
#' @param log_dir Character. Directory for log files.
#'
#' @return A list containing the rendered plots, exported tables, and the
#' log-file path.
#'
#' @details
#' This function is a pipeline for data collection, merging, and reporting. It
#' logs progress and statistics throughout. All side effects (file writes, logs)
#' are documented inline.
#'
#' @section EDAM:
#' - Topics:
#'   - `http://edamontology.org/topic_0780` (Plant biology)
#'   - `http://edamontology.org/topic_0154` (Small molecules)
#'   - `http://edamontology.org/topic_2258` (Cheminformatics)
#'   - `http://edamontology.org/topic_3293` (Phylogenetics)
#'   - `http://edamontology.org/topic_0637` (Taxonomy)
#' - Operations:
#'   - `http://edamontology.org/operation_0224` (Query and retrieval)
#'   - `http://edamontology.org/operation_2422` (Data retrieval)
#'   - `http://edamontology.org/operation_2990` (Classification)
#'   - `http://edamontology.org/operation_0324` (Phylogenetic analysis)
#'   - `http://edamontology.org/operation_0567`
#'     (Phylogenetic tree visualisation)
#' - Data:
#'   - `http://edamontology.org/data_0872` (Phylogenetic tree)
#'   - `http://edamontology.org/data_3272` (Species tree)
#'   - `http://edamontology.org/data_1463` (Small molecule structure)
#'   - `http://edamontology.org/data_2301` (SMILES string)
#'   - `http://edamontology.org/data_1086` (Compound identifier)
#'   - `http://edamontology.org/data_1868` (Taxon)
#'   - `http://edamontology.org/data_1872` (Taxonomic classification)
#' - Formats:
#'   - `http://edamontology.org/format_1196` (SMILES)
#'   - `http://edamontology.org/format_1199` (InChIKey)
#'   - `http://edamontology.org/format_3475` (TSV)
#'   - `http://edamontology.org/format_3508` (PDF)
#'   - `http://edamontology.org/format_3604` (SVG)
#'
#' @export
#'
#' @examples
#' NULL
ethnobotafever_tree <- function(
  taxon_name,
  n_multiple_genera,
  n_ethnobotanical_count_min,
  n_compounds_count_min,
  example_family_names,
  path_molecules,
  path_classes,
  path_ethnobotanical,
  export_tree_full,
  export_tree_phyto,
  export_tree_phyto_sub,
  export_table_classified,
  export_table_counts,
  export_table_counts_sub,
  local_cache,
  log_dir
) {
  if (
    !is.character(taxon_name) || length(taxon_name) != 1L || !nzchar(taxon_name)
  ) {
    stop(
      "taxon_name must be a single non-empty character string",
      taxon_name = taxon_name
    )
  }
  for (arg in c(
    "n_multiple_genera",
    "n_ethnobotanical_count_min",
    "n_compounds_count_min"
  )) {
    value <- get(arg, envir = environment())
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        !is.finite(value) ||
        value < 0
    ) {
      stop(sprintf("%s must be a single finite non-negative number", arg))
    }
  }
  if (!is.null(example_family_names) && !is.character(example_family_names)) {
    stop("example_family_names must be a character vector or NULL")
  }
  if (
    is.character(example_family_names) && is.null(names(example_family_names))
  ) {
    names(example_family_names) <- example_family_names
  }

  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  log_file <- file.path(log_dir, paste0(taxon_name, "_log.txt"))
  if (file.exists(log_file)) {
    file.remove(log_file)
  }
  previous_appender <- logger::log_appender()
  logger::log_appender(appender = logger::appender_tee(file = log_file))
  on.exit(
    logger::log_appender(
      appender = if (is.function(previous_appender)) {
        previous_appender
      } else {
        get(
          as.character(previous_appender),
          mode = "function",
          envir = asNamespace("logger")
        )
      }
    ),
    add = TRUE
  )

  resolve_input_table <- function(x, label) {
    if (is.null(x)) {
      return(NULL)
    }
    if (is.data.frame(x)) {
      return(as.data.frame(x, stringsAsFactors = FALSE))
    }
    if (is.character(x) && length(x) == 1L && file.exists(x)) {
      return(tidytable::fread(x))
    }
    stop(label, " must be a data.frame, an existing file path, or NULL")
  }

  log_unique_taxa <- function(df, label) {
    logger::log_info(
      "We found ",
      df |> tidytable::distinct(taxon) |> nrow(),
      paste0(" ", taxon_name, " ", label)
    )
  }

  add_connectivity <- function(df) {
    df |>
      tidytable::mutate(
        connectivity_layer = sub("-.*", "", compound_inchikey)
      ) |>
      tidytable::distinct(taxon, connectivity_layer, .keep_all = TRUE)
  }

  build_compound_classes <- function(df_taxon_smiles, classes_df) {
    df_taxon_smiles |>
      tidytable::distinct(taxon_name, compound_smiles_canonical) |>
      tidytable::left_join(
        classes_df,
        by = c("compound_smiles_canonical" = "query")
      ) |>
      tidytable::distinct(
        smiles_2D = compound_smiles_canonical,
        organism = taxon_name,
        best_candidate_1 = pathway,
        best_candidate_2 = superclass,
        best_candidate_3 = class
      ) |>
      tidytable::mutate(
        sample = organism,
        species = organism,
        best_candidate_1 = tidytable::if_else(
          condition = is.na(best_candidate_1),
          true = "Other",
          false = best_candidate_1
        )
      ) |>
      tidytable::distinct(
        smiles_2D,
        organism,
        best_candidate_1,
        best_candidate_2,
        .keep_all = TRUE
      )
  }

  taxon_qid <- taxon_name_to_qid(taxon_name)
  example_family_qids <- lapply(example_family_names, taxon_name_to_qid)

  taxa_wd <- get_subtaxa_wd(taxon_qid)
  example_family_taxa_wd <- lapply(example_family_qids, get_subtaxa_wd)

  table_ethnobotanical_wd <- taxon_qid |>
    get_uses_wd() |>
    tidytable::distinct(taxon, taxon_id_ott) |>
    tidytable::mutate(
      taxon_id_ott = normalize_ott_ids(taxon_id_ott),
      ethnobotanical = TRUE
    )
  log_unique_taxa(
    table_ethnobotanical_wd,
    "subtaxa with ethnobotanical use reported on Wikidata"
  )

  table_ethnobotanical_local <- resolve_input_table(
    path_ethnobotanical,
    "path_ethnobotanical"
  )
  if (!is.null(table_ethnobotanical_local)) {
    table_ethnobotanical_local$taxon_id_ott <- normalize_ott_ids(
      table_ethnobotanical_local$taxon_id_ott
    )
    table_ethnobotanical_local$ethnobotanical <- TRUE
    table_ethnobotanical_local <- tidytable::distinct(
      table_ethnobotanical_local
    )
    log_unique_taxa(
      table_ethnobotanical_local,
      "subtaxa with ethnobotanical use reported locally"
    )
    table_ethnobotanical_to_upload <- table_ethnobotanical_local |>
      tidytable::anti_join(table_ethnobotanical_wd)
    ethnobotanical_non_wd <- table_ethnobotanical_to_upload |>
      tidytable::distinct(taxon) |>
      nrow()
    logger::log_info(
      "... of which ",
      ethnobotanical_non_wd,
      paste0(
        " are not on Wikidata yet. ",
        ifelse(
          test = ethnobotanical_non_wd > 0,
          yes = "These should definitely be shared!",
          no = "Good job!"
        )
      )
    )
    table_ethnobotanical <- table_ethnobotanical_wd |>
      tidytable::bind_rows(table_ethnobotanical_to_upload)
    log_unique_taxa(
      table_ethnobotanical,
      "subtaxa with ethnobotanical use reported in total"
    )
  } else {
    table_ethnobotanical <- table_ethnobotanical_wd
  }

  logger::log_info(
    "We found ",
    nrow(taxa_wd),
    paste0(" ", taxon_name, " subtaxa in Wikidata")
  )
  taxa_gen <- taxa_wd |>
    tidytable::filter(taxon_rank == "Q34740") |>
    tidytable::distinct(taxon, taxon_name)
  logger::log_info("... ", nrow(taxa_gen), " unique genera")

  taxa_spe <- taxa_wd |>
    tidytable::filter(taxon_rank == "Q7432") |>
    tidytable::mutate(
      generic_epithet = sub(" .*", "", taxon_name)
    ) |>
    tidytable::distinct(taxon, generic_epithet)
  logger::log_info("... ", nrow(taxa_spe), " unique species")

  taxa_var <- taxa_wd |>
    tidytable::filter(taxon_rank == "Q767728") |>
    tidytable::mutate(
      generic_epithet = sub(" .*", "", taxon_name)
    ) |>
    tidytable::distinct(taxon, generic_epithet)
  logger::log_info("... and ", nrow(taxa_var), " unique varieties")

  for (fam_name in names(example_family_taxa_wd)) {
    logger::log_info(
      "In comparison, ",
      fam_name,
      " had ",
      nrow(example_family_taxa_wd[[fam_name]]),
      " subtaxa in Wikidata"
    )
  }

  taxa_ott_wd <- get_ott_ids_wd(taxon_qid)

  logger::log_info(
    "We found ",
    nrow(taxa_ott_wd),
    paste0(" ", taxon_name, " subtaxa with an Open Tree of Life ID in Wikidata")
  )
  taxa_gen_ott <- taxa_gen |>
    tidytable::inner_join(taxa_ott_wd) |>
    tidytable::mutate(taxon_id_ott = normalize_ott_ids(taxon_id_ott))
  logger::log_info("... ", nrow(taxa_gen_ott), " unique genera")

  taxa_spe_ott <- taxa_spe |>
    tidytable::inner_join(taxa_ott_wd) |>
    tidytable::mutate(taxon_id_ott = normalize_ott_ids(taxon_id_ott))
  logger::log_info("... ", nrow(taxa_spe_ott), " unique species")

  taxa_var_ott <- taxa_var |>
    tidytable::inner_join(taxa_ott_wd) |>
    tidytable::mutate(taxon_id_ott = normalize_ott_ids(taxon_id_ott))
  logger::log_info("... ", nrow(taxa_var_ott), " unique varieties")

  example_family_compounds_wd <- lapply(example_family_qids, get_compounds_wd)
  taxa_compounds_wd <- get_compounds_wd(taxon_qid)

  logger::log_info(
    "We found ",
    taxa_compounds_wd |> tidytable::distinct(taxon) |> nrow(),
    paste0(
      " ",
      taxon_name,
      " subtaxa with at least one chemical compound reported in Wikidata"
    )
  )
  taxa_compounds_local <- resolve_input_table(
    path_molecules,
    "path_molecules"
  )
  if (!is.null(taxa_compounds_local)) {
    taxa_compounds_local <- tidytable::distinct(
      taxa_compounds_local,
      compound,
      compound_smiles_canonical,
      compound_inchikey,
      taxon
    )
    logger::log_info(
      "We found ",
      taxa_compounds_local |> tidytable::distinct(taxon) |> nrow(),
      paste0(
        " ",
        taxon_name,
        " subtaxa with at least one chemical compound reported locally"
      )
    )
    taxa_compounds_to_upload <- taxa_compounds_local |>
      tidytable::anti_join(taxa_compounds_wd)
    compounds_non_wd <- taxa_compounds_to_upload |>
      tidytable::distinct(taxon) |>
      nrow()
    logger::log_info(
      "... of which ",
      compounds_non_wd,
      paste0(
        " are not on Wikidata yet. ",
        ifelse(
          test = compounds_non_wd > 0,
          yes = "These should definitely be shared!",
          no = "Good job!"
        )
      )
    )
    taxa_compounds_tot <- taxa_compounds_wd |>
      tidytable::bind_rows(taxa_compounds_to_upload)
    logger::log_info(
      "We found ",
      taxa_compounds_tot |> tidytable::distinct(taxon) |> nrow(),
      paste0(
        " ",
        taxon_name,
        " subtaxa with chemical compounds reported in total"
      )
    )
  } else {
    if (!is.null(path_molecules)) {
      logger::log_warn(
        "Local molecules table not available; using Wikidata-only compounds."
      )
    }
    taxa_compounds_tot <- taxa_compounds_wd
  }

  logger::log_info(
    "... ",
    taxa_compounds_tot |>
      tidytable::distinct(taxon, compound_inchikey) |>
      nrow(),
    " unique chemical compound-taxon pairs"
  )
  logger::log_info(
    "... ",
    taxa_compounds_tot |>
      tidytable::distinct(compound_inchikey) |>
      nrow(),
    " unique chemical compounds"
  )

  taxa_compounds <- add_connectivity(taxa_compounds_tot)
  example_family_taxa_compounds <- lapply(
    example_family_compounds_wd,
    add_connectivity
  )

  n_connectivities <- taxa_compounds |>
    tidytable::distinct(connectivity_layer) |>
    nrow()
  logger::log_info("... ", n_connectivities, " unique connectivities")

  for (fam_name in names(example_family_taxa_compounds)) {
    logger::log_info(
      "In comparison, ",
      fam_name,
      " had ",
      example_family_taxa_compounds[[fam_name]] |>
        tidytable::distinct(connectivity_layer) |>
        nrow(),
      " unique connectivities"
    )
  }

  lotus_compounds <- get_lotus_wd() |>
    add_connectivity()
  logger::log_info(
    "And ",
    lotus_compounds |> tidytable::distinct(connectivity_layer) |> nrow(),
    " unique connectivities were found among all taxa in Wikidata"
  )
  logger::log_info(
    "Among the ",
    n_connectivities,
    " unique connectivities found in ",
    taxon_name,
    " ..."
  )
  taxa_compounds_specific <- taxa_compounds |>
    tidytable::anti_join(
      lotus_compounds |>
        tidytable::anti_join(
          taxa_compounds,
          by = c("taxon" = "taxon")
        ),
      by = c("connectivity_layer" = "connectivity_layer")
    )
  logger::log_info(
    "... ",
    taxa_compounds_specific |>
      tidytable::distinct(connectivity_layer) |>
      nrow(),
    " were specific to the family ..."
  )
  taxa_compounds_specific_genus <- taxa_compounds_specific |>
    tidytable::inner_join(taxa_gen) |>
    tidytable::rename(generic_epithet = taxon_name) |>
    tidytable::bind_rows(
      taxa_compounds_specific |> tidytable::inner_join(taxa_spe)
    ) |>
    tidytable::bind_rows(
      taxa_compounds_specific |> tidytable::inner_join(taxa_var)
    ) |>
    tidytable::distinct(connectivity_layer, generic_epithet) |>
    tidytable::group_by(connectivity_layer) |>
    tidytable::count(sort = TRUE)
  logger::log_info(
    "... ... of which ",
    taxa_compounds_specific_genus |>
      tidytable::filter(n == 1L) |>
      nrow(),
    " were specific to a genus within the family"
  )
  logger::log_info(
    "... ... of which ",
    taxa_compounds_specific_genus |>
      tidytable::filter(n > 1L) |>
      nrow(),
    " were found in multiple genera within the family"
  )
  widespread_connectivities <- taxa_compounds_specific_genus |>
    tidytable::filter(n >= n_multiple_genera) |>
    tidytable::pull(connectivity_layer)
  logger::log_info(
    "... ... of which ",
    length(widespread_connectivities),
    " were found in at least ",
    n_multiple_genera,
    " genera within the family"
  )
  logger::log_info(
    "... ... ... these were ",
    paste0(widespread_connectivities, collapse = ", ")
  )

  taxa_gen_ott_cpd <- taxa_gen_ott |>
    tidytable::left_join(taxa_compounds)

  taxa_spe_ott_cpd <- taxa_spe_ott |>
    tidytable::left_join(taxa_compounds)

  taxa_var_ott_cpd <- taxa_var_ott |>
    tidytable::inner_join(taxa_compounds)

  # Compute genus-level compound and species counts in O(G + S + V)
  # rather than a 3-way Cartesian join across genus x species x variety
  gen_base <- tidytable::distinct(
    taxa_gen_ott_cpd,
    taxon,
    taxon_name,
    taxon_id_ott
  )
  spe_counts_by_genus <- taxa_spe_ott_cpd |>
    tidytable::summarize(
      species_count = sum(!is.na(unique(taxon))),
      .by = generic_epithet
    )
  cpd_counts_by_genus <- tidytable::bind_rows(
    tidytable::select(
      taxa_gen_ott_cpd,
      taxon,
      taxon_id_ott,
      taxon_name,
      compound
    ),
    tidytable::inner_join(
      gen_base,
      tidytable::select(
        taxa_spe_ott_cpd,
        taxon_name = generic_epithet,
        compound
      ),
      by = "taxon_name"
    ),
    tidytable::inner_join(
      gen_base,
      tidytable::select(
        taxa_var_ott_cpd,
        taxon_name = generic_epithet,
        compound
      ),
      by = "taxon_name"
    )
  ) |>
    tidytable::filter(!is.na(compound)) |>
    tidytable::summarize(
      compounds_count = tidytable::n_distinct(compound),
      .by = c(taxon, taxon_id_ott, taxon_name)
    )

  taxa_counted <- gen_base |>
    tidytable::left_join(
      cpd_counts_by_genus,
      by = c("taxon", "taxon_id_ott", "taxon_name")
    ) |>
    tidytable::left_join(
      spe_counts_by_genus,
      by = c("taxon_name" = "generic_epithet")
    ) |>
    tidytable::mutate(
      compounds_count = tidytable::replace_na(compounds_count, 0L),
      species_count = tidytable::replace_na(species_count, 0L)
    ) |>
    tidytable::relocate(taxon_name, .before = taxon_id_ott) |>
    tidytable::distinct() |>
    data.frame()

  taxa_ethno <- taxa_spe_ott_cpd |>
    tidytable::select(-compound) |>
    tidytable::distinct() |>
    tidytable::inner_join(table_ethnobotanical) |>
    tidytable::group_by(generic_epithet) |>
    tidytable::distinct(taxon_id_ott) |>
    tidytable::count() |>
    tidytable::rename(taxon_name = generic_epithet, ethnobotanical_count = n)

  taxa_ethno_counted <- taxa_counted |>
    tidytable::left_join(taxa_ethno) |>
    tidytable::mutate(
      ethnobotanical_count = tidytable::replace_na(ethnobotanical_count, 0L)
    )

  logger::log_info(
    "Among the ",
    taxa_ethno_counted |>
      tidytable::distinct(taxon_name, taxon, taxon_id_ott) |>
      nrow(),
    paste0(
      " ",
      taxon_name,
      " genera and their subtaxa with an Open Tree of Life ID in Wikidata ..."
    )
  )
  logger::log_info(
    "... ",
    taxa_ethno_counted |>
      tidytable::filter(ethnobotanical_count >= 1L) |>
      tidytable::distinct(taxon_name, taxon, taxon_id_ott) |>
      nrow(),
    " had an ethnobotanical use reported"
  )
  logger::log_info(
    "... ",
    taxa_ethno_counted |>
      tidytable::filter(compounds_count >= 1L) |>
      tidytable::distinct(taxon_name, taxon, taxon_id_ott) |>
      nrow(),
    " had at least one chemical compound reported in Wikidata"
  )
  logger::log_info(
    "... ",
    taxa_ethno_counted |>
      tidytable::filter(ethnobotanical_count >= 1L & compounds_count >= 1L) |>
      tidytable::distinct(taxon_name, taxon, taxon_id_ott) |>
      nrow(),
    " had both"
  )

  taxa_known <- taxa_ethno_counted |>
    tidytable::filter(
      ethnobotanical_count >= n_ethnobotanical_count_min |
        compounds_count >= n_compounds_count_min
    ) |>
    tidytable::select(-compounds_count) |>
    tidytable::inner_join(
      taxa_spe_ott_cpd |>
        tidytable::distinct(taxon, compound_inchikey, .keep_all = TRUE) |>
        tidytable::group_by(taxon) |>
        tidytable::add_count(compound_inchikey, name = "compounds_count") |>
        tidytable::mutate(
          compounds_count = ifelse(
            test = is.na(compound_inchikey),
            yes = 0,
            no = compounds_count
          )
        ) |>
        tidytable::ungroup(),
      by = c("taxon_name" = "generic_epithet")
    ) |>
    tidytable::left_join(
      table_ethnobotanical |>
        tidytable::select(-tidytable::any_of("taxon_name")),
      by = c("taxon_id_ott.y" = "taxon_id_ott")
    )
  logger::log_info(
    "... ",
    taxa_known |>
      tidytable::distinct(taxon_name) |>
      tidytable::arrange(taxon_name) |>
      tidytable::pull() |>
      paste(collapse = ", "),
    " had at least ",
    n_ethnobotanical_count_min,
    " ethnobotanical uses or ",
    n_compounds_count_min,
    " compounds reported"
  )
  logger::log_info(
    "... ... they encompass ",
    taxa_known |>
      tidytable::distinct(taxon_id_ott.y) |>
      nrow(),
    " species"
  )

  taxa_known_sub <- taxa_known |>
    tidytable::filter(!is.na(compound_inchikey) | ethnobotanical) |>
    tidytable::select(
      taxon = taxon.y,
      taxon_id_ott = taxon_id_ott.y,
      compounds_count,
      species_count,
      ethnobotanical_count
    ) |>
    tidytable::inner_join(
      taxa_wd |>
        tidytable::select(taxon, taxon_name) |>
        tidytable::distinct()
    ) |>
    tidytable::relocate(taxon_name, .after = taxon) |>
    tidytable::distinct()

  logger::log_info(
    "... ... ... of which ",
    taxa_known_sub |>
      tidytable::distinct(taxon_id_ott) |>
      nrow(),
    " had at least one compound or ethnobotanical use reported"
  )

  logger::log_trace("Retrieving compounds classification ...")
  smiles <- taxa_compounds |>
    tidytable::distinct(compound_smiles_canonical) |>
    tidytable::pull()
  classes_known <- resolve_input_table(path_classes, "path_classes")
  if (!is.null(classes_known)) {
    classes_known <- tidytable::distinct(classes_known)
    smiles_unknown <- smiles[!smiles %in% classes_known$query]
    if (length(smiles_unknown) > 0L) {
      classes <- smiles_unknown |>
        get_npclassifier_taxonomy() |>
        tidytable::bind_rows() |>
        tidytable::bind_rows(classes_known)
    } else {
      classes <- classes_known
    }
  } else {
    classes <- smiles |>
      get_npclassifier_taxonomy() |>
      tidytable::bind_rows()
  }
  if (is.character(path_classes) && length(path_classes) == 1L) {
    classes |>
      tidytable::fwrite(file = path_classes, sep = "\t")
  }

  gen_names <- tidytable::distinct(taxa_gen_ott_cpd, taxon_name)
  taxa_compounds_classes <- tidytable::bind_rows(
    taxa_gen_ott_cpd,
    tidytable::left_join(
      gen_names,
      taxa_spe_ott_cpd,
      by = c("taxon_name" = "generic_epithet")
    ),
    tidytable::left_join(
      gen_names,
      taxa_var_ott_cpd,
      by = c("taxon_name" = "generic_epithet")
    )
  ) |>
    tidytable::filter(!is.na(compound_smiles_canonical)) |>
    build_compound_classes(classes)

  taxa_compounds_classes_sub <- taxa_known |>
    tidytable::filter(!is.na(compound_inchikey)) |>
    tidytable::distinct(taxon, compound_smiles_canonical) |>
    tidytable::inner_join(
      tidytable::distinct(taxa_known_sub, taxon, taxon_name)
    ) |>
    build_compound_classes(classes)

  compounds_hierarchy <- taxa_compounds_classes |>
    prepare_hierarchy(type = "literature") |>
    prepare_plot() |>
    tidytable::rename(taxon_name = species)
  compounds_hierarchy_sub <- taxa_compounds_classes_sub |>
    prepare_hierarchy(type = "literature") |>
    prepare_plot() |>
    tidytable::rename(taxon_name = species)

  taxa_classes <- gen_names |>
    tidytable::inner_join(compounds_hierarchy) |>
    tidytable::relocate(taxon_name, .before = ids) |>
    tidytable::distinct(taxon_name, values, ids, color, .keep_all = TRUE)

  taxa_classes_sub <- taxa_compounds_classes_sub |>
    tidytable::rename(taxon_name = organism) |>
    tidytable::distinct(taxon_name) |>
    tidytable::inner_join(compounds_hierarchy_sub) |>
    tidytable::relocate(taxon_name, .before = ids) |>
    tidytable::distinct(taxon_name, values, ids, color, .keep_all = TRUE)

  taxa_ott_wd_ids <- unique(taxa_counted$taxon_id_ott)
  taxa_ott_wd_ids_sub <- unique(taxa_known_sub$taxon_id_ott)

  logger::log_trace("Retrieving Open Tree Taxonomy now... taking some time!")
  cache_dir <- local_cache
  if (is.null(cache_dir) || length(cache_dir) != 1L || !nzchar(cache_dir)) {
    cache_dir <- file.path("data")
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  trees_gen <- get_trees_cached(
    ott_ids = taxa_ott_wd_ids,
    df_counted = taxa_ethno_counted,
    taxon_name = taxon_name,
    cache_dir = cache_dir
  )
  trees_sub <- get_trees_cached(
    ott_ids = taxa_ott_wd_ids_sub,
    df_counted = taxa_known_sub,
    taxon_name = taxon_name,
    cache_dir = cache_dir
  )

  plots_gen <- plot_trees(
    taxa_not_in_tree = trees_gen$taxa_not_in_tree,
    tree_taxa_full = trees_gen$tree_taxa_full,
    tree_taxa_phyto = trees_gen$tree_taxa_phyto,
    df_counted = taxa_ethno_counted,
    df_classes = taxa_classes
  )

  plots_sub <- plot_trees(
    taxa_not_in_tree = trees_sub$taxa_not_in_tree,
    tree_taxa_full = trees_sub$tree_taxa_full,
    tree_taxa_phyto = trees_sub$tree_taxa_phyto,
    df_counted = taxa_known_sub,
    df_classes = taxa_classes_sub,
    genus = FALSE,
    drop = TRUE
  )

  logger::log_trace("Exporting ...")
  export_tree_bundle(
    plot_obj = plots_gen$plot_tree_taxa_bars,
    girafe_obj = plots_gen$interactive_tree_taxa_bars,
    layout_cfg = plots_gen$layout_full,
    file_path = export_tree_full,
    title = paste0(
      taxon_name,
      " - Full Genus Tree (Collapsed Unstudied Clades)"
    )
  )

  export_tree_circular <- sub(
    "\\.([A-Za-z0-9]+)$",
    "_circular_si.\\1",
    export_tree_full
  )
  export_tree_bundle(
    plot_obj = plots_gen$plot_tree_taxa_circular_bars,
    girafe_obj = plots_gen$interactive_tree_taxa_circular_bars,
    layout_cfg = plots_gen$layout_circular,
    file_path = export_tree_circular,
    title = paste0(
      taxon_name,
      " - Uncollapsed Circular Genus Tree (Supplementary)"
    )
  )

  export_tree_bundle(
    plot_obj = plots_gen$plot_tree_taxa_phyto_bars,
    girafe_obj = plots_gen$interactive_tree_taxa_phyto_bars,
    layout_cfg = plots_gen$layout_phyto,
    file_path = export_tree_phyto,
    title = paste0(taxon_name, " - Phytochemical & Ethnobotanical Genus Tree")
  )

  export_tree_bundle(
    plot_obj = plots_sub$plot_tree_taxa_phyto_bars,
    girafe_obj = plots_sub$interactive_tree_taxa_phyto_bars,
    layout_cfg = plots_sub$layout_phyto,
    file_path = export_tree_phyto_sub,
    title = paste0(taxon_name, " - Species-Level Phytochemical Tree")
  )

  itol_dir <- file.path(dirname(export_table_counts), "itol")
  export_itol_files(
    tree = trees_gen$tree_taxa_full,
    df_counted = taxa_ethno_counted,
    df_classes = taxa_classes,
    out_dir = itol_dir,
    prefix = paste0(taxon_name, "_full_genera"),
    genus = TRUE
  )
  export_itol_files(
    tree = trees_gen$tree_taxa_phyto,
    df_counted = taxa_ethno_counted,
    df_classes = taxa_classes,
    out_dir = itol_dir,
    prefix = paste0(taxon_name, "_phyto_genera"),
    genus = TRUE
  )
  export_itol_files(
    tree = trees_sub$tree_taxa_phyto,
    df_counted = taxa_known_sub,
    df_classes = taxa_classes_sub,
    out_dir = itol_dir,
    prefix = paste0(taxon_name, "_phyto_species"),
    genus = FALSE
  )

  taxa_full <- taxa_wd |>
    tidytable::inner_join(taxa_compounds) |>
    tidytable::left_join(
      classes,
      by = c("compound_smiles_canonical" = "query")
    ) |>
    tidytable::select(-taxon_rank)
  taxa_full |>
    tidytable::fwrite(
      file = export_table_classified,
      sep = "\t"
    )
  taxa_ethno_counted |>
    tidytable::fwrite(
      file = export_table_counts,
      sep = "\t"
    )
  taxa_known_sub |>
    tidytable::select(taxon_name, compounds_count) |>
    tidytable::arrange(
      sub(" .*", "", taxon_name),
      -compounds_count
    ) |>
    tidytable::fwrite(
      file = export_table_counts_sub,
      sep = "\t"
    )
  taxa_ethno_counted <- data.frame(taxa_ethno_counted)
  taxa_known_sub <- data.frame(taxa_known_sub)
  taxa_full <- data.frame(taxa_full)

  list(
    plots_gen = plots_gen,
    plots_sub = plots_sub,
    df_taxa_full = taxa_full,
    df_taxa_ethno_counted = taxa_ethno_counted,
    df_taxa_known_sub = taxa_known_sub,
    log_file = log_file
  )
}
