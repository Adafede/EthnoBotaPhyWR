#' @title Ethnobota fever
#'
#' @description
#' Collects and plots ethnobotanical and phytochemical data for a given taxon. This function performs data retrieval, merging, and logging, and produces summary statistics and outputs for downstream analysis and visualization.
#'
#' @include calculate_branches_values.R
#' @include get_npclassifier_taxonomy.R
#' @include get_results_wd.R
#' @include get_trees.R
#' @include plot_trees.R
#'
#' @param taxon_name Character. Name of the taxon of interest.
#' @param n_multiple_genera Integer. Minimum number of genera for filtering.
#' @param n_ethnobotanical_count_min Integer. Minimum ethnobotanical count for filtering.
#' @param n_compounds_count_min Integer. Minimum compound count for filtering.
#' @param example_family_names Character vector. Example family names for comparison.
#' @param path_molecules Character or NULL. Path to local molecules file.
#' @param path_classes Character or NULL. Path to local classes file.
#' @param path_ethnobotanical Character or NULL. Path to local ethnobotanical file.
#' @param export_tree_full Character. Path for the main full-tree PDF figure.
#' @param export_tree_phyto Character. Path for the phytochemical overview PDF figure.
#' @param export_tree_phyto_sub Character. Path for the species-level phytochemical PDF figure.
#' @param export_table_classified Character. Path for the classified compounds export.
#' @param export_table_counts Character. Path for the genus-level summary table export.
#' @param export_table_counts_sub Character or NULL. Path for the filtered species-level counts export.
#' @param local_cache Character. Directory for the persistent tree cache.
#' @param log_dir Character. Directory for log files.
#'
#' @return A list containing the rendered plots, exported tables, and the log-file path.
#'
#' @details
#' This function is a pipeline for data collection, merging, and reporting. It logs progress and statistics throughout. All side effects (file writes, logs) are documented inline.
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
  # Set up logging
  log_file <- paste0(log_dir, taxon_name, "_log.txt")
  if (file.exists(log_file)) {
    file.remove(log_file)
  }
  logger::log_appender(appender = logger::appender_tee(file = log_file))

  # Helper: log and count unique taxa
  log_unique_taxa <- function(df, label) {
    logger::log_info(
      "We found ",
      df |> tidytable::distinct(taxon) |> nrow(),
      paste0(" ", taxon_name, " ", label)
    )
  }

  # Retrieve ethnobotanical data from Wikidata
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

  # Optionally merge with local ethnobotanical data
  if (!is.null(path_ethnobotanical) && file.exists(path_ethnobotanical)) {
    table_ethnobotanical_local <- tryCatch(
      {
        local_dt <- tidytable::fread(path_ethnobotanical)
        local_dt$taxon_id_ott <- normalize_ott_ids(local_dt$taxon_id_ott)
        local_dt$ethnobotanical <- TRUE
        tidytable::distinct(local_dt)
      },
      error = function(e) {
        stop("Failed to read local ethnobotanical file: ", e$message)
      }
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

  taxon_qid <- taxon_name |>
    cascade::taxon_name_to_qid()

  example_family_qids <- example_family_names |>
    purrr::map(
      .f = cascade::taxon_name_to_qid
    )

  taxa_wd <- taxon_qid |>
    get_subtaxa_wd()

  example_family_taxa_wd <- example_family_qids |>
    purrr::map(
      .f = get_subtaxa_wd
    )

  logger::log_info(
    "We found ",
    taxa_wd |>
      nrow(),
    paste0(" ", taxon_name, " subtaxa in Wikidata")
  )
  taxa_gen <- taxa_wd |>
    tidytable::filter(taxon_rank == "Q34740") |>
    tidytable::distinct(taxon, taxon_name)
  logger::log_info(
    "... ",
    taxa_gen |>
      nrow(),
    " unique genera"
  )
  taxa_spe <- taxa_wd |>
    tidytable::filter(taxon_rank == "Q7432") |>
    tidytable::mutate(
      generic_epithet = taxon_name |>
        gsub(pattern = " .*", replacement = "")
    ) |>
    tidytable::distinct(taxon, generic_epithet)
  logger::log_info(
    "... ",
    taxa_spe |>
      nrow(),
    " unique species"
  )
  taxa_var <- taxa_wd |>
    tidytable::filter(taxon_rank == "Q767728") |>
    tidytable::mutate(
      generic_epithet = taxon_name |>
        gsub(pattern = " .*", replacement = "")
    ) |>
    tidytable::distinct(taxon, generic_epithet)
  logger::log_info(
    "... and ",
    taxa_var |>
      nrow(),
    " unique varieties"
  )

  example_family_taxa_wd |>
    purrr::imap(.f = function(taxa_wd, taxon_name) {
      log <- logger::log_info(
        "In comparison, ",
        taxon_name,
        " had ",
        taxa_wd |>
          nrow(),
        paste0(" subtaxa in Wikidata")
      )
      return(log$default$record)
    })

  taxa_ott_wd <- taxon_qid |>
    get_ott_ids_wd()

  logger::log_info(
    "We found ",
    taxa_ott_wd |>
      nrow(),
    paste0(" ", taxon_name, " subtaxa with an Open Tree of Life ID in Wikidata")
  )
  taxa_gen_ott <- taxa_gen |>
    tidytable::inner_join(taxa_ott_wd) |>
    tidytable::mutate(taxon_id_ott = normalize_ott_ids(taxon_id_ott))
  logger::log_info(
    "... ",
    taxa_gen_ott |>
      nrow(),
    " unique genera"
  )
  taxa_spe_ott <- taxa_spe |>
    tidytable::inner_join(taxa_ott_wd) |>
    tidytable::mutate(taxon_id_ott = normalize_ott_ids(taxon_id_ott))
  logger::log_info(
    "... ",
    taxa_spe_ott |>
      nrow(),
    " unique species"
  )
  taxa_var_ott <- taxa_var |>
    tidytable::inner_join(taxa_ott_wd) |>
    tidytable::mutate(taxon_id_ott = normalize_ott_ids(taxon_id_ott))
  logger::log_info(
    "... ",
    taxa_var_ott |>
      nrow(),
    " unique varieties"
  )

  example_family_taxa_compounds_wd <- example_family_qids |>
    purrr::map(.f = get_compounds_wd)

  taxa_compounds_wd <- taxon_qid |>
    get_compounds_wd()

  logger::log_info(
    "We found ",
    taxa_compounds_wd |>
      tidytable::distinct(taxon) |>
      nrow(),
    paste0(
      " ",
      taxon_name,
      " subtaxa with at least one chemical compound reported in Wikidata"
    )
  )
  if (!is.null(path_molecules) && file.exists(path_molecules)) {
    taxa_compounds_local <- path_molecules |>
      tidytable::fread() |>
      tidytable::distinct(
        compound,
        compound_smiles_canonical,
        compound_inchikey,
        taxon
      )
    logger::log_info(
      "We found ",
      taxa_compounds_local |>
        tidytable::distinct(taxon) |>
        nrow(),
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
      taxa_compounds_tot |>
        tidytable::distinct(taxon) |>
        nrow(),
      paste0(
        " ",
        taxon_name,
        " subtaxa with ethnobotanical use reported in total"
      )
    )
  } else {
    if (!is.null(path_molecules)) {
      logger::log_warn(
        "Local molecules file not found; using Wikidata-only compounds."
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

  taxa_compounds <- taxa_compounds_tot |>
    tidytable::mutate(
      connectivity_layer = compound_inchikey |>
        gsub(pattern = "-.*", replacement = "")
    ) |>
    tidytable::distinct(taxon, connectivity_layer, .keep_all = TRUE)

  example_family_taxa_compounds <- example_family_taxa_compounds_wd |>
    purrr::map(.f = function(df) {
      df |>
        tidytable::mutate(
          connectivity_layer = compound_inchikey |>
            gsub(pattern = "-.*", replacement = "")
        ) |>
        tidytable::distinct(taxon, connectivity_layer, .keep_all = TRUE)
    })

  logger::log_info(
    "... ",
    taxa_compounds |>
      tidytable::distinct(connectivity_layer) |>
      nrow(),
    " unique connectivities"
  )
  example_family_taxa_compounds |>
    purrr::imap(.f = function(taxa_compounds, taxon_name) {
      log <- logger::log_info(
        "In comparison, ",
        taxon_name,
        " had ",
        taxa_compounds |>
          tidytable::distinct(connectivity_layer) |>
          nrow(),
        paste0(" unique connectivities")
      )
      return(log$default$record)
    })

  lotus <- get_lotus_wd()
  lotus_compounds <- lotus |>
    tidytable::mutate(
      connectivity_layer = compound_inchikey |>
        gsub(pattern = "-.*", replacement = "")
    ) |>
    tidytable::distinct(taxon, connectivity_layer, .keep_all = TRUE)
  logger::log_info(
    "And ",
    lotus_compounds |>
      tidytable::distinct(connectivity_layer) |>
      nrow(),
    " unique connectivities were found among all taxa in Wikidata"
  )
  logger::log_info(
    "Among the ",
    taxa_compounds |>
      tidytable::distinct(connectivity_layer) |>
      nrow(),
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
      taxa_compounds_specific |>
        tidytable::inner_join(taxa_spe)
    ) |>
    tidytable::bind_rows(
      taxa_compounds_specific |>
        tidytable::inner_join(taxa_var)
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
  logger::log_info(
    "... ... of which ",
    taxa_compounds_specific_genus |>
      tidytable::filter(n >= n_multiple_genera) |>
      nrow(),
    " were found in at least ",
    n_multiple_genera,
    " genera within the family"
  )
  logger::log_info(
    "... ... ... these were ",
    taxa_compounds_specific_genus |>
      tidytable::filter(n >= n_multiple_genera) |>
      tidytable::pull(connectivity_layer) |>
      paste0(collapse = ", ")
  )

  taxa_gen_ott_cpd <- taxa_gen |>
    tidytable::inner_join(taxa_gen_ott) |>
    tidytable::left_join(taxa_compounds) |>
    tidytable::mutate(taxon_id_ott = normalize_ott_ids(taxon_id_ott))

  taxa_spe_ott_cpd <- taxa_spe |>
    tidytable::inner_join(taxa_spe_ott) |>
    tidytable::left_join(taxa_compounds) |>
    tidytable::mutate(taxon_id_ott = normalize_ott_ids(taxon_id_ott))

  taxa_var_ott_cpd <- taxa_var |>
    tidytable::inner_join(taxa_var_ott) |>
    tidytable::inner_join(taxa_compounds) |>
    tidytable::mutate(taxon_id_ott = normalize_ott_ids(taxon_id_ott))

  taxa_counted <- taxa_gen_ott_cpd |>
    tidytable::left_join(
      taxa_spe_ott_cpd,
      by = c("taxon_name" = "generic_epithet")
    ) |>
    tidytable::left_join(
      taxa_var_ott_cpd,
      by = c("taxon_name" = "generic_epithet")
    ) |>
    tidytable::group_by(taxon.x, taxon_id_ott.x, taxon_name) |>
    tidytable::summarize(
      compounds_count = length(unique(na.omit(
        c(compound, compound.x, compound.y)
      ))),
      species_count = sum(!is.na(unique(taxon.y)))
    ) |>
    tidytable::ungroup() |>
    # tidytable::mutate(
    #   compounds_count = tidytable::na_if(x = compounds_count, y = 0)
    # ) |>
    tidytable::rename(
      taxon = taxon.x,
      taxon_id_ott = taxon_id_ott.x
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
      tidytable::across(
        .cols = tidytable::matches("ethnobotanical_count"),
        .fns = function(x) {
          tidytable::replace_na(x, 0L)
        }
      )
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
      tidytable::filter(ethnobotanical_count >= 1L) |>
      tidytable::filter(compounds_count >= 1L) |>
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
        tidytable::select(-taxon_name),
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
    tidytable::filter(
      !compound_inchikey |>
        is.na() |
        ethnobotanical
    ) |>
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
    " had a at least one compound or ethnobotanical use reported"
  )

  logger::log_trace("Retrieving compounds classification ...")
  smiles <- taxa_compounds |>
    tidytable::distinct(compound_smiles_canonical) |>
    tidytable::pull()
  if (file.exists(path_classes)) {
    classes_known <- path_classes |>
      tidytable::fread(na.strings = "") |>
      # tidytable::filter(!is.na(glycoside)) |>
      tidytable::distinct()
    smiles_unknown <- smiles[!smiles %in% classes_known$query]
    if (smiles_unknown |> length() > 0L) {
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
  classes |>
    tidytable::fwrite(file = path_classes, sep = "\t")

  taxa_compounds_classes <- taxa_gen_ott_cpd |>
    tidytable::bind_rows(
      taxa_gen_ott_cpd |>
        tidytable::distinct(taxon_name) |>
        tidytable::left_join(
          taxa_spe_ott_cpd,
          by = c("taxon_name" = "generic_epithet")
        )
    ) |>
    tidytable::bind_rows(
      taxa_gen_ott_cpd |>
        tidytable::distinct(taxon_name) |>
        tidytable::left_join(
          taxa_var_ott_cpd,
          by = c("taxon_name" = "generic_epithet")
        )
    ) |>
    tidytable::filter(!is.na(compound_smiles_canonical)) |>
    tidytable::distinct(taxon_name, compound_smiles_canonical) |>
    tidytable::left_join(
      classes,
      by = c("compound_smiles_canonical" = "query")
    ) |>
    tidytable::distinct(
      smiles_2D = compound_smiles_canonical,
      organism = taxon_name,
      best_candidate_1 = pathway,
      best_candidate_2 = superclass,
      best_candidate_3 = class
    ) |>
    tidytable::mutate(sample = organism, species = organism) |>
    tidytable::mutate(
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
  taxa_compounds_classes_sub <- taxa_known |>
    tidytable::filter(!is.na(compound_inchikey)) |>
    tidytable::distinct(taxon, compound_smiles_canonical) |>
    tidytable::inner_join(
      taxa_known_sub |>
        tidytable::distinct(taxon, taxon_name)
    ) |>
    tidytable::distinct(taxon_name, compound_smiles_canonical) |>
    tidytable::left_join(
      classes,
      by = c("compound_smiles_canonical" = "query")
    ) |>
    tidytable::distinct(
      smiles_2D = compound_smiles_canonical,
      organism = taxon_name,
      best_candidate_1 = pathway,
      best_candidate_2 = superclass,
      best_candidate_3 = class
    ) |>
    tidytable::mutate(sample = organism, species = organism) |>
    tidytable::mutate(
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

  cascade_prepare_hierarchy <- get(
    "prepare_hierarchy",
    envir = asNamespace("cascade")
  )
  cascade_prepare_plot <- get(
    "prepare_plot",
    envir = asNamespace("cascade")
  )
  compounds_hierarchy <- taxa_compounds_classes |>
    cascade_prepare_hierarchy(type = "literature") |>
    cascade_prepare_plot() |>
    tidytable::rename(taxon_name = species)
  compounds_hierarchy_sub <- taxa_compounds_classes_sub |>
    cascade_prepare_hierarchy(type = "literature") |>
    cascade_prepare_plot() |>
    tidytable::rename(taxon_name = species)

  taxa_classes <- taxa_gen_ott_cpd |>
    tidytable::distinct(taxon_name) |>
    tidytable::inner_join(compounds_hierarchy) |>
    tidytable::relocate(taxon_name, .before = ids) |>
    tidytable::distinct(taxon_name, values, ids, color, .keep_all = TRUE)

  taxa_classes_sub <- taxa_compounds_classes_sub |>
    tidytable::rename(taxon_name = organism) |>
    tidytable::distinct(taxon_name) |>
    tidytable::inner_join(compounds_hierarchy_sub) |>
    tidytable::relocate(taxon_name, .before = ids) |>
    tidytable::distinct(taxon_name, values, ids, color, .keep_all = TRUE)

  taxa_ott_wd_ids <- taxa_counted$taxon_id_ott |>
    unique()

  taxa_ott_wd_ids_sub <- taxa_known_sub$taxon_id_ott |>
    unique()

  logger::log_trace("Retrieving Open Tree Taxonomy now... taking some time!")
  cache_dir <- resolve_cache_dir(local_cache)
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
  plots_gen$plot_tree_taxa_bars |>
    ggplot2::ggsave(
      file = export_tree_full,
      width = 21 * 2,
      height = 29.7 * 2,
      limitsize = FALSE,
      create.dir = TRUE
    )

  plots_gen$plot_tree_taxa_phyto_bars |>
    ggplot2::ggsave(
      file = export_tree_phyto,
      width = 18,
      height = 18,
      create.dir = TRUE
    )

  plots_sub$plot_tree_taxa_phyto_bars |>
    ggplot2::ggsave(
      file = export_tree_phyto_sub,
      width = 18,
      height = 29.7 * 1.7,
      limitsize = FALSE,
      create.dir = TRUE
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
      taxon_name |>
        gsub(pattern = " .*", replacement = ""),
      -compounds_count
    ) |>
    tidytable::fwrite(
      file = export_table_counts_sub,
      sep = "\t"
    )
  taxa_ethno_counted <- data.frame(taxa_ethno_counted)
  taxa_known_sub <- data.frame(taxa_known_sub)
  taxa_full <- data.frame(taxa_full)

  return(
    list(
      plots_gen = plots_gen,
      plots_sub = plots_sub,
      df_taxa_full = taxa_full,
      df_taxa_ethno_counted = taxa_ethno_counted,
      df_taxa_known_sub = taxa_known_sub,
      log_file = log_file
    )
  )
}
