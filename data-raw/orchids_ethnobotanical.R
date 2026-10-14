ethnobotanical_tsv <- "data-raw/Species_medicinal_output_with_ott_id.tsv"

# Read the raw ethnobotanical data
orchids_ethnobotanical <- tidytable::fread(ethnobotanical_tsv)

# Convert to data.frame for processing
orchids_ethnobotanical <- as.data.frame(orchids_ethnobotanical)

# Remove empty/NA-only columns
orchids_ethnobotanical <- orchids_ethnobotanical[,
  colSums(!is.na(orchids_ethnobotanical)) > 0,
  drop = FALSE
]

# Clean and standardize
orchids_ethnobotanical$taxon_name <- as.character(
  orchids_ethnobotanical$taxon_name
)
orchids_ethnobotanical$taxon <- as.character(orchids_ethnobotanical$taxon)
orchids_ethnobotanical$taxon_id_ott <- as.character(
  orchids_ethnobotanical$taxon_id_ott
)

# Remove rows with missing critical IDs
orchids_ethnobotanical <- orchids_ethnobotanical[
  !is.na(orchids_ethnobotanical$taxon_id_ott),
]

# Remove duplicates
orchids_ethnobotanical <- unique(orchids_ethnobotanical)

# Store for distribution
save(
  orchids_ethnobotanical,
  file = "data/orchids_ethnobotanical.rda"
)

cat("✓ orchids_ethnobotanical dataset prepared\n")
cat("  Rows: ", nrow(orchids_ethnobotanical), "\n")
cat("  Columns: ", ncol(orchids_ethnobotanical), "\n")
cat(
  "  Size: ",
  format(object.size(orchids_ethnobotanical), units = "auto"),
  "\n"
)
