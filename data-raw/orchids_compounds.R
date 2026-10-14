compounds_tsv <- "data-raw/orchids_table_unique_connectivity_classified.tsv"

# Read the raw compounds data
orchids_compounds <- tidytable::fread(compounds_tsv)

# Clean and standardize
orchids_compounds$taxon <- as.character(orchids_compounds$taxon)
orchids_compounds$compound <- as.character(orchids_compounds$compound)
orchids_compounds$glycoside <- as.logical(orchids_compounds$glycoside)

# Remove duplicates and convert to data.frame
orchids_compounds <- unique(as.data.frame(orchids_compounds))

# Store for documentation
save(
  orchids_compounds,
  file = "data/orchids_compounds.rda"
)

cat("✓ orchids_compounds dataset prepared\n")
cat("  Rows: ", nrow(orchids_compounds), "\n")
cat("  Columns: ", ncol(orchids_compounds), "\n")
cat("  Size: ", format(object.size(orchids_compounds), units = "auto"), "\n")
