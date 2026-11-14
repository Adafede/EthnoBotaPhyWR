compounds_tsv <- "data-raw/orchids_table_unique_connectivity_classified.tsv"

# Read the raw compounds data
orchids_compounds <- data.table::fread(
  compounds_tsv,
  fill = TRUE,
  na.strings = c("", "NA")
)

# Clean and standardize
char_cols <- c(
  "taxon",
  "taxon_name",
  "compound",
  "compound_smiles_canonical",
  "compound_inchikey",
  "connectivity_layer",
  "pathway",
  "superclass",
  "class"
)
for (col in char_cols) {
  val <- trimws(as.character(orchids_compounds[[col]]))
  val[!nzchar(val)] <- NA_character_
  orchids_compounds[[col]] <- val
}
orchids_compounds$glycoside <- as.logical(orchids_compounds$glycoside)

# Remove duplicates and convert to plain data.frame
orchids_compounds <- unique(as.data.frame(orchids_compounds))
class(orchids_compounds) <- "data.frame"
rownames(orchids_compounds) <- NULL

# Store for documentation
save(
  orchids_compounds,
  file = "data/orchids_compounds.rda",
  compress = "xz"
)

cat("✓ orchids_compounds dataset prepared\n")
cat("  Rows: ", nrow(orchids_compounds), "\n")
cat("  Columns: ", ncol(orchids_compounds), "\n")
cat("  Size: ", format(object.size(orchids_compounds), units = "auto"), "\n")
