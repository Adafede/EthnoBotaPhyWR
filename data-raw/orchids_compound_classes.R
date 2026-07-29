classes_tsv <- "data-raw/orchids_compounds_classes.tsv"

# Read the raw compound classification data
orchids_compound_classes <- tidytable::fread(classes_tsv)

# Clean and standardize
orchids_compound_classes$query <- as.character(orchids_compound_classes$query)
orchids_compound_classes$pathway <- as.character(
  orchids_compound_classes$pathway
)
orchids_compound_classes$superclass <- as.character(
  orchids_compound_classes$superclass
)
orchids_compound_classes$class <- as.character(orchids_compound_classes$class)
orchids_compound_classes$glycoside <- as.logical(
  orchids_compound_classes$glycoside
)

# Remove duplicates and convert to data.frame
orchids_compound_classes <- unique(as.data.frame(orchids_compound_classes))

# Store for distribution
save(
  orchids_compound_classes,
  file = "data/orchids_compound_classes.rda",
)

cat("✓ orchids_compound_classes dataset prepared\n")
cat("  Rows: ", nrow(orchids_compound_classes), "\n")
cat("  Columns: ", ncol(orchids_compound_classes), "\n")
cat(
  "  Size: ",
  format(object.size(orchids_compound_classes), units = "auto"),
  "\n"
)
