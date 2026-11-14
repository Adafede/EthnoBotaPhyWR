classes_tsv <- "data-raw/orchids_compounds_classes.tsv"

# Read the raw compound classification data
orchids_compound_classes <- data.table::fread(
  classes_tsv,
  fill = TRUE,
  na.strings = c("", "NA")
)

# Clean and standardize
for (col in c("query", "pathway", "superclass", "class")) {
  val <- trimws(as.character(orchids_compound_classes[[col]]))
  val[!nzchar(val)] <- NA_character_
  orchids_compound_classes[[col]] <- val
}
orchids_compound_classes$glycoside <- as.logical(
  orchids_compound_classes$glycoside
)

# Remove duplicates and convert to plain data.frame
orchids_compound_classes <- unique(as.data.frame(orchids_compound_classes))
class(orchids_compound_classes) <- "data.frame"
rownames(orchids_compound_classes) <- NULL

# Store for distribution
save(
  orchids_compound_classes,
  file = "data/orchids_compound_classes.rda",
  compress = "xz"
)

cat("✓ orchids_compound_classes dataset prepared\n")
cat("  Rows: ", nrow(orchids_compound_classes), "\n")
cat("  Columns: ", ncol(orchids_compound_classes), "\n")
cat(
  "  Size: ",
  format(object.size(orchids_compound_classes), units = "auto"),
  "\n"
)
