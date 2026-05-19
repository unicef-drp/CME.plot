# This script updates data/u5mr.iso.c and data/new_cnames

# Script to resave all .rda files in data/ folder with version = 2
# This ensures compatibility across different R versions

devtools::load_all(".")

update.data.new_cnames(IGME_round_input = 2026)

# Get all .rda files in the data directory
data_dir <- "data"
rda_files <- list.files(data_dir, pattern = "\\.rda$", full.names = TRUE)

cat("Found", length(rda_files), ".rda files:\n")
print(rda_files)
cat("\n")

# Process each .rda file
for (rda_file in rda_files) {
  cat("Processing:", rda_file, "\n")

  # Create a new environment to load into
  temp_env <- new.env()

  # Load the .rda file and get the names of objects loaded
  loaded_objects <- load(rda_file, envir = temp_env)

  cat("  - Loaded objects:", paste(loaded_objects, collapse = ", "), "\n")

  # Create a list to hold the objects for saving
  save_list <- mget(loaded_objects, envir = temp_env)

  # Save with version = 2
  save(list = loaded_objects,
       file = rda_file,
       envir = temp_env,
       version = 2,
       compress = "gzip")

  cat("  - Resaved with version = 2\n\n")
}

cat("All files have been resaved successfully!\n")
