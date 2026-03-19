generated_files <- c(
  MODEL_DATA_FILES$mens_train,
  MODEL_DATA_FILES$womens_train,
  MODEL_DATA_FILES$mens_tourney,
  MODEL_DATA_FILES$womens_tourney,
  MODEL_INPUT_FILES$mens_train,
  MODEL_INPUT_FILES$mens_valid,
  MODEL_INPUT_FILES$mens_features,
  MODEL_INPUT_FILES$womens_train,
  MODEL_INPUT_FILES$womens_valid,
  MODEL_INPUT_FILES$womens_features,
  PREDICTION_FILES$mens,
  PREDICTION_FILES$womens,
  PREDICTION_FILES$submission,
  Sys.glob(file.path(BRACKETS_DIR, "march_madness_bracket_pool_*.csv")),
  Sys.glob(file.path(BRACKETS_DIR, "march_madness_round_probabilities_*.csv")),
  MODEL_FILES$mens,
  MODEL_FILES$womens,
  FEATURE_FILES$mens,
  FEATURE_FILES$womens
)

generated_files <- unique(generated_files[file.exists(generated_files)])

if (length(generated_files) > 0) {
  file.remove(generated_files)
  message("Removed generated files:")
  message(paste(generated_files, collapse = "\n"))
} else {
  message("No generated files found.")
}
