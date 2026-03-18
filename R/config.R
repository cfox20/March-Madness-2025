DATA_DIR <- "data"
INPUT_DIR <- file.path(DATA_DIR, "1_input_data")
MODEL_DIR <- file.path(DATA_DIR, "2_models")
MODEL_DATA_DIR <- file.path(MODEL_DIR, "1_training_data")
TRAINED_MODEL_DIR <- file.path(MODEL_DIR, "2_saved_models")
MODEL_INPUT_DIR <- file.path(MODEL_DIR, "3_model_input_data")
PREDICTIONS_DIR <- file.path(DATA_DIR, "3_predictions")
BRACKETS_DIR <- file.path(DATA_DIR, "4_brackets")
PLOTS_DIR <- file.path(DATA_DIR, "5_plots")
SUBMISSIONS_DIR <- file.path(DATA_DIR, "6_kaggle_submissions")

latest_available_season <- function(data_dir = INPUT_DIR) {
  mens_seed_file <- file.path(data_dir, "MNCAATourneySeeds.csv")
  womens_seed_file <- file.path(data_dir, "WNCAATourneySeeds.csv")

  if (!file.exists(mens_seed_file) || !file.exists(womens_seed_file)) {
    return(as.integer(format(Sys.Date(), "%Y")) - 1L)
  }

  mens_latest <- max(utils::read.csv(mens_seed_file, stringsAsFactors = FALSE)[["Season"]], na.rm = TRUE)
  womens_latest <- max(utils::read.csv(womens_seed_file, stringsAsFactors = FALSE)[["Season"]], na.rm = TRUE)

  max(mens_latest, womens_latest)
}

TARGET_SEASON <- latest_available_season()

get_tournament_season <- function(options = NULL) {
  if (is.null(options) || is.null(options$tournament_season)) {
    return(TARGET_SEASON)
  }

  as.integer(options$tournament_season)
}

get_sample_submission_season <- function(data_dir = INPUT_DIR) {
  sample_submission_file <- file.path(data_dir, "SampleSubmissionStage2.csv")

  if (!file.exists(sample_submission_file)) {
    return(NA_integer_)
  }

  submission <- utils::read.csv(sample_submission_file, stringsAsFactors = FALSE, nrows = 1)

  if (!"ID" %in% names(submission) || nrow(submission) == 0) {
    return(NA_integer_)
  }

  as.integer(strsplit(submission$ID[[1]], "_", fixed = TRUE)[[1]][1])
}

get_bracket_output_file <- function(options = NULL) {
  file.path(
    BRACKETS_DIR,
    paste0("march_madness_bracket_pool_", get_tournament_season(options), ".csv")
  )
}

get_bracket_round_summary_file <- function(options = NULL) {
  file.path(
    BRACKETS_DIR,
    paste0("march_madness_round_probabilities_", get_tournament_season(options), ".csv")
  )
}

MODEL_FILES <- list(
  mens = file.path(TRAINED_MODEL_DIR, "xgb_mens.ubj"),
  womens = file.path(TRAINED_MODEL_DIR, "xgb_womens.ubj")
)

FEATURE_FILES <- list(
  mens = file.path(TRAINED_MODEL_DIR, "xgb_mens_features.rds"),
  womens = file.path(TRAINED_MODEL_DIR, "xgb_womens_features.rds")
)

CALIBRATION_FILES <- list(
  mens = file.path(TRAINED_MODEL_DIR, "xgb_mens_platt.rds"),
  womens = file.path(TRAINED_MODEL_DIR, "xgb_womens_platt.rds")
)

PREDICTION_FILES <- list(
  mens = file.path(PREDICTIONS_DIR, "mens_preds.rds"),
  womens = file.path(PREDICTIONS_DIR, "womens_preds.rds"),
  mens_seed_report = file.path(PREDICTIONS_DIR, "mens_seed_history_report.csv"),
  womens_seed_report = file.path(PREDICTIONS_DIR, "womens_seed_history_report.csv")
)

SUBMISSION_FILES <- list(
  kaggle = file.path(SUBMISSIONS_DIR, "xgb_submission.csv"),
  kaggle_review = file.path(SUBMISSIONS_DIR, "xgb_submission_review.csv")
)

MODEL_DATA_FILES <- list(
  mens_train = file.path(MODEL_DATA_DIR, "ff_mod_data.rds"),
  womens_train = file.path(MODEL_DATA_DIR, "w_ff_mod_data.rds"),
  mens_tourney = file.path(MODEL_DATA_DIR, "m_tourney_data.rds"),
  womens_tourney = file.path(MODEL_DATA_DIR, "w_tourney_data.rds")
)

MODEL_INPUT_FILES <- list(
  mens_train = file.path(MODEL_INPUT_DIR, "mens_train_input.csv"),
  mens_valid = file.path(MODEL_INPUT_DIR, "mens_valid_input.csv"),
  mens_features = file.path(MODEL_INPUT_DIR, "mens_feature_names.csv"),
  womens_train = file.path(MODEL_INPUT_DIR, "womens_train_input.csv"),
  womens_valid = file.path(MODEL_INPUT_DIR, "womens_valid_input.csv"),
  womens_features = file.path(MODEL_INPUT_DIR, "womens_feature_names.csv")
)
