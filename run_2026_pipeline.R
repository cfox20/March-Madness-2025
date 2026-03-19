source("R/config.R")
source("R/pipeline_options.R")

pipeline_options <- load_pipeline_options("pipeline_run_config.yml")

source_stage_env <- function(script_path, options) {
  stage_env <- new.env(parent = globalenv())
  stage_env$pipeline_options <- options
  stage_env$options <- options
  source(script_path, local = stage_env)
  stage_env
}

run_full_pipeline <- function(options = pipeline_options) {
  options <- resolve_pipeline_options(options)

  if (options$stages$prepare_data) {
    source_stage_env("prepare_model_data.R", options)
  }

  train_env <- source_stage_env("train_xgboost.R", options)
  predict_env <- source_stage_env("predict_matchups.R", options)
  bracket_env <- source_stage_env("create_brackets.R", options)

  if (options$stages$train_models) {
    train_env$run_train_xgboost(options)
  }

  if (options$stages$predict_matchups) {
    predict_env$run_predict_matchups(options)
  }

  if (options$stages$create_brackets) {
    bracket_env$run_create_brackets(options)
  }

  invisible(options)
}

run_full_pipeline(pipeline_options)
