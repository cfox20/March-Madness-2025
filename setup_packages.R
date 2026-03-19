required_packages <- c(
  "Metrics",
  "data.table",
  "dplyr",
  "furrr",
  "future",
  "glue",
  "here",
  "janitor",
  "mltools",
  "readr",
  "rlang",
  "shiny",
  "slider",
  "stringr",
  "tibble",
  "tidyr",
  "tidyverse",
  "xgboost"
  ,"yaml"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}
