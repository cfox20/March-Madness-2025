if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Install rsconnect first with install.packages('rsconnect').")
}

rsconnect::writeManifest(
  appDir = ".",
  appPrimaryDoc = "app.R"
)

message("manifest.json written. Commit the app folder to a public GitHub repo, then publish it from Posit Connect Cloud.")
