library(rsconnect)

# Replace these with your shinyapps.io account details before running.
account_name <- "YOUR_ACCOUNT_NAME"
app_name <- "march-madness-2026"
token <- Sys.getenv("SHINYAPPS_TOKEN")
secret <- Sys.getenv("SHINYAPPS_SECRET")

if (!nzchar(token) || !nzchar(secret)) {
  stop("Set SHINYAPPS_TOKEN and SHINYAPPS_SECRET in your environment before deploying.")
}

rsconnect::setAccountInfo(
  name = account_name,
  token = token,
  secret = secret
)

rsconnect::deployApp(
  appDir = ".",
  appName = app_name,
  launch.browser = TRUE
)
