# Connect Cloud Setup

This app is prepared to publish on Posit Connect Cloud.

## What is already set up

- `app.R` is the Shiny entrypoint.
- `.gitignore` now keeps the app data that the dashboard needs:
  - `data/1_input_data`
  - `data/3_predictions`
  - `data/4_brackets`
- `.rsconnectignore` excludes local pipeline/training files from direct `rsconnect`-style publishing.
- `deploy_connect_cloud.R` writes a `manifest.json` for the app.

## Before you publish

1. Make sure the app has the latest files you want to show:
   - input data in `data/1_input_data`
   - predictions in `data/3_predictions`
   - bracket outputs in `data/4_brackets`
2. Install `rsconnect` in R if needed:

```r
install.packages("rsconnect")
```

3. Generate the manifest:

```r
setwd("C:/Users/caleb_fox2/Documents/Fun Projects/March Madness Models/March-Madness-2026")
source("deploy_connect_cloud.R")
```

This creates `manifest.json`.

## Publish on Connect Cloud

1. Create a public GitHub repository for this app.
2. Commit and push this app folder, including:
   - `app.R`
   - `R/`
   - `data/1_input_data/`
   - `data/3_predictions/`
   - `data/4_brackets/`
   - `manifest.json`
3. Go to [https://connect.posit.cloud/](https://connect.posit.cloud/).
4. Sign in with GitHub.
5. Choose to publish from your GitHub repository.
6. Select this repository and set `app.R` as the primary file if prompted.
7. Publish the app.

## How to update the app later

1. Update the code and/or refresh the saved data files locally.
2. Regenerate `manifest.json`:

```r
source("deploy_connect_cloud.R")
```

3. Commit and push the changes to GitHub.
4. In Connect Cloud, republish or sync the app from the updated repo.

## Notes

- On the free Connect Cloud path, public GitHub repos are the expected route.
- If you update predictions or bracket outputs, those refreshed files must be committed to GitHub for the hosted app to see them.
