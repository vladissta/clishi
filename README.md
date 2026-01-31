# CliShi <img border="0" src="www/clishi_logo.png" alt="CLISHI logo" width="200" align="right"/>

Shiny application for interactive analysis and simulation. This repo contains the main app (`app.R`) plus modular scripts and static assets.

## Requirements

-   R (\>= 4.5 recommended)
-   Internet access for first-time package installation

## Download the app

Option A: Clone with Git:

``` bash
git clone https://github.com/BIOSTAT/CLISHI_PROJECT.git
cd CLISHI_PROJECT
```

Option B: Download a ZIP:

1. Go to the GitHub repository page.
2. Click **Code** → **Download ZIP**.
3. Unzip the archive and open the folder in R.

## Install dependencies

From the project root in R:

``` r
source("install.R")
```

## Run the app

From the project root in R:

``` r
shiny::runApp("app.R")
```

## Project structure

-   `app.R` - main Shiny app entry point
-   `scripts/` - modular code for UI, calculations, and plots
-   `www/` - static assets (CSS/JS/images)
-   `install.R` - installs required R packages

## Troubleshooting

-   If you see missing-package errors, rerun `source("install.R")`.
-   If a CRAN mirror is required, set one before installing:

``` r
options(repos = c(CRAN = "https://cloud.r-project.org"))
```

## Authors

-   [Bektur Berdibekov](https://github.com/BekturBerdibekov)
-   [Danil Tukanov](https://github.com/DataDaniel010)
-   [Daria Kuznetcova](https://github.com/daria-kuznetcova)
-   [Mariia Akhmetova](https://github.com/Hemofixic)
-   [Vladislav Stanin](https://github.com/vladissta)
-   [Evgeny Bakin](https://github.com/evgeny-bakin)
