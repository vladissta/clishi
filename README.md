# CliShi <img border="0" src="www/clishi_logo.png" width = 200 align="right"/>

Shiny application for interactive analysis and simulation. This repo contains the main app (`app.R`) plus modular scripts and static assets.

## Requirements

-   R (\>= 4.5 recommended)
-   Internet access for first-time package installation

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
