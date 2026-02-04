# CliShi <img border="0" src="www/clishi_logo.png" alt="CLISHI logo" width="200" align="right"/>

Interactive **Cli**nical Research Simulator - **Shi**ny Application. The simulator serves as a platform for learning important statistical concepts and interactive software for performing common clinical research tasks

## Motivation

- Researchers often do not code in R
- Interactive and visual approach makes easier to understand main statistical concepts
- Modelling clinical trials is an easy and effective way to explore statistical methods without having a real data
- Educational purposes: visual explanation of complex statistical ideas

**[Shiny]() allows to create interactive applications to make statistical decisions easier, visually and without coding**

## Contents

1. [Block 1. Hypothesis Modeling and Testing](#block-1-hypothesis-modeling-and-testing)
2. [Block 2. Evaluation of test results dependence on parameter values](#block-2-evaluation-of-test-results-dependence-on-parameter-values)
3. [Block 3. Sample size calculation using classical methods](#block-3-sample-size-calculation-using-classical-methods)
4. *New blocks are planned to be added...*

### Block 1. Hypothesis Modeling and Testing

<p align="center">
  <img src="./demo/block1.gif" alt="block1" width="600"/>
</p>

### Block 2. Evaluation of test results dependence on parameter values

<p align="center">
  <img src="./demo/block2.gif" alt="block1" width="600"/>
</p>

### Block 3. Sample size calculation using classical methods

<p align="center">
  <img src="./demo/block3.gif" alt="block1" width="600"/>
</p>

## Parallel computation speed

|**Number of CPUs used** |   1  |   2  |   3  |   4  |   5  |   6  |  7  |
|:-----------------------:|:----:|:----:|:----:|:----:|:----:|:----:|:---:|
|  **Simulation time of Chi-Square test**<br>(29 steps grid, 10000 samples), s | 49.5 | 30.6 | 22.6 | 18.9 | 16.5 | 15.5 |  14 |
| **Simulation time Brunner-Munzel test**<br>(9 steps grid, 1000 samples), s | 25 | 14.3 | 11.4 | 10.8 |  8.5 |  8.6 | 9.3 |

*Tested on 11th Gen Intel(R) Core(TM) i7-11800H @ 2.30GHz, 8 cores. OS: Windows*

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
