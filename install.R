# Install required packages for this project (from app.R)
install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing)) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

packages <- c(
  "ggplot2",
  "dplyr",
  "tidyr",
  "shiny",
  "shinydashboard",
  "DT",
  "shinyjs",
  "shinycssloaders",
  "rclipboard",
  "furrr",
  "future",
  "future.callr",
  "checkmate",
  "TrialSize"
)

install_if_missing(packages)
