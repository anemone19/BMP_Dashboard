# Install and load required packages 

# List of packages
packages <- c(
  "shiny",
  "tidyverse",
  "shinyWidgets",
  "shinydashboard",
  "ggplotify",
  "plotly",
  "sf",
  "rgdal",
  "terra",
  "RSQLite",
  "sp",
  "readr",
  "ggplot2",
  "scales",
  "foreach",
  "readtext"
)


# Install and load packages
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}



