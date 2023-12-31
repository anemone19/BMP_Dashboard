# Global script
# Load Libraries

# Shiny Packages
# library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(DT)
library(fresh)
library(imola)

# plotting and data prep packages
library(ggplot2)
library(tidyverse)
library(scales)
library(foreach)
library(plotly)
library(readtext)
library(ggrepel)
library(RColorBrewer)
library(leaflet)
library(viridis)

# geopackages packages
library(sf)
library(rgdal)
library(readr)
library(terra)
library(RSQLite)

# Load data & modules ------------------------------------------------------------------------

# Test two years of data --------------------------------------------------------------------

load("www/Team_Data/student_text_sep.RData")
load("www/all_years.RData")

source("Modules/valuebox_module.R")

# copy survey data and change year
all_years_copy <- all_years

all_years_copy$year <- "2023/2024"
all_years_copy <- all_years_copy %>% filter(Taxa == "Micromoths")

# combine into list of datasets
all_years_list <- list(all_years,all_years_copy)
  
for(i in seq_along(all_years_list)){
  unique_year <- unique(all_years_list[[i]]$year)
  unique_year <- unique_year[complete.cases(unique_year)]
  
  # Check if there's exactly one unique year
  if(length(unique_year) == 1){
    names(all_years_list)[i] <- unique_year
  } else {
    warning(paste("Data frame", i, "has multiple unique years or no year. Skipping."))
  }
}


# UI Variables -------------------------------------------------------------------------------------------

# extract years
years <- names(all_years_list)
# years <- years[complete.cases(years)]
# Data Table Options --------------------------------------------------------------------------------------

# table options 
# scrollX = "100px": Enables horizontal scrolling and sets the width of the scrollable area to 100 pixels.
# dom = "Bfrtip": Defines the order of datatable controls.
# scrollY = '300px': Enables vertical scrolling and sets the height of the scrollable area to 300 pixels.
# paging = FALSE: Disables pagination, so all rows are displayed in a single page.
# buttons = c("csv", "excel"): Adds buttons to export the table data to CSV and Excel formats.
# deferRender = TRUE: Delays the rendering of rows until they are actually needed for display. This can improve performance for large tables.
# searching = TRUE: Enables the search functionality to filter rows based on a search query.
# editable = TRUE: Allows the cells in the table to be edited.
# lengthChange = FALSE: Disables the option for the user to change the number of entries shown per page.
# rownames = FALSE: Disables the display of row names.
# escape = FALSE: Disables the escaping of HTML entities in the table, allowing raw HTML to be rendered.
# selection = "single": Allows only a single row to be selected at a time.

table_options <- function() {
  list(
    scrollX = "100px",
    dom = "Bfrtip",
    scrollY = '300px', 
    paging = FALSE,
    buttons = c("csv", "excel"),
    deferRender = TRUE,
    searching = TRUE,
    editable = TRUE,
    lengthChange = FALSE,
    rownames = FALSE,
    escape = FALSE,
    selection = "single"
  )
}

# to republish app, run code below or use button next to Run App
# insert path to app directory 
# NB: COMMENT OUT CODE WHEN RUNNING LOCALLY 
# rsconnect::deployApp("/Users/anecloete/Desktop/BMP_Dashboard",forceUpdate = T)
 
