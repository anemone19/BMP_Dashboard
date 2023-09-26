# ------------------------------------------
# LIBRARY IMPORTS
# ------------------------------------------

# Geospatial libraries
library(sf)
library(rgdal)
library(terra)
library(RSQLite)
library(sp)

# Data manipulation and visualization libraries
library(readr)
library(ggplot2)
library(tidyverse)
library(scales)
library(foreach)
library(plotly)
library(readtext)
library(cronR) # automated script running package
# cron_rstudioaddin()

# ------------------------------------------
# SHAREPOINT READ-IN
# ------------------------------------------

# pathname to Biodiversity Survey Master - CHANGE IF NOT ANE hehe

path_to_master <- "/Users/anecloete/Library/CloudStorage/OneDrive-UniversityofStAndrews/Biodiversity KPI mapping Master/Biodiversity Survey Master"

# Obtain all geopackage files from the specified directory
all_gpkgs <- list.files(
  path = paste(path_to_master),
  recursive = TRUE,
  pattern = "\\.gpkg$",
  full.names = TRUE
)

# Remove geopackages with 'Polygon' in their name (if necessary) and save
survey_polygons <- all_gpkgs[grepl("Habitat_Polygon", all_gpkgs)]

# all_gpkgs <- all_gpkgs[!grepl("Polygon", all_gpkgs)]

# Exclude gpkgs based on certain keywords in their names
pattern <- "Polygon|DESKTOP|PC|LAPTOP|Wills"
all_gpkgs <- all_gpkgs[!grepl(pattern, all_gpkgs)]

# excluded_keywords <- c("DESKTOP", "PC", "LAPTOP", "Wills")
# taxa_dat <- taxa_dat[!sapply(names(taxa_dat), function(x) any(sapply(excluded_keywords, grepl, x = x)))]

# Exclude specific problematic geopackage
problematic_gpkg <- paste0(path_to_master,"/Erica/Erica Backup/Ericabutterflies.gpkg")
all_gpkgs <- all_gpkgs[all_gpkgs != problematic_gpkg]

# Read all geopackages into a list
myfiles <- lapply(all_gpkgs, st_read)

# ------------------------------------------
# DATA PREPARATION & CLEANING
# ------------------------------------------

# Transform coordinate reference system from WGS 84 / Pseudo-Mercator to standard lat long (EPSG:4326)
taxa_dat <- lapply(myfiles, function(x) st_transform(x, 4326))

# Convert spatial data frames to regular data frames
taxa_dat <- lapply(taxa_dat, as.data.frame)

# Name each data frame in the list based on the geopackage filename
names(taxa_dat) <- basename(all_gpkgs)

# Remove any duplicate data frames by name
taxa_dat <- taxa_dat[!duplicated(names(taxa_dat))]

# ------------------------------------------
# TREE ENTRIES PACKAGE PREPARATION
# ------------------------------------------

# Modify certain columns for 'Tree Species Entries.gpkg' so that bind_rows works 
taxa_dat$`Tree Species Entries.gpkg` <- taxa_dat$`Tree Species Entries.gpkg` %>%
  rename(Species = 1, Date = 2) %>%
  mutate(
    Species = case_when(
      Species %in% c("unknown/other", "Unknown/other") ~ comments.unlisted.species,
      Species == "Unknown young pine " ~ "Unknown young pine",
      Species == "Willow x10" ~ "Willow",
      TRUE ~ Species
    ),
    taxa = "Vascular Plants",
    Observer = NA,
    photoid = NA,
    Count = NA,
    Other = NA,
    Speciesful = NA
  ) %>%
  filter(Species != "") %>%
  select(Species, Date, taxa, Count)


# Date Processing: Convert, split and extract components of the 'Date' column
taxa_dat <- lapply(taxa_dat, function(df) {
  
  df$Date <- as.character(df$Date)
  
  df <- df %>%
    separate(Date, into = c("date", "time"), sep = " (?=[^ ]+$)") %>%
    mutate(
      date = ymd(gsub("/", "-", date)),
      year = year(date),
      month = month(date),
      day = day(date)
    )
  return(df)
})

# Extract and remove the tree dataframe from the list
tree_data <- taxa_dat$`Tree Species Entries.gpkg`
taxa_dat$`Tree Species Entries.gpkg` <- NULL

# Combine all data frames in the list into a single data frame
taxa_comb <- bind_rows(taxa_dat)

# Clean and transform taxa data for further analysis
taxa_clean <- taxa_comb %>%
  drop_na(taxa) %>%
  as.character(df$Date) %>% 
  separate(Date, into = c("date", "time"), sep = " (?=[^ ]+$)") %>%
  mutate(
    date = ymd(gsub("/", "-", date)),
    year = year(date),
    month = month(date),
    day = day(date)
  ) %>% 
  filter(!(taxa == "hoverfly" & Observer == "Erica")) %>% # remove Erica hoverfly entries 
  unite(collapsed_species, specieslatin:seaweedlatin, sep = ",", na.rm = TRUE) %>%
  mutate_at(vars(Species, collapsed_species), na_if, "") %>%
  mutate(
    Species = coalesce(Species, SpeciesSci, Speciesfull, collapsed_species, species, Other),
    photoid = if_else(is.na(photoid), NA, paste0(photoid, ".jpg")),
    Count = ifelse(is.na(Count), 1, Count),
    taxa = recode(taxa, tree = "Vascular Plants"),
    year = recode(year, `2023` = "2022/2023", `2022` = "2022/2023")
  ) %>%
  filter(!str_detect(Species, "(?i)unknown"), taxa != "bee") %>%
  filter_at(vars(taxa, Observer), all_vars(!is.na(.))) %>%
  mutate(
    taxa = recode(taxa, 
                  plant = "Vascular Plants",
                  bird = "Birds",
                  macromoth = "Macromoths",
                  micromoth = "Micromoths",
                  butterfly = "Butterflies",
                  dragonfly = "Dragonflies",
                  hoverfly = "Hoverflies",
                  bat = "Bats",
                  amphibian = "Amphibians",
                  reptileamphibian = "Amphibians",
                  bumblebee = "Bumblebee",
                  mammal = "Mammals",
                  ladybird = "Ladybirds",
                  tree = "Vascular Plants"),
    year = recode(year,
                  "2023" = "2022/2023",
                  "2022" = "2022/2023"),
    Observer = recode(Observer,
                      Other1 = "Cori")) %>%
  select(Species, SpeciesSci, Count, date, Observer, taxa, photoid, geometry, year, day) %>%
  rename(
    Date = date,
    Taxa = taxa,
    PhotoID = photoid
  )

# tree data prep for bind
tree_data <- tree_data %>%
  mutate(
    year = ifelse(year == "2023" | year == "2022", "2022/2023", year),
    Count = 1 # add column count 
  ) %>%
  rename(
    Date = date,
    Taxa = taxa
  )

# Merge tree data with the cleaned data
all_years <- bind_rows(taxa_clean, tree_data)

# split into lat and long for mapping 
all_years <- all_years %>% mutate(long = unlist(map(geometry,1)),
           lat = unlist(map(geometry,2)))

# Save the resulting dataframe
save(all_years, file = "all_years.RData")
write.csv(all_years, file = "all_years.csv")

# Pull Photos ---------------------------------------------------------------

# read in all photo files within Survey_Photos in OneDrive Folder
all_Tphotos <- list.files(
  path =   paste0(path_to_master,"/Team_data"),
  recursive = TRUE,
  pattern = "\\.jpg$",
  full.names = FALSE
)

www_Tfolder_path <- file.path(getwd(), "www/Team_Data")

photo_move <- file.copy(
  from = file.path(paste0(path_to_master,"/Team_data"), all_Tphotos),
  to = file.path(paste(www_Tfolder_path), all_Tphotos)
)

# read in all photo files within Survey_Photos in OneDrive Folder
all_Sphotos <- list.files(
  path = paste0(path_to_master,"/Survey_Photos"),
  recursive = TRUE,
  pattern = "\\.jpg$",
  full.names = FALSE
)

www_Sfolder_path <- file.path(getwd(), "www/Survey_Photos")

photo_move <- file.copy(
  from = file.path(paste0(path_to_master,"/Survey_Photos"), all_Sphotos),
  to = file.path(paste(www_Sfolder_path), all_Sphotos)
)

# Pull Documents ---------------------------------------------------------------

# read in word document containing descriptions

# read in all photo files within Survey_Photos in OneDrive Folder
doc <- list.files(
  path = paste0(path_to_master,"/Team_Data"),
  recursive = TRUE,
  pattern = "student_aboutme.docx$",
  full.names = FALSE
)

www_Tfolder_path <- file.path(getwd(), "www/Team_Data")

doc_move <- file.copy(
  from = file.path(paste0(path_to_master,"/Team_Data"), doc),
  to = file.path(paste(www_Tfolder_path), doc)
)

# Specify the path to the Word document and the search word
# docx_path <- "/Users/anecloete/Library/CloudStorage/OneDrive-UniversityofStAndrews/Biodiversity KPI mapping Master/Team_Data/student_aboutme.docx"

student_text <- readtext("www/Team_Data/student_aboutme.docx")$text

# Split text into parts using new line character:
student_text_sep <- strsplit(student_text, "\n")[[1]]
save(student_text_sep, file = "www/Team_Data/student_text_sep.RData")

# ------------------------------------------
# SURVEY POLYGON 
# ------------------------------------------

# Read all geopackages into a list
survey_file <- st_read(survey_polygons)


# Transform coordinate reference system from WGS 84 / Pseudo-Mercator to standard lat long (EPSG:4326)
poly_sf <- st_as_sf(survey_file)
poly_sf <- st_transform(poly_sf, 4326)

poly_dat <- as.data.frame(poly_sf)

# split into lat and long for mapping 
poly_sf <- poly_dat %>% mutate(long = unlist(map(geometry,1)),
                                  lat = unlist(map(geometry,2)))

save(poly_sf,file="polygons_sf.RData")

# Create Master Geopackages ------------------------------------------------------------------------------------------
# write all sf to one geopackage
# ignore warning, it still creates the geopackges

# for(i in 1:length(myfiles)){
#   myfiles[[i]] <- myfiles[[i]] %>% mutate(taxa = ifelse(taxa == "tree","plant",taxa))
# }
#
#
# st_write(myfiles[[1]], "master_test.gpkg",
#   unique(paste0(substr(myfiles[[1]]$Observer, 1, 1), myfiles[[1]]$taxa)),
#   delete_dsn = TRUE
# )
#
# # append all other layers to master geopackages
# for (i in 2:length(myfiles)) {
#   st_write(myfiles[[i]], "master_test.gpkg",
#     unique(paste0(substr(myfiles[[i]]$Observer, 1, 1), myfiles[[i]]$taxa)),
#     append = TRUE
#   )
# }

# Dropbox file read-in ------------------------------------------------------------------
# library(rdrop2)
# # authenticate with token
# drop_auth(rdstoken = "droptoken.rds")
#
# # list files in dropbox, check master file is there
# drop_dir("/Biodiversity KPI mapping Master")
#
# # get folders of all observers in test final database
# files <- drop_dir("/Biodiversity KPI mapping Master/Test final database")
#
# # get all surveyor geopackages
# for (i in 1:length(files)) {
#   if (files[i, 1] == "folder") {
#     subfolder <- drop_dir(paste0("/Biodiversity KPI mapping Master/Test final database/", files[i, ]$name))
#     geopackages <- subfolder[grepl("\\.gpkg$", subfolder$name), ]
#
#     for (file in geopackages$path_display) {
#       print(file)
#       drop_download(file,
#                     overwrite = TRUE,
#                     local_path = "geopackages"
#       )
#     }
#   } else {
#     break
#   }
# }

# add tree geopackage, commented out because file does not get updated

# drop_download("/Biodiversity KPI mapping Master/Test final database/Tree Species Entries.gpkg",
#               overwrite = TRUE,
#               local_path = "geopackages"
# )
