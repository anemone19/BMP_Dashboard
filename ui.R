# UI for BMP Dashboard ---------------------------------------------------------------------------------------------------

# source tabItems
source("TabItems/Tab_KPI.R")
source("TabItems/Tab_Taxa_Explorer.R")
source("TabItems/Tab_Student_Engagement.R")
source("TabItems/Tab_About.R")
source("TabItems/Tab_Record_Finder.R")


# 1. HEADER ---------------------------------------------------------------------------------------------------

header <- dashboardHeader(
  title = "Biodiversity Monitoring Programme",
  titleWidth = 320
)

# 2. SIDEBAR ---------------------------------------------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar",
    
    # select survey year
    selectInput(inputId = "year", label = "Select year", choices = years),
    
    # About Page
    menuItem("About", tabName = "about", icon = icon("circle-info")),
    
    # first menu item: Key Performance Indices
    menuItem("Key Performance Indices", tabName = "kpi", icon = icon("gauge")),
    
    # second menu item: Taxa Explorer
    menuItem("Taxa Explorer", tabName = "taxa_expl", icon = icon("crow")),
    
    # Show panel only when taxa explorer sidebar is selected
    useShinyjs(),
    
    # select taxa
    div(
      id = "taxa_cond",
      conditionalPanel(
        "input.sidebar == 'taxa_expl'",
        selectInput(inputId = "taxa_select", label = "Select taxa", choices = NULL)
      )
    ),
    
    # third menu item: Student Engagement
    menuItem("Student Engagement", tabName = "stud_expl", icon = icon("graduation-cap")),
    
    # select Observer
    div(
      id = "student_cond",
      conditionalPanel(
        "input.sidebar == 'stud_expl'",
        selectInput(inputId = "student_select", label = "Select Student", choices = NULL)
      )
    ),
    
    # fourth menu item: Record & Photos
    menuItem("Record Finder", tabName = "record_finder", icon = icon("camera-retro"))
  )
)

# 3. BODY ---------------------------------------------------------------------------------------------------

body <- dashboardBody(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    Tab0,
    Tab1,
    Tab2,
    Tab3,
    Tab4
  )
)

# 4. UI Function ---------------------------------------------------------------------------------------------------

ui <- dashboardPage(header, sidebar, body)
