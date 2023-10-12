# TAB 0: About ----------------------------------------------------------------------------

# Tab Descriptions:

# Overview Panel Content ------------------------------------------------------------------------------

overview_panel <- tabPanel(
  title = "Overview",
  tags$div(
    class = "landing-wrapper",
    # child element 1: images
    tags$div(
      class = "landing-block background-content",
      # top left
      img(src = "group_photo2.jpeg"),
      
      # top right
      img(src = "TomButterfly020823HolBl.jpg"),
      
      # bottom left
      img(src = "TomMoth120823JulyHfly.jpg"),
      
      # bottom right
      img(src = "group_photo.jpeg")
    ),
    
    # child element 2: content
    tags$div(
      class = "landing-block foreground-content",
      tags$div(
        class = "foreground-text",
        # tags$h2("Biodiversity Monitoring Programme Data Dashboard"),
        tags$p("Welcome to the University of St Andrews Biodiversity Monitoring Program Data Dashboard! We are thrilled to have you here as we embark on a journey through the rich tapestry of nature. This data dashboard is the culmination of the hard work and dedication of our university students, who spent their summer immersed in the natural world, collecting invaluable biodiversity data."),
        tags$p("Explore the wonders of the diverse ecosystems that surround us and gain insights into the intricate web of life that thrives within our region. Our mission is not only to deepen our understanding of the natural world but also to contribute to the conservation efforts that safeguard these vital ecosystems."),
        tags$p("On this dashboard, you will find a wealth of information, from species counts and distribution maps to trends and observations. Whether you are a fellow researcher, a student, or simply a nature enthusiast, we invite you to delve into the data and be inspired by the beauty and complexity of our local environment."),
        tags$p("Together, we can make a difference in preserving and protecting the biodiversity of the University of St Andrews and beyond. Thank you for joining us on this journey, and may this dashboard serve as a valuable resource for your exploration and learning.")
      )
    )
  )
)

# KPI Panel Content ------------------------------------------------------------------------------

# Descriptions for server side valueboxes

# ABI score descriptions
par_s_h <- "Description of species and habitat score."

par_area <- "Description of area managed for biodiversity score"

par_engage <- "Description of biodiversity engagement score."

# BBI score descriptions
par_m_s <- "Description of mean species richness per hectare."

par_l_s <- "Description of local species richness per hectare."

par_fife <- "Description of Fife species richness per hectare."

# Panel code

kpi_panel <- tabPanel(
  title = "Key Performance Indices",
  h3("Annual Biodiversity Index (ABI)"),
  h5("‘Real’ KPI score for every year, scaled to 100 as the benchmark established in the first year. There are three separate measures: a species and habitat richness score, an area managed for biodiversity score and a biodiversity engagement score.
           The metric is used to measure goal achievement and identify project allocation or success."),
  fluidRow(
    valueBoxOutput("S_H_about"),
    valueBoxOutput("area_about"),
    valueBoxOutput("engagement_about")
  ),
  h3("Benchmark Biodiversity Index (BBI)"),
  h5("Benchmark average species score across all measured sites in a region and is used to assess institution performance & identify successful management plans."),
  fluidRow(
    valueBoxOutput("MS_about"),
    valueBoxOutput("local_about"),
    valueBoxOutput("fife_about")
  ),
  h3("Change Biodiversity Index (CBI)"),
  h5("Annual change at sites and is found by subtracting the present year’s Abi value from that of the preceding year.
           CBI is used to site potential/response to management and core performance number to graph rate of change.",
     "Note CBI metrics are only applicable for years other than the first year (2022/2023)."),
  br()
)

# Taxa Explorer Panel Content ------------------------------------------------------------------------------

taxa_explorer_panel <- tabPanel(
  title = "Taxa Explorer",
  h4("Description:"),
  p("The Taxa Explorer Tab allows you to explore the data collected by taxa, providing insights into species composition, geographical distribution, and detailed record information."),
  h4("User Guide:"),
  h5("1. Select Taxa:"),
  p("Use the dropdown menu to select a specific taxon (e.g., Bird, Mammals, Dragonflies). This choice will be the focus of your exploration."),
  h5("2. Summary Tab:"),
  p("Here you can view the pie chart representing the top 10 species composition and check the value boxes for the number of distinct species, total records, and the top observer.",
    "You can also see a bar chart showing the top 50 species with the most records collected!"
  ),
  h5("3. Map Tab:"),
  p(
    "Explore the leaflet map with markers representing records of the selected taxa to see where the records have been collected.",
    "Each marker on the map corresponds to a specific record, providing a spatial overview."
  ),
  h5("4. Species List and Records Tab:"),
  p("Below the map, a species list is shown where you can see which species have been observed. In the same panel, there is a list of records made for your chosen taxa.",
    "Click on a row in the Records data table to view the associated photo in a modal dialog."
  ),
  h4("Tips:"),
  p("Dynamic Updates: All visualizations and summaries dynamically update based on your taxa selection."),
  p("Interactivity: Click on elements such as data points in charts or rows in tables for more details."),
  p("Explore and Analyze: Use the app to gain insights into species distribution, observer contributions, and more."),
  h4("Important:"),
  p("Ensure that you've selected a taxon before exploring different tabs."),
  p("Some functionalities may depend on the availability of data such as photos.")
)

# Student Engagement Panel Content------------------------------------------------------------------------------

student_engagement_panel <- tabPanel(
  title = "Student Engagement",
  h4("Description:"),
  p(
    "The \"Student Engagement Explorer\" is a dedicated tab within the app designed to provide detailed insights into the biodiversity data collected by individual students. Users can select a specific student from a dropdown menu, and the tab dynamically updates to display key statistics, a photo of the student, and visualizations related to their biodiversity observations."
  ),
  h4("User Guide:"),
  h5("1. Select Student:"),
  p(
    "Utilize the dropdown menu to choose a specific student. The tab content will dynamically adjust based on your selection."
  ),
  h5("2. Student Overview:"),
  p(
    "A brief description of the student and their engagement is provided. Three boxes will showcase the total number of biodiversity records collected by the student, ",
    "the number of days during which the student conducted biodiversity surveys and the most frequently recorded taxonomic group by the student. "
  ),
  h5("3. Biodiversity Distribution Chart:"),
  p(
    "A bar chart visualizes the distribution of species recorded by the student across different taxonomic groups."
  ),
  h5("4. Detailed Data Table:"),
  p(
    "A detailed data table showcases the student's biodiversity records, including species names, counts, observation dates, associated taxa, and photo IDs."
  ),
  h4("Tips:"),
  p("The content dynamically updates based on the selected student, allowing you to explore different students' contributions."),
  p("Click on elements such as bars in the chart or rows in the table for more detailed information."),
  p("Gain insights into the student's engagement, top taxa recorded, and overall biodiversity contribution."),
  h4("Important:"),
  p("Ensure you've selected a student from the dropdown before exploring the tab."),
  p("Some functionalities may depend on the availability of specific data, such as photos and detailed taxonomic information."),
  p("Enjoy exploring the unique biodiversity contributions of each student with the Student Engagement Explorer!")
)

# Record Finder Panel Content ------------------------------------------------------------------------------

record_finder_panel <- tabPanel(
  title = "Record Finder",
  h4("Description:"),
  p("The Photo Finder provides you with the capability to search for and view survey record photos based on a unique PhotoID."),
  h4("User Guide:"),
  h5("1. Photo Gallery:"),
  p(
    "Upon entering the 'Record Finder' tab, you'll encounter a captivating photo gallery showcasing a selection of survey record images.",
  ),
  h5("2. Find a Specific Record:"),
  p(
    "In the central content area, you'll find an input box labeled 'Enter PhotoID (without extension).'",
    "Here, you can input the unique identifier which you found either in the Taxa Explorer or the Student Engagment tab and view the survey image associated with the specific record."
  ),
  h5("3. View Button:"),
  p(
    "After entering the PhotoID, click the 'View Image' button to retrieve and display the corresponding survey record photo."
  ),
  h5("4. Image Display:"),
  p(
    "If a matching record photo is found, it will be displayed prominently on the screen.",
  ),
  h4("Tips for Users:"),
  p("In case the entered PhotoID does not correspond to any available image, an error message will be displayed."),
  p("Ensure the PhotoID is entered correctly to view the specific record photo."),
  p("If an error occurs, double-check the PhotoID and try again."),
  p("Enjoy exploring the diverse survey record photos available in the gallery and discovering the details captured during biodiversity surveys."),
  h4("Important:"),
  p("The images in the gallery are randomly selected and do not necessarily correspond to the PhotoID you enter."),
  p("The success of the search depends on the availability of the specified survey record photo.")
)

# Tab Code --------------------------------------------------------------------------------------------------------------

Tab0 <- tabItem(
  tabName = "about",
  tabBox(
    width = "100%",
    id = "tabset1",
    overview_panel,
    kpi_panel,
    taxa_explorer_panel,
    student_engagement_panel,
    record_finder_panel
  )
)
