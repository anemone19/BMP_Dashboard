# TAB 0: About ----------------------------------------------------------------------------
# Create the Tab0

# Tab Descriptions: 

## Taxa Explorer: 

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
           CBI is used to site potential/response to management and core performance number to graph rate of change."),
    br()
)

taxa_explorer_panel <- tabPanel(
  title = "Taxa Explorer",
  p("Welcome to the Taxonomic Explorer app! This interactive tool allows you to explore and analyze taxonomic data, providing insights into species composition, geographical distribution, and detailed record information."),
  
  h4("1. Select Taxa:"),
  p("Use the dropdown menu to select a specific taxon (e.g., frog, dove, bee). This choice will be the focus of your exploration."),
  
  h4("2. Summary Tab:"),
  p(
    "Heading: Observe the dynamic heading reflecting your selected taxa.",
    "Pie Chart: View the pie chart representing the top 10 species composition.",
    "Summary Statistics: Check the value boxes for the number of distinct species, total records, and the top observer."
  ),
  
  h4("3. Map Tab:"),
  p(
    "Explore the leaflet map with markers representing records of the selected taxa.",
    "Each marker on the map corresponds to a specific record, providing a spatial overview."
  ),
  
  h4("4. Species List and Records Tab:"),
  p(
    "Species List: View a data table listing species and their respective number of records, sorted in descending order.",
    "Records: Explore a data table displaying all records for the selected taxa."
  ),
  
  h4("5. Photo Display:"),
  p("Click on a row in the Records data table to view the associated photo in a modal dialog."),
  
  h4("Tips:"),
  p(
    "Dynamic Updates: All visualizations and summaries dynamically update based on your taxa selection.",
    "Interactivity: Click on elements such as data points in charts or rows in tables for more details.",
    "Explore and Analyze: Use the app to gain insights into species distribution, observer contributions, and more."
  ),
  
  h4("Important:"),
  p(
    "- Ensure that you've selected a taxon before exploring different tabs.",
    "- Some functionalities may depend on the availability of data such as photos. "
  ),
)



Tab0 <- tabItem(
  tabName = "about",
    tabBox(
      width = "100%",
      id = "tabset1",
      tabPanel(
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
              img(src ="group_photo.jpeg")
              
            ),
            
            # child element 2: content
            tags$div(
              class = "landing-block foreground-content",
              tags$div(
                class = "foreground-text",
                # tags$h2("Biodiversity Monitoring Programme Data Dashboard"),
                tags$p("Welcome to the University of St Andrews BMP Data Dashboard! We are thrilled to have you here as we embark on a journey through the rich tapestry of nature. This data dashboard is the culmination of the hard work and dedication of our university students, who spent their summer immersed in the natural world, collecting invaluable biodiversity data."),
                tags$p("Explore the wonders of the diverse ecosystems that surround us and gain insights into the intricate web of life that thrives within our region. Our mission is not only to deepen our understanding of the natural world but also to contribute to the conservation efforts that safeguard these vital ecosystems."),
                tags$p("On this dashboard, you will find a wealth of information, from species counts and distribution maps to trends and observations. Whether you are a fellow researcher, a student, or simply a nature enthusiast, we invite you to delve into the data and be inspired by the beauty and complexity of our local environment."),
                tags$p("Together, we can make a difference in preserving and protecting the biodiversity of the University of St Andrews and beyond. Thank you for joining us on this journey, and may this dashboard serve as a valuable resource for your exploration and learning.")
              )
            )
          )

      ),
       
      kpi_panel,
      taxa_explorer_panel,
      tabPanel(
        title = "Student Engagement"
      ),
      tabPanel(
        title = "Record Explorer"
      )
    )
  
)
