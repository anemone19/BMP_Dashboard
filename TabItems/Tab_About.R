# TAB 0: About ----------------------------------------------------------------------------
# Create the Tab0
Tab0 <- tabItem(
  tabName = "about",
  tags$style("p {font-size: 15px} h2{font-size: 25px;}" ),
  fillPage(
    tabBox(
      id = "tabset1",
      height = "100%",
      width = "1000px",
      tabPanel(
        title = "Overview",
        div(style = "margin: auto; width: 85%",
          h2("Biodiversity Monitoring Programme (BMP) Data Dashboard"),
          p("Welcome to the University of St Andrews BMP Data Dashboard! We are thrilled to have you here as we embark on a journey through the rich tapestry of nature. This data dashboard is the culmination of the hard work and dedication of our university students, who spent their summer immersed in the natural world, collecting invaluable biodiversity data."),
          p("Explore the wonders of the diverse ecosystems that surround us and gain insights into the intricate web of life that thrives within our region. Our mission is not only to deepen our understanding of the natural world but also to contribute to the conservation efforts that safeguard these vital ecosystems."),
          p("On this dashboard, you will find a wealth of information, from species counts and distribution maps to trends and observations. Whether you are a fellow researcher, a student, or simply a nature enthusiast, we invite you to delve into the data and be inspired by the beauty and complexity of our local environment."),
          p("Together, we can make a difference in preserving and protecting the biodiversity of the University of St Andrews and beyond. Thank you for joining us on this journey, and may this dashboard serve as a valuable resource for your exploration and learning.")
        )
      ),
      tabPanel(
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
      ),
      tabPanel(
        title = "Taxa Explorer",
        paste(taxa_explorer_about)
      ),
      tabPanel(
        title = "Student Engagement"
      ),
      tabPanel(
        title = "Record Explorer"
      )
    )
  )
)
