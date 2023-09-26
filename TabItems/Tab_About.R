# TAB 0: About ----------------------------------------------------------------------------

Tab0 <- tabItem(
  tabName = "about",
  fillPage(
    tabBox(
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px",
      tabPanel(
        title = "Overview",
        "Data Dashboard for the Biodiversity Monitoring Programme of the University of St Andrews."
      ),
      
      tabPanel(
        title= "Key Performance Indices",
        h3("Annual Biodiversity Index"),
        fluidRow(
          valuebox_ui("S_H_about"),
          valuebox_ui("area_about"),
          valuebox_ui("engagement_about")
        ),
        h3("Benchmark Biodiversity Index"),
        fluidRow(
          valuebox_ui("MS_about"),
          valuebox_ui("local_about"),
          valuebox_ui("fife_about")
        ),
      ),
      
      tabPanel(
        title= "Taxa Explorer",
        paste(taxa_explorer_about)
      ),
      
      tabPanel(
        title= "Student Engagement"
      ),
      
      tabPanel(
        title= "Record Explorer"
      ),
      

      width = 12
    )
  )
)
