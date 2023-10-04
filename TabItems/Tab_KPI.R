# TAB: KPI  ----------------------------------------------------------------------------
Tab1 <- tabItem(
  tabName = "kpi",
  h3("Annual Biodiversity Index"),
  fluidRow(
    # valueBoxOutput("S_H"),
    valueBoxOutput("S_H"),
    valueBoxOutput("area"),
    valueBoxOutput("engagement")
  ),
  h3("Benchmark Biodiversity Index"),
  fluidRow(
    valueBoxOutput("MS"),
    valueBoxOutput("local"),
    valueBoxOutput("fife")
  ),
  
  div(
    conditionalPanel(
      "input.year !== '2022/2023'",
      h3("Change Biodiversity Index"),
      # place CBI metric / plot here
      box(
        collapsible = TRUE, width = 12,
        plotlyOutput("example_plot"), style = "display:block; overflow-x: scroll;"
      )
    )
  ),

  # taxa chart
  fluidRow(
    box(
      collapsible = TRUE, width = 12, title = "Number of species per taxa",
      plotlyOutput("taxa_bar", height = "300px"), style = "display:block; overflow-x: scroll;"
    )
  )
)
