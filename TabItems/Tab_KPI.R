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
  h3("Change Biodiversity Index"),
  fluidRow(),

  # taxa chart
  fluidRow(
    box(collapsible = TRUE, width = 12,
      plotlyOutput("taxa_bar", height = "300px"), style = 'display:block; overflow-x: scroll;'
    )
  )
)
