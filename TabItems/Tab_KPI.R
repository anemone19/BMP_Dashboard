# TAB: KPI  ----------------------------------------------------------------------------
Tab1 <- tabItem(
  tabName = "kpi",
  h3("Annual Biodiversity Index"),
  fluidRow(
    # valueBoxOutput("S_H"),
    valuebox_ui("S_H"),
    valuebox_ui("area"),
    valuebox_ui("engagement")
  ),
  h3("Benchmark Biodiversity Index"),
  fluidRow(
    valuebox_ui("MS"),
    valuebox_ui("local"),
    valuebox_ui("fife")
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
