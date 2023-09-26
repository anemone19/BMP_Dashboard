# TAB: STUDENT ENGAGEMENT ----------------------------------------------------------------------------
Tab3 <- tabItem(
  tabName = "stud_expl",
  tags$h3(
    style = "display: flex; align-items: center;",
    uiOutput("photoOutput"),
    textOutput("student_heading")
  ),
  fluidRow(column(textOutput("student_description"),width=12)),
  br(),
  fluidRow(
    # valuebox_ui("num_records"),
    # valuebox_ui("num_days"),
    # valuebox_ui("top_taxa")
    valueBoxOutput("num_records"),
    valueBoxOutput("num_days"),
    valueBoxOutput("top_taxa")
  ),
  fluidRow(
    box(collapsible = TRUE, width = 12,
      plotlyOutput("stud_plot", height = "300px"), style = 'display:block; overflow-x: scroll;'
    )
  ),
  fluidRow(
    box(DTOutput("student_table"), width=12)
  )
)
