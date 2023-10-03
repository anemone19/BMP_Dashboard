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
    tabBox(
      id = "box2",height=500,width=12,
      tabPanel("Species List", DTOutput("species_list_student")),
      tabPanel("Records", DTOutput("student_table"))
    )
  ),
  uiOutput("Modal_student")
)
