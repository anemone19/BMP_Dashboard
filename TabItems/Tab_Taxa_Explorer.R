
# TAB: TAXA EXPLORER ----------------------------------------------------------------------------

# Tab2 <- tabItem(
#   tabName = "taxa_expl",
#   tags$h3(
#     style = "display: flex; align-items: center;",
#     textOutput("taxa_heading")
#   ),
#   fluidRow(
#     # box(height=500,width=12,
#     #   div(style='width: fit-content; margin-left: auto; margin-right: auto;overflow-x: scroll;overflow-y: scroll;',
#     #         plotlyOutput("species_bar")))
#     box(title = "Top 50", collapsible = TRUE,height=500,width=12,
#       plotlyOutput("species_bar",inline=F, width= "100%")
#     )
#   ),
#   fluidRow(
#     box(collapsible = TRUE,
#         title = "Records",
#         DTOutput("taxa_table"), width=12)
#   ),
#   fluidRow(
#     box(collapsible = TRUE,
#         title = "Species List",
#         DTOutput("species_list"), width=12)
#   )
# )


Tab2 <- tabItem(
  tabName = "taxa_expl",
    h3(textOutput("taxa_heading"), align = "center"),
  br(),
    tabBox(
      id = "box1",height=500,width=12,
      tabPanel("Summary",
               fluidRow(
                 column(6, 
                        h5("Top 10 Species Composition"),
                        plotOutput("species_pie")),
                 column(6, offset = 1.5, 
                        br(),
                        br(),
                        br(),
                        fluidRow(
                          valueBoxOutput("num_species_taxa", width = 10), style ="margin: auto;"
                        ),
                        
                        fluidRow(
                          valueBoxOutput("num_records_taxa", width = 11), style ="margin: auto;"
                        ),
                        fluidRow(
                          valueBoxOutput("top_obs", width = 12), style ="margin: auto;"
                        )
                 )
               )
               ),
      tabPanel("Top 50 species", plotlyOutput("species_bar",inline=F, width= "100%")),
    ),
  box(
    id = "box1",height="100%",width=12,
    leafletOutput("MapPlot1")
  ),
    tabBox(
      id = "box2",height=500,width=12,
      tabPanel("Species List", DTOutput("species_list")),
      tabPanel("Records", DTOutput("taxa_table"))
    )
)
