# TAB 4: Record finder  ----------------------------------------------------------------------------

Tab4 <- tabItem(
  tabName = "record_finder",
  textInput("filename", "Enter PhotoID (without extension):"),
  actionButton("viewButton", "View Image"),
  fluidRow(
    box(
      imageOutput("imageOutput", width="auto",height="auto")
    )
  )
)

# Tab4 <- tabItem(
#   tabName = "record_finder",
#   tags$div(class="landing-wrapper",
#            
#            # child element 1: images
#            tags$div(class="landing-block background-content",
#                     
#                     # top left
#                     img(src="Survey_Photos/MeganCommonSnowberry.jpg"),
#                     
#                     # top right
#                     img(src="Survey_Photos/TomMoth310523CLute.jpg"),
#                     
#                     # bottom left
#                     img(src="Survey_Photos/Erica Bibio Marci.jpg"), 
#                     
#                     # bottom right
#                     
#                     img(src="Survey_Photos/Tombird140423mallard.jpg")
#                     
#            ),
#            
#            # child element 2: content
#            tags$div(class="landing-block foreground-content",
#                     tags$div(class="foreground-text",
#                              tags$h1("Welcome"),
#                              tags$p("This shiny app demonstrates
#                                                      how to create a 2 x 2 layout
#                                                               using css grid and
#                                                               overlaying content."),
#                              tags$p("Isn't this cool?"),
#                              tags$p("Yes it is!")
#                     )
#            )
#   )
# )
