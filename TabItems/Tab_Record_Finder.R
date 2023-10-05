# TAB 4: Record finder  ----------------------------------------------------------------------------

# read in all photo files within Survey_Photos in OneDrive Folder
all_Sphotos <- list.files(
  path = paste0("www/"),
  recursive = TRUE,
  pattern = "\\.jpg$",
  full.names = FALSE
)


random_photos <- sample(all_Sphotos, size = 10)

Tab4 <- tabItem(
  tabName = "record_finder",
  tags$div(
    class = "landing-wrapper-record",
    # child element 1: images
    tags$ul(
      class = "image-gallery",
      tags$li(img(src = paste(random_photos[1]))),
      tags$li(img(src = paste(random_photos[2]))),
      tags$li(img(src = paste(random_photos[3]))),
      tags$li(img(src = paste(random_photos[4]))),
      tags$li(img(src = paste(random_photos[5]))),
      tags$li(img(src = paste(random_photos[6]))),
      tags$li(img(src = paste(random_photos[7]))),
      tags$li(img(src = paste(random_photos[8]))),
      tags$li(img(src = paste(random_photos[9]))),
      tags$li(img(src = paste(random_photos[10])))
    ),

    # child element 2: content
    tags$div(
      class = "landing-block foreground-content",
      tags$div(
        class = "foreground-text",
        align = "center",
        textInput("PhotoID", "Enter PhotoID (with extension):"),
        actionButton("viewButton", "View Image", style = "display: 
                       block; background-color: #619e62; color: #ffffff; 
                       border: none; border-radius: 5px; cursor: pointer;
                       font-size; 16px; margin: auto"),
        br(),
        br(),
        uiOutput("imageOutput"),
      )
    )
  )
)

# SHCPiedWagtail.jpg
