# Value Box Module
# custom value box function 

customValueBox <- function(value = NULL, subtitle = NULL, icon = NULL, color, background, 
                           width = 4, href = NULL, height = "150px"){
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  style <- paste0("color: ", color, "; background-color: ", background, ";",
                  "width: ",width,  ";", "height: ", height)
  
  boxContent <- div(
    class = "small-box", style = style,
    div(
      class = "inner",
      h4(value),
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

# valuebox module ui function -------------------------------------------------


valuebox_ui <- function(id){
  ns <- NS(id) # define namespace for below
  uiOutput(ns("vb"))
}

# valuebox module server function ---------------------------------------------

valuebox_server <- function(id, value, title, subtitle, icon, color, background){
  moduleServer(id, function(input, output, session){
    output$vb <- renderValueBox({
      customValueBox(
        value = value, 
        title = title,
        subtitle = subtitle,
        icon = icon(icon),
        background = background,
        color = color
      )
    })
  })
}
