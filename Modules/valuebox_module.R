# Value Box Module -----------------------------------------------------------------------------------------

# custom value box function 

customValueBox <- function(value = NULL, subtitle = NULL, icon = NULL, color, background, 
                           width = 4, height = "150px"){
  
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
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}


# valuebox module server function ---------------------------------------------

valuebox_server <- function(id, value, subtitle, icon, color, background, 
                            width = 4, height = "150px"){
  moduleServer(id, function(input, output, session){
    output$vb <- renderValueBox({
      customValueBox(
        value = value, 
        subtitle = subtitle,
        icon = icon,
        background = background,
        color = color,
        width = width,
        height = height
      )
    })
  })
}
