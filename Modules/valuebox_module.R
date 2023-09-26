# Value Box Module

# custom value box function 

customValueBox <- function(value, subtitle, icon = NULL, color, background, width = 4, href = NULL) {
  # validateColor(color)
  # if (!is.null(icon))
  # tagAssert(icon, type = "i")
  
  style <- paste0("color: ", color, "; background-color: ", background, ";")
  
  boxContent <- div(
    class = "small-box", style = style,
    div(class = "inner", h4(value), p(subtitle)), if (!is.null(icon)) {
      div(class="icon", icon(paste(icon)))
    }
  )
  if (!is.null(href)) {
    boxContent <- a(href = href, boxContent)
  }
  div(class = if (!is.null(width)) {
    paste0("col-sm-", width)
  }, boxContent)
}

# valuebox module ui function -------------------------------------------------
valuebox_ui <- function(id){
  ns <- NS(id) # define namespace for below
  uiOutput(ns("vb"))
}

# valuebox module server function ---------------------------------------------

valuebox_server <- function(id, value, subtitle,  icon, color, background){
  moduleServer(id, function(input, output, session){
    
    output$vb <- renderUI({
      customValueBox(
        value = paste(value), 
        subtitle = paste(subtitle),
        icon = icon(paste(icon)),
        background = paste(background),
        color = paste(color)
      )
    })
  })
}

# output$num_records <- renderValueBox({
#   valueBox(
#     paste0(stud_df()$records), "Records collected",
#     icon = icon("table"),
#     color = "purple"
#   )
# })

# output$num_days <- renderValueBox({
#   valueBox(
#     paste0(stud_df()$days), "Days surveyed",
#     icon = icon("calendar"),
#     color = "purple"
#   )
# })
# 
# output$top_taxa <- renderValueBox({
#   valueBox(
#     paste0(stud_df()$top_taxa), "Top Taxa",
#     icon = icon("graduation-cap"),
#     color = "purple"
#   )
# })
# output$MS <- renderValueBox({
#   customValueBox(
#     paste0(25 + input$count, "%"), "Mean species score/ha",
#     icon = icon("star"),
#     background = "#FF5959",
#     color = "black"
#   )
# })

# output$local <- renderValueBox({
#   customValueBox(
#     paste0(25 + input$count, "%"), "Local species score/ha",
#     icon = icon("chart-area"),
#     background = "#FACF59",
#     color = "black"
#   )
# })

# output$fife <- renderValueBox({
#   customValueBox(
#     paste0(25 + input$count, "%"), "Fife species score/ha",
#     icon = icon("graduation-cap"),
#     background = "#49BEB7",
#     color = "black"
#   )
# })
