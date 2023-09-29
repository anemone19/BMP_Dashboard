
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

# TAB 2: TAXA EXPLORER -------------------------------------------------------------------

# Render and display the selected taxa name followed by " Explorer" as a heading in the UI.
output$taxa_heading <- renderText({
  paste(input$taxa_select," Explorer", sep = "")
})

# Create a reactive dataframe 'df' based on the selected taxa from the input.
# This dataframe filters the 'survey_data()' by the selected taxa and selects specific columns.
df <- reactive({
  survey_data() %>%
    filter(Taxa == input$taxa_select) %>%
    select(Species, Count, Observer, Date, PhotoID, geometry)
})

# Render a pie chart showing the distribution of species.
# The chart displays the top 10 species based on their count.
output$species_pie <- renderPlot({
  
  # Group by species, calculate the count and percentage for each species.
  
  pie <- df() %>%
    group_by(Species) %>% 
    summarise(Value = n()) %>%
    mutate(Percent = Value/sum(Value)) %>%
    arrange(desc(Value)) %>%
    slice_max(n = 10, order_by = Value)
  
  # Convert Species to a factor for plotting.
  pie$Species <- factor(pie$Species, levels = (as.character(pie$Species)))
  
  # Plot the pie chart with labels for each species.
  pie %>%
    mutate(csum = rev(cumsum(rev(Value))), 
           pos = Value/2 + lead(csum, 1),
           pos = if_else(is.na(pos), Value/2, pos)) %>%
    ggplot(aes(x="",y=Value,fill=Species))+
    geom_bar(width=1,stat="identity",color = "white") + 
    coord_polar("y") +
    theme_void() + 
    theme(legend.position = "none") +
    geom_label_repel(aes(y = pos, label = Species),
                     size = 4, nudge_x = 0.7, show.legend = FALSE, colour = "black") +
    scale_colour_gradientn(colours = rainbow(10))
})

# Render a value box showing the number of distinct species for the selected taxa.
# The icon changes based on the selected taxa.
output$num_species_taxa <- renderValueBox({
  
  # Create a named vector to map taxa names to icons
  icon_map <- c("frog", "dove", "bee", rep("bug", 6), "squirrel", "bug","leaf")
  names(icon_map) <- taxa_names()
  
  # Look up the icon for the selected taxa
  icon_taxa <- ifelse(!is.na(icon_map[input$taxa_select]), icon_map[input$taxa_select], "leaf")
  
  
  # Display the value box with the number of distinct species and the determined icon.
  customValueBox(value = paste(n_distinct(df()$Species)),
                 subtitle = "Species recorded",
                 icon = icon(icon_taxa),
                 background = "#A2A8D3",
                 color = "black",
                 height = "auto"
  )
})

# Render a value box showing the total number of records for the selected taxa.
output$num_records_taxa <- renderValueBox({
  customValueBox(value = nrow(df()),
                 subtitle = "Records collected",
                 icon = icon("clipboard"),
                 background = "#A2A8D3",
                 color = "black",
                 height = "auto"
  )
})

# Render a value box highlighting the top observer based on the number of records.
output$top_obs <- renderValueBox({
  
  # Group by observer, calculate the number of records for each observer, and sort in descending order.
  observer_list <- df() %>%
    group_by(Observer) %>% 
    summarise(Num_Records = n()) %>%
    arrange(desc(Num_Records))
  
  customValueBox(observer_list$Observer[1],
                 subtitle = paste("is the top observer with",observer_list$Num_Records[1],"records!"),
                 icon = icon("trophy"),
                 background = "#A2A8D3",
                 color = "black",
                 height = "auto"
  )
})

# Bar graph of number of records of species
# This graph displays the top 50 species based on the number of records
output$species_bar <- renderPlotly({
  
  df_plot <- df() %>%
    count(Species, sort = TRUE) %>%
    rename(Count = n) %>%
    slice_max(n = 50, order_by = Count) # Selecting top 50 species based on count
  
  ggplotly(
    ggplot(df_plot, aes(x = reorder(Species, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(
        x = "\nSpecies",
        y = "Number of Records\n"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1)) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))),
    tooltip = c("y")
  )
})

# Records datatable 
# This data table displays all records from the dataset
output$taxa_table <- DT::renderDT({
  df() %>%
    DT::datatable(
      options = table_options(),
      selection = "single" # has to outside out options
    )
})

# Output photo of record when row selected from above datatable 
# When a user selects a row in the data table, this observer displays the associated photo for the record
observeEvent(input$taxa_table_rows_selected, {
  selected_row <- input$taxa_table_rows_selected
  selected_photo <- df()[selected_row, ]$PhotoID
  
  if (!is.na(selected_photo)) {
    selected_data <- df()[selected_row, ]
    showModal(modalDialog(
      title = "Record Photo",
      
      output$imageOutput1 <- renderImage({
        list(src = paste0("www/Survey_Photos/",selected_photo), width = "100%", height ="100%")
      }, deleteFile = FALSE),
      easyClose = TRUE
    ))
  }
})

# Species list datatable
# This data table lists species and their respective number of records, sorted in descending order
output$species_list <- DT::renderDT({
  
  species_list <- df() %>% 
    group_by(Species) %>% 
    summarise(Num_Records = n()) %>%
    arrange(desc(Num_Records)) # Sorting by number of records in descending order
  
  species_list %>%
    DT::datatable(
      options = table_options(),
    )
})

# Leaflet Map 

output$MapPlot1 <- renderLeaflet({
  
  
  # Filter and select data
  map_df <- survey_data() %>%
    filter(Taxa == input$taxa_select) %>%
    select(Species, geometry, lat, long)
  
  leaflet(map_df) %>%
    addTiles() %>%
    # addProviderTiles(providers$Esri.WorldImagery) %>%
    # fitBounds(lng1 = min(map_df$long), 
    #           lat1 = min(map_df$lat), 
    #           lng2 = max(map_df$long), 
    #           lat2 = max(map_df$lat)) %>%
    addCircleMarkers(lng = ~long,
                     lat = ~lat, 
                     weight = 1,
                     label = ~Species,
                     labelOptions = labelOptions(noHide = F, textsize = "15px"),
                     clusterOptions = markerClusterOptions(),
                     data = map_df)
  
})