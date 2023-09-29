#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

server <- function(input, output, session) {

  # Fetch data for selected time, e.g.  2022/2023 or 2023/2024
  
  # Reactively subset data based on user input
  survey_data <- reactive({
    extract_df <- all_years_list[[input$year]]
    return(extract_df)
  })
  
  # taxa as options for TAB Taxa Explorer
  taxa_names <- reactive({
    sort(unique(survey_data()$Taxa))
  })
                    
                    
  # student names for TAB Student Engagement
  student_names <- reactive({
    survey_data() %>%
      distinct(Observer, .keep_all = TRUE) %>%
      drop_na(Observer) %>%
      pull(Observer)
  })
  
  observeEvent(input$year, {
      updateSelectInput(session, "taxa_select", choices = taxa_names())
  }, ignoreNULL = FALSE)
  
  observeEvent(input$year, {
    updateSelectInput(session, "student_select", choices = student_names())
  }, ignoreNULL = FALSE)
  
  # TAB 0: ABOUT ----------------------------------------------------------------------
  
  # gorup photos 
  
  output$groupPhoto1 <- renderImage({
    list(src = "www/Team_Data/group_photo.jpeg",
         alt = "Image description",
         width = "50px", # Set the width as needed
         height = "50px" # Maintain the aspect ratio
    )
  }, deleteFile = FALSE)
  
  output$groupPhoto2 <- renderImage({
    list(src = "www/Team_Data/group_photo2.jpeg",
         alt = "Image description",
         width = "100%", # Set the width as needed
         height = "auto" # Maintain the aspect ratio
    )
  }, deleteFile = FALSE)
  
  # ABI descriptions
  # Value boxes for displaying the descriptions of the three KPIs
  output$S_H_about <- renderValueBox(customValueBox(
    subtitle = paste(par_s_h),
    value = "Species and Habitat Score",
    background = "lightgray",
    color = "black"
  ))
  
  
  output$area_about <- renderValueBox(customValueBox(
    subtitle = paste(par_l_s),
    value = "Area Score",
    background = "lightgray",
    color = "black"
  ))
  
  output$engagement_about <- renderValueBox(customValueBox(
    subtitle = paste(par_engage),
    value = "Engagement Score",
    background = "lightgray",
    color = "black"
  ))
  

  
  # BBI descriptions 
  # Display descriptions of scores relating to BBI
  
  output$MS_about <- renderValueBox(customValueBox(
    subtitle = paste(par_m_s),
    value = "Local species score per hectare",
    background = "lightgray",
    color = "black"
  ))
  
  output$fife_about <- renderValueBox(customValueBox(
    subtitle = paste(par_fife),
    value = "Local species score per hectare",
    background = "lightgray",
    color = "black"
  ))
  
  output$local_about <- renderValueBox(customValueBox(
    subtitle = paste(par_l_s),
    value = "Local species score per hectare",
    background = "lightgray",
    color = "black"
  ))
  
  
  # TAB 1: KEY PERFORMANCE INDICES -----------------------------------

  ## ABI Valueboxes
  # These value boxes display the Species and Habitat Score, Area, and Engagement metrics
  
  # Value box for Species and Habitat Score
  output$S_H <- renderValueBox(customValueBox(
    subtitle = "Species and Habitat Score",
    value = "100",
    icon = icon("area-chart"),
    background = "#49BEB7",
    color = "black",
    height = "auto"
  ))
  
  # Value box for Area Score
  output$area <- renderValueBox(customValueBox(
    value = paste0(25 + input$count, "%"),
    subtitle = "Area",
    icon = icon("area-chart"),
    background = "#FACF59",
    color = "black",
    height = "auto"
  ))
  
  # Value box for Engagement Score
  output$engagement <- renderValueBox(customValueBox(
    value = paste0(25 + input$count, "%"),
    subtitle = "Engagement",
    icon = icon("graduation-cap"),
    background = "#FF5959",
    color = "black",
    height = "auto"
  ))

  
  ## ABI Plot
  # Bar plot showing the number of distinct species for each Taxa
  output$taxa_bar <- renderPlotly({
    df <- survey_data() %>%
      group_by(Taxa) %>%
      summarise(Count = n_distinct(Species))
    
    ggplotly(
      ggplot(df, aes(x = reorder(Taxa, -Count), y = Count)) +
        geom_bar(stat = "identity", fill = "lightblue", width = 0.2) +
        labs(
          x = "\nTaxa",
          y = "Number of Species\n"
        ) +
        theme_minimal() +
        theme(axis.text = element_text(size = 11),
              axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1)),
      tooltip = "y"
    )
  })
  
  ## BBI Valueboxes
  # These value boxes display benchmark scores such as Mean species score/ha, Local species score/ha, and Fife species score/ha
  # Value box for Mean species score per hectare
  output$MS <- renderValueBox(customValueBox(
    value = paste0(25 + input$count, "%"),
    subtitle = "Mean species score/ha",
    icon = icon("star"),
    background = "#FF5959",
    color = "black",
    height = "auto"
  ))
  
  # Value box for Local species score per hectare
  output$local <- renderValueBox(customValueBox(
    value = paste0(25 + input$count, "%"),
    subtitle = "Local species score/ha",
    icon = icon("area-chart"),
    background = "#FACF59",
    color = "black",
    height = "auto"
  ))
  
  # Value box for Fife species score per hectare
  output$fife <- renderValueBox(customValueBox(
    value = paste0(25 + input$count, "%"),
    subtitle = "Fife species score/ha",
    icon = icon("graduation-cap"),
    background = "#49BEB7",
    color = "black",
    height = "auto"
  ))

  
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
  

  # TAB 3: STUDENT ENGAGEMENT  ---------------------------------------------------
  
  # Heading for the student engagement section
  # This will dynamically display the name of the selected student followed by "Biodiversity Data"
  output$student_heading <- renderText({
    paste(input$student_select, "'s Biodiversity Data", sep = "")
  })
  
  # Description for the student engagement section
  # This fetches a description paragraph related to the selected student from the 'student_text_sep' dataset
  output$student_description <- renderText({
    search_word <- input$student_select
    search_paragraph <- grep(search_word, student_text_sep)[1]
    description <- student_text_sep[search_paragraph]
    description
  })
  
  # Display photo of the selected student
  # This dynamically fetches the photo based on the selected student's name
  output$photoOutput <- renderUI({
    name <- input$student_select
    image_path <- paste0("Team_Data/",name, ".jpg")
    div <- tags$div(
      tags$img(src = image_path, height = "100px", style = "margin-right: 10px; border-radius: 30px;")
    )
    div
  })
  
  # Reactive dataframe to filter the main dataset by the selected student
  stud_df <- reactive({
    survey_data() %>%
      filter(Observer == input$student_select)
  })
  
  # Reactive dataframe to summarize statistics related to the selected student for the value boxes
  sum_stud_df <- reactive({
    stud_df()  %>%
      summarize(
        days = n_distinct(Date),
        records = n(),
        top_taxa = names(table(Taxa))[which.max(table(Taxa))]
      )
  })
  
  # Value boxes to display statistics related to the selected student
  
  # Number of records collected by the student
  output$num_records <- renderValueBox({
    customValueBox(sum_stud_df()$records,
                   subtitle = "Records collected",
                   icon = icon("clipboard"),
                   background = "#FF5959",
                   color = "black",
                   height = "auto"
    )
  })
  
  # Number of days the student surveyed
  output$num_days <- renderValueBox({
    customValueBox(sum_stud_df()$days,
                   subtitle = "Days surveyed",
                   icon = icon("calendar"),
                   background = "#FACF59",
                   color = "black",
                   height = "auto"
    )
  })
  
  # Top Taxa recorded by the student
  output$top_taxa <- renderValueBox({
    customValueBox(
      value = sum_stud_df()$top_taxa,
      subtitle = "Top Taxa",
      icon =  icon("graduation-cap"),
      background = "#49BEB7",
      color = "black",
      height = "auto"
    )
  })
  
  # valuebox_server("num_records",
  #   value = paste(stud_df()$records),
  #   subtitle = "Records collected",
  #   icon = "table",
  #   background = "#FF5959",
  #   color = "black"
  # )
  
  # valuebox_server("num_days",
  #   value = paste(stud_df()$days),
  #   subtitle = "Days surveyed",
  #   icon = "calendar",
  #   background = "#FACF59",
  #   color = "black"
  # )
  
  # valuebox_server("top_Taxa",
  #   value = paste(stud_df()$top_Taxa),
  #   subtitle = "Top Taxa",
  #   icon = "graduation-cap",
  #   background = "#49BEB7",
  #   color = "black"
  # )
  
  # Number of species recorded per Taxa bar chart
  # This bar chart visualizes the count of species under each Taxa
  output$stud_plot <- renderPlotly({
    df <- stud_df() %>%
      count(Taxa, sort = TRUE) %>%
      rename(Count = n)
    
    ggplotly(
      ggplot(df, aes(x = reorder(Taxa,-Count), y = Count)) +
        geom_bar(stat = "identity", fill = "lightblue", width = 0.2) +
        labs(
          x = "\nTaxa",
          y = " Number of Records\n"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1)) +
        scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))),
      tooltip = "y"
    )
  })
  
  # Data table showcasing student data including species, count, date, taxa, and photo ID
  output$student_table <- DT::renderDT({
    stud_df() %>% select(Species,Count,Date,Taxa, PhotoID) %>%
      DT::datatable(
        options = table_options(),
      )
  })
  
  # TAB 4: RECORD FINDER -----------------------------------------------------------------------
  # This section handles the functionality to fetch and display a record photos 
  # based on user inputed photoID
  
  # Observer that listens to 'viewButton' click event
  # Upon activation, the observer displays survey photo matching inputted PhotoID. 
  # If the image is not found, it shows an error modal.
  observeEvent(input$viewButton, {
    filename <- paste0("www/Survey_Photos/", input$filename, ".jpg")
    
    if (file.exists(filename)) {
      
      showModal(modalDialog(
        title = "Record Photo",
        output$imageOutput <- renderImage({
          list(src = filename, width = "100%", height = "100%")
        }, deleteFile = FALSE),
        easyClose = TRUE
      ))
      

    } else {
      showModal(
        modalDialog(
          title = "Error",
          "Image not found. Please enter a valid filename."
        )
      )
    }
  })

}


