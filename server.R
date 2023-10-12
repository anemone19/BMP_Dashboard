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

  # Reactively update taxa names for Student Engagement dropdown 
  observeEvent(input$year,
    {
      if (input$year == "2022/2023") {
        updateSelectInput(session, "taxa_select", choices = taxa_names(), selected = "Butterflies") # I want the taxa explorer to open on butterflies as default
      } else {
        updateSelectInput(session, "taxa_select", choices = taxa_names())
      }
    },
    ignoreNULL = FALSE
  )

  # Reactively update student names for Student Engagement dropdown 
  observeEvent(input$year,
    {
      updateSelectInput(session, "student_select", choices = student_names())
    },
    ignoreNULL = FALSE
  )

  # TAB 0: ABOUT ----------------------------------------------------------------------

  # ABI descriptions
  # Value boxes for displaying the descriptions of the three KPIs
  
  output$S_H_about <- valuebox_server(
    id = "S_H_about",
    subtitle = paste(par_s_h),
    value = "Species and Habitat Score",
    background = "lightgray",
    color = "black",
    icon = NULL
  ) 
  
  output$area_about <- valuebox_server(
    id = "area_about",
    subtitle = paste(par_l_s),
    value = "Area Score",
    background = "lightgray",
    color = "black",
    icon = NULL
  ) 
  
  output$engagement_about <- valuebox_server(
    id = "engagement_about",
    subtitle = paste(par_engage),
    value = "Engagement Score",
    background = "lightgray",
    color = "black",
    icon = NULL
  ) 


  # BBI descriptions
  # Display descriptions of scores relating to BBI

  output$MS_about <- valuebox_server(
    id = "MS_about",
    subtitle = paste(par_m_s),
    value = "Local species score per hectare",
    background = "lightgray",
    color = "black",
    icon = NULL
  )

  output$fife_about <- valuebox_server(
    id = "fife_about",
    subtitle = paste(par_fife),
    value = "Local species score per hectare",
    background = "lightgray",
    color = "black",
    icon = NULL
  )

  output$local_about <- valuebox_server(
    id = "local_about",
    icon = NULL,
    subtitle = paste(par_l_s),
    value = "Local species score per hectare",
    background = "lightgray",
    color = "black"
  )


  # TAB 1: KEY PERFORMANCE INDICES -----------------------------------

  ## ABI Valueboxes
  # These value boxes display the Species and Habitat Score, Area, and Engagement metrics

  # Value box for Species and Habitat Score
  output$S_H <- valuebox_server(
    id ="S_H",
    subtitle = "Species and Habitat Score",
    value = "100",
    icon = icon("star", style = "font-size: 50px"),
    background = "#068D9D",
    color = "black",
    height = "auto"
  )

  # Value box for Area Score
  output$area <- valuebox_server(
    id ="area",
    value = "##",
    subtitle = "Area Score",
    icon = icon("area-chart", style = "font-size: 50px"),
    background = "#226891",
    color = "black",
    height = "auto"
  )

  # Value box for Engagement Score
  output$engagement <- valuebox_server(
    id = "engagement",
    value = "##",
    subtitle = "Engagement Score",
    icon = icon("graduation-cap", style = "font-size: 50px"),
    background = "#D9DBF1",
    color = "black",
    height = "a"
  )


  ## ABI Plot
  # Bar plot showing the number of distinct species for each Taxa
  output$taxa_bar <- renderPlotly({
    df <- survey_data() %>%
      group_by(Taxa) %>%
      summarise(Count = n_distinct(Species))

    ggplotly(
      ggplot(df, aes(x = reorder(Taxa, -Count), y = Count)) +
        geom_bar(stat = "identity", fill = "#028E9D", width = 0.2) +
        labs(
          x = "\nTaxa",
          y = "Number of Species\n"
        ) +
        theme_minimal() +
        theme(
          axis.text = element_text(size = 11),
          axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1)
        ),
      tooltip = "y"
    )
  })

  ## BBI Valueboxes
  # These value boxes display benchmark scores such as Mean species score/ha, Local species score/ha, and Fife species score/ha
  # Value box for Mean species score per hectare
  output$MS <- valuebox_server(
    id = "MS",
    value = "##",
    subtitle = "Mean species score/ha",
    icon = icon("calculator", style = "font-size: 50px"),
    background = "#068D9D",
    color = "black",
    height = "auto"
  )

  # Value box for Local species score per hectare
  output$local <- valuebox_server(
    id ="local",
    value = "##",
    subtitle = "Local species score/ha",
    icon = icon("location-dot", style = "font-size: 50px"),
    background = "#226891",
    color = "black",
    height = "auto"
  )

  # Value box for Fife species score per hectare
  output$fife <- valuebox_server(
    id = "fife",
    value = "##",
    subtitle = "Fife species score/ha",
    icon = icon("map", style = "font-size: 50px"),
    background = "#D9DBF1",
    color = "black",
    height = "auto"
  )

  # EXAMPLE PLOT FOR CHANGE, ONLY SHOWN WHEN NOT FIRST YEAR

  output$example_plot <- output$species_bar <- renderPlotly({
    example_df <- data.frame(
      Year = c("2022/2023", "2023/2024"),
      CBI = c(100, 89)
    )

    ggplotly(
      ggplot(example_df, aes(x = Year, y = CBI)) +
        geom_point(stat = "identity", colour = "#068D9D", size = 3) +
        labs(
          x = "\nYear",
          y = "CBI\n"
        ) +
        theme_light() +
        theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1)),
      tooltip = c("y")
    )
  })
  # TAB 2: TAXA EXPLORER -------------------------------------------------------------------

  # Render and display the selected taxa name followed by " Explorer" as a heading in the UI.
  # output$taxa_heading <- renderText({
  #   paste(input$taxa_select, " Explorer", sep = "")
  # })

  # Create a reactive dataframe 'df' based on the selected taxa from the input.
  # This dataframe filters the 'survey_data()' by the selected taxa and selects specific columns.
  taxa_df <- reactive({
    survey_data() %>%
      filter(Taxa == input$taxa_select) %>%
      select(Species, Count, Observer, Date, PhotoID, geometry)
  })

  # Render a pie chart showing the distribution of species.
  # The chart displays the top 10 species based on their count.
  output$species_pie <- renderPlot({
    
    # Group by species, calculate the count and percentage for each species.
    pie <- taxa_df() %>%
      group_by(Species) %>%
      summarise(Value = n()) %>% # count number of entries for each species
      mutate(Percent = Value / sum(Value)) %>% # calculate percentage 
      arrange(desc(Value)) %>% # arrange from highest to lowest
      slice(1:10) # take top 10 

    # Convert Species to a factor for plotting.
    pie$Species <- factor(pie$Species, levels = (as.character(pie$Species)))

    # Plot the pie chart with labels for each species.
    pie %>%
      mutate(
        csum = rev(cumsum(rev(Value))),
        pos = Value / 2 + lead(csum, 1),
        pos = if_else(is.na(pos), Value / 2, pos)
      ) %>%
      ggplot(aes(x = "", y = Value, fill = Species)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y") +
      theme_void() +
      theme(legend.position = "none") +
      geom_label_repel(aes(y = pos, label = Species),
        size = 4, nudge_x = 0.7, show.legend = FALSE, colour = "black"
      ) +
      scale_fill_brewer(palette = "Set3")
    # scale_colour_gradientn(colours = rainbow(10))
  })

  # Render a value box showing the number of distinct species for the selected taxa.
  # The icon changes based on the selected taxa.
  output$num_species_taxa <- renderValueBox({
    # Create a named vector to map taxa names to icons
    icon_map <- c("frog", "dove", "bee", rep("bug", 6), "squirrel", "bug", "leaf")
    names(icon_map) <- taxa_names()

    # Look up the icon for the selected taxa
    icon_taxa <- ifelse(!is.na(icon_map[input$taxa_select]), icon_map[input$taxa_select], "leaf")

    # Display the value box with the number of distinct species and the determined icon.
    customValueBox(
      value = paste(n_distinct(taxa_df()$Species)),
      subtitle = "Species recorded",
      icon = icon(icon_taxa, style = "font-size: 50px"),
      background = "#D9DBF1",
      color = "black",
      height = "auto"
    )
  })

  # Render a value box showing the total number of records for the selected taxa.
  output$num_records_taxa <- renderValueBox({customValueBox(
      value = nrow(taxa_df()),
      subtitle = "Records collected",
      icon = icon("clipboard", style = "font-size: 50px"),
      background = "#D9DBF1",
      color = "black",
      height = "auto"
    )
  })

  # Render a value box highlighting the top observer based on the number of records.
  output$top_obs <- renderValueBox({
    # Group by observer, calculate the number of records for each observer, and sort in descending order.
    observer_list <- taxa_df() %>%
      group_by(Observer) %>%
      summarise(Num_Records = n()) %>% # number of records per student
      arrange(desc(Num_Records)) # arrange in descending order
  
    #  observer_list$Num_Records[1] extracts the name of the top observer 
    
    customValueBox(observer_list$Observer[1],
      subtitle = paste("is the top observer with", observer_list$Num_Records[1], "records!"), 
      icon = icon("trophy", style = "font-size: 50px"),
      background = "#D9DBF1",
      color = "black",
      height = "auto"
    )
  })

  # Bar graph of number of records of species
  # This graph displays the top 50 species based on the number of records
  output$species_bar <- renderPlotly({
    
    df_plot <- taxa_df() %>%
      count(Species, sort = TRUE) %>%  
      rename(Count = n) %>%
      slice_max(n = 50, order_by = Count) # Selecting top 50 species based on count

    ggplotly(
      ggplot(df_plot, aes(x = reorder(Species, -Count), y = Count)) +
        geom_bar(stat = "identity", fill = "#068D9D") +
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
    taxa_df() %>% select(-geometry)%>%
      DT::datatable(
        options = table_options(),
        selection = "single" # has to be outside out options
      )
  })

  # Output photo of record when row selected from above datatable
  # When a user selects a row in the data table, this observer displays the associated photo for the record

  observeEvent(input$taxa_table_rows_selected, {
    selected_row <- input$taxa_table_rows_selected
    selected_photo <- taxa_df()[selected_row, ]$PhotoID

    if (!is.na(selected_photo)) {
      shinyjs::enable("customModal")
      shinyjs::runjs('$("#customModal").show();')
      shinyjs::runjs(paste('$("#modalImage_taxa").attr("src",', '"', selected_photo, '");', sep = ""))
    }
  })

  # Generate UI dynamically in the server
  output$Modal_taxa <- renderUI({
    fluidPage(
      # HTML div for the modal
      div(
        id = "customModal",
        style = "display: none;",
        div(
          id = "modalContent",
          style = "position: fixed;
          top: 50%;
          left: 50%; transform: translate(-50%, -50%);
          background-color: white;
          padding: 5px 10px 5px 10px;
          box-shadow: 0px 0px 5000px rgba(0, 0, 0, 0.5);
          border-radius: 5px;
          text-align: center;",
          h4("Record Photo"),
          img(id = "modalImage_taxa", style = "display: block; max-width: auto; max-height: 50vh; margin: auto"),
          br(),
          actionButton("closeModalBtn_taxa", "Close Modal", style = "display:
                       block; background-color: #619e62; color: #ffffff;
                       border: none; border-radius: 5px; cursor: pointer;
                       font-size; 16px; margin: auto")
        )
      )
    )
  })


  # Hide the modal when the close button is clicked
  observeEvent(input$closeModalBtn_taxa, {
    shinyjs::runjs('$("#customModal").hide();')
  })


  # Species list datatable
  # This data table lists species and their respective number of records, sorted in descending order
  output$species_list <- DT::renderDT({
    species_list <- taxa_df() %>%
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
      addCircleMarkers(
        lng = ~long,
        lat = ~lat,
        weight = 1,
        label = ~Species,
        labelOptions = labelOptions(noHide = F, textsize = "15px"),
        clusterOptions = markerClusterOptions(),
        data = map_df
      )
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

    # if student has not given description, return blank
    if (is.na(description)) {
      description <- ""
    } else {
      description
    }
  })

  # Display photo of the selected student
  # This dynamically fetches the photo based on the selected student's name
  output$photoOutput <- renderUI({
    name <- input$student_select
    if (file.exists(paste0("www/Team_Data/", name, ".jpg"))) {
      photo <- tags$div(
        tags$img(src = paste0("Team_Data/", name, ".jpg"), height = "100px", style = "margin-right: 10px; border-radius: 30px;")
      )
    } else {
      photo <- tags$div(
        icon("user-secret", style = "margin-right: 10px;font-size: 50px")
      )
    }
    photo
  })

  # Reactive dataframe to filter the main dataset by the selected student
  stud_df <- reactive({
    survey_data() %>%
      filter(Observer == input$student_select)
  })

  # Species list datatable
  # This data table lists species and their respective number of records, sorted in descending order
  output$species_list_student <- DT::renderDT({
    species_list <- stud_df() %>%
      group_by(Species) %>%
      summarise(Num_Records = n()) %>%
      arrange(desc(Num_Records)) # Sorting by number of records in descending order

    species_list %>%
      DT::datatable(
        options = table_options(),
      )
  })

  # Reactive dataframe to summarize statistics related to the selected student for the value boxes
  sum_stud_df <- reactive({
    stud_df() %>%
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
      background = "#068D9D",
      color = "black",
      height = "auto"
    )
  })

  # Number of days the student surveyed
  output$num_days <- renderValueBox({
    customValueBox(sum_stud_df()$days,
      subtitle = "Days surveyed",
      icon = icon("calendar"),
      background = "#226891",
      color = "black",
      height = "auto"
    )
  })

  # Top Taxa recorded by the student
  output$top_taxa <- renderValueBox({
    customValueBox(
      value = sum_stud_df()$top_taxa,
      subtitle = "Top Taxa",
      icon = icon("graduation-cap"),
      background = "#D9DBF1",
      color = "black",
      height = "auto"
    )
  })


  # Number of species recorded per Taxa bar chart
  # This bar chart visualizes the count of species under each Taxa
  output$stud_plot <- renderPlotly({
    df <- stud_df() %>%
      count(Taxa, sort = TRUE) %>%
      rename(Count = n)

    ggplotly(
      ggplot(df, aes(x = reorder(Taxa, -Count), y = Count)) +
        geom_bar(stat = "identity", fill = "#028E9D", width = 0.2) +
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
    stud_df() %>%
      select(Species, Count, Date, Taxa, PhotoID) %>%
      DT::datatable(
        options = table_options(),
        selection = "single"
      )
  })

  # Output photo of record when row selected from above datatable
  # When a user selects a row in the data table, this observer displays the associated photo for the record
  observeEvent(input$student_table_rows_selected, {
    selected_row <- input$student_table_rows_selected
    selected_photo <- stud_df()[selected_row, ]$PhotoID

    if (!is.na(selected_photo)) {
      shinyjs::enable("customModal")
      shinyjs::runjs('$("#customModal").show();')
      shinyjs::runjs(paste('$("#modalImage_student").attr("src",', '"', selected_photo, '");', sep = ""))
    }
  })

  # Hide the modal when the close button is clicked
  observeEvent(input$closeModalBtn_student, {
    shinyjs::runjs('$("#customModal").hide();')
  })

  # Generate UI dynamically in the server
  output$Modal_student <- renderUI({
    fluidPage(
      # Button to trigger the modal
      # HTML div for the modal
      div(
        id = "customModal",
        style = "display: none;",
        div(
          id = "modalContent",
          style = "position: fixed;
          top: 50%;
          left: 50%; transform: translate(-50%, -50%);
          background-color: white;
          padding: 5px 10px 5px 10px;
          box-shadow: 0px 0px 5000px rgba(0, 0, 0, 0.5);
          border-radius: 5px;
          text-align: center;",
          h4("Record Photo"),
          img(id = "modalImage_student", style = "display: block; max-width: auto; max-height: 50vh; margin: auto"),
          br(),
          actionButton("closeModalBtn_student", "Close Modal", style = "display:
                       block; background-color: #619e62; color: #ffffff;
                       border: none; border-radius: 5px; cursor: pointer;
                       font-size; 16px; margin: auto")
        )
      )
    )
  })


  # observeEvent(input$student_table_rows_selected, {
  #   selected_row <- input$student_table_rows_selected
  #   selected_photo <- stud_df()[selected_row, ]$PhotoID
  #
  #   if (!is.na(selected_photo)) {
  #     selected_data <- stud_df()[selected_row, ]
  #     showModal(modalDialog(
  #       title = "Record Photo",
  #       output$imageOutput1 <- renderImage(
  #         {
  #           list(src = paste0("www/Survey_Photos/", selected_photo), width = "100%", height = "auto")
  #         },
  #         deleteFile = FALSE
  #       ),
  #       easyClose = TRUE
  #     ))
  #   }
  # })

  # TAB 4: RECORD FINDER -----------------------------------------------------------------------
  # This section handles the functionality to fetch and display a record photos
  # based on user inputed photoID

  # Observer that listens to 'viewButton' click event
  # Upon activation, the observer displays survey photo matching inputted PhotoID.
  # If the image is not found, it shows an error modal.
  # Output photo of record when row selected from above datatable
  # When a user selects a row in the data table, this observer displays the associated photo for the record

  observeEvent(input$viewButton, {
    # Remove trailing space from PhotoID input
    cleaned_photo_id <- str_trim(input$PhotoID, side = "right")
    filename <- paste0("www/", cleaned_photo_id)

    if (file.exists(filename)) {
      output$imageOutput <- renderUI({
        tags$div(
          class = "record_image",
          tags$img(src = paste(input$PhotoID), style = "display: block; max-width: 100%; max-height: 50vh"),
        )
        
      })
      toggle("imageOutput")
    }
    # else {
    #   # output$error_message <- renderUI({
    #   # p("Image not found. Please enter a valid filename.")
    #   #   })
    #   # toggle("error_message")
    # }
  })
}
