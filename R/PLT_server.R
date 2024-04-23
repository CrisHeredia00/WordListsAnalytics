plt_server <- function(input, output, session) {

  #### UPLOAD DATA TAB ####
  # initialize "data" as a reactive variable
  myData <- shiny::reactiveVal(data.frame(ID=integer(), Concept=character(), Property=character(), stringsAsFactors=FALSE))

  # If user uploads data
  shiny::observeEvent(input$original_file, {
    shiny::req(input$original_file) # Check truth of data

    df <- readr::read_csv(input$original_file$datapath, locale = readr::locale(encoding = "ISO-8859-1")) # Read as CSV file

    # Check if the data has required columns and transform into a data frame
    if (!all(c("ID", "Concept", "Property") %in% colnames(df))) {
      stop("Data must have 'ID', 'Concept', and 'Property' columns")
    }
    df = data.frame(df)

    myData(df)  # Update the "data" reactive variable with uploaded CSV
  })

  # If user presses load example button
  shiny::observeEvent(input$load_example_data, {
    df <- data.frame(WordListsAnalytics::CPN_27)
    myData(df)  # Update the reactive variable with example data
  })



  # Clean data, based on user input
  cleaned_data <- shiny::reactive({
    data_clean <- myData()

    #It doesn't accept special characters

    # Input options for cleaning process
    if ("Change all data to lower case" %in% input$clean_options) {
      # data_clean <- data_clean %>% dplyr::mutate_if(is.character, tolower)
      #print(data_clean)
      data_clean <- dplyr::mutate_if(data_clean, is.character, tolower)
    }
    if ("Delete repeated rows" %in% input$clean_options) {
      data_clean <- unique(data_clean)
    }
    if ("Delete punctuation marks" %in% input$clean_options) {
      # data_clean <- data_clean %>% dplyr::mutate_all(~gsub("[[:punct:]]", "", .))
      data_clean <- dplyr::mutate_all(data_clean, function(x) gsub("[[:punct:]]", "", x))
    }
    if ("Delete all spaces" %in% input$clean_options) {
      # data_clean <- data_clean %>% dplyr::mutate_all(~gsub(" ", "", .))
      data_clean <- dplyr::mutate_all(data_clean, function(x) gsub(" ", "", x))
    }
    # Returns data after cleaning process
    return(data_clean)
  })

  # It calculates and returns the number of rows of data (show as panel information)
  number_rows_clean_data <- shiny::reactive({
    nrow(cleaned_data())
  })

  # Show cleaned data
  output$clean_data <- shiny::renderUI({
    if (is.null(cleaned_data()) || nrow(cleaned_data()) == 0) {
      shiny::HTML("<p style='color: red; font-size: 20px;'>No data available.</p>")
    } else {
      shiny::tableOutput("clean_data_table")
    }
  })

  output$clean_data_table <- shiny::renderTable({
    # cleaned_data() %>% utils::head(input$num_rows_up_data)
    head_data <- utils::head(cleaned_data(), input$num_rows_up_data)
  })

  # Download cleaned data
  output$download_data <- shiny::downloadHandler(
    filename = function() {
      paste("cleaned_data", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      utils::write.csv(cleaned_data(), file, row.names = FALSE)
    }
  )

  # It renders number of rows message of panel
  output$number_rows <- shiny::renderText({
    paste("Cleaned data has", nrow(cleaned_data()), "rows.")
  })



  #### ESTIMATIONS TAB ####

  # Reactive variable: Calculation of estimation table
  estimation_data <- shiny::reactive({
    if(nrow(cleaned_data()) > 0 ){
      stats::na.omit(generate_norms(cleaned_data()))
    }
  })

  # Reactive variable: Calculation of estimation table NA
  estimation_data_na <- shiny::reactive({
    if(nrow(cleaned_data()) > 0){
      generate_norms(cleaned_data())[!stats::complete.cases(generate_norms(cleaned_data())), ]
    }
  })


  # Render estimation table
  output$estimation_table <- shiny::renderUI({
    if (is.null(estimation_data()) || nrow(estimation_data()) == 0) {
      shiny::HTML("<p style='color: red; font-size: 20px;'>No data available.</p>")
    } else {
      shiny::tableOutput("estimation_data_table")
    }
  })

  output$estimation_data_table <- shiny::renderTable({
    estimation_data()
  })


  # Show estimation table NA
  output$estimation_table_na <- shiny::renderUI({
    if (is.null(estimation_data_na()) || nrow(estimation_data_na()) == 0) {
      shiny::HTML("<p style='color: red; font-size: 20px;'>No data available.</p>")
    } else {
      shiny::tableOutput("estimation_table_na_output")
    }
  })

  output$estimation_table_na_output <- shiny::renderTable({
    estimation_data_na()
  })

  # Download estimation table
  output$download_estimation_data <- shiny::downloadHandler(
    filename = function() {
      paste("estimation_table", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      utils::write.csv(estimation_data(), file, row.names = FALSE)
    }
  )


  #### ESTIMATE PARTICIPANT TAB ####

  # Estimation participant calculation
  participant_data <- shiny::reactive({
    if(!is.null(estimation_data()) && nrow(estimation_data()) > 0 && !is.null(input$coverage)){
      estimate_participant(estimation_data(), input$coverage)
    }
  })

  # Render participant table
  output$participant_table <- shiny::renderUI({
    if (is.null(participant_data()) || nrow(participant_data()) == 0) {
      shiny::HTML("<p style='color: red; font-size: 20px;'>No data available.</p>")
    } else {
      shiny::tableOutput("participant_table_output")
    }
  })

  output$participant_table_output <- shiny::renderTable({
    participant_data()
  })

  # Download participation data
  output$download_participation_data <- shiny::downloadHandler(
    filename = function() {
      paste("participation_table", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      utils::write.csv(participant_data(), file, row.names = FALSE)
    }
  )


  #### PROPERTY SIMULATOR TAB ####
  # Set the choices for the select box
  shiny::observe({
    tryCatch({
      if (!is.null(cleaned_data()) && nrow(cleaned_data()) > 0) {
        choices <- unique(cleaned_data()$Concept)
        shiny::updateSelectizeInput(session, "property_word", choices = choices, server = TRUE)
      }
    }, error = function(e) {
      # Handle the error gracefully
      print("Error: Invalid data uploaded.")
    })
  })

  # Estimation property_simulator
  property_simulator_data <- shiny::reactive({
    if (!is.null(cleaned_data())) {
      data_ps <- cleaned_data() # ps: Property Simulator

      # ID Transformation: Subject -> Number
      # If possible: transform numbers ID back to subjects

      # Step 1: Extract unique character values
      unique_chars <- unique(data_ps$ID)

      # Step 2: Create a mapping between characters and numeric values
      char_to_numeric <- stats::setNames(1:length(unique_chars), unique_chars)

      # Step 3: Replace character values with numeric values
      data_ps$ID <- char_to_numeric[data_ps$ID]

      # Filters data with the user input (Choose concept option of the panel)
      data_ps = data_ps[data_ps$Concept == input$property_word,]
      if (nrow(data_ps) == 0) {
        return(data.frame())  # return an empty data frame
      }
      # Function call to generate simulated data
      property_simulator(data_ps, input$new_words, input$number_subjects)
    }
  })


  # Render data from property simulator
  output$property_simulator_table <- shiny::renderUI({
    if (is.null(property_simulator_data()) || nrow(property_simulator_data()) == 0) {
      shiny::HTML("<p style='color: red; font-size: 20px;'>No data available.</p>")
    } else {
      shiny::tableOutput("property_simulator_table_output")
    }
  })

  output$property_simulator_table_output <- shiny::renderTable({
    property_simulator_data()
  })



  # Download button for property simulator data
  output$download_property_simulator_data <- shiny::downloadHandler(
    filename = function() {
      paste("property_simulator_table", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      utils::write.csv(property_simulator_data(), file, row.names = FALSE)
    }
  )



  #### PA DATA TAB ####
  # Set the choices for the select box of p(a) data
  shiny::observe({
    tryCatch({
      if (!is.null(pa_data())) {
        choices <- unique(pa_data()$Concept)
        shiny::updateSelectizeInput(session, "choose_pa_concept", choices = choices, server = TRUE)
      }
    }, error = function(e) {
      # Handle the error gracefully
      # You can display an error message or take other appropriate actions
      print("Error: Invalid pa_data()")
    })
  })

  # Renders text explaining s value
  output$s_explanation <- shiny::renderText({
    "The 's' value represents the average list length (nr. of properties) listed by participants for a given concept."
  })

  # Data for P(A)
  pa_data <- shiny::reactive({

    cleaned_data <- cleaned_data()
    cleaned_data_count <- dplyr::count(cleaned_data, "Concept" = cleaned_data$Concept, "ID" = cleaned_data$ID)
    grouped_data <- dplyr::group_by(cleaned_data_count, "Concept" = cleaned_data_count$Concept)
    concept_s <- dplyr::summarise(grouped_data, s = mean(grouped_data$n))


    # cleaned_data_count <- dplyr::count(cleaned_data(), Concept, ID)
    # grouped_data <- dplyr::group_by(cleaned_data_count, Concept)
    # concept_s <- dplyr::summarise(grouped_data, s = mean(n))
    # print(concept_s)

    # Version using pipe
    # concept_s <- cleaned_data() %>% dplyr::count(Concept, ID) %>% dplyr::group_by(Concept) %>% dplyr::summarise(s = mean(n))

    grouped_data <- dplyr::group_by(cleaned_data, "Concept" = cleaned_data$Concept, "Property" = cleaned_data$Property)
    summarized_data <- dplyr::summarize(grouped_data, Frequency = dplyr::n())
    df_combined <- dplyr::left_join(summarized_data, concept_s, by = c("Concept" = "Concept"))

    # df_combined <- dplyr::left_join(cleaned_data() %>%
    #                            dplyr::group_by(Concept, Property) %>%
    #                            dplyr::summarize(Frequency = dplyr::n()),
    #                          concept_s, by = dplyr::join_by(Concept == Concept))
    #print(df_combined)
    stats::na.omit(df_combined)
  })

  #Show p(a) table
  output$pa_table <- shiny::renderUI({
    if (is.null(pa_data()) || nrow(pa_data()) == 0) {
      shiny::HTML("<p style='color: red; font-size: 20px;'>No data available.</p>")
    } else {
      shiny::tableOutput("pa_table_output")
    }
  })

  output$pa_table_output <- shiny::renderTable({
    datos <- pa_data()[pa_data()$Concept == input$choose_pa_concept, ]
    datos
  })

  # Download p(a) table
  output$download_pa_data <- shiny::downloadHandler(
    filename = function() {
      paste("pa_data", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      utils::write.csv(pa_data(), file, row.names = FALSE)
    }
  )


  #### PA VALUES TAB ####

  # Set the choices for choosing concept 1
  shiny::observe({
    tryCatch({
      if (!is.null(pa_data())) {
        choices <- unique(pa_data()$Concept)
        shiny::updateSelectizeInput(session, "choose_pa_calculation_1", choices = choices, server = TRUE)
      }
    }, error = function(e) {
      # Handle the error gracefully
      # You can display an error message or take other appropriate actions
      print("Error: Invalid pa_data()")
    })
  })

  # Set the choices for choosing concept 2
  shiny::observe({
    tryCatch({
      if (!is.null(pa_data())) {
        choices <- unique(pa_data()$Concept)
        shiny::updateSelectizeInput(session, "choose_pa_calculation_2", choices = choices, server = TRUE)
      }
    }, error = function(e) {
      # Handle the error gracefully
      # You can display an error message or take other appropriate actions
      print("Error: Invalid pa_data()")
    })
  })

  # Creates reactive vaariable for pa_values
  pa_values_data <- shiny::reactiveValues(data = NULL)

  # In case it presses calculate individual p(a) button
  shiny::observeEvent(input$calculate_pa_one, {
    data_1 = pa_data()[pa_data()$Concept == input$choose_pa_calculation_1,]
    data_2 = pa_data()[pa_data()$Concept == input$choose_pa_calculation_2,]
    shiny::withProgress(message = "Running simulation...", value = 0, {
      pa_values_data$data <- pa_calculator(data_1 = data_1, data_2 = data_2,
                                           nr_repetitions = input$num_repetitions,
                                           max_iterations = input$max_iterations,
                                           nr_points_moving_avg = input$nr_points_moving_avg)
    })
  })

  # In case it presses calculate all values against themselves button
  shiny::observeEvent(input$calculate_pa_all, {
    shiny::withProgress(message = "Running simulation...", value = 0, {
      pa_values_data$data <- pa_calculator(data_1 = pa_data(), data_2 = NULL,
                                           nr_repetitions = input$num_repetitions,
                                           max_iterations = input$max_iterations,
                                           nr_points_moving_avg = input$nr_points_moving_avg)
    })
  })

  # Renders p(a) table
  output$pa_values_table <- shiny::renderUI({
    if (!is.null(pa_values_data$data) && nrow(pa_values_data$data) > 0) {
      shiny::tableOutput("pa_values_table_data")
    } else {
      shiny::tags$h3("P(a) values hasn't been calculated", style = "color: red; font-size: 20px;")
    }
  })

  output$pa_values_table_data <- shiny::renderTable({
    data <- utils::head(pa_values_data$data, input$num_rows_pa_value_data)
    data <- format(data, digits = 9)
    data
  })

  # Download p(a) table
  output$download_pa_values_data <- shiny::downloadHandler(
    filename = function() {
      paste("pa_values_table", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(pa_values_data$data, file, row.names = FALSE)
    }
  )



  #### Threshold and clustering tab ####

  # Graph concept data reactive variable
  graph_concept_data <- shiny::reactive({
    temp_concept_data = cleaned_data()[cleaned_data()$Concept == input$choose_graph_concept, c(1,3)] # It is automatically separating the concept
    value_counts <- table(temp_concept_data$ID)

    # Identify values that appear more than once
    values_to_keep <- names(value_counts[value_counts > 1])

    # Filter the data frame to keep only the desired values
    data_final <- temp_concept_data[temp_concept_data$ID %in% values_to_keep, ]

    unique_chars <- unique(data_final$ID)

    # Step 2: Create a mapping between characters and numeric values
    char_to_numeric <- stats::setNames(1:length(unique_chars), unique_chars)

    # Step 3: Replace character values with numeric values
    data_final$ID <- char_to_numeric[data_final$ID]

    data_final
  })

  #Graph data reactive value
  graph_data <- shiny::reactive({
    if(nrow(graph_concept_data()) > 1){ # To check if was correcly loaded
      datos = createClusterDataGraph(input$graph_range[1], input$graph_range[2], input$graph_step, graph_concept_data())
      datos
    }
  })

  # Download graph data
  output$download_graph_data <- shiny::downloadHandler(
    filename = function() {
      paste("graph_data", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      utils::write.csv(graph_data(), file, row.names = FALSE)
    }
  )

  # Download graph 1: Avg. no. of clusters per subject
  output$download_graph_1 <- shiny::downloadHandler(
    filename = function() {
      paste("graph_1_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Calculate text size based on image size
      standard_resolution <- sqrt(800 * 800)
      actual_resolution <- sqrt(input$size_graph_width * input$size_graph_height)
      base_text_size <- 14 * (actual_resolution / standard_resolution)
      title_text_size <- base_text_size * 1.5  # Make the title text 50% larger

      # Save the plot to a PNG file
      grDevices::png(file, width = input$size_graph_width, height = input$size_graph_height)
      p <- ggplot2::ggplot(graph_data()) + ggplot2::aes_string(x="threshold",y="meanClusters") + ggplot2::geom_line(size=2)+
        ggplot2::theme_bw() + ggplot2::labs(y="Average number of clusters per subject",color="Age",linetype="Age")+
        ggplot2::scale_y_continuous(breaks=seq(0, 11,0.5))+
        ggplot2::scale_x_continuous(breaks=seq(input$graph_range[1],input$graph_range[2], input$graph_step * 5 ))+
        ggplot2::theme(legend.background = ggplot2::element_rect(size=0.5, linetype="solid", colour ="black"))+
        ggplot2::theme(axis.text = ggplot2::element_text(size = base_text_size),axis.title = ggplot2::element_text(size = title_text_size))
      print(p)
      grDevices::dev.off()
    }
  )

  # Download graph 2: Avg. no. of shifts per subject
  output$download_graph_2 <- shiny::downloadHandler(
    filename = function() {
      paste("graph_2_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Calculate text size based on image size
      standard_resolution <- sqrt(800 * 800)
      actual_resolution <- sqrt(input$size_graph_width * input$size_graph_height)
      base_text_size <- 14 * (actual_resolution / standard_resolution)
      title_text_size <- base_text_size * 1.5  # Make the title text 50% larger

      # Save the plot to a PNG file
      grDevices::png(file, width = input$size_graph_width, height = input$size_graph_height)
      p <-  ggplot2::ggplot(graph_data()) + ggplot2::aes_string(x="threshold",y="meanClusters") + ggplot2::geom_line(size=2)+
        ggplot2::theme_bw() + ggplot2::labs(y="Average number of clusters per subject",color="Age",linetype="Age")+
        ggplot2::scale_y_continuous(breaks=seq(0, 11,0.5))+
        ggplot2::scale_x_continuous(breaks=seq(input$graph_range[1],input$graph_range[2], input$graph_step * 5 ))+
        ggplot2::theme(legend.background = ggplot2::element_rect(size=0.5, linetype="solid", colour ="black"))+
        ggplot2::theme(axis.text = ggplot2::element_text(size = base_text_size),axis.title = ggplot2::element_text(size = title_text_size))
      print(p)
      grDevices::dev.off()
    }
  )


  # ggplot graph 1: : Avg. no. of clusters per subject
  # Create a new reactive value
  output$grafico_1 <- shiny::renderPlot({
    # Check if there is data
    if (is.null(graph_data()) || nrow(graph_data()) == 0) {
      # If there is no data, return a blank plot with the message
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No data available",
                 colour = "red", size = 10, hjust = 0.5, vjust = 0.5)
    } else {
      # If there is data, create the ggplot graph
      ggplot2::ggplot(graph_data()) + ggplot2::aes_string(x="threshold",y="meanClusters") + ggplot2::geom_line(size=2)+
        ggplot2::theme_bw() + ggplot2::labs(y="Average number of clusters per subject",color="Age",linetype="Age")+
        ggplot2::scale_y_continuous(breaks=seq(0, 11,0.5))+
        ggplot2::scale_x_continuous(breaks=seq(input$graph_range[1],input$graph_range[2], input$graph_step * 5 ))+
        ggplot2::theme(legend.background = ggplot2::element_rect(size=0.5, linetype="solid", colour ="black"))+
        ggplot2::theme(axis.text = ggplot2::element_text(size = 12),axis.title = ggplot2::element_text(size = 14))
    }
  })



  # ggplot graph 2: Avg. no. of shifts per subject
  output$grafico_2 <- shiny::renderPlot({
    # Check if there is data
    if (is.null(graph_data()) || nrow(graph_data()) == 0) {
      # If there is no data, return a blank plot with the message
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No data available",
                 colour = "red", size = 10, hjust = 0.5, vjust = 0.5)
    } else {
      # If there is data, create the ggplot graph
      ggplot2::ggplot(graph_data()) + ggplot2::aes_string(x="threshold",y="meanShift") + ggplot2::geom_line(size=2)+
        ggplot2::theme_bw() + ggplot2::labs(y="Average number of shifts per subject",color="Age",linetype="Age")+
        ggplot2::scale_y_continuous(breaks=seq(0,50,0.5))+
        ggplot2::scale_x_continuous(breaks=seq(input$graph_range[1],input$graph_range[2],input$graph_step * 5 ))+
        ggplot2::theme(legend.background = ggplot2::element_rect(size=0.5, linetype="solid", colour ="black"))+
        ggplot2::theme(axis.text = ggplot2::element_text(size = 12),axis.title = ggplot2::element_text(size = 14))
    }
  })



  # Set the choices for the graph select concept box
  shiny::observe({
    tryCatch({
      choices <- unique(cleaned_data()$Concept)
      shiny::updateSelectizeInput(session, "choose_graph_concept", choices = choices, server = TRUE)
    }, error = function(e) {
      # Handle the error gracefully
      # You can display an error message or take other appropriate actions
      print("Error: Invalid cleaned_data()")
    })
  })


  # Obtain clustering threshold searching for the biggest value
  shiny::observe({
    tryCatch({
      if (!is.null(graph_data())) {
        valor_threshold <- graph_data()[(which.max(graph_data()$meanClusters)), "threshold"]
        shiny::updateNumericInput(session, "threshold_graph", value = valor_threshold)
      }
    }, error = function(e) {
      # Handle the error gracefully
      print("Error: Invalid graph_data()")
    })
  })

  # Number per shift data
  number_shift_data <- shiny::reactive({
    if(nrow(graph_concept_data()) > 1){ # Check if data was correctly loaded
      data = getValuesClusterKids(graph_concept_data(), input$threshold_graph)
      numeric_values <- data$Subject
      char_values <- names(char_to_numeric)[match(numeric_values, char_to_numeric)]
      data$Subjects <- char_values
      data
    }
  })

  # Download number per shift data
  output$download_number_shift_data <- shiny::downloadHandler(
    filename = function() {
      paste("number_shift_data", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      utils::write.csv(number_shift_data(), file, row.names = FALSE)
    }
  )

  # Graph cluster download
  output$download_graph_cluster <- shiny::downloadHandler(
    filename = function() {
      paste("graph_cluster_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Save the plot to a PNG file
      grDevices::png(file, width = input$size_graph_cluster_width, height = input$size_graph_cluster_height)
      p <-  clusterImage(graph_concept_data(), input$threshold_graph) # It doesn't require third argument because concept was already selected
      print(p)
      grDevices::dev.off()
    }
  )

  # Output cluster graph
  output$grafico_clusters <- shiny::renderPlot({
    # Check if there is data
    if (is.null(graph_concept_data()) || nrow(graph_concept_data()) == 0) {
      # If there is no data, return a blank plot with the message
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No data available",
                 colour = "red", size = 10, hjust = 0.5, vjust = 0.5)
    } else {
      # If there is data, create the ggplot graph
      a1<-graph_concept_data()
      a2<-input$threshold_graph
      a3<-input$choose_graph_concept

      temp = clusterImage(graph_concept_data(), input$threshold_graph) # It doesn't require third argument because concept was already selected
      temp[[1]]
    }
  })

  # Download clustering graph data
  output$download_cluster_data <- shiny::downloadHandler(
    filename = function() {
      paste("cluster_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      temp <- clusterImage(graph_concept_data(), input$threshold_graph) # input$choose_graph_concept
      utils::write.csv(temp[[2]], file, row.names = FALSE)
    }
  )
}
