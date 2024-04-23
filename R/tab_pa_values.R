tab_pa_values <- tabPanel("P(a) calculation",

     # Button text wrapper
     tags$head(
      tags$style(HTML("
        .responsive-text {
        white-space: normal !important;
        word-wrap: break-word !important;
      }
    "))),

         # Sidebar layout
         sidebarLayout(

           # Left panel: Inputs to calculate P(a) values
           sidebarPanel(
             # Numeric inputs
             numericInput("num_repetitions", "Number of repetitions", value = 50, min = 1),
             numericInput("max_iterations", "Number of iterations", value = 500, min = 1),
             numericInput("nr_points_moving_avg", "Moving average window size", value = 100, min = 1),

             # Button to calculate P(a) of concepts against themselves
             actionButton("calculate_pa_all", "Calculate p(a) for each concept with itself", width = "100%", class = "responsive-text"),
             tags$div(style = "height:20px;"),

             # Concepts
             selectizeInput("choose_pa_calculation_1", "Choose concept 1", choices = NULL),
             selectizeInput("choose_pa_calculation_2", "Choose concept 2", choices = NULL),
             actionButton("calculate_pa_one", "Calculate p(a) for concept 1 in relation to concept 2", width = "100%", class = "responsive-text"),
             tags$div(style = "height:20px;"),
             numericInput("num_rows_pa_value_data", "Number of rows to show", value = 50, min = 1),
           ),
           # Right Panel: Shows P(a) table
           mainPanel(
             # Download button for p(a) data
             downloadButton("download_pa_values_data", "Download P(a) Values Data"),

             # Show p(a) table
             tabPanel("P(a)", tableOutput("pa_values_table"))
           )
         )
)
