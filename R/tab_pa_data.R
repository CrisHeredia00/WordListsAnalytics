tab_pa_data <- tabPanel("Inputs to calculate p(a)",

         # Concept selection
         selectizeInput("choose_pa_concept", "Choose concept", choices = NULL),
         textOutput("s_explanation"),

         # Download button for p(a) data
         tags$div(style = "height:20px;"),
         downloadButton("download_pa_data", "Download P(a) Data"),

         # Show p(a) table
         tabPanel("P(a)", tableOutput("pa_table"))

)
