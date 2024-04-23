tab_property_simulator <- tabPanel("Data simulator",

         # Sidebar layout
         sidebarLayout(

           # Left Panel: Inputs and concept
           sidebarPanel(
             numericInput("new_words", label = h3("Additional unique properties"), value = 1, min = 1),
             numericInput("number_subjects", label = h3("Number of subjects to generate"), value = 1, min = 1),
             selectizeInput("property_word", "Choose concept", choices = NULL)

           ),
           # Right Panel: Simulated data
           mainPanel(
             # Download button for estimation data
             downloadButton("download_property_simulator_data", "Download Property simulator data"),

             # Table output of simulated data
             tabPanel("Property Simulator", tableOutput("property_simulator_table"))
           )
         )
)
