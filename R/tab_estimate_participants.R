tab_estimate_participants <- tabPanel("Sample size estimation",

         # Sidebar layout
         sidebarLayout(

           # Left panel: Coverage slider
           sidebarPanel(
             # coverage: fraction of the total incidence probabilities of the reported properties that are in the reference sample
             sliderInput("coverage", label = h3("Required coverage"), min = 0,
                         max = 1, value = 0.5, step = 0.01)

           ),
           # Right panel: Table showing Concept T_star S_hat_star and Warning
           mainPanel(
             # Download button for estimation data
             downloadButton("download_participation_data", "Download Participant estimation"),

             # Table output for participant estimation
             tabPanel("Participant estimation", tableOutput("participant_table"))
           )
         )
)
