tab_estimations <- tabPanel("Estimated parameters",
                           mainPanel(
                             # Download button for estimation data
                             downloadButton("download_estimation_data", "Download Estimation Data"),

                             # Table output for estimation table
                             h2("Omitted NAs"),
                             tabPanel("Estimations na", tableOutput("estimation_table_na")),
                             h2("Estimation table"),
                             tabPanel("Estimations", tableOutput("estimation_table"))
                           )
)
