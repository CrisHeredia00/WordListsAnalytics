tab_cluster_image <- tabPanel("Cluster and shifts",

    # This panel is generated using div, some style are set to improve visualization
    tags$style(HTML("
    .gray-background {
      background-color: #f7f7f7;
      padding: 15px;
      border-right: 1px solid #ddd;
    }
    #my-tab .btn {
      width: 100%;
      white-space: normal !important;
      word-wrap: break-word !important
    }")),

    # Panel code
    div(id = "my-tab",
        fluidRow(
          # First column: Includes all inputs to generate and show clusters
          column(2,
            div(class = "gray-background",
                 selectizeInput("choose_graph_concept", "Choose concept", choices = NULL),
                 h3("Threshold information"),
                 sliderInput("graph_range", "Graph range:", min = 0.001, max = 1, value = c(0.001, 0.5), step = 0.01),
                 numericInput("graph_step", "X axis step:", value = 0.01, min = 0.001, step =0.001),
                 downloadButton("download_graph_data", "Download graph data", class = "full-width-button"),
                 downloadButton("download_number_shift_data", "Download number of shift per subject", class = "full-width-button"),
                 h3("Clustering Graph"),
                 numericInput("threshold_graph", "Threshold for clustering:", value = 0, min = 0, step = 0.1),
                 h3("Download graphs"),
                 numericInput("size_graph_width", "Width (number of pixels):", value = 1600, min = 1, step =1),
                 numericInput("size_graph_height", "Height (number of pixels):", value = 800, min = 1, step =1),
                 tags$div(style = "height:5px;"),
                 downloadButton("download_graph_1", "Download graph cluster", class = "full-width-button"),
                 downloadButton("download_graph_2", "Download graph shift", class = "full-width-button"),
                 downloadButton("download_cluster_data", "Download cluster matrix", class = "full-width-button"),
            )
          ),
          # Second Column: Threshold plots
          column(5,
                 plotOutput("grafico_1"),
                 plotOutput("grafico_2"),
          ),
          # Third Column: Cluster image
          column(5,

                 plotOutput("grafico_clusters")
          )
        )
    )
)
