#Inputs
# original_file
# clean_options
# num_rows_up_data

#Outputs
# number_rows
# download_data
# clean_data

tab_upload_data <- tabPanel("Upload data",
                            # Sidebar layout
                            sidebarLayout(

                              # Left Panel: Load data, load example, cleaning options and no. of rows
                              sidebarPanel(
                                # Load CSV
                                fileInput("original_file", "Choose CSV File",accept = c(".csv")),
                                actionButton("load_example_data", "Load Example Data: CPN-27"),

                                # Checkbox for cleaning options
                                checkboxGroupInput("clean_options", "Select cleaning options:",
                                                   choices = list("Change all data to lower case",
                                                                  "Delete repeated rows",
                                                                  "Delete punctuation marks",
                                                                  "Delete all spaces"),
                                                   selected = list("Change all data to lower case",
                                                                   "Delete repeated rows",
                                                                   "Delete punctuation marks",
                                                                   "Delete all spaces")),
                                # Input for number of rows to show
                                numericInput("num_rows_up_data", "Number of Rows to Show", value = 10, min = 1),

                                # Shown number of rows of cleaned data
                                verbatimTextOutput("number_rows")
                              ),

                              # Right Panel: Download button and data
                              mainPanel(
                                # Download button for cleaned data
                                downloadButton("download_data", "Download Cleaned Data"),

                                # Table output for cleaned data
                                tabPanel("Cleaned Data", tableOutput("clean_data"))
                              )
                            )
)
