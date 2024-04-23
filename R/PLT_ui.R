plt_ui <- fluidPage(
  # App title
  titlePanel("WordListsAnalytics: Multiple data analysis tools for Property Listing Tasks."),

  tabsetPanel(
    tab_upload_data, # Panel for loading and cleaning data
    tab_estimations, # Panel to show Estimation Table
    tab_estimate_participants, # Panel showing coverage slider and participant estimation
    tab_property_simulator, # Panel showing concept input and simulated data
    tab_pa_data, # Panel showing frequency and "s" value by concept and property
    tab_pa_values, # Panel showing P(a) inputs and results
    tab_cluster_image # Panel with cluster input, generation and plotting
  )
)
