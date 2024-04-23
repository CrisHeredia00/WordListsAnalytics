#' PLT App function
#'
#' Main function of package. It executes a shiny application in local session.
#' The user can load data, generate new descriptive words, apply a new clustering model, and
#' use simulations to estimate the probability that two persons describe the same words
#' based on their descriptions.
#'
#' @return None (it executes a shiny application).
#'
#' @export
#'
#' @import shiny
#'
#' @examples
#' if(interactive()){
#'   WordListsAnalytics()
#' }
WordListsAnalytics <- function() {
  ui <- plt_ui

  server <- plt_server

  shiny::shinyApp(ui, server)
}

