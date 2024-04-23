#' CPN Example data
#'
#' The CPN120 dataset is a property listing task dataset over 120 concepts (60 concrete and 60 abstract). The dataset was generated
#' from 221 voluntary Chilean university students (71\% male, 28.5\% female, average age = 23.7 years with s.d. = 6.2 years).
#' Each participant listed up to 10 characteristics for each concept. The dataset had over 32,000 responses,
#' which were categorized into valid and invalid, obtaining 31,864 valid responses.
#'
#' @format A data frame with 31864 rows and 3 variables:
#' \describe{
#'   \item{ID}{ID for original subject}
#'   \item{Concept}{Concept asked}
#'   \item{Property}{Property given by the subject }
#'   ...
#' }
#' @source Fondecyt proyect #1200139, Chilean goverment
#' @usage data(CPN_120)
"CPN_120"
