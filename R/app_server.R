#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  callModule(setup, "setupui")
  callModule(subject, "subjectui")
  callModule(upload, "uploadui")
  callModule(processing, "processingui")
  callModule(analytics, "analyticsui")
}
