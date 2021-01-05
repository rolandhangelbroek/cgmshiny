#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(
      sidebarMenu(id = 'tabs',
                  menuItem('Study Setup', tabName = 'setup'),
                  menuItem('Data Upload', tabName = 'upload'),
                  menuItem('Data Processing', tabName = 'processing'),
                  menuItem('Subject Setup', tabName = 'subject'),
                  menuItem('Analytics', tabName = 'analytics')
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'setup',
                setupui("setupui")
        ),
        tabItem(tabName = 'processing',
                processingui("processingui")
        ),
        tabItem(tabName = 'analytics',
                analyticsui("analyticsui")
        ),
        tabItem(tabName = 'subject',
                subjectui("subjectui")
        ),
        tabItem(tabName = 'upload',
                uploadui("uploadui"))
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'cgmshiny'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

