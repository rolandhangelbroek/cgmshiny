#' setup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_setup_ui <- function(id){
  ns = NS(id)
  tagList(
    fluidRow(
      box(
        title = 'Study Setup',
        width = 4,
        textInput(ns('study_name'), label = 'Study Name'),
        textAreaInput(ns('study_description'), label = 'Study Description'),
        textInput(ns('no_participants'), label = 'Number of subjects'),
        sliderInput(ns('groups'), label = 'Study arms', min = 1, max = 10, value = 1),
        sliderInput(ns('periods'), label = 'Periods per subject', min = 1, max = 10, value = 1),
        actionButton(ns('save_button'), label = 'Save Study')
      ),
      box(
        title = 'Period Naming',
        width = 4,
        uiOutput(ns('period_naming'))
      ),
      box(
        title = 'Study arm Naming',
        width = 4,
        uiOutput(ns('intervention_naming'))
      )
    ),
    fluidRow(
      box(
        title = 'Studies',
        width = 12,
        DTOutput(ns('study_table'))
      )
    )
  )
}
    
#' setup Server Function
#'
#' @noRd 
mod_setup_server <- function(input, output, session, db, CONSTANTS, table_list, start_uploads){
  ns = session$ns
  
  observeEvent(input$save_button, {
    
    study_table = current_studies = tbl(db, 'studies')
    
    current_studies = study_table %>%
      collect() %>%
      .$study_name %>%
      unique
    
    if (input$study_name == '' || is.null(input$study_name)) {
      showNotification("Please add a study name", type = 'error')
    } else if (is.na(as.numeric(input$no_participants))) {
      showNotification("Number of subjects is not numeric", type = 'error')
    } else if (input$study_name %in% current_studies) {
      showNotification("Study name already exists", type = 'error')
    } else {
      showNotification(glue("Adding study: {input$study_name}"))
      
      new_study = tibble(study_name = input$study_name,
                         study_description = input$study_description,
                         study_periods = input$periods,
                         study_participants = as.integer(input$no_participants))
      
      period_names = tibble(
        study_name = input$study_name,
        period_id = seq_along(1:input$periods),
        period_name = sapply(1:input$periods, function (x) input[[glue('period_{x}_name')]])
      )
      
      group_names = tibble(
        study_name = input$study_name,
        group_name = sapply(1:input$groups, function (x) input[[glue('group_{x}_name')]])
      )
      
      dbSendQuery(db, "BEGIN")
      dbAppendTable(db,
                    'studies',
                    new_study)
      dbAppendTable(db,
                    'period_names',
                    period_names)
      dbAppendTable(db,
                    'group_names',
                    group_names)
      
      dbSendQuery(db, "END")
      
    }
  })
  
  output$study_table = renderDT({
    button_press = input$save_button
    current_studies = tbl(db, 'studies') %>% collect()
    return(current_studies)
  })
  
  output$period_naming = renderUI({
    tagList(
      lapply(seq_along(1:input$periods), function (x) textInput(ns(glue('period_{x}_name')), glue('Period {x} name: ')))
    )
  })
  
  output$intervention_naming = renderUI({
    tagList(
      lapply(seq_along(1:input$groups), function (x) textInput(ns(glue('group_{x}_name')), glue('Study arm {x} name: ')))
    )
  })
  
 
}
    
## To be copied in the UI
# mod_setup_ui("setup_ui_1")
    
## To be copied in the server
# callModule(mod_setup_server, "setup_ui_1")
 
