#' subject UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session,db,CONSTANTS Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_subject_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 3,
        uiOutput(ns('study_select_ui')),
        actionButton(ns('refresh_button'), 'Refresh Lists'),
        # actionButton(ns('get_data_button'), 'Get Previous Data'),
        actionButton(ns('save_button'), 'Save')
      ),
      box(
        width = 9,
        title = 'Data Preview',
        selectInput(ns('dataset_selection'), label = 'Plot Dataset', choices = '', selected = ''),
        plotlyOutput(ns('dataset_plot'))
      )
    ),
    uiOutput(ns('subject_table_ui'))
  )
}
    
#' subject Server Function
#'
#' @noRd 
mod_subject_server <- function(input, output, session, db, CONSTANTS, table_list, start_uploads){
  ns <- session$ns
  
  render_subject_input = function (id, periods) {
    subject_id = id
    
    period_tags = lapply(1:periods, function (period_number) {
      list(
        dateRangeInput(ns(glue('period_{subject_id}_{period_number}')), label = glue('Period {period_number} - {period_names()$period_name[period_number]}')),
        selectInput(ns(glue('source_{subject_id}_{period_number}')), label = glue('Period {period_number} - {period_names()$period_name[period_number]} - data source'), choices = '')
      )
    }
    )
    
    r = list(
      fluidRow(
        box(width = 12,
            textInput(ns(glue('subject_label_{subject_id}')), label = glue('Subject label for subject {subject_id}')),
            selectInput(ns(glue('subject_group_{subject_id}')), label = glue('Subject intervention group.'), choices = group_names()),
            period_tags
        )
      )
    )
    
    return(r)
  }
  
  get_subject_period_data = function (subject, period) {
    
    input_data = input[[glue('period_{subject}_{period}')]]
    period_source = input[[glue('source_{subject}_{period}')]]
    
    row = data.frame(subject_id = subject,
                     period_id = period,
                     period_start = input_data[1],
                     period_end = input_data[2],
                     period_source = period_source,
                     stringsAsFactors = FALSE)
    
    return(row)
    
  }
  
  
  get_subject_data = function (subject, study, periods) {
    subject_id = subject
    study_name = study
    subject_label_name = glue('subject_label_{subject_id}')
    subject_label = input[[subject_label_name]]
    subject_group_name = glue('subject_group_{subject_id}')
    subject_group = input[[subject_group_name]]
    
    subject_row = data.frame(study_name = study_name,
                             subject_id = subject_id,
                             subject_label = subject_label,
                             subject_group = subject_group,
                             stringsAsFactors = FALSE) 
    
    periods = lapply(1:periods, function (x) get_subject_period_data(subject = subject_id, period = x)) %>%
      dplyr::bind_rows()
    
    r = list(subject_row = subject_row,
             period_df = periods)
    
    return(r)
  } 
  
  period_names = reactive({
    study_selected = input$study_select
    
    period_data = dplyr::tbl(db, 'period_names') %>%
      dplyr::filter(study_name == study_selected) %>%
      dplyr::arrange(period_id) %>%
      dplyr::collect()
    
    return(period_data)
  })
  
  group_names = reactive({
    study_selected = input$study_select
    
    period_data = dplyr::tbl(db, 'group_names') %>%
      dplyr::filter(study_name == study_selected) %>%
      dplyr::arrange(group_name) %>%
      dplyr::collect() %>%
      dplyr::pull(group_name)
    
    return(period_data)
  })
  
  observeEvent(input$study_select, {
    req(input$study_select)
    
    study_sele = input$study_select
    
    study_info = dplyr::tbl(db, 'studies') %>%
      dplyr::collect() %>%
      dplyr::filter(study_name == study_sele) %>%
      as.data.frame(stringsAsFactors = FALSE)
    
    update_subject_data = dplyr::tbl(db, 'subjects') %>%
      dplyr::filter(study_name == study_sele) %>%
      dplyr::collect()
    
    if (nrow(update_subject_data) > 0) {
      apply(update_subject_data, 1, function (x) {
        
        sid = x['subject_id'] %>% unname
        lab = x['subject_label'] %>% unname
        grp = x['subject_group'] %>% unname
        
        updateTextInput(session, glue('subject_label_{sid}'), value = lab)
        updateSelectInput(session, glue('subject_group_{sid}'), selected = grp)
      })
    }
    
    
    update_period_data = dplyr::tbl(db, 'subject_periods') %>%
      dplyr::filter(study_name == study_sele) %>%
      dplyr::collect()
    
    if (nrow(update_period_data) > 0) {
      apply(update_period_data, 1, function (x) {
        
        sid = x['subject_id'] %>% unname
        pid = x['period_id'] %>% unname
        pstart = x['period_start'] %>% unname
        pend = x['period_end'] %>% unname
        psource = x['period_source'] %>% unname
        
        updateSelectInput(session, glue('source_{sid}_{pid}'), selected = psource)
        updateDateRangeInput(session, glue('period_{sid}_{pid}'), start = pstart, end = pend)
      })
    }
    
  })
  
  
  
  observeEvent(input$save_button, {
    req(input$study_select)
    
    study_sele = input$study_select
    
    study_info = dplyr::tbl(db, 'studies') %>%
      dplyr::collect() %>%
      dplyr::filter(study_name == study_sele) %>%
      as.data.frame(stringsAsFactors = FALSE)
    
    study_participants = study_info$study_participants[1]
    periods = study_info$study_periods[1]
    
    subject_data = lapply(1:study_participants, function (x) get_subject_data(subject = x, study = study_sele, periods = periods))
    
    subject_rows = lapply(subject_data, function (x) x[['subject_row']]) %>% 
      dplyr::bind_rows() %>%
      dplyr::mutate(subject_id = as.character(subject_id))
    
    old_subject_data = dplyr::tbl(db, 'subjects') %>%
      dplyr::filter(!study_name == study_sele) %>%
      dplyr::collect()
    
    new_subject_data = old_subject_data %>%
      dplyr::bind_rows(subject_rows) %>%
      dplyr::mutate(subject_id = as.character(subject_id),
             subject_label = ifelse(is.na(subject_label) | subject_label == '', subject_id, subject_label) %>% make.unique) 
    
    dbWriteTable(db, 'subjects', new_subject_data, overwrite = TRUE, temporary = FALSE)
    
    subject_periods = lapply(subject_data, function (x) x[['period_df']]) %>% 
      dplyr::bind_rows() %>%
      dplyr::mutate(subject_id = as.character(subject_id),
             period_start = as.character(period_start),
             period_end = as.character(period_end)) %>%
      dplyr::left_join(subject_rows) %>%
      dplyr::select(-subject_label)  
    
    old_period_data = dplyr::tbl(db, 'subject_periods') %>%
      dplyr::filter(!study_name == study_sele) %>%
      dplyr::collect()
    
    new_period_data = old_period_data %>%
      dplyr::bind_rows(subject_periods) %>%
      dplyr::mutate(subject_id = as.character(subject_id)) 
    
    dbWriteTable(db, 'subject_periods', new_period_data, overwrite = TRUE, temporary = FALSE)
    
  })
  
  output$study_select_ui = renderUI({
    p = input$refresh_button
    
    study_table = dplyr::tbl(db, 'studies')
    
    current_studies = study_table %>%
      dplyr::collect() %>%
      dplyr::pull(study_name) %>%
      unique
    
    selectInput(ns('study_select'), label = 'Study', choices = current_studies)
  })
  
  observe({
    
    req(input$study_select)
    
    study_sele = input$study_select
    
    study_info = dplyr::tbl(db, 'studies') %>%
      dplyr::collect() %>%
      dplyr::filter(study_name == study_sele) %>%
      as.data.frame(stringsAsFactors = FALSE)
    
    study_participants = study_info$study_participants[1]
    periods = study_info$study_periods[1]
    
    new_uploads = dplyr::tbl(db, 'interpolated_data') %>%
      dplyr::count(processed_name) %>%
      dplyr::collect() %>%
      dplyr::pull(processed_name)
    
    updateSelectInput(session, 'dataset_selection', label = 'Plot Dataset', choices = new_uploads, selected = new_uploads[1])
    
    for (participant in 1:study_participants) {
      for (period in 1:periods) {
        updateSelectInput(session, inputId = glue('source_{participant}_{period}'), label = glue('Period {period} - {period_names()$period_name[period]} - data source'), choices = new_uploads, selected = new_uploads[1])
      }
    }
    
  })
  
  output$subject_table_ui = renderUI({
    req(input$study_select)
    
    study_sele = input$study_select
    
    study_info = current_studies = dplyr::tbl(db, 'studies') %>%
      dplyr::collect() %>%
      dplyr::filter(study_name == study_sele) %>%
      as.data.frame(stringsAsFactors = FALSE)
    
    study_participants = study_info$study_participants[1]
    periods = study_info$study_periods[1]
    
    participants_ui_elements = lapply(1:study_participants,
                                      function (x) render_subject_input(id = x, periods = periods))
    
  })
  
  output$dataset_plot = renderPlotly({
    req(input$dataset_selection)
    
    dataset = input$dataset_selection
    
    full_data = dplyr::tbl(db, 'interpolated_data') %>%
      dplyr::filter(processed_name == dataset) %>%
      dplyr::collect() %>%
      dplyr::mutate(tijd = as.POSIXct(tijd, origin = '1970-01-01')) 
    
    full_data_ni = full_data %>%
      dplyr::filter(interpolated == FALSE)
    
    full_data_interp = full_data %>%
      dplyr::filter(interpolated == TRUE)
    
    full_data_missing = full_data %>%
      dplyr::mutate(missing = is.na(glucose),
             glucose = na.approx(glucose)) %>% 
      dplyr::filter(missing == TRUE)
    
    
    plot_ly(x = ~ tijd, y = ~ glucose, name = 'Glucose', data = full_data_ni, color = I("grey"), mode = 'lines', type = 'scatter') %>%
      add_trace(y = ~glucose, name = 'Glucose - Interpolated', mode = 'markers', color = I("orange"), data = full_data_interp) %>%
      add_trace(y = ~glucose, name = 'Glucose - Missing', mode = 'markers', color = I("red"), data = full_data_missing) %>%
      rangeslider() %>%
      layout(xaxis = list(title = ''),
             yaxis = list(title = 'Glucose')) 
    
  })
}
    
## To be copied in the UI
# mod_subject_ui("subject_ui_1")
    
## To be copied in the server
# callModule(mod_subject_server, "subject_ui_1")
 
