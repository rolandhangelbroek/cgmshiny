#' processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session,db,CONSTANTS Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_processing_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = 'Data Selection',
        uiOutput(ns('data_dropdown')),
        textInput(ns('data_name'), 'Processed Data File Name'),
        actionButton(ns('refresh_button'), 'Refresh List'),
        actionButton(ns('save_button'), 'Save Processed File')
      ), 
      box(
        title = 'Interpolation Settings',
        sliderInput(ns('segment_size'), label = 'Maximum Interpolation Gap (minutes)', min = 15, max = 300, step = 15, value = 60),
        # sliderInput(ns('chunk_distance'), label = 'Time Distance Between Chunks (hours)', min = 1, max = 300, step = 4, value = 100),
        sliderInput(ns('minute_rounding'), label = 'Round timestamp to whole minutes', min = 5, max = 30, step = 5, value = 15)
      )
    ),
    fluidRow(
      box(
        width = 12,
        selectInput(ns('pro_plot_coloring'), label = 'Color by: ', choices = c('missings', 'chunks', 'missings & chunks'), selected = 'missings'),
        plotlyOutput(ns('pro_overview'))
      )
    ),
    fluidRow(
      box(
        title = 'Data File',
        width = 12,
        DTOutput(ns('proc_data'))
      )
    )
  )
}

#' processing Server Function
#'
#' @noRd 
mod_processing_server <- function(input, output, session, db, CONSTANTS, table_list, start_uploads){
  ns = session$ns
  
  output$data_dropdown = renderUI({
    datasets = tbl(db, 'uploaded_files') %>%
      pull(file_name) %>%
      unique()
    
    available_raw = tbl(db, 'raw_uploads') %>%
      pull(file_name) %>%
      unique()
    
    list_files = intersect(datasets, available_raw)
    
    p = input$refresh_button
    
    selectInput(ns('data_selection'), 'Data File', choices = list_files, selected = list_files[1])
  })
  
  process_datafile = reactive({
    req(input$data_selection)
    req(input$segment_size)
    # req(input$chunk_distance)
    req(input$minute_rounding)
    
    data_file = input$data_selection
    fragment_size = input$segment_size * 60
    chunk_size = 100 * 60 * 60
    
    selected_data = tbl(db, 'raw_uploads') %>%
      filter(file_name == data_file) %>%
      collect() %>%
      mutate(tijd = as.POSIXct(tijd, origin = '1970-01-01')) %>%
      mutate(segment = ifelse((unix_t - lag(unix_t, 1)) > fragment_size, 1, 0),
             segment = replace_na(segment, 0) %>% cumsum,
             segment = segment + 1) %>%
      mutate(tijd = round_date(tijd, period(mins = input$minute_rounding))) %>%
      group_by(segment) %>%
      complete(tijd = seq(from = min(tijd), 
                          to = max(tijd),  
                          by = as.difftime(input$minute_rounding, units = 'mins'))) %>%
      mutate(interpolated = ifelse(is.na(glucose), TRUE, FALSE),
             glucose = na.approx(glucose, x = tijd)) %>%
      ungroup %>% 
      mutate(chunk = ifelse((unix_t - lag(unix_t, 1)) > chunk_size, 1, 0),
             chunk = replace_na(chunk, 0) %>% cumsum,
             chunk = chunk + 1,
             segment = segment %>% as.character,
             chunk = chunk %>% as.character) %>% 
      group_by(chunk) %>%
      complete(tijd = seq(from = min(tijd), 
                          to = max(tijd),  
                          by = as.difftime(input$minute_rounding, units = 'mins'))) %>%
      mutate(timestep = hour(tijd) * 60 + minute(tijd))
    
    return(selected_data)
  })
  
  
  output$proc_data = renderDT({
    req(process_datafile())
    
    process_datafile()
  })
  
  
  output$pro_overview = renderPlotly({

    req(process_datafile(), input$data_selection, input$segment_size)
    
    full_data = process_datafile() 
    
    full_data_ni = full_data %>%
      filter(interpolated == FALSE)
    
    full_data_interp = full_data %>%
      filter(interpolated == TRUE)
    
    full_data_missing = full_data %>%
      mutate(missing = is.na(glucose),
             glucose = na.approx(glucose, x = tijd)) %>% 
      filter(missing == TRUE)
    
    if (input$pro_plot_coloring == 'missings') {
      plot_ly(x = ~ tijd, y = ~ glucose, name = 'Glucose', data = full_data_ni, color = I("grey"), mode = 'lines', type = 'scatter') %>%
        add_trace(y = ~glucose, name = 'Glucose - Interpolated', mode = 'markers', color = I("orange"), data = full_data_interp) %>%
        add_trace(y = ~glucose, name = 'Glucose - Missing', mode = 'markers', color = I("red"), data = full_data_missing) %>%
        rangeslider() %>%
        layout(xaxis = list(title = ''),
               yaxis = list(title = 'Glucose')) 
    } else if (input$pro_plot_coloring == 'chunks') {
      plot_ly(x = ~ tijd, y = ~ glucose, data = full_data, color = ~ chunk, mode = 'lines', type = 'scatter') %>%
        rangeslider() %>%
        layout(xaxis = list(title = ''),
               yaxis = list(title = 'Glucose'))
    } else if (input$pro_plot_coloring == 'missings & chunks') {
      plot_ly(x = ~ tijd, y = ~ glucose, name = 'Glucose', data = full_data_ni, color = ~ chunk, mode = 'lines', type = 'scatter') %>%
        add_trace(y = ~glucose, name = 'Glucose - Interpolated', mode = 'markers', color = I("orange"), data = full_data_interp) %>%
        add_trace(y = ~glucose, name = 'Glucose - Missing', mode = 'markers', color = I("red"), data = full_data_missing) %>%
        rangeslider() %>%
        layout(xaxis = list(title = ''),
               yaxis = list(title = 'Glucose')) 
    }
    
  })
  
  
  observeEvent(input$save_button, {
    
    print(input$data_name)
    
    if (input$data_name == '' | is.null(input$data_name))  {
      
      showNotification('Please give the processed data file a name before saving', type = 'error', session = session)
      
    } else {

      interpolated_data = process_datafile() %>%
        mutate(processed_name = input$data_name)
      

      if (!'interpolated_data' %in% dbListTables(db)) {
        cat('Creating interpolated data table\n')
        dbCreateTable(db, 
                      'interpolated_data', 
                      interpolated_data)
      }
      
      dbSendQuery(db, "BEGIN")
      dbAppendTable(db,
                    'interpolated_data',
                    interpolated_data)
      dbSendQuery(db, "END")
    }
  })
  
}

