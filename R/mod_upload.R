#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session,db,CONSTANTS Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @importFrom shiny NS tagList 
mod_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = 'File Processing',
        width = 6,
        fileInput(ns('up_files'), label = 'Data', multiple = TRUE, accept = c('text/csv', 'text/plain')),
        textInput(ns('up_skip'), label = 'Skip Lines', value = '2'),
        selectInput(ns('up_timestamp_column'), label = 'Date & Time Columns', choices = NULL, multiple = TRUE),
        selectInput(ns('up_timestamp_format'), label = 'Date & Time Format', choices = c('dmy_hm', 'dmy_hms', 'ymd_hm', 'ymd_hms', 'mdy_hm', 'mdy_hms'), selected = 'dmy_hm'),
        selectInput(ns('up_glucose_column'), label = 'Glucose Column', choices = NULL),
        actionButton(ns('up_upload_button'), label = 'Add to database')
      ),
      box(
        title = 'Files in database',
        width = 6,
        DTOutput(ns('up_curr_files'))
      ),
      fluidRow(
        box(title = 'Preview - raw text',
            width = 12,
            DTOutput(ns('up_text_field'))
        )
      ),
      fluidRow(
        box(title = 'Preview - raw table after skipping lines',
            width = 12,
            DTOutput(ns('up_output_box'))
        )
      ),
      fluidRow(
        box(title = 'Preview - processed data',
            width = 12,
            DTOutput(ns('up_output_processed'))
        )
      )
    )
  )
}

#' upload Server Function
#'
#' @noRd 
mod_upload_server <- function(input, output, session, db, CONSTANTS, table_list, start_uploads){
  ns <- session$ns
  
  process_text = function (file_location, file_name) {
    readLines(file_location) %>%
      enframe(name = NULL, value = 'line')
  }
  
  process_upload = function (file_location, file_name, skip_arg) {
    
    tryCatch(
      {
        
        if (skip_arg == '') skip_arg = 0
        
        file_name = paste0(now() %>% as.character() %>% str_replace_all('[[:punct:]]', '_') %>% str_replace('\\ ', '_'), '__', file_name)
        
        df = fread(file_location, skip = skip_arg) %>% 
          clean_names() %>%
          as.data.frame() %>%
          mutate(file_name = file_name) 
        
        return(df)
        
      },
      error = function(e) {
        stop(safeError(e))
      })
  }
  
  finalize_upload = function (raw_df, dt_cols, gluc_col, ts_fmt) {
    if (any(dt_cols == '', gluc_col == '', is.null(dt_cols), is.null(gluc_col))) return(NULL)
    
    df = raw_df
    
    if (length(dt_cols) > 1) {
      df = df %>%
        unite(tijd, all_of(dt_cols), sep = ' ')
    } else{
      df$tijd = df[,dt_cols]
    }
    
    df$glucose = df[,gluc_col]
    
    if (is.character(df$glucose) & any(str_detect(df$glucose, fixed(',')))) {
      df$glucose = df$glucose %>%
        str_replace_all(fixed(','), '.') %>%
        as.numeric() 
    } else if (is.character(df$glucose)) {
      df$glucose = as.numeric(df$glucose)
    } 
    
    validate (
      need(!all(is.na(df$glucose)), 'No glucose data found or glucose column is not numeric. Please double check the glucose column.')
    ) 
    
    
    df = df %>%
      select(tijd, glucose, file_name) %>%
      mutate(tijd = do.call(ts_fmt, list(tijd)),
             unix_t = as.numeric(tijd)) %>%
      arrange(tijd) %>%
      na.omit() 
    
    return(df)
  }
  
  
  observeEvent(input$up_upload_button, {
    req(input$up_files)
    req(input$up_skip)
    req(input$up_timestamp_column)
    req(input$up_glucose_column)
    req(input$up_timestamp_format)
    
    
    skip = input$up_skip
    
    if (!is.na(as.numeric(skip))) {
      skip = as.numeric(skip) 
    } 
    
    
    current_files = tbl(db, 'uploaded_files') %>%
      collect()
    
    for (file in 1:nrow(input$up_files)) {
      
      p = input$up_files$datapath[file]
      n = input$up_files$name[file]
      df = process_upload(file_location = p, 
                          file_name = n, 
                          skip_arg = skip) %>%
        finalize_upload(dt_cols = input$up_timestamp_column,
                        gluc_col = input$up_glucose_column,
                        ts_fmt = input$up_timestamp_format)
      
      file_name = n
      file_name = paste0(now() %>% as.character() %>% str_replace_all('[[:punct:]]', '_') %>% str_replace('\\ ', '_'), '__', file_name)
      
      new_file = tibble(file_name = file_name, 
                        upload_date = now())
      
      dbWriteTable(conn = db, 
                   name = 'uploaded_files', 
                   value = new_file, 
                   temporary = FALSE,
                   append = TRUE)
      
      
      dbWriteTable(conn = db, 
                   name = 'raw_uploads', 
                   value = df, 
                   temporary = FALSE,
                   append = TRUE)
      
      showNotification(paste0("Data file added as: ", file_name))
      
    }
    
  })
  
  output$up_curr_files = DT::renderDataTable({
    
    doot = input$up_upload_button
    
    return(tbl(db, 'uploaded_files') %>% collect())
  })
  
  output$up_output_box = DT::renderDataTable({
    processed_df()
  })
  
  output$up_output_processed = DT::renderDataTable({
    final_df()
  })
  
  final_df = reactive({
    df = processed_df()
    
    finalize_upload(df,
                    dt_cols = input$up_timestamp_column,
                    gluc_col = input$up_glucose_column,
                    ts_fmt = input$up_timestamp_format)
  })
  
  processed_df = reactive({
    req(input$up_files)
    req(input$up_skip)
    
    skip = input$up_skip
    
    if (!is.na(as.numeric(skip))) {
      skip = as.numeric(skip) 
    } 
    
    list_of_df = list()
    
    for (file in 1:nrow(input$up_files)) {
      p = input$up_files$datapath[file]
      n = input$up_files$name[file]
      df_t = process_upload(file_location = p, 
                            file_name = n, 
                            skip_arg = skip)
      
      list_of_df[[n]] = df_t
    }
    
    df = bind_rows(list_of_df)
    
    return(df)
  })
  
  observe({
    
    df = processed_df()
    
    cols = colnames(df)
    
    if (!is.null(input$up_timestamp_column)) {
      current_timestamp = input$up_timestamp_column
      if (any(current_timestamp %in% cols)) {
        updateSelectInput(session, 'up_timestamp_column', choices = cols, selected = intersect(current_timestamp, cols)) 
      } else {
        updateSelectInput(session, 'up_timestamp_column', choices = cols) 
      }
    } else {
      updateSelectInput(session, 'up_timestamp_column', choices = cols)
    }
    
    if (!is.null(input$up_glucose_column)) {
      current_glucose = input$up_glucose_column
      
      if (current_glucose %in% cols ) {
        updateSelectInput(session, 'up_glucose_column', choices = cols, selected = current_glucose) 
      } else {
        updateSelectInput(session, 'up_glucose_column', choices = cols) 
      }
    } else {
      updateSelectInput(session, 'up_glucose_column', choices = cols)
    }
    
  })
  
  
  
  output$up_text_field = DT::renderDataTable({
    req(input$up_files)
    
    list_of_df = list()
    
    for (file in 1:nrow(input$up_files)) {
      p = input$up_files$datapath[file]
      n = input$up_files$name[file]
      df_t = process_text(file_location = p, file_name = n)
      
      list_of_df[[n]] = df_t
    }
    
    df = bind_rows(list_of_df)
    
    return(df)
  })
  
}

