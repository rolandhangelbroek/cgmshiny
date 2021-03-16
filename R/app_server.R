#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' 
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#' @import purrr
#' @import ggplot2
#' @import shiny
#' @import DBI
#' @importFrom readr write_tsv
#' @importFrom glue glue
#' @importFrom data.table fread
#' @import lubridate
#' @import zoo
#' @importFrom plotly plot_ly add_trace rangeslider layout plotlyOutput renderPlotly
#' @importFrom DT DTOutput renderDT
#' @importFrom stats mad median na.omit quantile sd
#' @import ggthemes
#' @import janitor
#' @import RSQLite
#' @import caTools
#' @noRd
#' 

app_server <- function( input, output, session ) {
  
  CONSTANTS = list(
    HEALTHY_RANGE_LOW = 4,
    HEALTHY_RANGE_HIGH = 10,
    HEALTHY_RANGE_VERY_LOW = 3,
    HEALTHY_RANGE_VERY_HIGH = 14,
    SLEEP_START = 0,
    SLEEP_END = 6,
    WAKE_START = 6,
    WAKE_END = 24
  )
  
  db =  dbConnect(
    RSQLite::SQLite(), 
    file.path(getwd(), 'db9.sqlite')
  )
  
  table_list = dbListTables(db)
  
  # Create empty tables if database is empty / doesn't exist
  if (!'studies' %in% table_list) {
    cat('Creating empty study table\n')
    study_schema = data.frame(study_name = character(),
                              study_description = character(),
                              study_periods = integer(),
                              study_participants = integer(),
                              stringsAsFactors = FALSE)
    
    dbCreateTable(db,
                  'studies',
                  study_schema)
    
  }
  
  if (!'healthy_range_low' %in% dbListFields(db, 'studies')) {
    dbWithTransaction(db, {
      dbExecute(db, "ALTER TABLE studies ADD COLUMN healthy_range_low INTEGER;")
      dbExecute(db, "ALTER TABLE studies ADD COLUMN healthy_range_high INTEGER;")
      dbExecute(db, "ALTER TABLE studies ADD COLUMN healthy_range_very_low INTEGER;")
      dbExecute(db, "ALTER TABLE studies ADD COLUMN healthy_range_very_high INTEGER;")
      dbExecute(db, "ALTER TABLE studies ADD COLUMN wake_start INTEGER;")
      dbExecute(db, "ALTER TABLE studies ADD COLUMN wake_end INTEGER;")
      dbExecute(db, "ALTER TABLE studies ADD COLUMN sleep_start INTEGER;")
      dbExecute(db, "ALTER TABLE studies ADD COLUMN sleep_end INTEGER;")

    })
  }
  
  if (!'subjects' %in% table_list) {
    cat('Creating empty subject table\n')
    subject_schema = data.frame(study_name = character(),
                                subject_id = character(),
                                subject_label = character(),
                                stringsAsFactors = FALSE)
    
    dbCreateTable(db,
                  'subjects',
                  subject_schema)
    
  }
  
  if (!'subject_periods' %in% table_list) {
    cat('Creating empty subject period table\n')
    subject_period_schema = data.frame(subject_id = character(),
                                       study_name = character(),
                                       period_id = integer(),
                                       period_start = character(),
                                       period_end = character(),
                                       period_source = character(),
                                       stringsAsFactors = FALSE)
    
    dbCreateTable(db,
                  'subject_periods',
                  subject_period_schema)
    
  }
  
  if (!'period_names' %in% table_list) {
    cat('Creating empty period name table\n')
    subject_period_schema = data.frame(study_name = character(),
                                       period_id = integer(),
                                       period_name = character(),
                                       stringsAsFactors = FALSE)
    
    dbCreateTable(db,
                  'period_names',
                  subject_period_schema)
    
  }
  
  if (!'group_names' %in% table_list) {
    cat('Creating empty group names table\n')
    
    group_schema = data.frame(study_name = character(),
                              group_name = character(),
                              stringsAsFactors = FALSE)
    
    dbCreateTable(db,
                  'group_names',
                  group_schema)
  }
  
  
  if (!'uploaded_files' %in% table_list) {
    cat('Creating empty uploaded files table\n')
    
    empty_uploaded_files = tibble(file_name = '',
                                  upload_date = lubridate::now())
    
    dbCreateTable(db, 
                  'uploaded_files', 
                  empty_uploaded_files)
  }
  
  if ('interpolated_data' %in% table_list) {
    
    start_uploads = tbl(db, 'interpolated_data') %>%
      count(processed_name) %>%
      collect %>%
      .$processed_name
    
  } else {
    
    start_uploads = c('')
    
  }
  
  callModule(mod_setup_server, "setup_ui_1", db, CONSTANTS, table_list, start_uploads)
  callModule(mod_subject_server, "subject_ui_1", db, CONSTANTS, table_list, start_uploads)
  callModule(mod_upload_server, "upload_ui_1", db, CONSTANTS, table_list, start_uploads)
  callModule(mod_processing_server, "processing_ui_1", db, CONSTANTS, table_list, start_uploads)
  callModule(mod_analytics_server, "analytics_ui_1", db, CONSTANTS, table_list, start_uploads)
  
  onStop(function() {
    cat("Closing database connection\n")
    
    dbDisconnect(db)
  })
}
