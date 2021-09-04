#' analytics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom readr write_tsv
mod_analytics_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = 'Data Selection',
        uiOutput(ns('study_select_ui')),
        selectInput(ns('summary_level'), label = 'Summarize by', choices = c('Period' = 'period', 'Day' = 'day', 'Meals' = 'meals', 'Custom Meals' = 'cust_meals'), selected = 'period')
      ),
      tabBox(
        id = 'meal_tabs',
        
        tabPanel(
          title = 'Meal Specification',
          # column(
          #   width = 6,
          textInput(ns('breakfast_start'), label = 'Breakfast Start', value = "07:00"),
          textInput(ns('lunch_start'), label = 'Lunch Start', value = "12:00"),
          textInput(ns('dinner_start'), label = 'Dinner Start', value = "18:00"),
          # ),
          # column(
          #   width = 6,
          textInput(ns('breakfast_end'), label = 'Breakfast End', value = '8:00'),
          textInput(ns('lunch_end'), label = 'Lunch End', value = '13:30'),
          textInput(ns('dinner_end'), label = 'Dinner End', value = '19:30')
          # )
        ),
        tabPanel(
          title = 'Custom Meal Specification',
          fileInput(ns('meal_file'), label = 'Meal file', multiple = FALSE, accept = c('text/csv', 'text/plain')),
          selectInput(ns('meal_timestamp_format'), label = 'Date & Time Format', choices = c('dmy_hm', 'dmy_hms', 'ymd_hm', 'ymd_hms', 'mdy_hm', 'mdy_hms'), selected = 'dmy_hm'),
          sliderInput(ns('meal_duration'), label = 'Meal duration (minutes)', min = 5, max = 300, step = 5, value = 90)
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = 'Summary Data',
        DTOutput(ns('data')),
        downloadButton(ns('download_summary'), label = 'Download Summary Statistics'),
        downloadButton(ns('download_raw'), label = 'Download Processed Data'),
      )
    ),
    fluidRow(
      tabBox(
        width = 12,
        title = 'Figures',
        tabPanel(
          title = 'All Data Plot',
          plotOutput(ns('all_plot'), height = '500px')
        ),
        tabPanel(
          title = 'Group Plot',
          plotOutput(ns('group_plot'), height = '500px')
        ),
        tabPanel(
          title = 'Subject / Period Plot',
          uiOutput(ns('period_label_select_ui')),
          plotOutput(ns('period_specific_plot'), height = '500px')
        )
      )
    )
  )
}

#' analytics Server Function
#'
#' @noRd 
mod_analytics_server <- function(input, output, session, db, CONSTANTS, table_list, start_uploads){
  ns = session$ns
  
  get_subject_data = reactive({
    req(input$study_select)
    
    study_selection = input$study_select
    
    subjects = tbl(db, 'subjects') %>%
      filter(study_name == study_selection) %>%
      collect()
    
    subject_periods = tbl(db, 'subject_periods') %>%
      filter(study_name == study_selection) %>%
      collect()
    
    period_names = tbl(db, 'period_names') %>%
      filter(study_name == study_selection) %>%
      collect()
    
    subject_info = subjects %>%
      left_join(subject_periods) %>%
      left_join(period_names)
    
    if (nrow(subject_info) < 1) return(NULL)
    
    return(subject_info)
  })
  
  study_settings = reactive({
    req(input$study_select)
    study_selection = input$study_select
    
    study_info = dplyr::tbl(db, 'studies') %>%
      dplyr::collect() %>%
      dplyr::filter(study_name == study_selection) %>%
      as.data.frame(stringsAsFactors = FALSE)
    
    cutoff_data = list(
      HEALTHY_RANGE_LOW = ifelse(is.na(study_info$healthy_range_low[1]), 4, study_info$healthy_range_low[1]),
      HEALTHY_RANGE_HIGH = ifelse(is.na(study_info$healthy_range_high[1]), 10, study_info$healthy_range_high[1]),
      HEALTHY_RANGE_VERY_LOW = ifelse(is.na(study_info$healthy_range_very_low[1]), 3, study_info$healthy_range_very_low[1]),
      HEALTHY_RANGE_VERY_HIGH = ifelse(is.na(study_info$healthy_range_very_high[1]), 14, study_info$healthy_range_very_high[1]),
      SLEEP_START = ifelse(is.na(study_info$sleep_start[1]), 0, study_info$sleep_start[1]),
      SLEEP_END = ifelse(is.na(study_info$sleep_end[1]), 6, study_info$sleep_end[1]),
      WAKE_START = ifelse(is.na(study_info$wake_start[1]), 6, study_info$wake_start[1]),
      WAKE_END =  ifelse(is.na(study_info$wake_end[1]), 24, study_info$wake_end[1])
    )
    
    return(cutoff_data)
  })
  
  meal_df = reactive({
    p = input$meal_file$datapath
    n = input$meal_file$name
    df_t = process_upload(file_location = p, 
                          file_name = n, 
                          skip_arg = 0)
    
    df_t = df_t[,1:3]
    
    names(df_t) = c('subject_id', 'start_time', 'meal_name')
    
    ts_fmt = input$meal_timestamp_format
    
    df_t = distinct(df_t)
    
    df_t = df_t %>%
      ungroup %>%
      mutate(start_time = do.call(ts_fmt, list(start_time))) %>%
      group_by(subject_id) %>%
      mutate(end_time = start_time + minutes(input$meal_duration),
             meal_name = glue('{meal_name}_{day(start_time)}_{month(start_time)}_{year(start_time)}')) %>%
      ungroup
    
    print(head(df_t))
    
    return(df_t)
  })
  
  
  get_period_data = reactive({
    req(get_subject_data())
    subject_data = get_subject_data() 
    
    if (is.null(subject_data)) return(NULL)
    
    subject_data = subject_data %>%
      mutate(subject_period_label = glue('{subject_label}_{period_name}'))
    
    selected_periods = subject_data$period_source %>% unique
    
    interpolated_data = tbl(db, 'interpolated_data') %>%
      filter(processed_name %in% selected_periods) %>%
      collect() %>%
      select(tijd, glucose, processed_name, interpolated) %>%
      mutate(tijd = as.POSIXct(tijd, origin = '1970-01-01'),
             subject_period_label = NA) 
    
    get_period = function (subj_row_id) {
      subj_row = subject_data[subj_row_id,]
      
      pstart = subj_row$period_start[1] %>% ymd() 
      pend = subj_row$period_end[1] %>% ymd() 
      
      subj_df = interpolated_data %>%
        as.data.frame %>%
        filter(processed_name == subj_row$period_source,
               date(tijd) >= pstart & date(tijd) < pend) %>%
        mutate(subject_period_label = subj_row$subject_period_label,
               period_name = subj_row$period_name,
               subject_id = subj_row$subject_label,
               subject_group = subj_row$subject_group)
      
      return(subj_df)
      
    }
    
    interpolated_data = map_dfr(1:nrow(subject_data), get_period)
    
    return(interpolated_data)
  })
  
  summary_period_data = reactive({
    req(get_period_data(), study_settings())
    
    period_data = get_period_data()
    study_settings = study_settings()
    
    if (is.null(period_data)) return(NULL)
    
    if (input$summary_level == 'period') {
      
      summary_df = period_data %>% 
        arrange(tijd) %>%
        group_by(subject_period_label) %>%
        mutate(day = date(tijd) %>% as.character %>% as.factor %>% as.numeric,
               date = date(tijd))
      
      
      
    } else if (input$summary_level == 'day') {
      
      summary_df = period_data %>%
        group_by(subject_period_label) %>%
        mutate(day = date(tijd) %>% as.character %>% as.factor %>% as.numeric,
               date = date(tijd)) %>%
        ungroup %>%
        group_by(day, subject_period_label)
      
    } else if (input$summary_level == 'meals') {
      
      dinner_start = input$dinner_start %>% str_split_fixed('[[:punct:]]', 2) %>% as.numeric %>% {.[1] * 60 + .[2]}
      dinner_end = input$dinner_end %>% str_split_fixed('[[:punct:]]', 2) %>% as.numeric %>% {.[1] * 60 + .[2]}
      lunch_start = input$lunch_start %>% str_split_fixed('[[:punct:]]', 2) %>% as.numeric %>% {.[1] * 60 + .[2]}
      lunch_end = input$lunch_end %>% str_split_fixed('[[:punct:]]', 2) %>% as.numeric %>% {.[1] * 60 + .[2]}
      breakfast_start = input$breakfast_start %>% str_split_fixed('[[:punct:]]', 2) %>% as.numeric %>% {.[1] * 60 + .[2]}
      breakfast_end = input$breakfast_end %>% str_split_fixed('[[:punct:]]', 2) %>% as.numeric %>% {.[1] * 60 + .[2]}
      
      summary_df = period_data %>%
        group_by(subject_period_label) %>%
        mutate(day = date(tijd) %>% as.character %>% as.factor %>% as.numeric,
               date = date(tijd),
               timestep = hour(tijd) * 60 + minute(tijd),
               meal = case_when(
                 timestep >= breakfast_start & timestep <= breakfast_end ~ glue('Breakfast - {input$breakfast_start} - {input$breakfast_end}'),
                 timestep >= lunch_start & timestep <= lunch_end ~ glue('Lunch - {input$lunch_start} - {input$lunch_end}'),
                 timestep >= dinner_start & timestep <= dinner_end ~ glue('Dinner - {input$dinner_start} - {input$dinner_end}'),
                 TRUE ~ 'Between meals'
               )) %>%
        ungroup %>%
        group_by(day, subject_period_label, meal)
    } else if (input$summary_level == 'cust_meals') {
      bloop = meal_df() %>%
        as.list %>%
        transpose
      
      parse_meal_data = function (gluc_df, meal_data) {
        gluc_df %>%
          filter(tijd >= as.POSIXct(meal_data$start_time[1], origin = "1970-01-01"),
                 tijd <=as.POSIXct(meal_data$end_time[1], origin = "1970-01-01"),
                 subject_id == meal_data$subject_id[1]) %>%
          mutate(subject_period_label = meal_data$meal_name)
      }
      
      print(bloop)
      
      summary_df = map_dfr(bloop, parse_meal_data, gluc_df = period_data) %>%
        arrange(tijd) %>%
        group_by(subject_period_label) %>%
        mutate(day = date(tijd) %>% as.character %>% as.factor %>% as.numeric,
               date = date(tijd))
      
      
      
    }
    
    summary_df_general = summary_df %>%
      summarize(subject_id = first(subject_id),
                subject_group = first(subject_group),
                period = first(period_name),
                value_count = n(),
                date = first(date),
                period_start = min(tijd) %>% as.character(),
                period_end = max(tijd) %>% as.character(),
                missings = sum(is.na(glucose)),
                missing_percentage = missings / value_count * 100,
                interpolated_percentage = sum(interpolated, na.rm = TRUE) / value_count * 100)
    
    summary_df_all_values = summary_df %>%
      calculate_analytics_metrics(const = study_settings)
    
    summary_df_wake = summary_df %>%
      filter(hour(tijd) >= study_settings$WAKE_START,
             hour(tijd) < study_settings$WAKE_END) %>%
      calculate_analytics_metrics(prefix = 'wake', const = study_settings)
    
    summary_df_sleep = summary_df %>%
      filter(hour(tijd) >= study_settings$SLEEP_START,
             hour(tijd) < study_settings$SLEEP_END) %>%
      calculate_analytics_metrics(prefix = 'sleep', const = study_settings)
    
    summary_df = full_join(summary_df_general, summary_df_all_values) %>%
      full_join(summary_df_wake) %>%
      full_join(summary_df_sleep)
    
    return(summary_df)
    
  })
  
  output$study_select_ui = renderUI({
    p = input$refresh_button
    
    study_table = tbl(db, 'studies')
    
    current_studies = study_table %>%
      collect() %>%
      .$study_name %>%
      unique
    
    selectInput(ns('study_select'), label = 'Study', choices = current_studies)
  })
  
  output$data = renderDT({
    req(summary_period_data())
    
    if (is.null(summary_period_data())) {
      data.frame(message = 'Empty data. Did you correctly specify subject data?')
    } else {
      summary_period_data()
    }
    # %>% DT::formatDate('period_start', "toLocaleString") %>% DT::formatDate('period_end', "toLocaleString")
  }, options = list(scrollX = TRUE))
  
  output$all_plot = renderPlot({
    req( get_period_data())
    
    study_settings = study_settings()
    
    full_data = get_period_data() %>%
      mutate(timestep = hour(tijd) * 60 + minute(tijd)) 
    
    df_summary = full_data %>%
      group_by(timestep) %>%
      summarize(value_count = n(),
                missings = sum(is.na(glucose)),
                interpolated_n = sum(interpolated, na.rm = TRUE),
                mean_glucose = mean(glucose, na.rm = TRUE),
                sd_glucose = sd(glucose, na.rm = TRUE),
                median_glucose = median(glucose, na.rm = TRUE),
                low_q_glucose = quantile(glucose, 0.25, na.rm = TRUE),
                high_q_glucose = quantile(glucose, 0.75, na.rm = TRUE),
                low_10_glucose = quantile(glucose, 0.1, na.rm = TRUE),
                high_90_glucose = quantile(glucose, 0.9, na.rm = TRUE)) %>%
      mutate(missing_percentage = missings / value_count * 100,
             interpolated_percentage = interpolated_n / value_count * 100)
    
    timesteps = unique(full_data$timestep)
    
    markup = tibble(xlabel = paste0(df_summary$timestep %/% 60, ':00'),
                    timestep = df_summary$timestep) %>%
      filter(timestep %% 120 == 0)
    
    plt = ggplot(aes(x = timestep), data = df_summary) +
      geom_hline(yintercept = study_settings$HEALTHY_RANGE_LOW, alpha = 1/3, linetype = 'dashed', size = 1.2, color = 'orange') +
      geom_hline(yintercept = study_settings$HEALTHY_RANGE_HIGH, alpha = 1/3, linetype = 'dashed', size = 1.2, color = 'orange') +
      geom_hline(yintercept = study_settings$HEALTHY_RANGE_VERY_LOW, alpha = 1/3, linetype = 'dashed', size = 1.2, color = 'red') +
      geom_hline(yintercept = study_settings$HEALTHY_RANGE_VERY_HIGH, alpha = 1/3, linetype = 'dashed', size = 1.2, color = 'red') +
      geom_line(aes(y = median_glucose), color = 'blue') +
      geom_ribbon(aes(ymin = low_10_glucose, ymax = high_90_glucose), fill = 'blue', alpha = 1/6) +
      geom_ribbon(aes(ymin = low_q_glucose, ymax = high_q_glucose), fill = 'blue', alpha = 1/6) +
      # geom_point(aes(x = timestep, y = glucose), alpha = 1/15, data = full_data) +
      theme_minimal(base_size = 15) +
      ylab('Glucose (mmol/L)') +
      scale_x_continuous(name = '', breaks = markup$timestep, labels = markup$xlabel) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    
    return(plt)
  })
  
  output$group_plot = renderPlot({
    
    req(get_period_data())
    
    study_settings = study_settings()
    
    full_data = get_period_data() %>%
      mutate(timestep = hour(tijd) * 60 + minute(tijd)) 
    
    df_summary = full_data %>%
      group_by(timestep, subject_group) %>%
      summarize(value_count = n(),
                missings = sum(is.na(glucose)),
                interpolated_n = sum(interpolated, na.rm = TRUE),
                mean_glucose = mean(glucose, na.rm = TRUE),
                sd_glucose = sd(glucose, na.rm = TRUE),
                median_glucose = median(glucose, na.rm = TRUE),
                low_q_glucose = quantile(glucose, 0.25, na.rm = TRUE),
                high_q_glucose = quantile(glucose, 0.75, na.rm = TRUE),
                low_10_glucose = quantile(glucose, 0.1, na.rm = TRUE),
                high_90_glucose = quantile(glucose, 0.9, na.rm = TRUE)) %>%
      mutate(missing_percentage = missings / value_count * 100,
             interpolated_percentage = interpolated_n / value_count * 100)
    
    markup = tibble(xlabel = paste0(df_summary$timestep %/% 60, ':00'),
                    timestep = df_summary$timestep) %>%
      filter(timestep %% 120 == 0)
    
    plt = ggplot(aes(x = timestep), data = df_summary) +
      geom_hline(yintercept = study_settings$HEALTHY_RANGE_LOW, alpha = 1/3, linetype = 'dashed', size = 1.2, color = 'orange') +
      geom_hline(yintercept = study_settings$HEALTHY_RANGE_HIGH, alpha = 1/3, linetype = 'dashed', size = 1.2, color = 'orange') +
      geom_hline(yintercept = study_settings$HEALTHY_RANGE_VERY_LOW, alpha = 1/3, linetype = 'dashed', size = 1.2, color = 'red') +
      geom_hline(yintercept = study_settings$HEALTHY_RANGE_VERY_HIGH, alpha = 1/3, linetype = 'dashed', size = 1.2, color = 'red') +
      geom_line(aes(y = median_glucose, color = subject_group, fill = subject_group)) +
      geom_ribbon(aes(ymin = low_10_glucose, ymax = high_90_glucose, color = subject_group, fill = subject_group), alpha = 1/6) +
      geom_ribbon(aes(ymin = low_q_glucose, ymax = high_q_glucose, color = subject_group, fill = subject_group), alpha = 1/6) +
      # geom_jitter(aes(x = timestep, y = glucose), alpha = 1/50, data = full_data) +
      theme_minimal(base_size = 15) +
      facet_wrap(subject_group ~ ., ncol = 2, scales = 'free_y') +
      ylab('Glucose (mmol/L)') +
      scale_color_calc() +
      scale_fill_calc() +
      scale_x_continuous(name = '', breaks = markup$timestep, labels = markup$xlabel) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    
    return(plt)
  })
  
  output$download_summary = downloadHandler(
    filename = function() {
      paste0(as.character(now()), '-', input$study_select, "summary.tsv")
    },
    content = function(file) {
      df = summary_period_data()
      
      write_tsv(df, path = file)
    },
    contentType = 'text/tsv'
  )
  
  output$download_raw = downloadHandler(
    filename = function() {
      paste0(as.character(now()), '-', input$study_select, "_raw.tsv")
    },
    content = function(file) {
      df = get_period_data()
      
      write_tsv(df, path = file)
    },
    contentType = 'text/tsv'
  )
  
  output$period_label_select_ui = renderUI({
    req(get_period_data())
    all_data = get_period_data()
    
    options = all_data$subject_period_label %>% unique
    
    selectInput(ns('period_label_select'), label = 'Subject / Period', choices = options, selected = options[1])
    
  })
  
  output$period_specific_plot = renderPlot({
    req(get_period_data())
    subj_period = input$period_label_select
    study_settings = study_settings()
    
    
    full_data = get_period_data() %>%
      filter(subject_period_label == subj_period) %>%
      mutate(timestep = hour(tijd) * 60 + minute(tijd)) 
    
    df_summary = full_data %>%
      group_by(timestep) %>%
      summarize(value_count = n(),
                missings = sum(is.na(glucose)),
                interpolated_n = sum(interpolated, na.rm = TRUE),
                mean_glucose = mean(glucose, na.rm = TRUE),
                sd_glucose = sd(glucose, na.rm = TRUE),
                median_glucose = median(glucose, na.rm = TRUE),
                low_q_glucose = quantile(glucose, 0.25, na.rm = TRUE),
                high_q_glucose = quantile(glucose, 0.75, na.rm = TRUE),
                low_10_glucose = quantile(glucose, 0.1, na.rm = TRUE),
                high_90_glucose = quantile(glucose, 0.9, na.rm = TRUE)) %>%
      mutate(missing_percentage = missings / value_count * 100,
             interpolated_percentage = interpolated_n / value_count * 100)
    
    markup = tibble(xlabel = paste0(df_summary$timestep %/% 60, ':00'),
                    timestep = df_summary$timestep) %>%
      filter(timestep %% 120 == 0)
    
    plt = ggplot(aes(x = timestep), data = df_summary) +
      geom_hline(yintercept = study_settings$HEALTHY_RANGE_LOW, alpha = 1/3, linetype = 'dashed', size = 1.2, color = 'orange') +
      geom_hline(yintercept = study_settings$HEALTHY_RANGE_HIGH, alpha = 1/3, linetype = 'dashed', size = 1.2, color = 'orange') +
      geom_hline(yintercept = study_settings$HEALTHY_RANGE_VERY_LOW, alpha = 1/3, linetype = 'dashed', size = 1.2, color = 'red') +
      geom_hline(yintercept = study_settings$HEALTHY_RANGE_VERY_HIGH, alpha = 1/3, linetype = 'dashed', size = 1.2, color = 'red') +
      geom_line(aes(y = median_glucose), color = 'blue') +
      geom_ribbon(aes(ymin = low_10_glucose, ymax = high_90_glucose), fill = 'blue', alpha = 1/6) +
      geom_ribbon(aes(ymin = low_q_glucose, ymax = high_q_glucose), fill = 'blue', alpha = 1/6) +
      # geom_jitter(aes(x = timestep, y = glucose), alpha = 1/50, data = full_data) +
      theme_minimal(base_size = 15) +
      ylab('Glucose (mmol/L)') +
      scale_x_continuous(name = '', breaks = markup$timestep, labels = markup$xlabel) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    
    return(plt)
  })
}

