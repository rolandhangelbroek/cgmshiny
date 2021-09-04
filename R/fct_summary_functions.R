iAUC_trapz = function (x, y, na.rm = FALSE) {
  if (na.rm == TRUE) {
    missing = is.na(y)
    y = y[!missing]
    x = x[!missing]
  }
  
  if (length(x) < 1 | length(y) < 1) return(NA)
  
  x = as.numeric(x) / 60
  
  x = x - x[1]
  y = y - y[1]
  
  trapz(x, y)
}

process_upload = function (file_location, file_name, skip_arg) {
  
  tryCatch(
    {
      
      if (skip_arg == '') skip_arg = 0
      
      file_name = paste0(format(now(), "%Y%m%d_%H%M%S_"), file_name)
      
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

AUC_trapz = function (x, y, na.rm = FALSE) {
  if (na.rm == TRUE) {
    missing = is.na(y)
    y = y[!missing]
    x = x[!missing]
  }
  
  if (length(x) < 1 | length(y) < 1) return(NA)
  
  x = as.numeric(x) / 60
  
  x = x - x[1]
  
  trapz(x, y)
}

AUC_timed = function (t, gluc, lb, ub, mode = 'AUC', ...) {
  
  
  df = tibble(time = t, glucose = gluc) %>%
    filter(hour(time) >= lb, hour(time) < ub)
  
  if (nrow(df) == 0) return(NA)
  
  if (mode == 'AUC') {
    ret = AUC_trapz(df$time, df$glucose, ...)
  } else if (mode == 'iAUC') {
    ret = iAUC_trapz(df$time, df$glucose, ...)
  }
  
  return(ret)
}

calculate_MODD = function (timestamp, glucose) {
  df = tibble(timestamp, glucose) %>%
    mutate(date = as.character(lubridate::date(timestamp)))
  
  if (df$date %>% unique %>% length < 2) return(NA)
  
  modd = df %>%
    arrange(date) %>%
    group_by(timepoint = glue('{hour(timestamp)}_{minute(timestamp)}')) %>%
    mutate(glucose_diff = glucose - dplyr::lag(glucose, 1)) %>%
    na.omit() %>%
    pull(glucose_diff) %>%
    abs() %>%
    mean(na.rm = TRUE)
  
  return(modd)
}

calculate_MAGE = function (timestamp, glucose) {
  tibble(timestamp, glucose) %>%
    na.omit() %>%
    mutate(sd_glucose = sd(glucose),
           mean_glucose = mean(glucose),
           glucose_diff = abs(glucose - mean_glucose)) %>%
    filter(glucose_diff > sd_glucose) %>%
    pull(glucose_diff) %>%
    mean()
}

calculate_analytics_metrics = function (df, prefix  = NULL, const = CONSTANTS) {
  df = df %>%
    summarize(mean_glucose = mean(glucose, na.rm = TRUE),
              sd_glucose = sd(glucose, na.rm = TRUE),
              mad_glucose = mad(glucose, na.rm = TRUE),
              median_glucose = median(glucose, na.rm = TRUE),
              low_q_glucose = quantile(glucose, 0.25, na.rm = TRUE),
              high_q_glucose = quantile(glucose, 0.75, na.rm = TRUE),
              low_10_glucose = quantile(glucose, 0.1, na.rm = TRUE),
              high_90_glucose = quantile(glucose, 0.9, na.rm = TRUE),
              tir_normal_percent = sum(glucose >= const$HEALTHY_RANGE_LOW & glucose < const$HEALTHY_RANGE_HIGH, na.rm = TRUE) / sum(!is.na(glucose)),
              tir_low_percent = sum(glucose >= const$HEALTHY_RANGE_VERY_LOW & glucose < const$HEALTHY_RANGE_LOW, na.rm = TRUE) / sum(!is.na(glucose)),
              tir_verylow_percent = sum(glucose < const$HEALTHY_RANGE_VERY_LOW, na.rm = TRUE) / sum(!is.na(glucose)),
              tir_high_percent = sum(glucose >= const$HEALTHY_RANGE_HIGH & glucose < const$HEALTHY_RANGE_VERY_HIGH, na.rm = TRUE) / sum(!is.na(glucose)),
              tir_veryhigh_percent = sum(glucose >= const$HEALTHY_RANGE_VERY_HIGH, na.rm = TRUE) / sum(!is.na(glucose)),
              tir_timeabove7.8mmolL = sum(glucose > 7.8, na.rm = TRUE) / sum(!is.na(glucose)),
              AUC = AUC_trapz(tijd, glucose),
              AUC_ignore_missing = AUC_trapz(tijd, glucose, na.rm = TRUE),
              iAUC = iAUC_trapz(tijd, glucose),
              iAUC_ignore_missing = iAUC_trapz(tijd, glucose, na.rm = TRUE),
              MODD = calculate_MODD(tijd, glucose),
              MAGE = calculate_MAGE(tijd, glucose))
  
  if (is.null(prefix)) {
    return(df)
  } else {
    summary_start_col = which(colnames(df) == 'mean_glucose')
    
    colnames(df)[summary_start_col:ncol(df)] = paste(prefix, colnames(df)[summary_start_col:ncol(df)], sep = '_')
    
    return(df)
  }
}

