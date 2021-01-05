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
              iAUC_ignore_missing = iAUC_trapz(tijd, glucose, na.rm = TRUE))
  
  if (is.null(prefix)) {
    return(df)
  } else {
    summary_start_col = which(colnames(df) == 'mean_glucose')
    
    colnames(df)[summary_start_col:ncol(df)] = paste(prefix, colnames(df)[summary_start_col:ncol(df)], sep = '_')
    
    return(df)
  }
}

