split_whaleinfo_rows <- function(data) {
  # Ensure 'whaleinfo' column exists
  if (!"whaleinfo" %in% colnames(data)) {
    stop("The 'whaleinfo' column is missing in the data.")
  }
  
  # Replace any semicolons with commas (if used as separators)
  data$whaleinfo <- gsub(";", ",", data$whaleinfo)
  
  # Split 'whaleinfo' entries by commas and trim whitespace
  data$whale_list <- strsplit(as.character(data$whaleinfo), "\\s*,\\s*")
  
  # Unnest the data to create a row for each whale ID
  data <- unnest(data, cols = c(whale_list))
  
  # Rename the unlisted whale info column back to 'whaleinfo'
  data$EGNO <- data$whale_list
  data$whale_list <- NULL
  
  # Add grading columns for image assessment
  data$camera_focus <- NA
  data$body_straightness <- NA
  data$body_roll <- NA
  data$body_arch <- NA
  data$body_pitch <- NA
  data$body_length_measurability <- NA
  data$body_width_measurability <- NA
  
  return(data)
}
