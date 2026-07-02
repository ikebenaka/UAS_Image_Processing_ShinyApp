# source the platform scripts
functions_dir <- file.path(getwd(), "functions")
source(file.path(functions_dir, "schema_helpers.R"))
source(file.path(functions_dir, "process_evo_pro.R"))
source(file.path(functions_dir, "process_evo_dual.R"))
source(file.path(functions_dir, "process_aph.R"))
source(file.path(functions_dir, "process_astro.R"))

# Main orchestration function
process_flight_data <- function(flight_date_directory, timeoff_pro, timeoff_dual, permit, species, pilot, status_message,
                                baro_offset_pro = 0, baro_offset_dual = 0,
                                baro_offset_aph = 0, baro_offset_astro = 0,
                                astro_image_source = "uncorrected") {

  warning_msgs <- character()
  
  # Discover subdirectories
  all_dirs      <- list.dirs(flight_date_directory, full.names = TRUE, recursive = FALSE)
  evo_pro_dirs  <- all_dirs[grepl("EVO II Pro",  basename(all_dirs), ignore.case = TRUE)]
  evo_dual_dirs <- all_dirs[grepl("EVO II Dual", basename(all_dirs), ignore.case = TRUE)]
  aph_dirs      <- all_dirs[grepl("APH",          basename(all_dirs), ignore.case = TRUE)]
  astro_dirs    <- all_dirs[grepl("Astro$",       basename(all_dirs), ignore.case = TRUE)]

# Dispatch to platform-specific processors, capturing their warnings
for (d in evo_pro_dirs) {warning_msgs <- c(warning_msgs, process_evo_pro(d, timeoff_pro, species, pilot, permit, flight_date_directory, baro_offset_m = baro_offset_pro))}
for (d in evo_dual_dirs) {warning_msgs <- c(warning_msgs, process_evo_dual(d, timeoff_dual, species, pilot, permit, flight_date_directory, baro_offset_m = baro_offset_dual))}
for (d in aph_dirs) {warning_msgs <- c(warning_msgs, process_aph(d, species, pilot, permit, flight_date_directory, baro_offset_m = baro_offset_aph))}
for (d in astro_dirs) {warning_msgs <- c(warning_msgs,process_astro(d, species, pilot, permit, flight_date_directory, baro_offset_m = baro_offset_astro, astro_image_source = astro_image_source))}

# Final status
if (length(warning_msgs) > 0) {status_message(paste("Processing completed with messages:",
      paste(warning_msgs, collapse = "\n")))
  
} else {
  status_message("Processing completed without warnings!")
}
}
