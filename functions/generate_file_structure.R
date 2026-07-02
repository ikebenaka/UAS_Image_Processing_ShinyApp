# Function to generate folder structure
generate_folder_structure <- function(date_folder, platforms) {
  created_folders <- list()
  dir.create(date_folder, recursive = TRUE, showWarnings = FALSE)
  created_folders[[length(created_folders) + 1]] <- date_folder
  
  for (platform in platforms) {
    platform_folder <- switch(platform,
                              "APH-22" = "APH-22",
                              "APH-28" = "APH-28",
                              "EVO II Pro" = "EVO II Pro",
                              "EVO II Dual" = "EVO II Dual",
                              "FreeFly Astro" = "Astro",
                              platform
    )
    
    platform_path <- file.path(date_folder, platform_folder)
    
    dir.create(platform_path, recursive = TRUE, showWarnings = FALSE)
    created_folders[[length(created_folders) + 1]] <- platform_path
    
    if (platform %in% c("EVO II Pro", "EVO II Dual")) {
      subfolders <- c("jpg", "video", "EVO_logs", "log")
      if (platform == "EVO II Dual") {
        subfolders <- c(subfolders, "thermal")
      }
      for (subfolder in subfolders) {
        subfolder_path <- file.path(platform_path, subfolder)
        dir.create(subfolder_path, recursive = TRUE, showWarnings = FALSE)
        created_folders[[length(created_folders) + 1]] <- subfolder_path
      }
    } else if (platform %in% c("APH-22", "APH-28")) {
      subfolder_path <- file.path(platform_path, "jpg")
      dir.create(subfolder_path, recursive = TRUE, showWarnings = FALSE)
      created_folders[[length(created_folders) + 1]] <- subfolder_path
    } else if (platform == "FreeFly Astro") {
      subfolders <- c(
        "jpg",
        "jpg/card01",
        "jpg/thumbdrive",
        "video",
        "video/card01",
        "video_frames",
        "video_frames/card01",
        "log",
        "flight_logs",
        "jpg_corr",
        "video_corr",
        "video_corr/card01"
      )
      for (subfolder in subfolders) {
        subfolder_path <- file.path(platform_path, subfolder)
        dir.create(subfolder_path, recursive = TRUE, showWarnings = FALSE)
        created_folders[[length(created_folders) + 1]] <- subfolder_path
      }
    }
  }
  
  return(paste0(unlist(created_folders), collapse = "\n"))
}
