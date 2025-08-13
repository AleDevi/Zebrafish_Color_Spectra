# R Profile for Zebrafish Color Spectra Project
# This file sets up the R environment automatically

# Add user library to the library path
user_lib <- "~/R/library"
if (dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
  cat("User library added to path:", user_lib, "\n")
}

# Set working directory to project root
if (file.exists("Zebrafish_Color_Spectra.Rproj")) {
  setwd(dirname(normalizePath("Zebrafish_Color_Spectra.Rproj")))
  cat("Working directory set to:", getwd(), "\n")
}

# Load configuration if available
if (file.exists("src/config.R")) {
  source("src/config.R")
  cat("Configuration loaded from src/config.R\n")
} else if (file.exists("config.R")) {
  source("config.R")
  cat("Configuration loaded from config.R\n")
}
