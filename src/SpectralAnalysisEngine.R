# Zebrafish Color Spectra Analysis
# Single-pass processing: reads files, processes data, and generates output all at once

# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(zoo)

# Load configuration
source("src/config.R")

# Main processing function - does everything in one pass
process_spectral_data_single_pass <- function() {
  cat("=== Starting Single-Pass Spectral Analysis ===\n")
  
  # Check if data directory exists
  if (!dir.exists(DATA_DIR)) {
    stop("Error: Data directory '", DATA_DIR, "' not found")
  }
  
  # Check if wavelength file exists
  if (!file.exists(WAVELENGTH_FILE)) {
    stop("Error: Wavelength file '", WAVELENGTH_FILE, "' not found")
  }
  
  cat("✓ Data directory and wavelength file found\n")
  
  # Read wavelength reference file
  cat("Reading wavelength reference file...\n")
  wavelength_data <- tryCatch({
    read.table(WAVELENGTH_FILE, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  }, error = function(e) {
    stop("Error reading wavelength file: ", e$message)
  })
  
  # Clean wavelength data
  wavelength_data <- wavelength_data %>%
    filter(!is.na(Wavelength)) %>%
    filter(Wavelength >= WAVELENGTH_FILTER_START, Wavelength <= WAVELENGTH_FILTER_END)
  
  cat("✓ Wavelength data loaded:", nrow(wavelength_data), "wavelengths\n")
  
  # Find all TSV files
  tsv_files <- list.files(DATA_DIR, pattern = TSV_PATTERN, full.names = TRUE)
  if (length(tsv_files) == 0) {
    stop("No .tsv files found in '", DATA_DIR, "'")
  }
  
  cat("Found", length(tsv_files), "TSV files\n")
  
  # Process all files in one pass
  all_processed_data <- list()
  color_worker_data <- list()
  
  for (file_path in tsv_files) {
    file_name <- basename(file_path)
    cat("Processing:", file_name, "...\n")
    
    # Extract ID and body part from meaningful filename
    # Handle both old format (F1_LD) and new format (Female1_LateralDorsal)
    file_info <- str_match(file_name, "([FM]ale\\d+)_([A-Za-z]+)\\.tsv")
    if (is.na(file_info[1, 1])) {
      # Try old format as fallback
      file_info <- str_match(file_name, "([FM]\\d+)_([A-Z]+)\\.tsv")
      if (is.na(file_info[1, 1])) {
        cat("Warning: Could not parse filename format for", file_name, "- skipping\n")
        next
      }
    }
    
    individual_id <- file_info[1, 2]
    body_part <- file_info[1, 3]
    
    # Clean up body part names for consistency
    body_part_clean <- case_when(
      grepl("LateralDorsal", body_part, ignore.case = TRUE) ~ "LateralDorsal",
      grepl("LateralUpper", body_part, ignore.case = TRUE) ~ "LateralUpper", 
      grepl("Tail", body_part, ignore.case = TRUE) ~ "Tail",
      grepl("LD", body_part) ~ "LateralDorsal",
      grepl("LU", body_part) ~ "LateralUpper",
      grepl("T", body_part) ~ "Tail",
      TRUE ~ body_part
    )
    
    # Read spectral data
    spectral_data <- tryCatch({
      read.table(file_path, header = FALSE, sep = "\t", skip = SKIP_ROWS, 
                 stringsAsFactors = FALSE, col.names = c("Wavelength", "Reflectance"))
    }, error = function(e) {
      cat("Warning: Error reading", file_name, "-", e$message, "- skipping\n")
      return(NULL)
    })
    
    if (is.null(spectral_data) || nrow(spectral_data) < REQUIRED_MIN_ROWS) {
      cat("Warning: Insufficient data in", file_name, "- skipping\n")
      next
    }
    
    # Clean and process spectral data
    processed_data <- spectral_data %>%
      filter(!is.na(Wavelength), !is.na(Reflectance)) %>%
      filter(Wavelength >= MIN_WAVELENGTH, Wavelength <= MAX_WAVELENGTH) %>%
      mutate(
        Wavelength = as.numeric(Wavelength),
        Reflectance = as.numeric(Reflectance),
        ID = individual_id,
        Body = body_part_clean
      )
    
    # Apply smoothing
    processed_data <- processed_data %>%
      group_by(ID, Body) %>%
      mutate(
        Reflectance_smoothed = rollmean(Reflectance, SMOOTHING_FILTER_SIZE, fill = "extend", align = "center")
      ) %>%
      ungroup()
    
    # Round wavelengths for consistency
    processed_data <- processed_data %>%
      mutate(NM_rounded = round(Wavelength / 5) * 5)
    
    # Store processed data
    all_processed_data[[file_name]] <- processed_data
    
    # Prepare color worker data (kept in memory, not saved to files)
    color_worker_data[[file_name]] <- processed_data %>%
      select(Wavelength = NM_rounded, Type = Body, Reflectance = Reflectance_smoothed) %>%
      mutate(
        X = 0,  # Placeholder for X coordinate
        Y = 0,  # Placeholder for Y coordinate
        ID = paste0(individual_id, "_", body_part_clean)
      )
    
    cat("✓ Processed", file_name, "-", nrow(processed_data), "data points\n")
    cat("  Individual:", individual_id, "| Body Part:", body_part_clean, "\n")
  }
  
  # Combine all processed data
  if (length(all_processed_data) == 0) {
    stop("No valid data files were processed")
  }
  
  combined_data <- bind_rows(all_processed_data, .id = "Source_File")
  
  cat("\n=== Processing Complete ===\n")
  cat("Total data points:", nrow(combined_data), "\n")
  cat("Files processed:", length(all_processed_data), "\n")
  cat("Individuals:", length(unique(combined_data$ID)), "\n")
  cat("Body parts:", paste(unique(combined_data$Body), collapse = ", "), "\n")
  
  # Save main processed data
  cat("\nSaving processed data...\n")
  write.csv(combined_data, PROCESSED_DATA_FILE, row.names = FALSE)
  cat("✓ Main data saved to:", PROCESSED_DATA_FILE, "\n")
  
  # Color Worker data is kept in memory, not saved to files
  cat("\nColor Worker data prepared in memory (", length(color_worker_data), "samples)\n")
  cat("Data available for further analysis or export if needed\n")
  
  # Generate summary statistics
  cat("\nGenerating summary statistics...\n")
  summary_stats <- combined_data %>%
    group_by(Body) %>%
    summarise(
      Mean_Reflectance = round(mean(Reflectance, na.rm = TRUE), 2),
      Min_Reflectance = round(min(Reflectance, na.rm = TRUE), 2),
      Max_Reflectance = round(max(Reflectance, na.rm = TRUE), 2),
      Data_Points = n(),
      .groups = "drop"
    )
  
  print(summary_stats)
  
  # Create basic plot
  cat("\nGenerating spectral plot...\n")
  spectral_plot <- ggplot(combined_data, aes(x = NM_rounded, y = Reflectance_smoothed, color = Body)) +
    geom_line(alpha = 0.7) +
    geom_point(alpha = 0.3, size = 0.5) +
    facet_wrap(~ID, scales = "free_y") +
    labs(
      title = "Zebrafish Spectral Reflectance by Individual and Body Part",
      x = "Wavelength (nm)",
      y = "Reflectance (%)",
      color = "Body Part"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Save plot
  plot_filename <- "data/spectral_plot.png"
  ggsave(plot_filename, spectral_plot, width = 12, height = 8, dpi = 300)
  cat("✓ Spectral plot saved to:", plot_filename, "\n")
  
  cat("\n=== Single-Pass Analysis Complete! ===\n")
  cat("All files processed, data saved, and Color Worker data prepared in memory.\n")
  
  return(list(
    processed_data = combined_data,
    color_worker_data = color_worker_data,  # Available in memory for further use
    summary_stats = summary_stats,
    plot = spectral_plot
  ))
}

# Run the analysis if this script is executed directly
if (interactive()) {
  cat("Interactive mode detected. Run process_spectral_data_single_pass() to start analysis.\n")
} else {
  # Non-interactive mode - run automatically
  process_spectral_data_single_pass()
}

