#!/usr/bin/env Rscript

# Comprehensive Runner for Zebrafish Color Spectra Analysis
# This script handles spectral analysis with data kept in memory

cat("=== 🐟 Zebrafish Color Spectra Analysis ===\n")
cat("Single-pass processing with data kept in memory\n\n")

# Check if we're in the right directory
if (!file.exists("src/SpectralAnalysisEngine.R")) {
  stop("Error: Please run this script from the project root directory")
}

# Load the main analysis engine
cat("Loading spectral analysis engine...\n")
source("src/SpectralAnalysisEngine.R")

# Run the complete single-pass analysis
cat("\n🚀 Starting single-pass analysis...\n")
cat("This will process all files, generate plots, and prepare Color Worker data in memory.\n\n")

tryCatch({
  # Run the complete analysis
  results <- process_spectral_data_single_pass()
  
  cat("\n✅ Analysis completed successfully!\n")
  cat("📊 Files processed:", length(unique(results$processed_data$Source_File)), "\n")
  cat("📈 Total data points:", nrow(results$processed_data), "\n")
  cat("👥 Individuals:", length(unique(results$processed_data$ID)), "\n")
  cat("🔬 Body parts:", paste(unique(results$processed_data$Body), collapse = ", "), "\n")
  
  cat("\n📁 Generated files:\n")
  cat("  • Main data: processed_spectral_data.csv\n")
  cat("  • Spectral plot: data/spectral_plot.png\n")
  cat("  • Color Worker data: Available in memory for further analysis\n")
  
  cat("\n💾 Data in memory:\n")
  cat("  • Processed data:", nrow(results$processed_data), "rows\n")
  cat("  • Color Worker data:", length(results$color_worker_data), "samples\n")
  cat("  • Summary statistics: Available\n")
  cat("  • Spectral plot: Generated and saved\n")
  
  cat("\n🎉 All processing complete! Data is ready for analysis.\n")
  
}, error = function(e) {
  cat("\n❌ Error during analysis:", e$message, "\n")
  cat("Please check your data files and configuration.\n")
  quit(status = 1)
})
