# Configuration file for Zebrafish Color Spectra Analysis
# Update these values according to your system and data structure

# Data directory configuration
DATA_DIR <- "data/Experimental"  # Relative path to your data directory
WAVELENGTH_FILE <- "data/Experimental/Wavelength.txt"  # Path to wavelength reference file

# Spectral processing parameters
MIN_WAVELENGTH <- 280  # Minimum wavelength in nm
MAX_WAVELENGTH <- 700  # Maximum wavelength in nm
SMOOTHING_FILTER_SIZE <- 50  # Rolling mean window size for smoothing

# Wavelength filtering parameters (specific to your spectrometer)
# These values filter out unwanted wavelength ranges
FILTER_START <- 278  # Start of filter range
FILTER_END <- 1458   # End of filter range
WAVELENGTH_FILTER_START <- 278   # Start of wavelength file filter
WAVELENGTH_FILTER_END <- 2056    # End of wavelength file filter

# Color worker data preparation (kept in memory, not saved to files)
COLOR_WORKER_MIN_WAVELENGTH <- 400  # Minimum wavelength for color worker (visible light)
COLOR_WORKER_MAX_WAVELENGTH <- 700  # Maximum wavelength for color worker

# File patterns
TSV_PATTERN <- "\\.tsv$"  # Pattern to match .tsv files
SKIP_ROWS <- 9  # Number of rows to skip when reading spectral data

# Output file names
PROCESSED_DATA_FILE <- "data/processed_spectral_data.csv"  # Main output file
LOG_FILE <- "data/processing_log.txt"  # Processing log file

# Validation settings
REQUIRED_MIN_ROWS <- 1458  # Minimum rows required for wavelength filtering
REQUIRED_MIN_COLS <- 1     # Minimum columns required per file
