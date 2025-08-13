# Zebrafish Color Spectra Analysis

A comprehensive R-based analysis pipeline for processing and analyzing spectral reflectance data from Ocean Optics spectrometers, specifically designed for zebrafish color studies.

## 🚀 **Single-Pass Processing**

This project now features a **streamlined single-pass approach** that processes all data, generates plots, and prepares Color Worker data in memory. No more waiting between steps or unnecessary file clutter!

## 📁 Project Structure

```
Zebrafish_Color_Spectra/
├── src/                           # Source code
│   ├── SpectralAnalysisEngine.R   # Main spectral analysis engine
│   └── config.R                   # Configuration parameters
├── data/                          # Data files
│   └── Experimental/              # Experimental data
│       ├── Female1_LateralDorsal.tsv
│       ├── Male1_Tail.tsv
│       ├── Female2_LateralUpper.tsv
│       └── Wavelength.txt
├── app.R                          # Shiny web interface
├── run_spectral_analysis.R        # Comprehensive analysis runner
├── .Rprofile                      # R environment setup
└── README.md                      # This file
```

## 🎯 **Key Features**

- **Single-Pass Processing**: Everything happens in one operation
- **Memory-Based Storage**: Color Worker data kept in memory, not cluttering disk
- **Meaningful File Naming**: Descriptive filenames (e.g., `Female1_LateralDorsal.tsv`)
- **Automated Workflow**: Upload files → Click button → Get all results
- **Interactive Shiny UI**: User-friendly web interface
- **Comprehensive Output**: Plots, data files, and in-memory Color Worker data
- **Error Handling**: Robust error checking and user feedback

## 🚀 **Quick Start**

### **Option 1: Shiny Web Interface (Recommended)**
```bash
# Launch the interactive web app
R -e "shiny::runApp('.', port = 3838)"
```
Then open your browser to `http://localhost:3838`

### **Option 2: Command Line (Single Script)**
```bash
# Run complete analysis with one command
Rscript run_spectral_analysis.R
```

### **Option 3: Interactive R Session**
```r
# Load and run the analysis engine
source("src/SpectralAnalysisEngine.R")
results <- process_spectral_data_single_pass()

# Access Color Worker data in memory
color_worker_data <- results$color_worker_data
```

## 📊 **Data Requirements**

### **Input Files**
- **Spectral Data**: `.tsv` files with meaningful names (e.g., `Female1_LateralDorsal.tsv`)
- **Wavelength Reference**: `Wavelength.txt` file in the data directory
- **File Format**: Ocean Optics spectrometer output with 9 header rows

### **Naming Convention**
- **Individual_SexNumber_BodyPart.tsv**
- **Examples**: 
  - `Female1_LateralDorsal.tsv`
  - `Male1_Tail.tsv`
  - `Female2_LateralUpper.tsv`

## ⚙️ **Configuration**

Edit `src/config.R` to customize:
- Data directory paths
- Wavelength ranges
- Smoothing parameters
- Output file locations

## 📦 **Dependencies**

Required R packages:
- `dplyr` - Data manipulation
- `tidyr` - Data tidying
- `stringr` - String operations
- `ggplot2` - Plotting
- `zoo` - Time series analysis
- `shiny` - Web interface

## 🔧 **Installation**

```bash
# Install R packages
R -e "install.packages(c('dplyr', 'tidyr', 'stringr', 'ggplot2', 'zoo', 'shiny'), lib='~/R/library')"
```

## 📈 **Output Files**

The single-pass analysis generates:
1. **Main Data**: `processed_spectral_data.csv`
2. **Spectral Plot**: `spectral_plot.png`
3. **Color Worker Data**: Available in memory for further analysis
4. **Summary Statistics**: Console output with processing details

## 🎨 **Color Worker Integration**

Color Worker data is prepared in memory with:
- Wavelength data in 5nm increments
- Reflectance values with smoothing applied
- Proper formatting for color analysis workflows
- **No disk clutter** - data stays in memory for immediate use

## 🐛 **Troubleshooting**

### **Common Issues**
1. **Missing packages**: Ensure all required R packages are installed
2. **File paths**: Ensure data is in the correct directories
3. **Permissions**: Check file read/write permissions
4. **Format errors**: Verify TSV file structure matches requirements

## 📝 **Notes**

- **Single-Pass Processing**: All analysis steps are now consolidated into one function
- **Memory-Based**: Color Worker data kept in memory, not saved to disk
- **Meaningful Names**: Files use descriptive names instead of abbreviations
- **Automated Workflow**: No manual intervention required between steps
- **Error Handling**: Comprehensive error checking and user feedback
- **Scalability**: Handles multiple files and different naming conventions
- **Streamlined**: Removed redundant files and scripts for cleaner project structure
- **Efficient**: No unnecessary file I/O operations

## 🤝 **Contributing**

This project uses a modular architecture with:
- Clear separation of concerns
- Configurable parameters
- Comprehensive error handling
- User-friendly interfaces

## 📄 **License**

This project is designed for scientific research and educational purposes.
