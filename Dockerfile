# Dockerfile for Zebrafish Color Spectra Analysis
# Multi-stage build for optimized R environment

FROM rocker/r-ver:4.1.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libhdf5-dev \
    libnetcdf-dev \
    libjq-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libv8-dev \
    libgsl-dev \
    libcairo2-dev \
    libtiff5-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libhdf5-dev \
    libnetcdf-dev \
    libjq-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libv8-dev \
    libgsl-dev \
    libcairo2-dev \
    libtiff5-dev \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /app

# Copy R scripts and configuration
COPY src/ ./src/
COPY app.R ./
COPY run_spectral_analysis.R ./
COPY .Rprofile ./

# Create data directories
RUN mkdir -p data/Experimental

# Copy example data
COPY data/Experimental/ ./data/Experimental/

# Install R packages
RUN R -e "install.packages(c('dplyr', 'tidyr', 'stringr', 'ggplot2', 'zoo', 'shiny', 'DT'), repos='https://cran.rstudio.com/')"

# Expose port for Shiny app
EXPOSE 3838

# Set environment variables
ENV R_LIBS=/usr/local/lib/R/site-library
ENV SHINY_HOST=0.0.0.0
ENV SHINY_PORT=3838

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

# Default command to run the Shiny app
CMD ["R", "-e", "shiny::runApp('.', host = '0.0.0.0', port = 3838)"]
