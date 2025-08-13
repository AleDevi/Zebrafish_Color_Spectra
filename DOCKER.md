# 🐳 Docker Setup for Zebrafish Color Spectra Analysis

This document explains how to run the Zebrafish Color Spectra Analysis application using Docker.

## 🚀 Quick Start

### Prerequisites
- Docker installed and running
- docker-compose installed
- Ports 3838 and 8787 available

### Run with Script (Recommended)
```bash
./run-docker.sh
```

### Manual Setup
```bash
# Build and start the application
docker-compose up --build -d

# Check status
docker-compose ps

# View logs
docker-compose logs -f
```

## 🌐 Access Points

### Shiny Application
- **URL**: http://localhost:3838
- **Purpose**: Main analysis interface
- **Features**: Single-pass processing, data exploration, spectral plots

### RStudio Server (Optional)
- **URL**: http://localhost:8787
- **Username**: rstudio
- **Password**: zebrafish123
- **Purpose**: Development and debugging

## 🐳 Docker Services

### 1. Zebrafish Analysis (Main Service)
- **Container**: `zebrafish-color-spectra`
- **Port**: 3838
- **Purpose**: Runs the Shiny application
- **Health Check**: Automatic monitoring every 30s

### 2. RStudio Server (Development)
- **Container**: `zebrafish-rstudio`
- **Port**: 8787
- **Purpose**: R development environment
- **Volumes**: Mounted source code and data

## 📁 Volume Mounts

### Data Persistence
- `./data` → `/app/data` (Main application)
- `./data` → `/home/rstudio/data` (RStudio)

### Development
- `./src` → `/app/src` (Main application)
- `./src` → `/home/rstudio/src` (RStudio)

## 🔧 Docker Commands

### Basic Operations
```bash
# Start services
docker-compose up -d

# Stop services
docker-compose down

# Restart services
docker-compose restart

# View logs
docker-compose logs -f

# Rebuild and start
docker-compose up --build -d
```

### Container Management
```bash
# View running containers
docker ps

# Access container shell
docker exec -it zebrafish-color-spectra bash

# View container logs
docker logs zebrafish-color-spectra

# Stop specific service
docker-compose stop zebrafish-analysis
```

## 🏗️ Building Custom Images

### Build Locally
```bash
docker build -t zebrafish-analysis:latest .
```

### Run Custom Image
```bash
docker run -p 3838:3838 -v $(pwd)/data:/app/data zebrafish-analysis:latest
```

## 🔍 Troubleshooting

### Common Issues

#### Port Already in Use
```bash
# Check what's using the port
lsof -i :3838

# Kill the process or change port in docker-compose.yml
```

#### Container Won't Start
```bash
# Check logs
docker-compose logs

# Check container status
docker-compose ps

# Restart with rebuild
docker-compose down
docker-compose up --build -d
```

#### R Package Issues
```bash
# Rebuild with fresh packages
docker-compose down
docker-compose build --no-cache
docker-compose up -d
```

### Health Checks
The application includes automatic health checks:
- **Interval**: 30 seconds
- **Timeout**: 10 seconds
- **Retries**: 3 attempts
- **Start Period**: 40 seconds

## 📊 Monitoring

### Container Status
```bash
# View all containers and their health
docker-compose ps

# View resource usage
docker stats
```

### Application Health
```bash
# Check if app responds
curl -f http://localhost:3838/

# View application logs
docker-compose logs zebrafish-analysis
```

## 🔒 Security Notes

- **RStudio Password**: Change default password in production
- **Port Exposure**: Only expose necessary ports
- **Volume Mounts**: Be careful with source code mounting in production
- **Network**: Uses isolated bridge network

## 🚀 Production Deployment

### Environment Variables
```bash
# Set production values
export SHINY_HOST=0.0.0.0
export SHINY_PORT=3838
export R_LIBS=/usr/local/lib/R/site-library
```

### Scaling
```bash
# Scale the main service
docker-compose up -d --scale zebrafish-analysis=2
```

### Persistent Storage
```bash
# Use named volumes for production
volumes:
  - zebrafish-data:/app/data
```

## 📚 Additional Resources

- [Docker Documentation](https://docs.docker.com/)
- [Docker Compose Reference](https://docs.docker.com/compose/)
- [Rocker Images](https://rocker-project.org/)
- [Shiny Server](https://shiny.rstudio.com/)

## 🤝 Support

For issues with the Docker setup:
1. Check the troubleshooting section
2. View container logs
3. Verify Docker environment
4. Check port availability
