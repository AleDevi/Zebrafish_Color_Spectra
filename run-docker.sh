#!/bin/bash

# Zebrafish Color Spectra Analysis - Docker Runner
# This script builds and runs the analysis application in Docker

echo "🐟 Zebrafish Color Spectra Analysis - Docker Setup"
echo "=================================================="

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
    echo "❌ Docker is not running. Please start Docker first."
    exit 1
fi

# Check if docker-compose is available
if ! command -v docker-compose &> /dev/null; then
    echo "❌ docker-compose is not installed. Please install it first."
    exit 1
fi

echo "✅ Docker environment ready"

# Build and start the application
echo "🚀 Building and starting the application..."
docker-compose up --build -d

# Wait for the app to be ready
echo "⏳ Waiting for the application to start..."
sleep 10

# Check if the app is running
if curl -f http://localhost:3838/ > /dev/null 2>&1; then
    echo "✅ Application is running successfully!"
    echo ""
    echo "🌐 Access your application at:"
    echo "   • Shiny UI: http://localhost:3838"
    echo "   • RStudio Server: http://localhost:8787 (password: zebrafish123)"
    echo ""
    echo "📊 To view logs: docker-compose logs -f"
    echo "🛑 To stop: docker-compose down"
    echo "🔄 To restart: docker-compose restart"
else
    echo "❌ Application failed to start. Check logs with: docker-compose logs"
    exit 1
fi
