#!/usr/bin/env bash

echo "Redeploying website!"

echo "Getting latest Docker container"
docker pull ghcr.io/sondr3/web:latest

echo "Stopping service..."
docker stop web

echo "Removing old container..."
docker container rm web

echo "Redeploying service"
docker run -itd --pull always --restart unless-stopped -p 8082:8082 --name web ghcr.io/sondr3/web:latest

echo "And we're live again!"
