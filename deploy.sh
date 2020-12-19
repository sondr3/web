#!/usr/bin/env bash

echo "Deploying website"

echo "Building new container"
docker build -t web .

echo "Copying built files"
docker cp web:/usr/src/web/public ./public