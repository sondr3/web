#!/usr/bin/env bash

echo "Deploying website"

echo "Building new container"
docker build -t web .

echo "Create tag"
docker stop web
docker container rm web
docker run -itd --name web web

echo "Copying built files"
rm -rf ../public
docker cp web:/usr/src/web/public ../
