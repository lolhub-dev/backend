#!/usr/bin/bash
docker run --name lolhub_db --rm -v /data/db:/data/db -p 27017:27017 -d mongo 
