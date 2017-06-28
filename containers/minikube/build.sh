#!/bin/sh

eval $(minikube docker-env)
docker build -t hello-node:v3 .
