# Standard makefile for a Go Service in Spark.

SHELL := /bin/bash
REPO_ROOT := $(shell git rev-parse --show-toplevel)
GO_PACKAGE_PATHS := .

.PHONY: all
all: local

include $(REPO_ROOT)/src/templates/go/base.Makefile

PROJECT_RELATIVE_PATH := $(subst $(REPO_ROOT)/src/rubrik/,,$(CURDIR))
DOCKER_IMAGE := $(subst /,-,$(PROJECT_RELATIVE_PATH))

.PHONY: docker
docker: build-k8s clean-docker-context docker-context
	docker build \
		--tag $(DOCKER_IMAGE) \
		--label "exocompute=v1" \
		-f - .docker_context < Dockerfile

.PHONY: clean-docker-context
clean-docker-context:
	-rm -rf .docker_context
	-mkdir .docker_context

.PHONY: docker-context
docker-context::
	cp $(PACKAGE) .docker_context/main

push-minikube: REPO=$(shell minikube ip):5000
push-minikube:
	docker tag $(DOCKER_IMAGE) $(REPO)/$(DOCKER_IMAGE)
	docker push $(REPO)/$(DOCKER_IMAGE)

push-gcp: GCP_PROJ=spark-dev-004
push-gcp: REPO=gcr.io/$(GCP_PROJ)
push-gcp:
	docker tag $(DOCKER_IMAGE) $(REPO)/$(DOCKER_IMAGE)
	docker push $(REPO)/$(DOCKER_IMAGE)
