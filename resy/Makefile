MAIN=main.py

all: run

setup:
	poetry install

build:
	poetry build

run:
	poetry run python $(MAIN)

format:
	poetry run isort $(MAIN)
	poetry run yapf -i -p $(MAIN)

check:
	poetry run mypy --strict $(MAIN)
