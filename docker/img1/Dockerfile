FROM alpine:3.7

RUN apk add --no-cache build-base linux-headers python3-dev

WORKDIR /app
COPY requirements.txt .

RUN pip3 install -r /app/requirements.txt --no-cache-dir

COPY main .
