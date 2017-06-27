#!/bin/sh

java \
  -XX:-HeapDumpOnOutOfMemoryError \
  -XX:+HeapDumpOnOutOfMemoryError \
  -XX:HeapDumpPath=/xxx/ \
  -XX:HeapDumpPath=/dev/null \
  ConsumeHeap
