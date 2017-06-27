#!/bin/sh

if [ ! -x hello ]; then
  echo "Please build hello binary first"
  exit 1
fi

acbuild begin
acbuild set-name example.com/hello
acbuild copy hello /bin/hello
acbuild set-exec /bin/hello
acbuild port add www tcp 5000
acbuild label add version 0.0.1
acbuild label add arch amd64
acbuild label add os linux
acbuild annotation add authors "Carly Containers <carly@example.com>"
acbuild write hello-0.0.1-linux-amd64.aci
acbuild end
