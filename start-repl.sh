#!/bin/bash

if [ $# -lt 2 ]; then
    echo "Usage: start_repl.sh JAVA_BIN PORT"
    exit 1
fi

JAR_FILES=`find third-party -name "*.jar" | tr '\n' :`
JAVA_BIN=$1
PORT=$2

$JAVA_BIN -cp $JAR_FILES clojure.main ./src/client.clj $PORT
