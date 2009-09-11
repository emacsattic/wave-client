#!/bin/bash -x

if [ $# < 2 ]; then
    echo "Usage: start_repl.sh JAVA_BIN PORT"
fi

JAR_FILES=`find third-party -name "*.jar" | tr '\n' :`
JAVA_BIN=$1
PORT=$2

$JAVA_BIN -cp $JAR_FILES clojure.main /Users/ahyatt/src/wave-client-for-emacs/src/client.clj $PORT