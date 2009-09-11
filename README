Using
-----

The codebase is divided into two parts: a Java-based client that talks
to a Wave server, and an emacs library that talks to the Java client.

Requirements:
  - Java 1.6 JRE
  - [TEMPORARY] A FedOne server to connect to.  See next section for details.


Running your own test server
----------------------------

Right now, this Wave client cannot use an actual Wave server, so until
it can, you have to do the temporary step of downloading, compiling
and running the wave-protocol FedOne test server. You can just skip
this step by using a FedOne server someone else has made available.
But, if you don't know of any, download the code by:

   hg clone https://wave-protocol.googlecode.com/hg/ wave-protocol

Modify wave-protocol/run-server.sh, and start a server.


Manually connecting to the Wave client
--------------------------------------

As part of development of the client, or for debugging purposes, you
may need to start your connection to the Java client.  To connect, use
start-repl.sh:

start-repl.sh JAVA_BIN PORT

This is made for use directly by the emacs library, which will pass
the value of the path to the Java executable and the port on which the
repl should run on.  This is convenient for emacs to pass it's
user-configured variables in, but perhaps awkward for manual runs.

Once you have run this, you should be able to telnet to your specified
port to interact with the REPL.


Architecture
------------     

+----------------+       +-------------------+
| Wave server    |       | Clojure interface |
|                +---+   | and REPL          |
+----------------+   |   +-------------------+
                     +---| FedOne client     |
                 RPC     +------+------------+
                                | TCP-based
                                | REPL
                         +------+------------+
                         | wave-client       |
                         | package           |
                         +-------------------+
                         + emacs             |
                         +-------------------+

This package has one binary executable, and one emacs library.  The
executable uses Clojure to implement an API on top of wave-protocol's
FedOne client.

The wave-client package for emacs will start the Clojure REPL binary,
and connect to it by a simple TCP network connection to the REPL.
