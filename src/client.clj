;; Copyright 2009 Google Inc.
;;
;; Licensed under the Apache License, Version 2.0 (the "License"); you
;; may not use this file except in compliance with the License.  You
;; may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
;; implied.  See the License for the specific language governing
;; permissions and limitations under the License.

(ns
    #^{:author "Andrew Hyatt",
       :doc "Wave Client Code - starts REPL on socket"}
  wave-for-emacs.client
  (:import (org.waveprotocol.wave.examples.fedone.waveclient.common ClientBackend ClientUtils))
  (:use clojure.contrib.server-socket))

(defn open-backend [user-at-domain server port]
  #^{:doc "Open a new backend, with `user-at-domain', which must be of
           the form user@domain, where the domain is something like
           wavesandbox.com.  The server is the FedOne server that must
           be running (this will change)."}
  (def backend (new ClientBackend user-at-domain server port)))

(defn get-waves []
  #^{:doc "Get a list of wave beans"}
  (map bean (seq (ClientUtils/getIndexEntries (.getIndexWave backend)))))

(defn get-user-id []
  #^{:doc "Get the user info of the current user"}
  (bean (.getUserId backend)))

(defn new-wave []
  #^{:doc "Create a new wave"}
  (.createConversationWave backend))

(let [port (new Integer (first *command-line-args*))]
  (clojure.contrib.server-socket/create-repl-server port))
