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
  (:import (org.waveprotocol.wave.model.document.operation DocInitializationCursor))
  (:import (org.waveprotocol.wave.model.document.operation.impl InitializationCursorAdapter))
  (:use clojure.contrib.server-socket))

(defn open-backend [user-at-domain server port]
  #^{:doc "Open a new backend, with `user-at-domain', which must be of
           the form user@domain, where the domain is something like
           wavesandbox.com.  The server is the FedOne server that must
           be running (this will change)."}
  (def backend (new ClientBackend user-at-domain server port)))

(defn clojure-type-dispatch [obj]
  #^{:doc "Returns the type of common clojure objects. One
           of (:seq, :map, :default)"}
  (cond (seq? obj) :seq
        (map? obj) :map
        true :default))

(defmulti elispify clojure-type-dispatch)

(defmethod elispify :map [mapobj]
  (map (fn [pair] `(~(key pair) . ~(val pair))) (seq mapobj)))

(defmethod elispify :seq [seq]
  seq)

(defmethod elispify :default [obj]
  obj)

(defn get-participants [wave-id]
  #^{:doc "Get the list of participants from the Wave indentified by
           WaveId instance WAVE-ID."}
  (map (fn [participant-id] (.getAddress participant-id))
       (seq
        (.getParticipants
         (ClientUtils/getConversationRoot (.getWave backend wave-id))))))

(defn get-wave-summary [wave]
  #^{:doc "Get the basic information of a wave: the id and the digest,
           and the participants."}
  (elispify {:id (.serialise (.getWaveId wave))
             :digest (.getDigest wave)
             :participants (get-participants (.getWaveId wave))}))

(defn get-waves []
  #^{:doc "Get a list of waves.  Each wave should have info about the id
           the summary line, and the participants"}
  (map get-wave-summary
       (seq (ClientUtils/getIndexEntries (.getIndexWave backend)))))

(defn get-user-id []
  #^{:doc "Get the user info of the current user"}
  (bean (.getUserId backend)))

(defn new-wave []
  #^{:doc "Create a new wave"}
  (.createConversationWave backend))

(defn nest-wavelet-data [l]
  #^{:doc "Convert a linear output of text and tag beginning and endings
           to a recursively nested format."}
  (loop [accum '()
         data-list l]
    (if (empty? data-list)
      accum
      (let [head (first data-list)
            new-accum
            (cond
                (string? head) (concat (butlast accum)
                                       (list (concat (last accum)
                                                     (list head))))
              (vector? head) (concat accum
                                     (list (list head)))
              (= 'end head) (concat (butlast (butlast accum))
                                    (concat (last (butlast accum))
                                            (list (last accum))))
              true (throw (new IllegalArgumentException
                               (format "Unknown value: %s" head))))]
        (recur new-accum (rest data-list))))))

(defn get-wave [wave-id]
  #^{:doc "Open a Wave given by WaveId instance WAVE-ID.  This return a
           list of wavelets, each one which is a nested list of data.
           The elements of this are strings and element groups with a
           type symbol and attribute map, such as '(bold {} 'foo' 'bar)."}
  (let [docmap (atom '())]
    (doseq [wavelet (seq (.getWavelets (.getWave backend wave-id)))]
      (doseq [doc-op (seq (.values (.getDocuments wavelet)))]
        (.apply doc-op
                (new InitializationCursorAdapter
                     (proxy
                         [DocInitializationCursor]
                         []
                       (characters [s]
                                   (swap! docmap concat (list s)))
                       (elementStart [type attr]
                                     (swap! docmap concat
                                            (list [(symbol type) attr])))
                       (elementEnd []
                                   (swap! docmap concat (list 'end)))
                       (annotationBoundary [boundary-map]))))))
    (nest-wavelet-data @docmap)))

(let [port (new Integer (first *command-line-args*))]
  (clojure.contrib.server-socket/create-repl-server port))
