;;; wave-client-websocket.el --- Methods to communicate with the Wave server

;; Copyright (c) 2010 Andrew Hyatt
;;
;; Author: Andrew Hyatt <ahyatt at gmail dot com>
;; Maintainer: Andrew Hyatt <ahyatt at gmail dot com>
;;
;; Licensed to the Apache Software Foundation (ASF) under one
;; or more contributor license agreements. See the NOTICE file
;; distributed with this work for additional information
;; regarding copyright ownership. The ASF licenses this file
;; to you under the Apache License, Version 2.0 (the
;; "License"); you may not use this file except in compliance
;; with the License. You may obtain a copy of the License at
;;
;;   http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing,
;; software distributed under the License is distributed on an
;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;; KIND, either express or implied. See the License for the
;; specific language governing permissions and limitations
;; under the License.


;;; Commentary:

;; This code represents a replaceable backend layer for the Emacs wave
;; mode.  It is responsible for communicating with the Wave server.
;;
;; This backend is using the new FedOne websocket-based client/server
;; protocol.
;;


;;; Code:
(eval-and-compile
  (require 'wave-util)
  (require 'url))

(defvar wave-client-ws-process nil
  "The process with the webserver connection.")

(defvar wave-client-queue nil
  "A queue of JSON strings to be processed.")

(defvar wave-client-sequence-num 0
  "The sequence number to be used.")

(defun wave-client-ws-open (url)
  "Open a websocket connection to URL."
  (let ((url-request-method "GET")
        (url-request-extra-headers `(("Upgrade" . "WebSocket")
                                     ("Connection" . "Upgrade")
                                     ("WebSocket-Host" .
                                      ,(url-host (url-generic-parse-url url)))
                                     ("WebSocket-Origin" . ,system-name))))
    ;; wave-client-ws-accept doesn't actually get called...
    (save-excursion
      (set-buffer (url-retrieve url 'wave-client-ws-closed))
      (wave-client-ws-accept))))

(defun wave-client-ws-closed (status)
  "Called when the connection is closed with STATUS."
  (if (eq (car status) :error)
      (signal (caadr status) (cdadr status)))
  (setq wave-client-ws-process nil)
  (setq wave-client-sequence-num 0)
  (wave-debug "Connection closed"))

(defun wave-client-ws-accept ()
  "Accept the websocket connection."
  (unless wave-client-ws-process
    (setq wave-client-ws-process (get-buffer-process (current-buffer)))
    (unless wave-client-ws-process (error "No websocket process found!"))
    (set-process-filter wave-client-ws-process 'wave-client-filter)))

(defun wave-client-filter (process output)
  "Filter OUTPUT from our websocket PROCESS.  As websocket
responses come back, parse them and queue them up for later
retrieval."
  ;; We don't do anything unless we have a valid websocket packet
  (when (and (equal (substring output 0 1) (unibyte-string ?\0))
               (equal (substring output
                                 (- (length output) 1))
                      (unibyte-string ?\377)))
    (let ((json-object-type 'plist)
          (json-key-type nil))
      (setq wave-client-queue
          (append wave-client-queue
                  (list
                   (let ((response (json-read-from-string
                                    (substring output 1
                                               (- (length output) 1)))))
                     (cons (plist-get response :messageType)
                           (json-read-from-string
                            (plist-get response :messageJson))))))))))

(defun wave-client-ws-send-raw (text)
  "Send the raw TEXT as a websocket packet."
  (unless wave-client-ws-process
    (error "No webserver process to send data to!"))
  (process-send-string wave-client-ws-process
                       (concat (unibyte-string ?\0) text
                               (unibyte-string ?\377))))

(defun wave-client-send (obj type)
  "Send OBJ of operation type TYPE to the websocket.
This is wrapped in the standard wrapper, to the websocket.  TYPE
is 'open or 'submit."
  (wave-client-ws-send-raw
   (json-encode
    `(:version 0
               :sequenceNumber ,(incf wave-client-sequence-num)
               :messageType ,(cond ((eq type 'open)
                                    "waveserver.ProtocolOpenRequest")
                                   ((eq type 'submit)
                                    "waveserver.ProtocolSubmitRequest")
                                   (t (error "Unknown type of operation")))
               :messageJson ,(json-encode obj)))))

(provide 'wave-client-websocket)

;;; wave-client-websocket.el ends here
