;;; wave-client.el --- Methods to communicate with the Wave server

;; Copyright (c) 2009 Andrew Hyatt
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

;; This code represents a replaceable backend layer for the emacs wave
;; mode.  It is responsible for communicating with the Wave server.
;;
;; This backend uses a direct connection to https://wave.google.com.
;;
;; WARNING: This code utilizes the unpublished client/server protocol
;; that wave.google.com's javascript uses to communicate with the wave
;; servers. This is not a stable protocol supported by Google. This
;; protocol is the Google Wave team's internal protocol and is under
;; active development, so it may change at any time without regards
;; for this or any other client.

(eval-and-compile
  (require 'cl)
  (require 'wave-client-websocket)
  (require 'wave-client-browser-channel))

(defgroup wave-client nil
  "Wave client for emacs.")

;;;###autoload
(defcustom wave-client-user ""
  "Name of the Wave user to connect as.  Do NOT include the
domain for hosted accounts such as wavesandbox."
  :type 'string 
  :group 'wave-client)

;;;###autoload
(defcustom wave-client-domain nil
  "Domain of the Wave server (such as `wavesandbox.com'), or nil
for the default domain."
  :type  '(choice (const :tag "Query when needed" nil)
                  (string  :tag "Domain"))
  :group 'wave-client)

(defvar wave-client-temp-output
  ""
  "Temporary output to the REPL buffer.")

(defvar wave-debug
  nil
  "Boolean indicating whether debug output should be sent to
wave-client-debug-buffer")

(defconst wave-debug-buffer
  "*Wave Client Debug Buffer*"
  "Name of the buffer wear debug output is sent to")

(defun wave-client-domain ()
  (or wave-client-domain "googlewave.com"))

(defun wave-client-email-address ()
  "Return the email address of the user."
  (if (string-match "@" wave-client-user)
      wave-client-user
    (concat wave-client-user "@" (wave-client-domain))))

(defun wave-debug (str &rest args)
  "Send STR to the `wave-debug-buffer', with newline, with format
args ARGS."
  (when wave-debug
    (save-excursion
      (set-buffer
       (get-buffer-create wave-debug-buffer))
      (goto-char (point-max))
      (insert (apply 'format (append (list str) args)))
      (insert "\n"))))

(defvar wave-client-connection-method 'browser-channel
  "A symbol, either browser-channel or websocket.")


;; Functions for the Wave mode to use:

(defun wave-client-send-delta (delta wavelet-name)
  "Send DELTA to the wave server."
  (ecase wave-client-connection-method
    (browser-channel (wave-bc-send-delta delta wavelet-name))
    (websocket (wave-ws-send-delta delta wavelet-name))))

(defun wave-get-inbox (&optional callback)
  "List all waves in the inbox. For the exact format, see
http://code.google.com/p/wave-client-for-emacs/wiki/WaveClientDataSpec"
  (ecase wave-client-connection-method
    (browser-channel (wave-bc-get-inbox))
    (websocket (wave-ws-get-inbox callback))))

(defun wave-get-wave (wave-id &optional callback)
  "Get a wave given a WAVE-ID.  For the exact format, see
http://code.google.com/p/wave-client-for-emacs/wiki/WaveClientDataSpec"
  (ecase wave-client-connection-method
    (browser-channel (wave-bc-get-wave wave-id))
    (websocket (wave-ws-get-wave wave-id callback))))

(provide 'wave-client)

;;; wave-client.el ends here
