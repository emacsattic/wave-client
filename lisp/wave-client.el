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
;; The present form of communication takes place through a
;; Clojure-based REPL on top of the FedOne codebase.  That REPL
;; actually handles the real client/server communication.
;;
;; When a general client/server spec is implemented, we should use it
;; here instead of the FedOne REPL.  In that case, most of the private
;; functions would be replaced.

(require 'net-utils)

;;; Code:
(defcustom wave-client-server "localhost"
  "The server host (without a port) that the wave client will connect to."
  :group 'wave-client)

(defcustom wave-client-port 8123
  "The REPL port that the wave client will connect to."
  :group 'wave-client
  :type 'integer)

(defcustom wave-client-java-location "/usr/bin/java"
  "The location of the java 1.6 executable (used for starting the server)."
  :group 'wave-client
  :type '(file :must-match t))

(defcustom wave-client-root-dir ""
  "Location of the root of the wave-client-for-emacs directory."
  :group 'wave-client
  :type '(file :must-match t))

(defcustom wave-client-user ""
  "Name of the Wave user to connect as."
  :group 'wave-client)

(defcustom wave-client-domain ""
  "Domain of the Wave server (such as `wavesandbox.com').  Should
match the domain in the FedOne server's `run-config.sh' file."
  :group 'wave-client)

(defcustom wave-client-server-hostname "localhost"
  "The hostname of of the server the wave client REPL must connect to.
This should be the hostname that the FedOne server is running on."
  :group 'wave-client)

(defcustom wave-client-server-port "9876"
  "The port of the server the wave client REPL must connect to.
This should be the port on which the FedOne server is running."
  :group 'wave-client
  :type 'integer)

(defvar wave-client-repl-buf-name
  " *wave client repl*"
  "The buffer name of the REPL to the wave client.")

(defconst wave-client-package-name
  "wave-for-emacs.client"
  "The name of the package of the Clojure REPL.")

(defconst wave-client-process-name
  "wave client repl"
  "The name of the process for the Clojure REPL.")

(defvar wave-client-temp-output
  ""
  "Temporary output to the REPL buffer.")

(defconst wave-client-has-ouput
  nil
  "This boolean indicates that the REPL has output to process.")

(defvar wave-client-output
  nil
  "Completed output from the REPL buffer.")

(defun wave-client-assert-connected ()
  (unless (buffer-live-p wave-client-repl-buf-name)
    (error "Wave client not running")))

(defun wave-client-start-fedone ()
  "Start the FedOne client process, which we talk to via a network port."
  (let ((default-directory
          (replace-regexp-in-string "\\([^/]\\)$" "\\1/"
                                    wave-client-root-dir))
        (buffer-name " *fedone client*"))
                          ""
    (start-process "wave client startrepl"
                   buffer-name
                   (concat wave-client-root-dir
                           "start-repl.sh")
                   wave-client-java-location
                   (int-to-string wave-client-port))))

(defun wave-client-process-filter (process output)
  "Stores the output from a process."
  (let ((finished t) ; for now, let's just always consider us
                     ; finished, otherwise we get some errors with
                     ; false positives here
        (temp-output output))
    ;; Delete the prompts, if there are any
    (setq temp-output (replace-regexp-in-string "^.*=>\s*" "" temp-output))
    ;; Sometimes there are stray nills around, presumably from our
    ;; flush statement.  Let's just get rid of those quietly, if
    ;; there's something else there.
    (when (string-match "(" temp-output)
      (setq temp-output
            (replace-regexp-in-string
             "\n" ""
             (replace-regexp-in-string "^nil$" "" temp-output))))
    ;; Delete newlines
    (setq temp-output (replace-regexp-in-string "\n$" "" temp-output))
    (setq wave-client-temp-output
          (concat wave-client-temp-output temp-output))
    (when finished
      (setq wave-client-output wave-client-temp-output)
      (setq wave-client-has-output t)
      (setq wave-client-temp-output ""))))

(defun wave-client-start-repl ()
  "Connect to the FedOne client REPL process."
  (save-excursion
    (let ((attempt-num 0)
          connected)
      (while (and (not connected) (< attempt-num 3))
        (condition-case nil
            (progn
              (open-network-stream wave-client-process-name
                                   nil
                                   wave-client-server wave-client-port)
              (setq connected t))
          (error
           (if (= attempt-num 2)
               (error "Wave REPL did not start in time")
             (message "Waiting for wave REPL to be ready")
             (sleep-for 1)
             (incf attempt-num))))))
    (set-process-filter (get-process wave-client-process-name)
                        'wave-client-process-filter)))

(defun wave-client-connect ()
  "Start up a connection to the Wave server."
  (interactive)
  (wave-client-start-fedone)
  (wave-client-start-repl)
  ;; get the first prompt
  (accept-process-output nil 0.5)
  ;; output to this is retrieved separately
  (wave-client-eval '(in-ns 'wave-for-emacs.client))
  (wave-client-eval `(open-backend
                     ,(concat wave-client-user "@" wave-client-domain)
                     ,wave-client-server-hostname
                     ,(string-to-int wave-client-server-port))))

(defun wave-client-escapify-clojure (text)
  "Escape any clojure-outputted symbols in TEXT that emacs cannot
read in the `read-from-string' method."
  (replace-regexp-in-string "\\(#<.*>\\)" "\"\\1\"" text))

(defmacro wave-client-eval (&rest rest)
  "Evaluate REST, a quoted s-expression in the FedOne REPL.
Since the REPL is actually evaluating clojure, the code should
actually be clojure code."
  `(car (read-from-string
         (wave-client-escapify-clojure
          (wave-client-send-and-receive
           (concat (replace-regexp-in-string
                    "\\\\." "."
                    (prin1-to-string ,@rest))))))))

(defun wave-client-send-and-receive (text)
  "Send TEXT to the FedOne REPL, and return the text result."
  (setq wave-client-has-output nil)
  (setq wave-client-output nil)
  (process-send-string wave-client-process-name text)
  (accept-process-output nil)
  (process-send-string wave-client-process-name "(flush)")
  (accept-process-output nil)
  (while (not wave-client-has-output)
    (message "Waiting for Wave REPL output")
    (accept-process-output nil 0.5))
  (if (equal "" wave-client-output)
      "nil"
    wave-client-output))


;; Functions for the Wave mode to use:

(defun wave-inbox ()
  "List all waves in the inbox."
  (wave-client-eval '(get-waves)))

(provide 'wave-client)

;;; wave-client.el ends here
