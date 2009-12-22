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

(require 'cl)
(require 'json)
(require 'url)

(defcustom wave-client-user ""
  "Name of the Wave user to connect as."
  :group 'wave-client)

(defcustom wave-client-password nil
  "Name of the Wave user to connect as."
  :group 'wave-client)

;; TODO(ahyatt): Use this when making connections, instead of
;; hardcoding to the default main instance of wave.google.com.
(defcustom wave-client-domain ""
  "Domain of the Wave server (such as `wavesandbox.com').  Should
match the domain in the FedOne server's `run-config.sh' file."
  :group 'wave-client)

(defconst wave-client-process-buf-name
  " *wave client*"
  "The buffer name of the FedOne client process")

(defvar wave-client-temp-output
  ""
  "Temporary output to the REPL buffer.")

(defvar wave-client-debug
  nil
  "Boolean indicating whether debug output should be sent to
wave-client-debug-buffer")

(defvar wave-client-auth-cookie
  nil
  "Auth cookie to use for Wave.")

(defvar wave-client-session
  nil
  "The Wave current session")

(defconst wave-client-debug-buffer
  "*Wave Client Debug Buffer*"
  "Name of the buffer wear debug output is sent to")

(defun wave-client-debug (str)
  "Send STR to the `wave-client-debug-buffer', with newline."
  (when wave-client-debug
    (save-excursion
      (set-buffer
       (get-buffer-create wave-client-debug-buffer))
      (goto-char (point-max))
      (insert str)
      (insert "\n"))))

(defun wave-client-assert-connected ()
  (unless (and wave-client-auth-cookie
               wave-client-session)
    (error "Wave client not running")))

(defun wave-client-get-auth-cookie ()
  "Return the auth cookie for this user."
  (save-excursion
    (set-buffer
     (wave-client-curl "https://www.google.com/accounts/ClientLogin"
                       `(("Email" . ,wave-client-user)
                         ("Passwd" . ,(or wave-client-password
                                          (let ((password
                                                 (read-passwd "Password: ")))
                                            (setq wave-client-password password)
                                            password)))
                         ("accountType" . "GOOGLE")
                         ("service" . "wave")
                         ("source" . "emacs-wave")) '()))
    (goto-char (point-min))
    (search-forward-regexp "Auth=\\(.*\\)$")
    (match-string 1)))

(defun wave-client-curl (url data cookies)
  "Execute curl with a given URL and cookie DATA, return the
buffer with the result."
  (let ((buf (generate-new-buffer wave-client-process-buf-name)))
    (apply 'call-process (append (list "curl" nil buf nil url)
                                 (mapcan (lambda (c)
                                           (list "-d"
                                                 (concat
                                                  (url-hexify-string (car c))
                                                  "="
                                                  (url-hexify-string (cdr c)))))
                                           data)
                                 (list
                                  "-b"
                                  (mapconcat
                                   (lambda (c)
                                     (concat
                                      (url-hexify-string (car c))
                                      "="
                                      (url-hexify-string (cdr c))))
                                   cookies "; "))))
    buf))

(defun wave-client-json-read (text &optional object-type)
  "Munge the text of the json TEXT to be in the precise form
  json.el expects.  We encode according to OBJECT-TYPE which
  default to plist."
  (let ((json-object-type (or object-type 'plist))
        (json-key-type nil))
    (json-read-from-string
     (let ((double-quoted-text (or (replace-regexp-in-string "'" "\"" text)
                                   text)))
       (or (replace-regexp-in-string
            "\\([{,]\\)\\([[:word:]_]+\\):"
            "\\1\"\\2\":" double-quoted-text) double-quoted-text)))))

(defun wave-client-get-waves ()
  "Get a list of waves.  Also has the side effect of populating
the `wave-client-session' variable."
  (let ((auth-cookie (or wave-client-auth-cookie
                         (let ((cookie (wave-client-get-auth-cookie)))
                           (setq wave-client-get-auth-cookie cookie)
                           cookie))))
    (save-excursion
      (set-buffer (wave-client-curl "https://wave.google.com/wave/"
                                    '()
                                    `(("WAVE" . ,auth-cookie))))
      (goto-char (point-min))
      (search-forward-regexp "__session = \\({.*}\\);var")
      (setq wave-client-session (wave-client-json-read
                                 (match-string 1)))
      (search-forward-regexp "json = \\({\"r\":\"^d1\".*}\\);")
      (wave-client-json-read (match-string 1)))))

(defun wave-client-extract-waves (wave-plist)
  "Extract information from the raw WAVE-PLIST, transforming it
into the format defined by `wave-inbox'."
  (mapcar (lambda (wave)
            (list
             (cons :id (plist-get wave :1))
             (cons :digest (plist-get (plist-get wave :9) :1))
             (cons :author (plist-get wave :4))))
          (plist-get (plist-get wave-plist :p) :1)))

;; Functions for the Wave mode to use:

(defun wave-inbox ()
  "List all waves in the inbox. Output format is an alist of waves,
each having a `:digest',`:author', and `id'.  The author is the
first contributor to the first wavelet, which should be the
person who contributed the wave."
  (wave-client-extract-waves (wave-client-get-waves)))

(provide 'wave-client)

;;; wave-client.el ends here
