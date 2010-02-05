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

(require 'cl)
(require 'json)
(require 'url)
(defgroup wave-client  nil
  "Wave client for emacs.")
;;;###autoload
(defcustom wave-client-user ""
  "Name of the Wave user to connect as.  Do NOT include the
domain for hosted accounts such wavesandbox."
  :type 'string 
  :group 'wave-client)
;;;###autoload
(defcustom wave-client-password nil
  "Name of the Wave user to connect as."
  :type '(choice (const :tag "Query when needed" nil)
				       (string  :tag "Password"))
  :group 'wave-client)

;; TODO(ahyatt): Use this when making connections, instead of
;; hardcoding to the default main instance of wave.google.com.
;;;###autoload
(defcustom wave-client-domain nil
  "Domain of the Wave server (such as `wavesandbox.com'), or nil
for the default domain."
  :type  '(choice (const :tag "Query when needed" nil)
				       (string  :tag "Domain"))
  :group 'wave-client)

(defconst wave-client-process-buf-name
  "*wave client*"
  "The buffer of the curl process")

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

(defvar wave-client-gsession
  nil
  "The Wave client gsession, used for creating channels.")

(defvar wave-client-rid
  nil
  "ID of the channel request.  A random number computed by us,
incremented for every request.")

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

(defun wave-client-get-url (&optional path)
  "Returns a proper URL given a PATH, which must start with a /
to the end, if given.  Uses `wave-client-domain'."
  (when (eq wave-client-domain "")
    (error "wave-client-domain empty, should probably be nil"))
  (format "https://wave.google.com%s%s"
          (if wave-client-domain
                        (concat "/a/" wave-client-domain)
                      "/wave")
          (or path "/")))

(defun wave-client-reset ()
  "Clear the stored session and auth token.  Useful for when you
  are switching domains."
  (interactive)
  (setq wave-client-auth-cookie nil)
  (setq wave-client-session nil)
  (setq wave-client-password nil))

(defun wave-client-get-wave-raw (wave-id)
  "Get the wave given from the WAVE-ID, as a plist data structure
that is a direct conversion from the JSON."
  (unless (and wave-client-auth-cookie wave-client-session)
    ;; To re-fetch both auth and session
    (wave-client-get-waves))
  (save-excursion
    (unwind-protect
        (progn
          (set-buffer
           (wave-client-curl
            (wave-client-get-url
             (format "/wfe/fetch/%s/%s?v=3"
                     (url-hexify-string wave-id)
                     (plist-get (plist-get wave-client-session :userProfile)
                                :id))) '()
                                `(("WAVE" . ,wave-client-auth-cookie))))
          (goto-char (point-min))
          (re-search-forward "{")
          (wave-client-json-read (substring (buffer-string)
					    (- (point) 2))))
      (wave-client-kill-current-process-buffer))))

(defun wave-client-get-auth-cookie ()
  "Return the auth cookie for this user."
  (save-excursion
    (unwind-protect
        (progn
          (set-buffer
           (wave-client-curl "https://www.google.com/accounts/ClientLogin"
                             `(("Email" .
                                ,(concat wave-client-user
                                         (when wave-client-domain
                                           (concat "@" wave-client-domain))))
                               ("Passwd" .
                                ,(or wave-client-password
                                     (let ((password
                                            (read-passwd "Password: ")))
                                       (setq wave-client-password password)
                                       password)))
                               ("accountType" . ,(if wave-client-domain
                                                     "HOSTED_OR_GOOGLE"
                                                   "GOOGLE"))
                               ("service" . "wave")
                               ("source" . "emacs-wave")) '() t))
          (goto-char (point-min))
          (search-forward-regexp "Auth=\\(.*\\)$")
          (url-cookie-store "WAVE" (match-string 1) nil
                            "wave.google.com" "/" t)
          (match-string 1))
      (unless (wave-client-kill-current-process-buffer)
        (setq wave-client-password nil)))))

(defun wave-client-kill-current-process-buffer ()
  "Kill the current buffer, if it is a temporary buffer,
otherwise do nothing."
  (when (string-match wave-client-process-buf-name
                      (buffer-name))
    (kill-buffer)))

(defun wave-client-fetch (url data &optional method)
  "Execute curl with a given URL and DATA params, return the
buffer with the result."
  (let ((url-request-data (mapconcat (lambda (c)
                                       (concat
                                        (url-hexify-string (car c))
                                        "="
                                        (url-hexify-string (cdr c))))
                                     data "&"))
        (url-request-method (or method "GET")))
    (url-retrieve-synchronously url)))

(defun wave-client-curl (url data cookies &optional post)
  "Execute curl with a given URL and cookie DATA, return the
buffer with the result."
  (let* ((buf (generate-new-buffer wave-client-process-buf-name))
         (retval
          (apply 'call-process (append (list "curl" nil buf nil
                                             url "-f" "-s")
                                       (unless post (list "-G"))
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
                                         cookies "; "))))))
    (when (= retval 22)
        (error "HTTP error loading page %s" url))
    buf))

(defun wave-client-json-read (text &optional do-extra-munging object-type)
  "Munge the text of the json TEXT to be in the precise form
  json.el expects.  We encode according to OBJECT-TYPE which
  default to plist.  If DO-EXTRA-MUNGING is turned on, then we
  subsitute \" for \' and make sure each key is in quotes.  This
  is not a great thing to do, but we need it for the kind of JSON
  that Wave returns on HTML pages."
  (let ((json-object-type (or object-type 'plist))
        (json-key-type nil))
    (json-read-from-string
     (if do-extra-munging
	 (let ((double-quoted-text (or (replace-regexp-in-string "'" "\"" text)
				       text)))
	   (or (replace-regexp-in-string
		"\\([{,]\\)\\([[:word:]_]+\\):"
		"\\1\"\\2\":" double-quoted-text) double-quoted-text))
       text))))

(defun wave-client-get-waves ()
  "Get a list of waves.  Also has the side effect of populating
the `wave-client-session' variable."
  (let ((auth-cookie (or wave-client-auth-cookie
                         (let ((cookie (wave-client-get-auth-cookie)))
                           (setq wave-client-auth-cookie cookie)
                           cookie))))
    (save-excursion
      (unwind-protect
          (progn
            (set-buffer (wave-client-curl (wave-client-get-url)
                                          '()
                                          `(("WAVE" . ,auth-cookie))))
            (goto-char (point-min))
            (search-forward-regexp "__session = \\({.*}\\);var")
            (setq wave-client-session (wave-client-json-read
                                       (match-string 1) t))
            (search-forward-regexp "json = \\({\"r\":\"^d1\".*}\\);")
            (wave-client-json-read (match-string 1) t))
        (wave-client-kill-current-process-buffer)))))

(defun wave-client-extract-waves (wave-plist)
  "Extract information from the raw WAVE-PLIST, transforming it
into the format defined by `wave-inbox'."
  (mapcar (lambda (wave)
            (list :id (plist-get wave :1)
                  :unread (plist-get wave :7)  ;; int
                  :digest (plist-get (plist-get wave :9) :1)
                  :creator (plist-get wave :4)))
          (plist-get (plist-get wave-plist :p) :1)))

(defun wave-client-extract-boundary (boundary)
  "Extract information about the boundary"
  (let ((result '(boundary)))
    (if (plist-get boundary :1)
        (add-to-list 'result 'empty) t)
    (if (> (length (plist-get boundary :2)) 0)
        (add-to-list 'result
                     (list 'end (plist-get boundary :2)) t))
    (if (> (length (plist-get boundary :3)) 0)
        (add-to-list 'result
                     (list 'change
                           (mapcar
                            (lambda (update)
                              (list :key (plist-get update :1)
                                :oldvalue (plist-get update :2)
                                :newvalue (plist-get update :3)))
                            (plist-get boundary :3))) t))
    result))

(defun wave-client-extract-blip (blip-plist)
  "Extract information about a single blip from the raw
  BLIP-PLIST."
  (list :blip-id (plist-get blip-plist :1)
        :authors (plist-get blip-plist :7)
        :modified-time
              (wave-client-extract-long (plist-get blip-plist :3))
        :ops (mapcar
              (lambda (token)
                (let ((start-element (plist-get token :4))
                      (text (plist-get token :2))
                      (end-element (plist-get token :5))
                      (boundary (plist-get token :1)))
                  (cond (start-element
                         (intern (plist-get start-element :1)))
                        (text text)
                        (end-element 'end)
                        (boundary
                         (wave-client-extract-boundary boundary))
                        (t 'unknown))))
              (plist-get (plist-get blip-plist :16) :2))
        :children (mapcar (lambda (child)
                            (plist-get child :6))
              (plist-get blip-plist :5))))

(defun wave-client-extract-long (long)
  "From a 2-byte long, extract a single long integer"
  ;; TODO(ahyatt) Don't know how to do this yet
  0)

(defun wave-client-extract-wavelet (wavelet-plist)
  "Extract information about a wavelet from the raw WAVE-PLIST,
  transforming it into the format detined by `wave-get-wave'"
  (let* ((wavelet (plist-get wavelet-plist :1))
         (metadata (plist-get wavelet :1))
         (blips (plist-get wavelet :2))
         (blips-hashtable (make-hash-table)))
    (dolist (blip (append blips '()))
      (let ((extracted-blip (wave-client-extract-blip blip)))
        (puthash (intern (plist-get extracted-blip :blip-id))
                 extracted-blip blips-hashtable)))
    (list :participants (plist-get metadata :5)
          :creator (plist-get metadata :3)
          :wave-id (plist-get metadata :1)
          :wavelet-id (plist-get metadata :2)
          :creation-time (wave-client-extract-long
                          (plist-get metadata :4))
          :root-blip-id (plist-get metadata :6)
          :blips blips-hashtable)))

(defun wave-client-extract-wave (wave-plist)
  "Extract information about a single wave from the raw
  WAVE-PLIST, transforming it into the format defined by
  `wave-get-wave'"
  (mapcar 'wave-client-extract-wavelet (plist-get wave-plist :1)))

(defun wave-client-populate-gsession ()
  "Get the gsessionid that Wave uses to keep track of channels."
  (save-excursion
    (unwind-protect
        (progn (set-buffer
                (wave-client-curl (wave-client-get-url "/wfe/testLogin?gsessionid=unknown")
                                  ;; we need something to trigger a curl post
                                  '(("not" . "used"))
                                  `(("WAVE" . ,wave-client-auth-cookie))
                                  t))
               (goto-char (point-min))
               (re-search-forward "{")
               (setq wave-client-gsession
                     (plist-get (wave-client-json-read
                                 (substring (buffer-string)
                                            (- (point) 2)))
                                :1)))
            (wave-client-kill-current-process-buffer))))

(defun wave-client-update-to-list (update-list kind)
  "Transforms an update-list (given as an emacs array) to a list
of updates with key KIND."
  (let ((ordered-list
         (mapcan (lambda (update)
                   (let ((order (elt update 0))
                         (data (elt update 1)))
                     (when (string= (elt data 0) kind)
                (list (cons order (elt data 1))))))
                 update-list)))
    (sort ordered-list (lambda (a b) (< (car a) (car b))))
    (mapcar 'cdr ordered-list)))

(defun wave-client-get-channel-sid ()
  "Get the SID neceesary to open a Wave channel connection."
  (setq wave-client-rid (abs (random 100000)))
  (wave-client-populate-gsession)
  (save-excursion
    (unwind-protect
        (progn
          (set-buffer
           (wave-client-fetch
            (wave-client-get-url
             (concat "/wfe/channel?gsessionid=" wave-client-gsession
                     "&VER=7&RID=" (int-to-string wave-client-rid)))
            nil "POST"))
          (goto-char (point-min))
          ;; ignore chunk size
          (re-search-forward "\\[")
          (car (wave-client-update-to-list
                (json-read-from-string (substring (buffer-string)
                                                  (- (point) 2)))
                "c")))
      (wave-client-kill-current-process-buffer))))

;; Functions for the Wave mode to use:

(defun wave-inbox ()
  "List all waves in the inbox. For the exact format, see
http://code.google.com/p/wave-client-for-emacs/wiki/WaveClientDataSpec"
  (wave-client-extract-waves (wave-client-get-waves)))

(defun wave-get-wave (wave-id)
  "Get a wave given a WAVE-ID.  For the exact format, see
http://code.google.com/p/wave-client-for-emacs/wiki/WaveClientDataSpec"
  (wave-client-extract-wave (wave-client-get-wave-raw wave-id)))

(provide 'wave-client)

;;; wave-client.el ends here
