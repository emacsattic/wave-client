;;; wave-client-browser-channel.el --- TODO

;; Copyright (c) 2009 Andrew Hyatt
;;
;; Author: Andrew Hyatt <ahyatt at gmail dot com>
;; Maintainer: Andrew Hyatt <ahyatt at gmail dot com>
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

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
  (require 'json)
  (require 'url)
  (require 'tls)
  (require 'wave-util)
  (require 'wave-data))

(defcustom wave-client-server
  "https://wave.google.com"
  "The server url, without any trailing backslash"
  :type 'string
  :group 'wave-client)

(defcustom wave-client-attachment-server
  "https://wave.googleusercontent.com"
  "The attachment server url, without any trailing backslash"
  :type 'string
  :group 'wave-client)

(defconst wave-client-process-buf-name
  "*wave client*"
  "The buffer of the curl process")

(defconst wave-client-get-buf-name
  "*wave client GET*"
  "The buffer of the Wave hanging GET." )

(defconst wave-client-task-type
  '((echo . 0) (view-submit . 1100))
  "Task type to ids sent with browser channel requests.")

(defvar wave-client-auth-cookie
  nil
  "Auth cookie to use for Wave.")

(defvar wave-client-session
  nil
  "The Wave current session")

(defvar wave-client-gsession
  nil
  "The Wave client gsession, used for creating channels.")

(defvar wave-client-sid
  nil
  "The SID of the channel request.  Stays constant throughout the
  session.")

(defvar wave-client-rid
  nil
  "ID of the channel request.  A random number computed by us,
incremented for every request.")

(defvar wave-client-identifier
  nil
  "A random string used as a client identifier.")

(defvar wave-client-request-num 0
  "The request number, which monotonically increases.")

(defvar wave-client-bc-get-conn nil
  "The connection for the browser channel GET process.")

(defvar wave-client-last-array-id 0
  "Last seen array number")

(defvar wave-client-queue '()
  "A queue of json messages from the GET browser channel")

(defconst wave-bc-op-to-message-type
  '((add-participant . 5975636)
    (remove-participant . 5975541)
    (blip-submit . 5926533)
    (mutate-document . 9184307)))

(defun wave-bc-message-type (op)
  "From an operation OP, return the relevant message type"
  (cdr (assoc op wave-bc-op-to-message-type)))

(defun wave-client-assert-connected ()
  (unless (and wave-client-auth-cookie
               wave-client-session)
    (error "Wave client not running")))

(defun wave-client-path-prefix ()
  (if wave-client-domain
              (concat "/a/" wave-client-domain)
    "/wave"))

(defun wave-client-get-url (&optional path)
  "Returns a proper URL given a PATH, which must start with a /
to the end, if given.  Uses `wave-client-domain'."
  (when (eq wave-client-domain "")
    (error "wave-client-domain empty, should probably be nil"))
  (format "%s%s%s"
          wave-client-server
          (wave-client-path-prefix)
          (or path "/")))

(defun wave-client-reset-browser-channel ()
  (wave-debug "Resetting browser channel")
  (let ((conn-buf (process-buffer wave-client-bc-get-conn)))
    (delete-process wave-client-bc-get-conn)
    (kill-buffer conn-buf))
  (setq wave-client-gsession nil
        wave-client-sid nil
        wave-client-rid nil
        wave-client-request-num 0
        wave-client-last-array-id 0))

(defun wave-client-reset ()
  "Clear the stored session and auth token.  Useful for when you
  are switching domains."
  (interactive)
  (setq wave-client-auth-cookie nil
        wave-client-session nil)
  (wave-client-reset-browser-channel))

(defun wave-client-get-wave-raw (wave-id)
  "Get the wave given from the WAVE-ID, as a plist data structure
that is a direct conversion from the JSON."
  (unless (and wave-client-auth-cookie wave-client-session)
    ;; To re-fetch both auth and session
    (wave-client-get-waves))
  (wave-client-get-json
   (format "/wfe/fetch/%s/%s?v=3"
           (url-hexify-string wave-id)
           (plist-get (plist-get wave-client-session :userProfile)
                      :id))))

(defun wave-client-get-new-auth-cookie ()
  "Return the auth cookie for this user."
  (unless wave-client-user
    (error (concat "You need to at least set `wave-client-user' "
                   "before logging into Wave.")))
  (save-excursion
    (unwind-protect
        (progn
          (set-buffer
           (wave-client-curl
            "https://www.google.com/accounts/ClientLogin"
            `(("Email" .
               ,(concat wave-client-user
                        (when wave-client-domain
                          (concat "@" wave-client-domain))))
              ("Passwd" .
               ,(read-passwd
                 (format
                  "Password for %s: "
                  (concat wave-client-user
                          (when wave-client-domain
                            (concat "@" wave-client-domain))))))
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
      (wave-client-kill-current-process-buffer))))

(defun wave-client-kill-current-process-buffer ()
  "Kill the current buffer, if it is a temporary buffer,
otherwise do nothing."
  (when (get-buffer wave-client-process-buf-name)
    (kill-buffer wave-client-process-buf-name)))

(defun wave-client-fetch (url data &optional method)
  "Execute curl with a given URL and DATA params, return the
buffer with the result."
  (wave-debug "Fetching data from %s" url)
  (let ((url-request-data (mapconcat (lambda (c)
                                       (concat
                                        (url-hexify-string (car c))
                                        "="
                                        (url-hexify-string (cdr c))))
                                     data "&"))
        (url-request-method (or method "GET")))
    (url-retrieve url 'wave-client-accept (list wave-client-gsession))))

(defun wave-client-accept (status &optional gsession)
  "Accept the next set of browser channel items."
  (cond ((eq (car status) :redirect)
         (wave-debug "Redirect to: %s" (cadr status)))
        ((eq (car status) :error)
         (signal (caadr status) (cdadr status))))
  (wave-debug "accept, buffer-string: %s" (buffer-string))
  ;; If the gsession is no longer current, do not accept anything.
  (if (not (equal wave-client-gsession gsession))
      (wave-debug "gsessions don't match.  This must be a leftover.")
      (while (re-search-forward "^[[:digit:]]+$" nil t)
        (let* ((len (string-to-number (match-string 0)))
               (end (+ (point) len 1))
               (json (json-read-from-string (buffer-substring (+ 1 (point)) end)))
               (last (aref json (- (length json) 1))))
          (setq wave-client-last-array-id (aref last 0))
          (wave-debug "setting aref to %d" wave-client-last-array-id)
          (setq wave-client-queue
                (append wave-client-queue
                        (remove-if-not (lambda (e) (equal (aref (cdr e) 0) "wfe"))
                                       (mapcar (lambda (elem)
                                                 (cons (aref elem 0)
                                                       (if (stringp (aref elem 1))
                                                           (json-read-from-string
                                                            (aref elem 1))
                                                         (aref elem 1)))) json))))
          ;; We need to kill the browser channel if it stops
          (when (and (vectorp (aref last 1)) (equal (aref (aref last 1) 0) "stop"))
            (wave-debug "Browser channel stopped, re-opening after 5 secs.")
            (wave-client-reset-browser-channel))))))

(defun wave-client-curl (url data cookies &optional post)
  "Execute curl with a given URL and cookie DATA, return the
buffer with the result."
  (wave-debug "Sending %s request to url %s, with data %s"
              (if post "POST" "GET") url data)
  (let* ((buf (generate-new-buffer wave-client-process-buf-name)))
    (with-current-buffer buf
      (set-buffer-multibyte nil))
    (let ((retval
           (apply 'call-process "curl" nil buf nil
                  (nconc (list url "-f" "-s")
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
        (error "HTTP error loading page %s" url)))
    buf))

(defun wave-client-json-read (text &optional do-extra-munging object-type)
  "Munge the text of the json TEXT to be in the precise form
  json.el expects.  We encode according to OBJECT-TYPE which
  default to plist.  If DO-EXTRA-MUNGING is turned on, then we
  subsitute \" for \' and make sure each key is in quotes.  This
  is not a great thing to do, but we need it for the kind of JSON
  that Wave returns on HTML pages."
  (wave-debug "Reading json: %s" text)
  (let* ((json-object-type (or object-type 'plist))
         (json-key-type nil)
         (text (or (replace-regexp-in-string "\n" "" text)))
         (text (or (replace-regexp-in-string ",," ",\"\"," text))))
    (json-read-from-string
     (if do-extra-munging
	 (let* ((double-quoted-text (or (replace-regexp-in-string "'" "\"" text)
				       text)))
	   (or (replace-regexp-in-string
		"\\([{,]\\)\\([[:word:]_]+\\):"
		"\\1\"\\2\":" double-quoted-text) double-quoted-text))
       text))))

(defun wave-client-auth-cookies ()
  (unless wave-client-auth-cookie
    (setq wave-client-auth-cookie (wave-client-get-new-auth-cookie)))
  `(("WAVE" . ,wave-client-auth-cookie)))

(defun wave-client-get-waves ()
  "Get a list of waves.  Also has the side effect of populating
the `wave-client-session' variable."
  (save-excursion
    (unwind-protect
        (progn
          (set-buffer (wave-client-curl (wave-client-get-url)
                                        '()
                                        (wave-client-auth-cookies)))
          (goto-char (point-min))
          (search-forward-regexp "__session = \\({.*}\\);var")
          (setq wave-client-session (wave-client-json-read
                                     (match-string 1) t))
          (search-forward-regexp "json = \\({\"r\":\"^d1\".*}\\);")
          (wave-client-json-read (match-string 1) t))
      (wave-client-kill-current-process-buffer))))

(defun wave-client-extract-waves (wave-plist)
  "Extract information from the raw WAVE-PLIST, transforming it
into the format defined by `wave-get-inbox'."
  (mapcar (lambda (wave)
            (list :id (plist-get wave :1)
                  :unread (plist-get wave :7)  ;; int
                  :digest (plist-get (plist-get wave :9) :1)
                  :creator (plist-get wave :4)))
          (plist-get (plist-get wave-plist :p) :1)))

(defun wave-client-extract-boundary (boundary)
  "Extract information about the boundary"
  ;; We use @ because @ is not a valid first character in an XML name,
  ;; so there will be no ambiguity with element starts.
  (let ((result '(@boundary)))
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

(defun wave-client-extract-attributes (raw-attributes)
  (mapcan (lambda (raw-attr)
            (list (intern (plist-get raw-attr :1))
                  (plist-get raw-attr :2)))
          raw-attributes))

(defun wave-client-extract-blip (blip-plist)
  "Extract information about a single blip from the raw
  BLIP-PLIST."
  (wave-make-doc
   :doc-id (intern (plist-get blip-plist :1))
   :contributors (plist-get blip-plist :7)
   :last-modified-version (wave-client-extract-long (plist-get blip-plist :15))
   :last-modified-time (wave-client-extract-long (plist-get blip-plist :3))
   :content (mapcar
             (lambda (token)
               (let ((start-element (plist-get token :4))
                     (text (plist-get token :2))
                     (end-element (plist-get token :5))
                     (boundary (plist-get token :1)))
                 (cond (start-element
                        (list (intern (plist-get start-element :1))
                              (wave-client-extract-attributes
                               (plist-get start-element :2))))
                       (text text)
                       (end-element 'end)
                       (boundary
                        (wave-client-extract-boundary boundary))
                       (t 'unknown))))
             (plist-get (plist-get blip-plist :16) :2))))

(defun wave-client-extract-long (long)
  "From a 2-byte long, extract a single long integer"
  (if (zerop (aref long 1))
      (aref long 0)
    ;; TODO(ahyatt): handle this case; perhaps use a float?
    nil))

(defun wave-client-extract-wavelet (wavelet-plist)
  "Extract information about a wavelet from the raw WAVE-PLIST,
  transforming it into the format defined by `wave-get-wave'"
  (let* ((wavelet (plist-get wavelet-plist :1))
         (metadata (plist-get wavelet :1))
         (blips (plist-get wavelet :2))
         (blips-hashtable (make-hash-table)))
    (loop for blip across blips do
          (let ((extracted-blip (wave-client-extract-blip blip)))
            (puthash (wave-doc-doc-id extracted-blip)
                     extracted-blip blips-hashtable)))
    (wave-make-wavelet
     :wavelet-name (cons (plist-get metadata :1) (plist-get metadata :2))
     :creator (plist-get metadata :3)
     :version (cons (wave-client-extract-long (plist-get metadata :7))
                    (plist-get metadata :9))
     :last-modified-time (wave-client-extract-long (plist-get metadata :8))
     :creation-time (wave-client-extract-long (plist-get metadata :4))
     :participants (plist-get metadata :5)
     :docs blips-hashtable)))

(defun wave-client-extract-wave (wave-plist)
  "Extract information about a single wave from the raw
  WAVE-PLIST, transforming it into the format defined by
  `wave-get-wave'"
  (mapcar 'wave-client-extract-wavelet (plist-get wave-plist :1)))

(defun wave-client-get-json (url-suffix &optional data noerror)
  "Return the JSON from a Wave servlet with URL-SUFFIX.  If DATA
is defined, we will do a POST with the data."
  (save-excursion
    (unwind-protect
        (progn
          (set-buffer
           (wave-client-curl
            (wave-client-get-url url-suffix)
            ;; we need something to trigger a curl post
            data
            (wave-client-auth-cookies)
            (not (null data))))
          (goto-char (point-min))
          (if (re-search-forward "{\\|\\[" nil noerror)
            (wave-client-json-read
             (substring (buffer-string)
                        (- (point) 2)))
            (buffer-string)))
      (wave-client-kill-current-process-buffer))))

;;; Protos definitions see:
;;; http://www.waveprotocol.org/draft-protocol-specs/draft-protocol-spec#anchor11
;;; Note the numerical values there don't always seem to be right.  We
;;; use the correct version.

(defproto wave-submit-delta-request
  (wave-id :1)
  (wavelet-id :2)
  (delta :3))

(defproto wave-delta
  (version :1)
  (user-id :3)
  (op-list :4)
  (pre-signature :7))

(defproto wave-op
  (type-id :1)
  (add-participant :4)
  (remove-participant :5)
  (blip-submit :10)
  (mutate-document :23))

(defproto wave-add-participant
  (user-id :1))

(defproto wave-remove-participant
  (user-id :1))

(defproto wave-mutate-document
  (document-id :1)
  (operation :2))

(defproto wave-blip-submit
  (blip-id :1))

(defproto wave-document-operation
  (component :2))

(defproto wave-component
  (annotation-boundary :1)
  (text :2)
  (element-start :4)
  (element-end :5)
  (retain-item-count :6))

(defproto wave-key-value-pair
  (key :1)
  (value :2))

(defproto wave-element-start
  (type :1)
  (attributes :2))

(defun wave-client-populate-gsession ()
  "Get the gsessionid that Wave uses to keep track of channels."
  (setq wave-client-gsession
        (plist-get
         (wave-client-get-json "/wfe/testLogin?gsessionid=unknown"
                               '(("not" . "used")))
         :1)))

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
    (wave-debug "ordered-list = %s" ordered-list)
    (let ((last-id (caar (last ordered-list))))
      (when last-id (setf wave-client-last-array-id last-id)))
    (mapcar 'cdr ordered-list)))

(defun wave-client-get-inc-rid ()
  "Get and increment the rid as a string.  This returns the rid
before it is incremented."
  (incf wave-client-rid)
  (int-to-string (- wave-client-rid 1)))

(defun wave-client-get-channel-sid ()
  "Get the SID neceesary to open a Wave channel connection."
  (unless wave-client-sid
    (setq wave-client-rid (abs (random 100000)))
    (unless wave-client-gsession
      (wave-client-populate-gsession))
    (setq wave-client-sid
          (car (wave-client-update-to-list
                (wave-client-get-json
                 (concat "/wfe/channel?gsessionid=" wave-client-gsession
                         "&VER=7&RID=" (wave-client-get-inc-rid))
                 '(("not" . "used")))
                "c"))))
  wave-client-sid)

(defun wave-client-post-to-browser-channel (data)
  "Post not-yet-jsonified DATA to a browser-channel.  DATA is a
list of data pieces to post."
  (wave-client-assert-connected)
  (wave-debug "Posting to browser channel: %s" data)
  (let* ((sid (wave-client-get-channel-sid))
         (url (concat "/wfe/channel?gsessionid=" wave-client-gsession
                       "&VER=8&SID=" sid "&RID=" (wave-client-get-inc-rid)
                       "&AID=" (int-to-string wave-client-last-array-id)))
         (i 0))
    (unwind-protect
        (wave-client-get-json url
                          (append
                           (list (cons "count" (int-to-string (length data))))
                           (list (cons "ofs"
                                       (int-to-string wave-client-request-num)))
                           (mapcar (lambda (d)
                                     (let ((retval
                                            (cons
                                             (concat "req" (int-to-string i)
                                                     "_key")
                                             (json-encode-list d))))
                                       (incf i)
                                       retval)) data)) t)
      (wave-client-kill-current-process-buffer))
    (wave-client-get-browser-channel)))

(defun wave-client-wrap-browser-channel-req (data type)
  "Return an json object wrapping the data for posting to a
  browser channel, not yet converted to a string.  TYPE is an
  symbol of the set mapped by `wave-client-task-type'."
  (unless wave-client-identifier
    ;; TODO(ahyatt): The identifier should be a random string, but for
    ;; now, we just have it be a random number string, which is
    ;; somewhat lame.
    (setq wave-client-identifier (int-to-string (random 100000))))
  (list :a wave-client-identifier
        :r (incf wave-client-request-num)
        :t (cdr (assoc type wave-client-task-type))
        :p data))

(defun wave-client-get-browser-channel ()
  "Get the response from the latest browser-channel"
  (let* ((sid (wave-client-get-channel-sid))
         (url-struct (url-generic-parse-url wave-client-server))
         (path (concat (wave-client-path-prefix)
                       "/wfe/channel?gsessionid="
                                   wave-client-gsession
                                   "&VER=8&SID=" sid "&AID="
                                   (int-to-string wave-client-last-array-id)
                                   "&RID=rpc&CI=0&TYPE=xmlhttp&t=")))
    (setq wave-client-bc-get-conn
          (open-tls-stream "Wave Client" wave-client-get-buf-name
                           (url-host url-struct)
                           (url-port url-struct)))
    (process-send-string wave-client-bc-get-conn
                         (format "GET %s HTTP/1.1\r\n" path))
    (process-send-string wave-client-bc-get-conn
                         (format "Host: %s\r\nCookie: WAVE=%s\r\n\r\n"
                                 (url-host url-struct)
                                 wave-client-auth-cookie))
    (lexical-let ((cur-gsession wave-client-gsession))
      (set-process-filter wave-client-bc-get-conn
                          (lambda (process output)
                            (with-temp-buffer
                              (insert output)
                              (goto-char (point-min))
                              (wave-client-accept nil cur-gsession)))))))

(defun wave-client-browser-channel-send-echo (test-id)
  "Test whether we can post to a browser channel once and get a
  reply.  TEST-ID is anything unique to echo."
  (let ((req (wave-client-wrap-browser-channel-req `(:1 ,test-id) 'echo)))
      (wave-client-post-to-browser-channel (list req))))

(defun wave-client-browser-channel-echo-test ()
  (wave-client-reset-browser-channel)
  (wave-client-browser-channel-send-echo "ABC")
  (wave-client-get-browser-channel)
  (wave-client-browser-channel-send-echo "DEF")
  wave-client-queue)

(defun wave-bc-add-participant-to-proto (op)
  (wave-op-proto
   :type-id (wave-bc-message-type 'add-participant)
   :add-participant
   (wave-add-participant-proto :user-id
                               (wave-add-participant-address op))))

(defun wave-bc-remove-participant-to-proto (op)
  (wave-op-proto
   :type-id (wave-bc-message-type 'remove-participant)
   :remove-participant
   (wave-remove-participant-proto :user-id
                                  (wave-remove-participant-address op))))

(defun wave-bc-blip-submit-to-proto (op)
  (wave-op-proto
   :type-id (wave-bc-message-type 'blip-submit)
   :blip-submit
   (wave-blip-submit-proto :blip-id (wave-blip-submit-blip-id op))))

(defun wave-bc-doc-op-to-proto (op)
  (wave-op-proto
   :type-id (wave-bc-message-type 'mutate-document)
   :mutate-document
   (wave-mutate-document-proto
    :document-id (wave-doc-op-doc-id op)
    :operation
    (wave-document-operation-proto
     :component
     (apply 'vector
            (mapcar
             (lambda (c)
               (etypecase c
                 (wave-text
                  (wave-component-proto
                   :text (wave-text-text c)))
                 (wave-element-start
                  (wave-component-proto
                   :element-start
                   (wave-element-start-proto
                    :type (wave-element-start-type c)
                    :attributes
                    (apply 'vector
                           (mapcar
                            (lambda (p)
                              (wave-key-value-pair-proto
                               :key (wave-key-value-pair-key p)
                               :value (wave-key-value-pair-value p)))
                            (wave-element-start-attributes c))))))
                 (wave-element-end
                  (wave-component-proto
                   :element-end t))
                 (wave-retain-item-count
                  (wave-component-proto
                   :retain-item-count (wave-retain-item-count-num c)))))
             (wave-doc-op-components op)))))))

(defun wave-bc-delta-to-proto (delta wavelet-name)
  "Convert DELTA of WAVELET-NAME to a protocol buffer."
  (wave-submit-delta-request-proto
   :wave-id (car wavelet-name)
   :wavelet-id (cdr wavelet-name)
   :delta (wave-delta-proto
           :version (vector (car (wave-delta-pre-version delta)) 0)
           :user-id (wave-delta-author delta)
           :pre-signature (cdr (wave-delta-pre-version delta))
           :op-list (apply 'vector
                           (mapcar
                            (lambda (op)
                              (etypecase op
                                (wave-add-participant
                                 (wave-bc-add-participant-to-proto op))
                                (wave-remove-participant
                                 (wave-bc-remove-participant-to-proto op))
                                (wave-blip-submit
                                 (wave-bc-blip-submit-to-proto op))
                                (wave-doc-op
                                 (wave-bc-doc-op-to-proto op))))
                            (wave-delta-ops delta))))))

;; Functions for the Wave mode to use:

(defun wave-bc-send-delta (delta wavelet-name)
  (wave-client-post-to-browser-channel
   (list (wave-client-wrap-browser-channel-req (wave-bc-delta-to-proto
                                                delta wavelet-name)
                                               'view-submit))))

(defun wave-bc-get-inbox ()
  (wave-client-extract-waves (wave-client-get-waves)))

(defun wave-bc-get-wave (wave-id)
  (wave-client-extract-wave (wave-client-get-wave-raw wave-id)))

(provide 'wave-client-browser-channel)

;;; wave-client.el ends here
