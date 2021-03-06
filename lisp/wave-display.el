;;; wave-display.el --- Defines wave-display-mode.
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

;; This file contains code related to displaying and manipulating a
;; single wave.
;;
;; This file right now is a work in progress, and can so far only
;; display a wave, read-only, without updates.

(eval-when-compile
  (require 'cl))
(eval-and-compile
  (require 'wave-client)
  (require 'wave-edit)
  (require 'wave-update))

;;; Code:

(defface wave-wavelet-header
  '((((class color)) (:foreground "Dark Green"))
    (t (:italic t) (:bold t)))
  "Face used for the header of a wave."
  :group 'wave-display)

(defface wave-wavelet-participants
  '((((class color)) (:foreground "Dark Green"))
    (t (:italic t) (:bold t)))
  "Face used for wavelet participants."
  :group 'wave-display)

(defface wave-title
  '((t (:bold t)))
  "Face used for text that has the title annotation."
  :group 'wave-display)

(defface wave-blip-authors
  '((((class color)) (:foreground "Dark Green"))
    (t (:italic t)))
  "Face used for blip authors."
  :group 'wave-display)

(defface wave-blip-unread
  '((((class color)) (:foreground "DarkOrchid2"))
    (t (:bold t)))
  "Face used for unread markers."
  :group 'wave-display)

(defvar wave-display-buffer-format "*Wave: %s*"
  "The format argument, which must have one %s, for a Wave's
  buffer name.")

(defface wave-insert-marker
  '((((class color)) (:background "Red"))
    (t (:bold t)))
  "Face used for the insert marker."
  :group 'wave-display)

(defconst wave-blip-buffer-name
  "*Wave Message*")

(defvar wave-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'wave-display-next-blip)
    (define-key map "p" 'wave-display-previous-blip)
    (define-key map "R" 'wave-display-toggle-debugging-info)
    (define-key map "i" 'wave-display-insert-blip)
    (define-key map "g" 'wave-display-refresh)
    (define-key map "q" 'wave-kill-wave-display)
    (define-key map "a" 'wave-display-add-participant)
    map)
  "Keybindings for wave mode.")

(defvar wave-display-wave-id nil
  "The wave id (a string) being displayed in this buffer.")
(make-variable-buffer-local 'wave-display-wave-id)

(defvar wave-display-ewoc nil
  "The ewoc used for wave display.  One entry per display node.")
(make-variable-buffer-local 'wave-display-wavelet-ewoc)

(defvar wave-display-show-debugging-info nil
  "Whether to display raw data.")
(make-variable-buffer-local 'wave-display-debugging-info)

(defvar wave-display-blips (make-hash-table :test 'equal)
  "Blip id to blip hashtable")
(make-variable-buffer-local 'wave-display-blips)

(defvar wave-display-wave-read-state
  nil
  "An alist that maps wavelet id strings to wavelet read states.")
(make-variable-buffer-local 'wave-display-wave-read-state)

;; TODO(ohler): The default values for these should be nil, and we
;; should create a new hash table for every buffer.  Aren't we mixing
;; data from different waves otherwise?
(defvar wave-display-conversations
  (make-hash-table :test 'equal)
  "Hash table for conversations")
(make-variable-buffer-local 'wave-display-conversations)

;; With federation, it's hard to predict the prefix of things, so we
;; need to do suffix-only testing for our hash table.
(define-hash-table-test 'wave-suffix-equal
  (lambda (a b)
    (equal (wave-id-suffix a) (wave-id-suffix b)))
  (lambda (item) (sxhash (wave-id-suffix item))))

(defvar wave-display-wavelets
  (make-hash-table :test 'wave-suffix-equal)
  "Hash table of wavelet ids to objects of type `wave-display-header'.")
(make-variable-buffer-local 'wave-display-wavelets)

(defvar wave-display-attachments nil
  "Hash table of pairs of wavelet id & attachment id to objects of type `wave-attachment-info'.")

(defvar wave-display-saved-window-configuration
  nil
  "The window configuration before an insert or edit takes
  place.")
(make-variable-buffer-local 'wave-display-saved-window-configuration)

(defstruct (wave-wavelet-read-state (:constructor wave-make-wavelet-read-state))
  (blips (make-hash-table))
  (raw nil)
  (all nil)
  (participants nil))

(defun wave-kill-wave-display ()
  "Kill the wave display buffer, bringing the wave list back."
  (interactive)
  (kill-buffer)
  (let ((list-buffer (get-buffer wave-list-buffer-name)))
    (when list-buffer
      (switch-to-buffer list-buffer))))

(defun wave-display-toggle-debugging-info ()
  (interactive)
  (setq wave-display-show-debugging-info (not wave-display-show-debugging-info))
  (ewoc-refresh wave-display-ewoc)
  (goto-char (point-min)))

(defun wave-display-get-boolean (attributes attribute-name)
  (let ((value (plist-get attribute-name attributes)))
    (cond
     ((null value) nil)
     ((equal value "true") t)
     ((equal value "false") nil)
     (t (error "Unexpected value for attribute `%s': %S"
               attribute-name value)))))

(defun wave-display-format-user (user)
  "Given a USER such as 'ahyatt@googlewave.com', return a
  shortened form without redundant domain info, such as
  'ahyatt'."
  (replace-regexp-in-string (concat "@" (wave-client-domain))
                            ""
                            user))

(defun wave-display-highlight-blip ()
  "Apply `hl-line' face to the current blip."
  (remove-overlays)
  (let ((element (ewoc-locate wave-display-ewoc)))
    (when (and element
               (not (wave-display-node-should-be-skipped-p (ewoc-data element)))
               ;; If in header, don't highlight first element.
               (>= (point) (ewoc-location element)))
      (overlay-put (make-overlay
                    (ewoc-location element)
                    (let ((next (ewoc-next wave-display-ewoc element)))
                      (if next
                          (ewoc-location next)
                        ;; Apparently, ewoc doesn't know where the
                        ;; last element ends.  point-max is OK as long
                        ;; as we have no footer.
                        (point-max))))
                   'face 'hl-line))))

(defun wave-display-node-should-be-skipped-p (node)
  (or (typep node 'wave-display-header)
      (and (not wave-display-show-debugging-info)
           (wave-display-node-is-debugging-info node))))

(defun wave-display-move (ewoc-fn element)
  (loop
   (setq element (funcall ewoc-fn wave-display-ewoc element))
   (when (null element)
     (wave-debug "No further nodes")
     (return))
   (unless (wave-display-node-should-be-skipped-p (ewoc-data element))
     (goto-char (ewoc-location element))
     (wave-display-maybe-mark-unread element)
     (return))))

(defun wave-display-user-data-wavelet ()
  "Return the user data wavelet."
  (concat "user+" (wave-client-email-address)))

(defun wave-display-maybe-mark-unread (blip-node)
  "If a blip is unread, mark it read."
  (let ((blip (ewoc-data blip-node)))
    (when (wave-display-blip-unreadp blip)
      (let* ((user-wavelet-suffix (wave-display-user-data-wavelet))
             (conv-wavelet-suffix "conv+root")
             (user-header (gethash user-wavelet-suffix wave-display-wavelets))
             (conv-header (gethash conv-wavelet-suffix wave-display-wavelets)))
        (unless user-header (error "Could not find user header!"))
        (unless conv-header (error "Could not find conv header!"))
        (wave-update-mark-blip-read
         (wave-display-blip-blip-id blip)
         user-header
         conv-header
         (wave-wavelet-read-state-raw
          (cdr (assoc (cdr (wave-display-header-wavelet-name conv-header))
                      wave-display-wave-read-state)))))
      (wave-display-refresh))))

(defun wave-display-wavelet-name-at-point ()
  (let ((node (ewoc-data (ewoc-locate wave-display-ewoc))))
    (etypecase node
      (wave-display-blip (wave-display-blip-wavelet-name node))
      (wave-display-header (wave-display-header-wavelet-name node)))))

(defun wave-display-add-participant (wavelet-name participant-id)
  (interactive
   (let* ((wavelet-name (wave-display-wavelet-name-at-point))
          (address (read-string
                    (format "Add participant to wavelet %s: " (cdr wavelet-name)))))
     (list wavelet-name address)))
  (wave-update-add-participant
   wavelet-name
   (wave-display-header-version
    (gethash (cdr wavelet-name) wave-display-wavelets))
   participant-id)
  ;; perhaps need some delay before doing this?
  (wave-display-refresh))

(defun wave-display-next-blip ()
  "Moves to the next blip from the cursor."
  (interactive)
  (wave-display-move 'ewoc-next (ewoc-locate wave-display-ewoc)))

(defun wave-display-previous-blip ()
  "Moves to the previous blip from the cursor."
  (interactive)
  (wave-display-move 'ewoc-prev (ewoc-locate wave-display-ewoc)))

(defun wave-display-users (users face)
  "Display a list of USERS, using FACE styling."
  (let ((begin (point)))
    (insert (mapconcat 'wave-display-format-user
                       users ", "))
    (add-text-properties begin (point)
                         (list 'face face))))

;; Each ewoc entry is a display node, a contiguous section of the
;; buffer that can be navigated to.  For now, each blip corresponds to
;; a display node, but this may change depending on how we decide to
;; display inline replies.  There is an additional display node for
;; the wavelet participant list.
(defstruct (wave-display-node (:constructor nil))
  indentation-level
  (is-debugging-info nil)
  )

(defstruct (wave-display-header (:constructor wave-display-make-header)
                                (:include wave-display-node))
  participants
  wavelet-name
  ;; A distinction version (pair of version and distinction).
  version
  )

(defstruct (wave-display-blip (:constructor wave-display-make-blip)
                              (:include wave-display-node))
  wavelet-name
  blip-id
  raw-blip
  collapsedp
  unreadp
  )

;; These are only displayed for debugging.
(defstruct (wave-display-raw-doc (:constructor wave-display-make-raw-doc)
                                 (:include wave-display-node))
  raw-blip
  )

(defun wave-display-header (node)
  (let ((level (wave-display-node-indentation-level node))
        (wavelet-name (wave-display-header-wavelet-name node)))
    (indent-to (* 2 level))
    (let ((begin (point)))
      (insert (format "%s/%s" (car wavelet-name) (cdr wavelet-name)))
      (add-text-properties begin (point)
                           (list 'face 'wave-wavelet-header)))
    (insert "\n")
    (indent-to (* 2 level))
    (insert "Participants: ")
    (wave-display-users (wave-display-header-participants node)
                        'wave-wavelet-participants)
    (insert "\n\n")))

(defun wave-display-rgb-to-color (rgb-string)
  "Convert RGB-STRING to an emacs color."
  (if (string-match "rgb(\\(\\w+\\), \\(\\w+\\), \\(\\w+\\))" rgb-string)
   (let ((red (* (string-to-number (match-string 1 rgb-string))
                 255))
         (green (* (string-to-number (match-string 2 rgb-string)) 255))
         (blue (* (string-to-number (match-string 3 rgb-string)) 255)))
     (car (tty-color-approximate (list red green blue))))
   "white"))

(defun wave-display-handle-boundary (begin end key val)
  "Handle a particular boundary from points BEGIN to END."
  (cond
   ((equal key "lang")) ;;nothing to do
   ((equal key "conv/title")
    (add-text-properties begin end
                         '(face wave-title)))
   ((or (member key '("style/fontWeight" "style/textDecoration"
                      "style/fontStyle")))
    (add-text-properties begin end
                         (list 'face (list (intern val)))))
   ((equal key "style/color")
    (add-text-properties begin end
                         `(face (:foreground
                                 ,(wave-display-rgb-to-color val)))))
   ((or (member key '("link/auto"
                      "link/manual"
                      "link/wave"
                      "spell"
                      "style/fontFamily"))
        (string-match "^user/d/" key)
        (string-match "^user/e/" key))
    ;; TODO: Implement these
    )
   (t (wave-debug
       "Don't know how to handle end-boundary of type %s"
       key))))

(defun wave-display-blip (node)
  (let* ((blip (wave-display-blip-raw-blip node))
         (blip-id (wave-doc-doc-id blip))
         (level (wave-display-node-indentation-level node))
         (wavelet-name (wave-display-blip-wavelet-name node)))
    (indent-to (* 2 level))
    (when (wave-display-blip-unreadp node)
      ;; This indicator is somewhat lame but demonstrates that we can
      ;; do it.
      (let ((begin (point)))
        (insert "(unread) ")
        (add-text-properties begin (point)
                             '(face wave-blip-unread))))
    (wave-display-users (wave-doc-contributors blip)
                        'wave-blip-authors)
    (let ((op-stack '())
          (current-paragraph-start (point))
          (boundaries '()))
      (dolist (op (wave-doc-content blip))
        (cond ((stringp op) (insert op))
              ((eq op 'end)
               (let ((closed-op (pop op-stack)))
                 (case (caar closed-op)
                   ((line body)
                    (let ((left-margin (* 2 level)))
                      (fill-region-as-paragraph current-paragraph-start (point)
                                                'left))
                    (insert "\n")
                    (setq current-paragraph-start (point)))
                   (w:image
                    ;; Nothing to do
                    )
                   (w:caption
                    ;; TODO(ahyatt) Format with caption face
                    )
                   (t (wave-debug
                       "Don't know how to close out %s"
                       (car closed-op))))))
              ((and (listp op) (eq '@boundary (car op)))
               (dolist (boundary (cdr op))
                 (ecase (car boundary)
                   (change
                    (dolist (changes (cdr boundary))
                      (dolist (change changes)
                        (add-to-list
                         'boundaries
                         (cons (plist-get change :key)
                               (cons change (point)))))))
                   (end
                    (loop for key across (cadr boundary)
                          for bounds = (cdr (assoc key boundaries))
                          for begin = (cdr bounds)
                          for val = (plist-get (car bounds) :newvalue)
                          do
                          (wave-display-handle-boundary begin (point)
                                                        key val))))))
              ((listp op)
               (push (cons op (point)) op-stack)
               (case (first op)
                 (w:image
                  (cond ((not (display-images-p))
                         (insert " [IMAGE (not supported on your emacs)]\n"))
                        ((eql wave-client-connection-method 'websocket)
                         (insert " [IMAGE (not supported with websocket)]\n"))
                        (t
                         (let* ((attachment-id (plist-get (second op)
                                                          'attachment))
                                (thumbnail (wave-display-get-thumbnail
                                            (cdr wavelet-name)
                                            attachment-id)))
                           (if (null thumbnail)
                               (insert " [IMAGE (failed to get thumbnail)]\n")
                             (insert "\n"
                                     (propertize
                                      "[IMAGE]"
                                      'display
                                      `(image
                                        ;; TODO(ohler): Make
                                        ;; wave-display-get-thumbnail return the
                                        ;; type as well as the data.  I've seen
                                        ;; thumbnails that were jpgs, and perhaps
                                        ;; other types occur as well.
                                        :type png
                                        :margin 2
                                        :data ,thumbnail))
                                     "\n"))))))))
              (t (message "Don't know how to deal with op: %s" op)))))
    (insert "\n")))

(defun wave-display-get-thumbnail (wavelet-id attachment-id)
  (let ((attachment-info (gethash (cons wavelet-id attachment-id) wave-display-attachments)))
    (if (null attachment-info)
        (progn (warn "Attachment %S not found in wavelet %S" attachment-id wavelet-id)
               nil)
      (let ((url-suffix (plist-get (wave-attachment-info-plist attachment-info) 'thumbnail_url)))
        (let ((url (let
                       ;; TODO(ohler): solve this without rebinding this variable
                       ((wave-client-server wave-client-attachment-server))
                     (wave-client-get-url url-suffix))))
          (let ((data (wave-client-curl url nil (wave-client-auth-cookies))))
            (with-current-buffer data
              (prog1 (buffer-string)
                (kill-buffer)))))))))

(defun wave-display-raw-doc (node)
  (let* ((raw-blip (wave-display-raw-doc-raw-blip node))
         (level (wave-display-node-indentation-level node)))
    (indent-to (* 2 level))
    (pprint raw-blip (current-buffer))
    (insert "\n\n")))

(defun wave-display-node-printer (node)
  (unless (and (wave-display-node-is-debugging-info node)
               (not wave-display-show-debugging-info))
    (etypecase node
      (wave-display-header (wave-display-header node))
      (wave-display-blip (wave-display-blip node))
      (wave-display-raw-doc (wave-display-raw-doc node)))))

(defun wave-display-compute-blip-unread-p (wavelet-name raw-blip)
  (let* ((blip-id (wave-doc-doc-id raw-blip))
         (blip-modified-version (wave-doc-last-modified-version raw-blip))
         (read-state (cdr (assoc (cdr wavelet-name)
                                 wave-display-wave-read-state)))
         (blip-read-version
          (gethash blip-id (wave-wavelet-read-state-blips read-state)))
         (all-read-version (wave-wavelet-read-state-all read-state)))
    ;; If there is no user-data wavelet, then consider everything to be read.
    (and (gethash (wave-display-user-data-wavelet) wave-display-wavelets)
         (or (null blip-read-version)
             (< blip-read-version blip-modified-version))
         (or (null all-read-version)
             (< all-read-version blip-modified-version)))))

(defun* wave-display-add-conversation (ewoc wavelet)
  "Parse conversation in WAVELET and add the resulting display nodes to EWOC."
  (let* ((blip-table (wave-wavelet-docs wavelet))
         (manifest (gethash 'conversation blip-table))
         (wavelet-name (wave-wavelet-wavelet-name wavelet)))
    (if (null manifest)
        (ewoc-set-hf ewoc "No manifest?!\n" "")
      (ewoc-set-hf ewoc "" "")
      (ewoc-enter-last ewoc
                       (wave-display-make-header
                        :indentation-level 0
                        :version (wave-wavelet-version wavelet)
                        :participants (wave-wavelet-participants wavelet)
                        :wavelet-name wavelet-name))
      (let ((content (wave-doc-content manifest)))
        (assert (not (null content)) nil "Empty manifest: %S" manifest)
        (assert (eql (caar content) 'conversation) nil
                "Manifest does not begin with conversation element: %S"
                manifest)
        (assert (eql (car (last content)) 'end))
        (let ((thread-stack '())
              (op-stack '())
              (conversation-found nil))
          (dolist (op content)
            (cond
             ((eq op 'end)
              (assert op-stack)
              (when (eq (pop op-stack) 'thread)
                (assert thread-stack)
                (pop thread-stack)))
             ((stringp op)
              (error "Found text in manifest: %S" op))
             (t
              (assert (listp op))
              (ecase (car op)
                (conversation
                 (cond
                  (conversation-found
                   (warn
                    "Found two conversations in manifest, displaying only one")
                   (return-from wave-display-add-conversation))
                  (op-stack
                   (error "Conversation element not at top-level"))
                  (t
                   (setq conversation-found t)
                   (push `(0 nil nil) thread-stack)
                   (push 'conversation op-stack)
                   (puthash (car wavelet-name)
                            content
                            wave-display-conversations))))
                (@boundary
                 (warn "Found annotations in manifest, ignoring"))
                (thread
                 (push `(,(1+ (caar thread-stack))
                         ,(plist-get (second op) 'id)
                         ,(wave-display-get-boolean 'inline (second op)))
                       thread-stack)
                 (push 'thread op-stack))
                (blip
                 (let ((blip-id (intern (plist-get (second op) 'id)))
                       (deleted (wave-display-get-boolean (second op)
                                                          'deleted)))
                   (assert blip-id nil "No blip id in %S" op)
                   (unless deleted
                     (let ((raw-blip (gethash blip-id blip-table)))
                       (if (null raw-blip)
                           (warn "Blip in manifest but no data: %S"
                                 blip-id)
                         (destructuring-bind (level thread-id inlinep)
                             (car thread-stack)
                           (ewoc-enter-last
                            ewoc
                            ;; TODO: implement inline
                            (wave-display-make-blip
                             :wavelet-name wavelet-name
                             :raw-blip raw-blip
                             :blip-id blip-id
                             :indentation-level level
                             :collapsedp nil
                             :unreadp (wave-display-compute-blip-unread-p
                                       wavelet-name raw-blip)))))))
                   (push 'blip op-stack)))
                (peer
                 ;; ignore
                 (push 'peer op-stack))))))
          (unless conversation-found
            (warn "No conversation element found: %S" manifest)))))))

(defun wave-display-add-raw-wavelet (ewoc wavelet)
  (let ((blip-table (wave-wavelet-docs wavelet))
        (header (wave-display-make-header
                 :indentation-level 0
                 :wavelet-name (wave-wavelet-wavelet-name wavelet)
                 :participants (wave-wavelet-participants wavelet)
                 :version (wave-wavelet-version wavelet)
                 :is-debugging-info t)))
    (puthash (cdr (wave-display-header-wavelet-name header))
             header wave-display-wavelets)
    (ewoc-enter-last ewoc header)
    (maphash
     (lambda (key value)
       (ewoc-enter-last ewoc
                        (wave-display-make-raw-doc
                         :indentation-level 0
                         :raw-blip value
                         :is-debugging-info t)))
     blip-table)))

(defun wave-display-parse-version-from-string (string)
  (let ((number (string-to-number string)))
    (check-type number (integer 0 *))
    number))

(defun wave-display-process-m/read (content)
  "Populates `wave-display-wave-read-state' from CONTENT."
  (let ((wavelet-read-state nil)
        (blip nil)
        (participants nil)
        (all nil)
        (op-stack '()))
    (wave-debug "m/read raw: %s" content)
    (dolist (op content)
      (cond
       ((eq op 'end)
        (assert op-stack)
        (ecase (pop op-stack)
          (wavelet (assert wavelet-read-state)
                   (setq wavelet-read-state nil))
          (blip (assert blip)
                (setq blip nil))
          (participants (assert participants)
                        (setq participants nil))
          (all (assert all)
               (setq all nil))))
       ((stringp op)
        (error "Found text in m/read: %S" op))
       (t
        (assert (listp op) t)
        (ecase (car op)
          (wavelet
           (assert (null op-stack))
           (assert (null wavelet-read-state))
           (let ((i (plist-get (second op) 'i)))
             (setq wavelet-read-state
                   (or (cdr (assoc i wave-display-wave-read-state))
                       (let ((new-state (wave-make-wavelet-read-state)))
                         (push (cons i new-state) wave-display-wave-read-state)
                         new-state)))
             (setf (wave-wavelet-read-state-raw wavelet-read-state)
                   content))
           (push 'wavelet op-stack))
          (blip
           (assert wavelet-read-state)
           (assert (null blip))
           (assert (null participants))
           (assert (null all))
           (setq blip (intern (plist-get (second op) 'i)))
           (let ((v (wave-display-parse-version-from-string
                     (plist-get (second op) 'v))))
             (puthash blip v
                      (wave-wavelet-read-state-blips wavelet-read-state)))
           (push 'blip op-stack))
          (participants
           (assert wavelet-read-state)
           (assert (null blip))
           (assert (null participants))
           (assert (null all))
           (setq participants t)
           (let ((v (wave-display-parse-version-from-string
                     (plist-get (second op) 'v))))
             (setf (wave-wavelet-read-state-participants wavelet-read-state)
                   v))
           (push 'participants op-stack))
          (all
           (assert wavelet-read-state)
           (assert (null blip))
           (assert (null participants))
           (assert (null all))
           (setq all t)
           (let ((v (wave-display-parse-version-from-string
                     (plist-get (second op) 'v))))
             (setf (wave-wavelet-read-state-all wavelet-read-state)
                   v))
           (push 'all op-stack))
          (@boundary
           (warn "Found annotations in m/read, ignoring"))))))))

(defun wave-display-process-user-data (wavelet)
  (let* ((blip-table (wave-wavelet-docs wavelet))
         (m/read (gethash 'm/read blip-table)))
    (unless (null m/read)
      (wave-display-process-m/read (wave-doc-content m/read)))))

(defun wave-display (wave-label wave-id)
  "Display in a new or re-used buffer the wave WAVE-ID in wave-display-mode.
Returns the new buffer."
  (let ((buf-name (format wave-display-buffer-format wave-label)))
    (set-buffer (get-buffer-create buf-name))
    (let ((inhibit-read-only t))
      (buffer-disable-undo)
      (erase-buffer)
      (wave-display-mode)
      (setq wave-display-wave-id wave-id))
    (wave-display-refresh)
    (current-buffer)))

(defun wave-display-insert-blip ()
  "Insert a blip in the current wavelet."
  (interactive)
  (setq wave-display-saved-window-configuration
        (cons (current-window-configuration) (point-marker)))
  (delete-other-windows)
  (let ((compose-buf (get-buffer-create wave-blip-buffer-name))
        (current-buf (current-buffer))
        (blip (ewoc-data (ewoc-locate wave-display-ewoc))))
    (split-window-vertically -10)
    (switch-to-buffer compose-buf)
    (setq wave-edit-parent-buf current-buf)
    (setq wave-edit-previous-blip
          (wave-display-blip-blip-id blip))
    (setq wave-edit-wavelet-id
          (car (wave-display-blip-wavelet-name blip)))
    (wave-edit-mode)))

(defstruct (wave-attachment-info (:constructor wave-make-attachment-info))
  (id (assert nil))
  (plist (assert nil)))

(defun wave-display-collect-attachment-info (wavelet)
  (let ((attachments (make-hash-table :test 'equal)))
    (maphash (lambda (doc-id doc)
               (when (string-match "\\`attach\\+\\(.*\\)\\'" (symbol-name doc-id))
                 (let ((attachment-id (match-string 1 (symbol-name doc-id)))
                       (accu (list))
                       (op-stack '()))
                   (dolist (op (wave-doc-content doc))
                     (cond
                      ((eq op 'end)
                       (assert op-stack)
                       (pop op-stack))
                      ((stringp op)
                       (error "Found text in attachment data doc %s: %S" doc-id op))
                      (t
                       (assert (listp op) t)
                       (ecase (car op)
                         (node
                          (assert (null op-stack))
                          (let ((key (plist-get (second op) 'key))
                                (value (plist-get (second op) 'value)))
                            (setq accu (list* (intern key) value accu)))
                          (push (car op) op-stack))
                         (@boundary
                          (warn "Found annotations in attachment data doc %s, ignoring"
                                doc-id))))))
                   (puthash (cons (cdr (wave-wavelet-wavelet-name wavelet)) attachment-id)
                            (wave-make-attachment-info :id attachment-id
                                                       :plist accu)
                            attachments))))
             (wave-wavelet-docs wavelet))
    attachments))

(defun wave-display-refresh-buffer (wavelets)
  "Do the actual work of refreshing, given the data."
  (let ((inhibit-read-only t)
        (prev-point (point)))
    (erase-buffer)
    (let ((ewoc (ewoc-create 'wave-display-node-printer nil nil t)))
      (setq wave-display-ewoc ewoc)
      (let ((conv-wavelets
             (remove-if-not
              (lambda (wavelet)
                (string-match "!conv\\+"
                              (cdr (wave-wavelet-wavelet-name wavelet))))
              wavelets))
            (user-wavelets
             (remove-if-not
              (lambda (wavelet)
                (string-match "!user\\+"
                              (cdr (wave-wavelet-wavelet-name wavelet))))
              wavelets)))
        ;; First, create read state table.
        (setq wave-display-wave-read-state
              (mapcar (lambda (wavelet)
                        (cons (cdr (wave-wavelet-wavelet-name wavelet))
                              (wave-make-wavelet-read-state)))
                      conv-wavelets))
        (assert (<= (length user-wavelets) 1))
        ;; Then, populate read state.
        (dolist (wavelet user-wavelets)
          (wave-display-process-user-data wavelet))
        ;; Then, collect attachment metadata.
        (dolist (wavelet conv-wavelets)
          (set (make-local-variable 'wave-display-attachments)
               (wave-display-collect-attachment-info wavelet)))
        ;; Then, render conversations.
        (dolist (wavelet conv-wavelets)
          (wave-display-add-conversation ewoc wavelet))
        ;; Finally, add all raw wavelets, for debugging.
        (dolist (wavelet wavelets)
          (wave-display-add-raw-wavelet ewoc wavelet))
        (goto-char prev-point)))))

(defun wave-display-refresh ()
  (interactive)
  (assert (eql major-mode 'wave-display-mode))
  (lexical-let ((buf (current-buffer)))
    (wave-display-refresh-buffer
     (wave-get-wave wave-display-wave-id
                    (lambda (wavelets)
                      (with-current-buffer buf
                        (wave-display-refresh-buffer wavelets)))))))

(define-derived-mode wave-display-mode nil "Wave"
  "Mode for displaying a wave.

\\{wave-mode-map}"
  :group 'wave-display
  (use-local-map wave-mode-map)
  (buffer-disable-undo)
  (setq buffer-read-only t
        show-trailing-whitespace nil
        truncate-lines t
        ;;selective-display t
        ;;selective-display-ellipses t
        )
  (add-hook 'post-command-hook 'wave-display-highlight-blip t t)
  (setq left-margin-width 1))

(provide 'wave-display)

;;; wave-display.el ends here
