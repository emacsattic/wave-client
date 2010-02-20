;;; wave-display.el --- Defines wave-display-mode.
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

;; This file contains code related to displaying and manipulating a
;; single wave.
;;
;; This file right now is a work in progress, and can so far only
;; display a wave, read-only, without updates.

(eval-when-compile
  (require 'cl))
(eval-and-compile
  (require 'wave-client))

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

(defvar wave-display-buffer-format "*Wave %s*"
  "The format argument, which must have one %s, for a Wave's
  buffer name.")

(defvar wave-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'wave-display-next-blip)
    (define-key map "p" 'wave-display-previous-blip)
    (define-key map "R" 'wave-display-toggle-debugging-info)
    map)
  "Keybindings for wave mode.")

(defvar wave-display-ewoc nil
  "The ewoc used for wave display.  One entry per display node.")
(make-variable-buffer-local 'wave-display-wavelet-ewoc)

(defvar wave-display-show-debugging-info nil
  "Whether to display raw data.")
(make-variable-buffer-local 'wave-display-debugging-info)

(defun wave-display-toggle-debugging-info ()
  (interactive)
  (setq wave-display-show-debugging-info (not wave-display-show-debugging-info))
  (ewoc-refresh wave-display-ewoc))

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
  (replace-regexp-in-string (concat "@"
                                    (or wave-client-domain "googlewave.com"))
                            ""
                            user))

(defun wave-display-highlight-blip ()
  "Apply `hl-line' face to the current blip."
  (remove-overlays)
  (let ((node (ewoc-locate wave-display-ewoc)))
    (when (and node (>= (point) (ewoc-location node)))
      (overlay-put (make-overlay
                    (ewoc-location node)
                    (let ((next (ewoc-next wave-display-ewoc node)))
                      (if next
                          (ewoc-location next)
                        ;; Apparently, ewoc doesn't know where the
                        ;; last element ends.  point-max is OK as long
                        ;; as we have no footer.
                        (point-max))))
                   'face 'hl-line))))

(defun wave-display-move (ewoc-fn node)
  (loop
   (setq node (funcall ewoc-fn wave-display-ewoc node))
   (when (null node)
     (message "No further nodes")
     (return))
   (when (or wave-display-show-debugging-info
             (not (wave-display-node-is-debugging-info (ewoc-data node))))
     (goto-char (ewoc-location node))
     (return))))

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
    (set-text-properties begin (point)
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
  )

(defstruct (wave-display-blip (:constructor wave-display-make-blip)
                              (:include wave-display-node))
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
      (set-text-properties begin (point)
                           (list 'face 'wave-wavelet-header)))
    (insert "\n")
    (indent-to (* 2 level))
    (insert "Participants: ")
    (wave-display-users (wave-display-header-participants node)
                        'wave-wavelet-participants)
    (insert "\n\n")))

(defun wave-display-blip (node)
  (let* ((blip (wave-display-blip-raw-blip node))
         (level (wave-display-node-indentation-level node)))
    (indent-to (* 2 level))
    (wave-display-users (plist-get blip :authors)
                        'wave-blip-authors)
    (let ((op-stack '())
          (current-paragraph-start (point))
          (boundaries '()))
      (dolist (op (plist-get blip :content))
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
                    ;; TODO(ahyatt): Display inline image, if
                    ;; possible
                    (insert "[IMAGE]\n"))
                   (w:caption
                    ;; TODO(ahyatt) Format with caption face
                    )
                   (t (message
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
                    (loop for key across (cadr boundary) do
                          (cond
                           ((equal key "lang")) ;;nothing to do
                           ((equal key "conv/title")
                            (add-text-properties
                             (cddr (assoc key boundaries))
                             (point)
                             '(face wave-title)))
                           (t (message
                               "Don't know how to handle end-boundary of type %s"
                               key))))))))
              ((listp op)
               (push (cons op (point)) op-stack))
              (t (message "Don't know how to deal with op: %s" op)))))
    (insert "\n")))

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

(defun* wave-display-add-conversation (ewoc wavelet)
  "Parse conversation in WAVELET and add the resulting display nodes to EWOC."
  (let* ((blip-table (plist-get wavelet :blips))
         (manifest (gethash 'conversation blip-table)))
    (if (null manifest)
        (ewoc-set-hf ewoc "No manifest?!\n" "")
      (ewoc-set-hf ewoc "[header]\n" "")
      (ewoc-enter-last ewoc
                       (wave-display-make-header
                        :indentation-level 0
                        :participants (plist-get wavelet :participants)
                        :wavelet-name (plist-get wavelet :wavelet-name)))
      (let ((content (plist-get manifest :content)))
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
                   (return-from 'wave-display-add-conversation))
                  (op-stack
                   (error "Conversation element not at top-level"))
                  (t
                   (setq conversation-found t)
                   (push `(0 nil nil) thread-stack)
                   (push 'conversation op-stack))))
                (@boundary
                 (warn "Found annotations in manifest, ignoring"))
                (thread
                 (push `(,(1+ (caar thread-stack))
                         ,(plist-get (second op) 'id)
                         ,(wave-display-get-boolean 'inline (second op)))
                       thread-stack)
                 (push 'thread op-stack))
                (blip
                 (let ((blip-id (plist-get (second op) 'id))
                       (deleted (wave-display-get-boolean (second op)
                                                          'deleted)))
                   (assert blip-id nil "No blip id in %S" op)
                   (unless deleted
                     (let ((raw-blip (gethash (intern blip-id) blip-table)))
                       (if (null raw-blip)
                           (warn "Blip in manifest but no data: %S"
                                 blip-id)
                         (destructuring-bind (level thread-id inlinep)
                             (car thread-stack)
                           (ewoc-enter-last
                            ewoc
                            ;; TODO: implement inline
                            (wave-display-make-blip
                             :raw-blip raw-blip
                             :indentation-level level
                             :collapsedp nil
                             :unreadp nil))))))
                   (push 'blip op-stack)))
                (peer
                 ;; ignore
                 (push 'peer op-stack))))))
          (unless conversation-found
            (warn "No conversation element found: %S" manifest)))))))

(defun wave-display-add-raw-wavelet (ewoc wavelet)
  (let ((blip-table (plist-get wavelet :blips)))
    (ewoc-enter-last ewoc
                     (wave-display-make-header
                      :indentation-level 0
                      :wavelet-name (plist-get wavelet :wavelet-name)
                      :participants (plist-get wavelet :participants)
                      :is-debugging-info t))
    (maphash
     (lambda (key value)
       (ewoc-enter-last ewoc
                        (wave-display-make-raw-doc
                         :indentation-level 0
                         :raw-blip value
                         :is-debugging-info t)))
     blip-table)))

(defun wave-display (wave-label wave-data)
  "Display in a new or re-used buffer the wave from WAVE-DATA in
wave-list-mode.  Returns the new buffer."
  (let ((buf-name (format wave-display-buffer-format wave-label)))
    (set-buffer (get-buffer-create buf-name))
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((ewoc (ewoc-create 'wave-display-node-printer nil nil t)))
      (setq wave-display-ewoc ewoc)
      (dolist (wavelet wave-data)
        (wave-display-add-raw-wavelet ewoc wavelet)
        (when (string-match "!conv\\+" (cdr (plist-get wavelet :wavelet-name)))
          (wave-display-add-conversation ewoc wavelet))))
    (goto-char (point-min))
    (wave-display-mode)
    (current-buffer)))

(define-derived-mode wave-display-mode nil "Wave"
  "Mode for displaying a wave,
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
  (add-hook 'post-command-hook 'wave-display-highlight-blip t t))

(provide 'wave-display)

;;; wave-display.el ends here
