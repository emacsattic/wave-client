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

(require 'wave-client)

;;; Code:

(defface wave-blip-authors
  '((((class color)) (:foreground "Green"))
    (t (:italic t)))
  "Face used for blip authors."
  :group 'wave-display)

(defface wave-wavelet-participants
  '((((class color)) (:foreground "Green"))
    (t (:italic t) (:bold t)))
  "Face used for blip authors."
  :group 'wave-display)

(defface wave-title
  '((t (:bold t)))
  "Face used for the title of a wave."
  :group 'wave-display)

(defvar wave-display-buffer-format "*Wave %s*"
  "The format argument, which must have one %s, for a Wave's
  buffer name.")

(defvar wave-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'wave-display-next-blip)
    (define-key map "p" 'wave-display-previous-blip)
    map)
  "Keybindings for wave mode.")

(defvar wave-display-blips '()
  "Buffer-local blip to location alist.")

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
  (when wave-display-blips
    (remove-overlays)
    (let ((blips wave-display-blips))
      (while (and (cdr blips) (>= (point) (cadadr blips)))
        (setq blips (cdr blips)))
      ;; If we're at the end, just don't do anything.
      (overlay-put (make-overlay (cadar blips) (cddar blips))
                   'face 'hl-line))))

(defun wave-display-next-blip ()
  "Moves to the next blip from the cursor."
  (interactive)
  (let ((blips wave-display-blips))
    (while (and blips (>= (point) (cadar blips)))
      (setq blips (cdr blips)))
    ;; If we're at the end, just don't do anything.
    (when blips
        (goto-char (cadar blips)))))

(defun wave-display-previous-blip ()
  "Moves to the next blip from the cursor."
  (interactive)
  (let ((blips (reverse wave-display-blips)))
    (while (and blips (<= (point) (cadar blips)))
      (setq blips (cdr blips)))
    ;; If we're at the end, just don't do anything.
    (when blips
        (goto-char (cadar blips)))))

(defun wave-display-users (users face)
  "Display a list of USERS, using FACE styling."
  (let ((begin (point)))
    (insert (mapconcat 'wave-display-format-user
                               users ", "))
    (set-text-properties begin (point)
                         (list 'face face))))

(defun wave-display-blip (blip-id blips level)
  "Display blip with id BLIP-ID, using data from hashtable BLIPS"
  (let ((blip (gethash (intern blip-id) blips))
        (op-stack '())
        (boundaries '())
        (start (point)))
    (indent-to (* 2 level))
    (wave-display-users (plist-get blip :authors)
                        'wave-blip-authors)
    (dolist (op (plist-get blip :ops))
      (cond ((stringp op) (insert op))
            ((eq op 'end)
             (let ((closed-op (car op-stack)))
               (cond ((eq (car closed-op) 'line)
                      (insert "\n"))
                     ((eq (car closed-op) 'body)
                      (insert "\n")
                      (indent-region (cdr closed-op)
                                     (point)
                                     (* 2 level)))
                     ((eq (car closed-op) 'w:image)
                      ;; TODO(ahyatt): Display inline image, if
                      ;; possible
                      (insert "[IMAGE]\n"))
                     ((eq (car closed-op) 'w:caption)
                      ;; TODO(ahyatt) Format with caption face
                      )
                     (t (message
                         "Don't know how to close out %s"
                         (car closed-op)))))
             (setq op-stack (cdr op-stack)))
            ((symbolp op)
             (setq op-stack (cons (cons op (point)) op-stack)))
            ((and (listp op) (eq 'boundary (car op)))
             (dolist (boundary (cdr op))
               (cond ((eq (car boundary) 'change)
                      (dolist (changes (cdr boundary))
                        (dolist (change changes)
                          (add-to-list
                           'boundaries
                           (cons (plist-get change :key)
                                 (cons change (point)))))))
                      ((eq (car boundary) 'end)
                        (dolist (key (append (cadr boundary) '()))
                          (cond ((equal key "lang"))  ;;nothing to do
                                ((equal key "conv/title")
                                 (add-text-properties
                                  (cddr (assoc key boundaries))
                                  (point)
                                  '(face wave-title)))
                                (t (message
                                    "Don't know how to handle end-boundary of type %s"
                                    key))))))))
            (t (message "Dont know how to deal with op: %s" op))))
    (insert "\n")
    (setq wave-display-blips (cons (cons blip-id (cons start (point))) wave-display-blips))
    (dolist (child-id (plist-get blip :children))
      (wave-display-blip child-id blips (+ level 1)))))

(defun wave-display-wavelet (wavelet)
  "Display a WAVELET."
  (when (plist-get wavelet :root-blip-id)
    (insert "Participants: ")
    (wave-display-users (plist-get wavelet :participants)
                        'wave-wavelet-participants)
    (insert "\n")
    (wave-display-blip (plist-get wavelet :root-blip-id)
                       (plist-get wavelet :blips) 0)
    (setq wave-display-blips (sort wave-display-blips
                                  (lambda (a b)
                                    (< (cadr a)
                                       (cadr b)))))))

(defun wave-display (wave-label wave-data)
  "Display in a new or re-used buffer the wave from WAVE-DATA in
wave-list-mode.  Returns the new buffer."
  (let ((buf-name (format wave-display-buffer-format wave-label)))
    (set-buffer (get-buffer-create buf-name))
    (setq buffer-read-only nil)
    (erase-buffer)
    (make-variable-buffer-local 'wave-display-blips)
    (setq wave-display-blips nil)
    (dolist (wavelet wave-data)
      (wave-display-wavelet wavelet))
    (goto-char (point-min))
    (wave-display-mode)
    (current-buffer)))

(defun wave-display-mode ()
  "Turn on wave-display mode."
  (setq major-mode 'wave-mode)
  (setq mode-name "Wave")
  (use-local-map wave-mode-map)
  (buffer-disable-undo)
  (setq buffer-read-only t
        show-trailing-whitespace nil
        truncate-lines t
        selective-display t
        selective-display-ellipses t)
  (add-hook 'post-command-hook 'wave-display-highlight-blip t t)
  (run-mode-hooks))

(provide 'wave-display)

;;; wave-display.el ends here
