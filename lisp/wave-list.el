;;; wave-list.el --- Defines wave-list-mode, for managing waves.
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
;; This file contains code related to displaying, opening, and
;; manipulating waves.
;;
;; This file right now is a work in progress, and can so far only
;; display a list of waves.  We also need to open them, archive them,
;; mute them, and be able to get different lists (with searches).

(require 'wave-client)
(require 'wave-display)

;;; Code:
(defface wave-list-author
  '((((class color)) (:foreground "Green"))
    (t (:italic t)))
  "Face used for Wave authors."
  :group 'wave-list-faces)

(defface wave-list-unread-summary
  '((t (:bold t)))
  "Face used for unread Wave summaries"
  :group 'wave-list-faces)

(defface wave-list-read-summary
  '(())
  "Face used for read Wave summaries"
  :group 'wave-list-faces)

(defvar wave-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "\r" 'wave-list-open)
    (define-key map "g" 'wave-list-refresh)
    map)
  "Keybindings for wave-list mode.")

(defvar wave-list-waves ()
  "The data rendered in the list of waves.")
(make-variable-buffer-local 'wave-list-waves)

(defconst wave-list-buffer-name "*Wave List*")

(defun wave-list-username-only (full-username)
  "Return the username portion of an email address FULL_USERNAME.

If no @ symbol is found, return FULL-USERNAME unmodified."
  (substring full-username 0
             (or (string-match "@" full-username)
                 (length full-username))))

(defun wave-list-str-truncate (s max-width)
  "Truncate string S to MAX-WIDTH chars."
  (substring s 0 (min (length s) max-width)))

(defun wave-list-open ()
  "Open the wave at point in the list."
  (interactive)
  (switch-to-buffer
   (wave-display (get-text-property (point) 'summary)
                 (get-text-property (point) 'wave-id))))

(defun wave-list-render-wave-list (wave-list)
  "Render WAVE-LIST, a list of wave summary alists to a buffer.

Every wave takes up one line."
  (let ((inhibit-read-only t))
    (setq wave-list-waves wave-list)
    (erase-buffer)
    (dolist (summary-alist wave-list)
    (let ((num-unread (plist-get summary-alist :unread)))
      (insert
       (let* ((unread-length 5)
              (author-length 15)
              (digest-length
               (- (window-width) author-length unread-length 2)))
         (format
          (concat "%-" (int-to-string unread-length)
                  "s %-" (int-to-string digest-length)
                  "s %-" (int-to-string author-length)
                  "s\n")
          (if (and num-unread (> num-unread 0)) num-unread "")
          (wave-list-str-truncate (plist-get summary-alist :digest)
                                  digest-length)
          (wave-list-str-truncate
           (replace-regexp-in-string (concat "@" (wave-client-domain))
                                     ""
                                     (plist-get summary-alist :creator))
           author-length))))
      (let ((bol (save-excursion (beginning-of-line 0) (point)))
            (pre-author (save-excursion (re-search-backward " \\w+")(point)))
            (eol (save-excursion (end-of-line 0) (point))))
        (add-text-properties bol pre-author `(face
                                              ,(if (> num-unread 0)
                                                   'wave-list-unread-summary
                                                 'wave-list-read-summary)))
        (add-text-properties (+ 1 pre-author) eol '(face wave-list-author))
        (add-text-properties bol eol (list 'summary (plist-get summary-alist :digest)))
        (add-text-properties bol eol (list 'wave-id (plist-get summary-alist :id)))))
    (goto-char (point-max))
    (unless (eql (point) (point-min))
    (backward-char)
    (kill-line)
    (goto-char (point-min))))))

(defun wave-list-refresh ()
  (interactive)
  (assert (eql major-mode 'wave-list-mode))
  (let* ((inhibit-read-only t)
         (prev-point (point))
         (inbox
          (lexical-let ((buf (current-buffer)))
            (wave-get-inbox (lambda (inbox)
                              (with-current-buffer buf
                                (wave-list-render-wave-list
                                 (wave-client-ws-translate-inbox inbox))))))))
    (wave-list-render-wave-list inbox)
    (goto-char prev-point)))

(define-derived-mode wave-list-mode nil "Wave List"
  "Mode for displaying a list of waves.

Each line in the mode represents a Wave that can be opened.
The wave client must be connected here.
\\{wave-list-mode-map}"
  :group 'wave-list
  (use-local-map wave-list-mode-map)
  (buffer-disable-undo)
  (setq buffer-read-only t
	show-trailing-whitespace nil
        truncate-lines t
        selective-display t
        selective-display-ellipses t)
  (hl-line-mode))

(defun wave-list ()
  (interactive)
  (set-buffer (get-buffer-create wave-list-buffer-name))
  (wave-list-mode)
  (set-window-buffer (get-buffer-window (current-buffer))
                     wave-list-buffer-name)
  (wave-list-refresh))

(provide 'wave-list)

;;; wave-list.el ends here
