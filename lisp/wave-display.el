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
;; This file contains code related to displaying and manipulating a
;; single wave.
;;
;; This file right now is a work in progress, and can so far only
;; display a wave, read-only, without updates.

(require 'wave-client)

;;; Code:

(defvar wave-display-buffer-format "*Wave %s*"
  "The format argument, which must have one %s, for a Wave's
  buffer name.")

(defvar wave-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'wave-display-next-blip)
    (define-key map "p" 'wave-display-previous-blip)
    map)
  "Keybindings for wave mode.")

(defun wave-display-format-user (user)
  "Given a USER such as 'ahyatt@googlewave.com', return a
  shortened form without redundant domain info, such as
  'ahyatt'."
  (replace-regexp-in-string (concat "@"
                                    (or wave-client-domain "googlewave.com"))
                            ""
                            user))

(defun wave-display-blip (blip-id blips level)
  "Display blip with id BLIP-ID, using data from BLIPS"
  (let ((blip (cdr (assoc blip-id blips)))
        (op-stack '())
        (boundaries '()))
    (indent-to (* 2 level))
    (insert (format "[%s]: "
                    (mapconcat 'wave-display-format-user
                               (cdr (assoc :authors blip))
                               ", ")))
    (dolist (op (cdr (assoc :ops blip)))
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
                        (add-to-list 'boundaries
                              (cons (cdr (assoc :key changes))
                                    (cons changes (point))))))
                      ((eq (car boundary) 'end)
                        (dolist (key (append (cdr (cdr boundary)) '()))
                          (cond ((eq key "lang"))  ;;nothing to do
                                ((eq key "conv/title")
                                 ;; TODO(ahyatt): Apply title face here
                                 )
                                (t (message
                                    "Don't know how to handle end-boundary of type %s"
                                    key))))))))
            (t (message "Dont know how to deal with op: %s" op))))
    (dolist (child-id (cdr (assoc :children blip)))
      (wave-display-blip child-id blips (+ level 1)))))

(defun wave-display-wavelet (wavelet)
  "Display a WAVELET."
  (when (assoc :root-blip-id wavelet)
    (insert "Participants: "
            (mapconcat 'wave-display-format-user
                       (cdr (assoc :participants wavelet)) ", ")
            "\n")
    (wave-display-blip (cdr (assoc :root-blip-id wavelet))
                       (cdr (assoc :blips wavelet)) 0)))

(defun wave-display (wave-label wave-data)
  "Display in a new or re-used buffer the wave from WAVE-DATA in
wave-list-mode.  Returns the new buffer."
  (let ((buf-name (format wave-display-buffer-format wave-label)))
    (set-buffer (get-buffer-create buf-name))
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; First wavelet seems uninteresting
    (dolist (wavelet (cdr wave-data))
      (wave-display-wavelet wavelet))
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
  (run-mode-hooks))

(provide 'wave-display)

;;; wave-display.el ends here
