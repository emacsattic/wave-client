;;; wave-edit.el --- Defines wave-edit-mode and has the edit
;; processing logic.
;; Copyright (c) 2010 Andrew Hyatt
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

;; This file contains code related to editing a blip.

;;; Code:

(defvar wave-edit-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'wave-edit-finish)
    map))

(defvar wave-edit-pre-command-buffer nil
  "A temporary buffer with the pre-command state copied in.")
(make-variable-buffer-local 'wave-edit-pre-command-buffer)

(defvar wave-edit-parent-buf nil
  "The parent buffer of this edited blip.")
(make-variable-buffer-local 'wave-edit-parent-buf)

(defvar wave-edit-previous-blip nil
  "The previous blip id in the conversation.")
(make-variable-buffer-local 'wave-edit-previous-blip)

(defvar wave-edit-wavelet-id nil
  "The wavelet id the edit is in.")
(make-variable-buffer-local 'wave-edit-wavelet-id)

(defun wave-edit-finish ()
  "Finish editing the wave."
  ;; TODO(ahyatt) Execute this on killing the buffer too.
  (interactive)
  (kill-buffer wave-edit-pre-command-buffer)
  (setq wave-edit-pre-command-buffer nil)
  (with-current-buffer wave-edit-parent-wave
    (set-window-configuration (car wave-display-saved-window-configuration))
    (goto-char (marker-position (cdr wave-display-saved-window-configuration)))
    (setq wave-display-saved-window-configuration nil))
  (kill-buffer))

(defun wave-edit-note-before-state ()
  "Note the state of the buffer before the command is run.
If we are in draft mode, though, do nothing."
  (save-excursion
    (let ((edit-buffer (current-buffer)))
      (unless wave-edit-pre-command-buffer
        (setq wave-edit-pre-command-buffer
              (generate-new-buffer " *wave pre-command*")))
      (with-current-buffer wave-edit-pre-command-buffer
        (erase-buffer)
        (insert-buffer-substring edit-buffer)))))

(defun wave-edit-maybe-send-updates ()
  "Send updates of the buffer to Wave."
  ;; TODO(ahyatt): Implement draft mode
  (when wave-edit-pre-command-buffer
    (let* ((new (current-buffer))
           (old wave-edit-pre-command-buffer)
           (old-endpos (with-current-buffer old (point-max)))
           (new-endpos (point-max)))
      ;; Initially, let's just handle two scenarios:
      ;; 1) Text appended
      ;; 2) Text deleted
      (cond ((> (buffer-size new)
                (buffer-size old))
             ;; need wavelet-id, blip-id
             (wave-debug "Appended %s:"(buffer-substring-no-properties
                                        old-endpos new-endpos)))))))

(defun wave-edit-new-blip ()
  "Create a new blip."
  (let* ((wavelet-id wave-edit-wavelet-id)
         (conv-data (with-current-buffer wave-edit-parent-buf
                      (gethash wavelet-id
                               wave-display-conversations)))
         (conv-wavelet-id (concat (wave-client-domain) "!conv+root"))
         (wavelet-version (wave-display-header-version
                           (with-current-buffer wave-edit-parent-buf
                             (gethash conv-wavelet-id wave-display-wavelets))))
         (num-to-skip
          (+ 1
             (position-if (lambda (elem)
                            (and (listp elem)
                                 (eq (car elem) 'blip)
                                 (equal (cadadr elem)
                                        (symbol-name wave-edit-previous-blip))))
                          conv-data)))
         (num-left (- (length conv-data) num-to-skip)))
    (wave-update-new-blip (cons wavelet-id conv-wavelet-id)
                          wavelet-version
                          num-to-skip num-left)))

(define-minor-mode wave-edit-mode
  "Mode for editing a wave's blip." nil "Wave Edit"
  wave-edit-keymap
  :group 'wave-edit
  ;; We use pre-command-hook and post-command-hook instead of
  ;; before-change-functions and after-change-functions, so that we
  ;; can update the position of the cursor.
  (make-local-variable 'pre-command-hook)
  (make-local-variable 'post-command-hook)
  (wave-edit-new-blip)
  (add-to-list 'pre-command-hook 'wave-edit-note-before-state)
  (add-to-list 'post-command-hook 'wave-edit-maybe-send-updates))

(provide 'wave-edit)

;;; wave-edit.el ends here