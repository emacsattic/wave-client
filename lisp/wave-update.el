;;; wave-update.el --- Defines methods to help with updating waves.
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

;; This file contains code related to manipulating parts of a wave.

(require 'wave-client)

(defun wave-update-start (name &rest prop-alist)
  "Start an element with name NAME."
  (wave-make-element-start
   :type name
   :attributes (mapcar (lambda (kv)
                         (wave-make-key-value-pair
                          :key (car kv)
                          :value (cdr kv))) prop-alist)))

(defun wave-update-end ()
  "End the innermost element.  Just for balance with
`wave-update-start'."
  (wave-make-element-end))

(defun wave-update-skip (num)
  "Structure to skip NUM elements."
  (wave-make-retain-item-count :num num))

(defun wave-update-mark-blip-read (blip-id user-header conv-header
                                           m-read)
  "Mark a blip read."
  (let* ((pos (position-if (lambda (elem)
                             (eq (car elem) 'blip)) m-read))
         (new-read-op
          (wave-update-start "blip" (cons "i" blip-id)
                             (cons
                              "v"
                              (car
                               (wave-display-header-version conv-header)))))
         (op (if m-read
                  (vector
                   (wave-update-skip pos)
                   new-read-op
                   (wave-update-end)
                   (wave-update-skip (- (length m-read) pos)))
                ;; adding to m/read
                (vector (wave-update-start "wavelet"
                                           (cons "i"
                                                 (cdr (wave-display-header-wavelet-name
                                                       conv-header))))
                        (wave-update-end)
                        new-read-op
                        (wave-update-end)))))
    (wave-update-submit (wave-display-header-wavelet-name user-header)
                        (wave-display-header-version user-header)
                        (list (wave-make-doc-op :doc-id "m/read" :components op)))))

(defun wave-update-submit (wavelet-name wavelet-version ops)
  (check-type ops sequence)
  (wave-client-send-delta (wave-make-delta
                           :pre-version wavelet-version
                           :author (wave-client-email-address)
                           :ops (coerce ops 'vector)) wavelet-name))

(defun wave-update-add-participant (wavelet-name wavelet-version
                                                 participant-address)
  (wave-update-submit wavelet-name wavelet-version
                      (list (wave-make-add-participant
                             :address participant-address))))

(defun wave-update-new-blip-id ()
  "Generate a new blip id."
  (format "b+%s" (wave-random-b64-string 9)))

(defun wave-update-insert-text (conv-wavelet-name wavelet-version
                                                  blip-id text
                                                  num-to-skip rest-to-skip)
  "Insert text into a new blip."
  (wave-update-submit conv-wavelet-name
                      wavelet-version
                      (list (wave-make-doc-op
                             :doc-id blip-id
                             :components
                             (apply 'vector
                                    (append
                                     (when (and num-to-skip (> num-to-skip 0))
                                       (list (wave-update-skip num-to-skip)))
                                     (list (wave-make-text :text text))
                                     (when (and rest-to-skip (> rest-to-skip 0))
                                       (list (wave-update-skip rest-to-skip)))))))))

(defun wave-update-process-text (text)
  "Transform TEXT into a sequence of line breaks and text."
  (mapcan (lambda (line-text)
            (when (> (length line-text) 0)
              (list (wave-update-start "line")
                  (wave-update-end)
                  (wave-make-text :text line-text))))
          (split-string text "\n")))

(defun wave-update-new-blip (conv-wavelet-name wavelet-version
                                               conv-to-skip
                                               rest-to-skip text)
  "Create a new blip."
  (let* ((new-blip-id (wave-update-new-blip-id))
         (new-blip `[,(wave-update-start "body")
                     ,@(wave-update-process-text text)
                     ,(wave-update-end)])
         (conversation-ins (vector
                            (wave-update-skip conv-to-skip)
                            (wave-update-start "blip"
                                               `("id" . ,new-blip-id ))
                            (wave-update-end)
                            (wave-update-skip rest-to-skip))))
    (wave-debug "Adding new blip: %s" new-blip-id)
    (wave-update-submit conv-wavelet-name wavelet-version
                        (list (wave-make-blip-submit
                               :blip-id new-blip-id)
                              (wave-make-doc-op
                               :doc-id new-blip-id
                               :components new-blip)
                              (wave-make-doc-op
                               :doc-id "conversation"
                               :components conversation-ins)))
    new-blip-id))

(provide 'wave-update)

;;; wave-update.el ends here
