;;; wave-update.el --- Defines methods to help with updating waves.
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

;; This file contains code related to manipulating parts of a wave.

(require 'wave-client)

(defconst wave-update-op-to-message-type
  '((add-participant . 5975636)
    (remove-participant . 5975541)
    (blip-submit . 5926533)
    (mutate-document . 9184307)))

(defun wave-update-message-type (op)
  "From an operation OP, return the relevant message type"
  (cdr (assoc op wave-update-op-to-message-type)))

(defmacro defproto (name &rest struct-elems)
  "Replacement for defstruct, allows for proto field number
  mappings"
  (let ((struct-elems (if (stringp (car struct-elems))
                          (cdr struct-elems)
                        struct-elems)))
    `(progn (defstruct ,name
                ,@(mapcar (lambda (property) (list (car property) nil :read-only t))
                          struct-elems))
              (defun ,(intern (concat (symbol-name name) "-to-proto")) (item)
                (let ((plist '()))
                  (dolist (field-def (quote ,struct-elems))
                    (let ((field-val
                           (funcall
                            (intern
                              (concat (symbol-name (quote ,name))
                                      "-"
                                      (symbol-name (car field-def)))) item)))
                      (when field-val
                        (setq plist
                              (plist-put plist (cadr field-def) field-val)))))
                  plist))
              (defun ,(intern (concat (symbol-name name) "-proto"))
                (&rest fields)
                (,(intern (concat (symbol-name name) "-to-proto"))
                 (apply (quote ,(intern (concat "make-" (symbol-name name))))
                        fields))))))

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
  (op-list :4))

(defproto wave-op
  (type-id :1)
  (add-participant :4)
  (remove-participant :5)
  (blip-submit :10)
  (mutate-document :23))

(defproto wave-add-participant
  (user-id :1))

(defproto wave-mutate-document
  (document-id :1)
  (operation :2))

(defproto wave-document-operation
  (component :2))

(defproto wave-component
  (annotation-boundary :1)
  (element-start :4)
  (element-end :5)
  (retain-item-count :6))

(defproto wave-key-value-pair
  (key :1)
  (value :2))

(defproto wave-element-start
  (type :1)
  (attributes :2))

(defun wave-update-start-proto (name &rest prop-alist)
  "Start an element with name NAME."
  (wave-component-proto :element-start
                        (wave-element-start-proto
                         :type name
                         :attributes
                         (apply 'vector
                                (mapcar (lambda (kv)
                                          (wave-key-value-pair-proto
                                           :key (car kv)
                                           :value (cdr kv))) prop-alist)))))

(defun wave-update-end-proto ()
  "End the innermost element."
  (wave-component-proto :element-end t))

(defun wave-update-skip-proto (num)
  "Proto to skip NUM elements."
  (wave-component-proto
   :retain-item-count num))

(defun wave-update-blip-version-proto (blip-id version)
  "Make a proto storing the blip version start-element.  Must be
  closed by a close-element."
  (wave-element-start-proto
           :type "blip"
           :attributes (vector
                        (wave-key-value-pair-proto
                         :key "i" :value blip-id)
                        (wave-key-value-pair-proto
                         :key "v"
                         :value version))))

(defun wave-update-wavelet-id-proto (wavelet-id)
  "Make a wavelet-id proto.  Must be closed by a close-element."
  (wave-element-start-proto :type "wavelet"
                            :attributes
                            (vector
                             (wave-key-value-pair-proto
                              :key "i" :value
                              (cdr wavelet-id)))))

(defun wave-update-mark-blip-read (blip-id user-header conv-header
                                           m-read)
  "Mark a blip read."
  (let* ((pos (position-if (lambda (elem)
                             (eq (car elem) 'blip)) m-read))
         (new-read-op
          (wave-update-blip-version-proto blip-id (wave-display-header-version conv-header)))
         (ops (if m-read
                  (vector
                   (wave-component-proto
                    :retain-item-count pos)
                   (wave-component-proto
                    :element-start
                    new-read-op)
                   (wave-component-proto :element-end t)
                   (wave-component-proto
                    :retain-item-count (- (length m-read) pos)))
                ;; adding to m/read
                (vector (wave-update-wavelet-id-proto
                         (wave-display-header-wavelet-name
                          conv-header))
                        (wave-component-proto :element-end t)
                        new-read-op
                        (wave-component-proto :element-end t)))))
    (wave-client-send-delta
     (wave-update-mutation (car (wave-display-header-wavelet-name user-header))
                           (cdr (wave-display-header-wavelet-name user-header))
                           blip-id (wave-display-header-version user-header)
                           "m/read" ops))))

(defun wave-update-mutation (wave-id wavelet-id wavelet-version document-id ops)
  "Produce a mutation WAVELET-ID in WAVE-ID."
  (wave-submit-delta-request-proto
   :wave-id wave-id
   :wavelet-id wavelet-id
   :delta
   (wave-delta-proto
    :version (vector wavelet-version 0)
    :user-id (wave-client-email-address)
    :op-list (vector (wave-op-proto
                      :type-id (wave-update-message-type 'mutate-document)
                      :mutate-document
                      (wave-mutate-document-proto
                       :document-id document-id
                       :operation
                       (wave-document-operation-proto :component ops)))))))

(defun wave-update-submit (wavelet-name wavelet-version ops)
  (check-type ops sequence)
  (check-type wavelet-version (integer 0 *))
  (wave-client-send-delta
   (wave-submit-delta-request-proto
    :wave-id (car wavelet-name)
    :wavelet-id (cdr wavelet-name)
    :delta
    (wave-delta-proto
     ;; TODO(ohler): add support for versions above 2**31
     :version (vector wavelet-version 0)
     :user-id (wave-client-email-address)
     :op-list (coerce ops 'vector)))))

(defun wave-update-add-participant (wavelet-name wavelet-version
                                                 participant-address)
  (wave-update-submit wavelet-name wavelet-version
                      (list
                       (wave-op-proto
                        :type-id (wave-update-message-type 'add-participant)
                        :add-participant
                        (wave-add-participant-proto
                         :user-id participant-address)))))

(defun wave-update-new-blip-id ()
  "Generate a new blip id."
  (format "b+%s" (wave-random-b64-string 9)))

(defun wave-update-new-blip (conv-wavelet-name wavelet-version
                                               conv-to-skip
                                               rest-to-skip)
  "Create a new blip."
  (let ((new-blip-id (wave-update-new-blip-id)))
    (wave-client-send-delta
   (let ((empty-blip-ops (vector
                          (wave-update-start-proto "body")
                          (wave-update-start-proto "line")
                          (wave-update-end-proto) (wave-update-end-proto)))
         (conversation-ops (vector
                            (wave-update-skip-proto conv-to-skip)
                            (wave-update-start-proto "blip"
                                                     `("id" . ,new-blip-id ))
                            (wave-update-end-proto)
                            (wave-update-skip-proto rest-to-skip))))
     (wave-submit-delta-request-proto
      :wave-id (car conv-wavelet-name)
      :wavelet-id (cdr conv-wavelet-name)
      :delta
      (wave-delta-proto
       :version (vector wavelet-version 0)
       :user-id (wave-client-email-address)
       :op-list (vector (wave-op-proto
                         :type-id (wave-update-message-type 'mutate-document)
                         :mutate-document
                         (wave-mutate-document-proto
                          :document-id new-blip-id
                          :operation
                          (wave-document-operation-proto
                           :component empty-blip-ops)))
                        (wave-op-proto
                         :type-id
                         (wave-update-message-type 'mutate-document)
                         :mutate-document
                         (wave-mutate-document-proto
                          :document-id "conversation"
                          :operation
                          (wave-document-operation-proto
                           :component conversation-ops))))))))))

(defun wave-update-blip-append-text (wavelet-name blip-id text)
  )

(provide 'wave-update)

;;; wave-update.el ends here
