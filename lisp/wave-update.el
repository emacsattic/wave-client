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
  '((add-blip . 6086091) (add-participant . 5975636)
    (remove-participant . 5975541) (delete-blip . 6125027)
    (set-tombstone . 8348011) (blip-submit . 5926533)
    (mutate-document . 9184307)))

(defun wave-update-message-type (op)
  "From an operation OP, return the relevant message type"
  (cdr (assoc op wave-update-op-to-message-type)))

(defun wave-update-make-delta (wave-id wavelet-id user-id delta-version
                                       op mutation)
  `(:1 ,wave-id :2 ,wavelet-id :3 mutation :4 "" :5 [0 0]))

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
                  plist)))))

;;; Protos definitions see:
;;; http://www.waveprotocol.org/draft-protocol-specs/draft-protocol-spec#anchor11
;;; Note the numerical values there don't always seem to be right.  We
;;; use the correct version.

(defproto wave-op-component
  (element-start :4)
  (element-end :5)
  (retain-item-count :6))

(defproto wave-key-value-pair
  (key :1)
  (value :2))

(defproto element-start
  (type :1)
  (attribute :2))

(provide 'wave-update)

;;; wave-update.el ends here
