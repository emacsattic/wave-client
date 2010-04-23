;;; wave-util.el --- Common utility methods for the project.

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
;;
;; Utility functions used by the rest of the project code.  This file
;; should have no other dependencies.

;;; Code:
(defvar wave-debug
  nil
  "Whether to debug or not.
If we do, debug output is be sent to wave-client-debug-buffer")

(defconst wave-debug-buffer
  "*Wave Client Debug Buffer*"
  "Name of the buffer wear debug output is sent to.")

(defun wave-debug (str &rest args)
  "Send STR to the `wave-debug-buffer', with format args ARGS."
  (when wave-debug
    (save-excursion
      (set-buffer
       (get-buffer-create wave-debug-buffer))
      (goto-char (point-max))
      (insert (apply 'format (append (list str) args)))
      (insert "\n"))))

(provide 'wave-util)

;;; wave-util.el ends here
