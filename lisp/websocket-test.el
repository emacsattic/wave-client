;;; websocket-test.el --- Unit tests for the websocket layer

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
;; This defines and runs ert unit tests.  You can download ert from:
;; http://github.com/ohler/ert

(require 'ert)
(require 'cl)

(defun websocket-test-get-filtered-response (outputs)
  (let ((packet-data nil)
        (websocket
         (make-websocket :conn "fake-conn" :first-response nil
                         :filter (lambda (packet) (push packet packet-data))
                         :close-callback (lambda (not-called) (assert nil))
                         :url "ws://foo/bar"
                         :v75 nil)))
    (dolist (output outputs)
      (websocket-outer-filter websocket output))
    (nreverse packet-data)))

(ert-deftest websocket-filter-basic ()
  (should (equal
           '("foo")
           (websocket-test-get-filtered-response '("\0foo\377"))))
  (should (equal
           '("foo" "bar")
           (websocket-test-get-filtered-response
            '("\0foo\377\0bar\377"))))
  (should (equal
           '("foo" "bar")
           (websocket-test-get-filtered-response
            '("\0foo\377" "\0bar\377")))))

(ert-deftest websocket-filter-inflight-packets ()
  (should (equal
           '("foo" "bar")
           (websocket-test-get-filtered-response
            '("\0foo\377\0b" "a" "r\377"))))
  (should (equal
           '("foo" "bar")
           (websocket-test-get-filtered-response
            '("\0foo\377\0ba" "r\377baz")))))

(ert-run-tests-interactively 'websocket-filter-basic)
(ert-run-tests-interactively 'websocket-filter-inflight-packets)