(require 'ert)

(defun wave-hash-table-to-alist (map)
  (let ((alist (list)))
    (maphash (lambda (key value)
               (push (cons key value) alist))
             map)
    alist))

(defun wave-equal-hash (a b)
  "Like `equal', but descends into hash tables."
  (null (wave-explain-not-equal-hash a b)))

(defun wave-explain-not-equal-hash (a b)
  (if (not (equal (type-of a) (type-of b)))
      `(different-types ,a ,b)
    (etypecase a
      (cons
       (let ((a-proper-p (ert-proper-list-p a))
             (b-proper-p (ert-proper-list-p b)))
         (if (not (eql (not a-proper-p) (not b-proper-p)))
             `(one-list-proper-one-improper ,a ,b)
           (if a-proper-p
               (if (not (equal (length a) (length b)))
                   ;; This would be even more helpful if it showed
                   ;; something like what `set-difference' would
                   ;; return.
                   `(proper-lists-of-different-length ,a ,b)
                 (loop for i from 0
                       for ai in a
                       for bi in b
                       for xi = (wave-explain-not-equal-hash ai bi)
                       do (when xi (return `(list-elt ,i ,xi)))))
             (let ((car-x (wave-explain-not-equal-hash (car a) (car b))))
               (if car-x
                   `(car ,car-x)
                 (let ((cdr-x (wave-explain-not-equal-hash (cdr a) (cdr b))))
                   (if cdr-x
                       `(cdr ,cdr-x))
                   nil)))))))
      (array (if (not (equal (length a) (length b)))
                 `(arrays-of-different-length ,a ,b)
               (loop for i from 0
                     for ai across a
                     for bi across b
                     for xi = (wave-explain-not-equal-hash ai bi)
                     do (when xi (return `(array-elt ,i ,xi))))))
      (hash-table (if (not (equal (hash-table-count a)
                                  (hash-table-count b)))
                      `(hash-tables-of-different-size ,a ,b)
                    (block different
                      (let ((not-present (make-symbol "not-present")))
                        (maphash (lambda (key value1)
                                   (let* ((value2 (gethash key b not-present))
                                          (here (wave-explain-not-equal-hash
                                                 value1
                                                 value2)))
                                     (when here
                                       (return-from different
                                         `(hash-key ,key ,here
                                                    ,(wave-hash-table-to-alist
                                                     a)
                                                    ,(wave-hash-table-to-alist
                                                     b))))))
                                 a))
                      nil)))
      (atom (if (not (equal a b))
                `(different-atoms ,(ert-explain-format-atom a)
                                  ,(ert-explain-format-atom b))
              nil)))))
(put 'wave-equal-hash 'ert-explainer 'wave-explain-not-equal-hash)

(ert-deftest wave-apply-doc-op ()
  (should (equal '() (wave-apply-doc-op '() '[])))
  (should (equal '("abc12foo")
                 (wave-apply-doc-op
                  '("x12y")
                  '[(:characters "abc")
                    (:delete_characters "x")
                    (:retain_item_count 2)
                    (:characters "foo")
                    (:delete_characters "y")])))
  (should
   (equal '((foo (xx "yy" zz "123")))
          (wave-apply-doc-op
           '((e (a 1 b 2)))
           '[(:element_start
              (:type "foo"
                     :attribute [(:key "xx" :value "yy")
                                 (:key "zz" :value "123")]))
             (:delete_element_start
              (:type "e"
                     :attribute [(:key "a" :value "1")
                                 (:key "b" :value "2")]))]))))

(ert-deftest wave-apply-delta ()
  (should
   (wave-equal-hash
    (wave-make-wavelet
     :wavelet-name '("a" . "b")
     :creator "foo"
     :version '(2 . 5)
     :creation-time 2
     :last-modified-time 17
     :participants '("foo")
     :docs (let ((map (make-hash-table)))
             (puthash 'doc1 (wave-make-doc :doc-id 'doc1
                                           :contributors '("foo")
                                           :last-modified-version 2
                                           :last-modified-time 17
                                           :content '("bar"))
                      map)
             map))
    (wave-apply-delta
     (wave-make-wavelet :wavelet-name '("a" . "b")
                        :creator "foo"
                        :creation-time 2)
     (wave-make-delta :author "foo"
                      :pre-version '(0 . 0)
                      :post-version '(2 . 5)
                      :timestamp 17
                      :ops (vector
                            (wave-make-add-participant :address "foo")
                            (wave-make-doc-op :doc-id 'doc1
                                              :components
                                              '[(:characters "bar")])))))))

(ert-deftest wave-client-ws-parse-wavelet-name ()
  (should (equal '("wave.com!w+4ks3" . "wave.com!conversation+root")
                 (wave-client-ws-parse-wavelet-name "wave://wave.com/w+4ks3/conversation+root")))
  (should (equal '("tudalin.lv!profile+bob@tudalin.lv" . "tudalin.lv!profile+root")
                 (wave-client-ws-parse-wavelet-name "wave://tudalin.lv/profile+bob@tudalin.lv/profile+root")))
  (should (equal '("wave.com!w+4Kl2" . "privatereply.com!conversation+3sG7")
                 (wave-client-ws-parse-wavelet-name "wave://privatereply.com/wave.com!w+4Kl2/conversation+3sG7"))))
