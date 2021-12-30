;;; titlecase-tests.el --- Title-case phrases tests -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Campbell Barton

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Usage:

;; These tests are typically executed by a shell script, e.g:
;;
;;   ./tests/titlecase-tests.sh
;;
;; If you wish to run this directly from the command line,
;; you may use the following command:
;;
;;    emacs -batch -l tests/titlecase-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

;; Setup load path.
(add-to-list 'load-path (concat (file-name-directory load-file-name) ".."))
(require 'titlecase)

;; Simplify test declaration.
(defmacro ert-deftest-decl-pair (test-id text-initial text-expected)
  "Create a test named TEST-ID using TEXT-INITIAL TEXT-EXPECTED as a result."
  `(ert-deftest ,test-id ()
     (with-temp-buffer
       (insert ,text-initial)
       (titlecase-region (point-min) (point-max))
       (should (equal ,text-expected (buffer-string))))))

(defmacro ert-deftest-decl-nop (test-id text)
  `(ert-deftest-decl-pair ,test-id ,text ,text))

(defmacro ert-deftest-decl-geneated-data-char-range (test-id pass len seed max)
  "Create a test named TEST-ID that uses random data."
  `(ert-deftest ,test-id ()
     ;; Initialize the seed.
     (random (format "%d" ,seed))
     (let ((max-char-minus-1 (1- ,max)))
       (dotimes (_ ,pass)
         (with-temp-buffer
           (dotimes (_ ,len)
             (insert (char-to-string (1+ (random max-char-minus-1)))))
           (let ((text-initial (buffer-string)))
             (titlecase-region (point-min) (point-max))
             ;; Simply test only the case changed.
             (should (equal (downcase text-initial)
                            (downcase (buffer-string))))))))))

(defmacro ert-deftest-decl-geneated-ascii (test-id pass len seed)
  `(ert-deftest-decl-geneated-data-char-range ,test-id ,pass ,len ,seed 128))

;; FIXME: can cause bugs, these look to be errors in emacs it's self.
(defmacro ert-deftest-decl-geneated-unicode (test-id pass len seed)
  ;; See emacs's own: MAX_CHAR = 0x3FFFFF = 4194303 (inclusive).
  `(ert-deftest-decl-geneated-data-char-range ,test-id ,pass ,len ,seed 4194303))


;; Tests.
(ert-deftest-decl-pair
 nop_1
 ""
 "")
(ert-deftest-decl-pair
 simple_1
 "hello world"
 "Hello World")
(ert-deftest-decl-pair
 simple_2
 "HELLO WORLD"
 "Hello World")
(ert-deftest-decl-pair
 contractions_1 ;; See issue #4.
 "i can't live with or without you"
 "I Can't Live With or without You")
(ert-deftest-decl-pair
 apostrophe_1
 "don't work"
 "Don't Work")
(ert-deftest-decl-pair
 non_ascii_single_1 ;; See issue #1.
 "π"
 "Π")
(ert-deftest-decl-pair
 hyphen_1
 "from a-z"
 "From A-Z")
(ert-deftest-decl-pair
 non_ascii_single_2
 "α π ω"
 "Α Π Ω")
(ert-deftest-decl-pair
 non_ascii_multiple_1
 "απω απω"
 "Απω Απω")
(ert-deftest-decl-pair ;; See issue #11.
 special-chars-1
 "⁽"
 "⁽")
(ert-deftest-decl-pair
 punctuation-colon-1
 "test: of mice and men"
 "Test: Of Mice and Men")
(ert-deftest-decl-pair
 punctuation-semicolon-1
 "test; of mice and men"
 "Test; Of Mice and Men")

(ert-deftest-decl-pair
 punctuation-newline_1
 "test\nof mice and men"
 "Test\nOf Mice and Men")
(ert-deftest-decl-pair
 punctuation-newline_2
 "test \nof mice and men"
 "Test \nOf Mice and Men")
(ert-deftest-decl-pair
 punctuation-newline_3
 "test@\nof mice and men"
 "Test@\nOf Mice and Men")
(ert-deftest-decl-pair
 punctuation-newline_4
 "test\n@of mice and men"
 "Test\n@Of Mice and Men")


;; ---------------------------------------------------------------------------
;; Test (Generated Data)
;;
;; This is mainly a stress test to ensure garbage input doesn't cause errors.
;; Actual correctness is not ensured.

;; NOTE: to avoid this taking overly long, the number of tests has been reduced,
;; Use 10,000+ for more extensive stress testing.
(defconst titlecase-tests-passes 128)
(defconst titlecase-tests-default-seed 9876543210)

(ert-deftest-decl-geneated-ascii
 generated_1_chars titlecase-tests-passes 1 titlecase-tests-default-seed)
(ert-deftest-decl-geneated-ascii
 generated_2_chars titlecase-tests-passes 2 titlecase-tests-default-seed)
(ert-deftest-decl-geneated-ascii
 generated_5_chars titlecase-tests-passes 5 titlecase-tests-default-seed)
(ert-deftest-decl-geneated-ascii
 generated_64_chars titlecase-tests-passes 64 titlecase-tests-default-seed)


(provide 'titlecase-tests)
;;; titlecase-tests.el ends here
