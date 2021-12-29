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
       (titlecase-dwim)
       (should (equal ,text-expected (buffer-string))))))

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

(provide 'titlecase-tests)
;;; titlecase-tests.el ends here
