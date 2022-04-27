;;; titlecase.el --- Title-case phrases -*- lexical-binding: t; -*-

;; Author: Case Duckworth <acdw@acdw.net>
;; Maintainer: Case Duckworth <acdw@acdw.net>
;; Version: 0.3.0
;; URL: https://github.com/duckwork/titlecase.el
;; Package-Requires: ((emacs "25.1"))

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

;;; Commentary:

;; This library strives to be the most accurate possible with title-casing
;; sentences, lines, and regions of text in English prose according to a number
;; of styles guides' capitalization rules.  It is necessarily a best-effort; due
;; to the vaguaries of written English it's impossible to completely correctly
;; capitalize aribtrary titles.  So be sure to proofread and copy-edit your
;; titles before sending them off to be published, and never trust a computer.

;; INSTALLATION and USE:

;; Make sure both titlecase.el and titlecase-data.el are in your `load-path',
;; and `require' titlecase.  You should then be able to call the interactive
;; functions defined in this file.

;;; CUSTOMIZATION:

;; Only two customization options are probably going to be of any interest:
;; `titlecase-style' (the style to use for capitalizing titles), and
;; `titlecase-dwim-non-region-function', which determines what to do when
;; `titlecase-dwim' isn't acting on a region.

;; If you want to use your own title-casing code, or a third party, you can
;; customize `titlecase-command' to something other than its default.  One
;; possibility is titlecase.pl, written John Gruber and Aristotle Pagaltzis:
;; https://github.com/ap/titlecase.

;;; Code:

(require 'browse-url)                   ; `browse-url-button-regexp'
(require 'cl-lib)                       ; `cl-loop'
(require 'seq)                          ; `seq-some'
(require 'thingatpt)                    ; `bounds-of-thing-at-point'
(require 'titlecase-data)

(defgroup titlecase nil
  "Customization for title-casing phrases."
  :prefix "titlecase-"
  :group 'text)

(defvar titlecase-styles '((chicago . "Chicago Style")
                           (apa . "APA Style")
                           (mla . "MLA Style")
                           (ap . "AP Style")
                           (bluebook . "Bluebook Style")
                           (ama . "AMA Style")
                           (nyt . "New York Times Style")
                           (wikipedia . "Wikipedia Style")
                           (sentence . "Sentence style"))
  "Available styles for title-casing.")

(defvar titlecase-default-case-function #'capitalize-word
  "What to do to a word when a style doesn't specify what to do.")

(defcustom titlecase-normalize-functions '(titlecase--lowercase-all-caps)
  "List of functions for normalizing input before title-casing.
Each function will be passed 3 arguments: the beginning and
ending points of the region to operate on, as well as the
title-casing style.  They are called one after another in order
in a `save-excursion' block."
  :type '(repeat function))

(defcustom titlecase-skip-words-regexps (list "\\b[[:upper:]]+\\b"
                                              browse-url-button-regexp)
  "Regexps of words to skip when titlecasing.
Each regexp in this list will be tested on each word considered
for title-casing, and if the regexp matches the entire word, the
word will be skipped.

NOTE: These regexps will be matched against the title-cased
region /after/ normalizing it, which means that, by default, if
the region is in all-caps before calling `titlecase-region', it
will be downcased before title-casing.  Thus, some of these
regexps might not match when expected.  This behavior is a
trade-off between possible user expectations.  To change this
behavior, customize `titlecase-normalize-functions'."
  :type '(repeat regexp))

(defcustom titlecase-style 'wikipedia
  "Which style to use when title-casing."
  :type (cons 'choice (cl-loop
                       for style in titlecase-styles
                       collect (list 'const :tag (cdr style) (car style))
                       into choices
                       finally return choices)))

(defcustom titlecase-force-cap-after-punc "[.?!\\/;:\n\r]"
  "Regexp to force the next word capitalized."
  :type 'regexp)

(defcustom titlecase-dwim-non-region-function #'titlecase-line
  "What to do with `titlecase-dwim' when region isn't active.
Recommended: `titlecase-line' or `titlecase-sentence'."
  :type 'function)

(defcustom titlecase-command #'titlecase--region-with-style
  "Command to use for titlecasing titles.
This option can be one of two things:

A string value, or a list of string values, is interpreted as a
system command to run using `call-process-region' on a temp
buffer containing the text to titlecase.  Just a string is
interpreted as the command to run, with no arguments.  A list of
strings will pass those strings as aruguments to the command-line
program.  In that list, the symbol `style' will be replaced with
the the string of the title-casing style that's passed.

A function value is interpreted as the function to call on the
region.  The function will be called with three arguments: the
beginning and end of the region, and the style (see
`titlecase-style') to capitalize it in.")

(defun titlecase--region-with-style-impl (begin end style)
  "Title-case implementation.
`titlecase-force-cap-after-punc' must be handled by the caller.
This is expected to have run in a block that uses `save-excursion' and
`save-match-data'.  See documentation for `titlecase--region-with-style'
for docs on BEGIN, END and STYLE."
  (let ( ;; Constants during this function's runtime.
        (case-fold-search nil)
        (downcase-word-list (symbol-value
                             (intern (format "titlecase-lowercase-%s"
                                             style)))))

    ;; Normalize the text in region by calling `titlecase-normalize-functions'
    ;; in order.
    (dolist (fn titlecase-normalize-functions)
      (save-excursion
        (funcall fn begin end style)))

    ;; Skip blank lines & white-space (where `current-word' would return nil).
    ;; It's important this uses the same logic that `current-word' uses to scan
    ;; for words, or this may be nil when it's not expected. See #11.
    (goto-char begin)
    (skip-syntax-forward "^w" end)
    (setq begin (point))

    ;; And loop over the rest.
    (catch :done
      (while (< (point) end)
        (let ((this-word (current-word)))
          (cond
           ;; Skip words matching `titlecase-skip-words-regexps'.
           ((looking-at (format "%s"
                                (mapconcat #'identity
                                           titlecase-skip-words-regexps
                                           "\\|")))
            (goto-char (match-end 0))
            ;; TODO: Document what this does (it's late)
            (when (>= (point) end)
              (throw :done 'skipped)))
           ;; Phrasal verbs!
           ((and (memq style titlecase-styles-capitalize-phrasal-verbs)
                 (member (downcase this-word)
                         (mapcar #'car titlecase-phrasal-verbs)))
            ;; We need to do a little state machine thingy here.
            (let ((next-words (assoc this-word titlecase-phrasal-verbs))
                  (bail-pt (point)))
              ;; Take care of the first word --- this is inelegant.
              (capitalize-word 1)
              (skip-syntax-forward "^w" end)
              (setq this-word (current-word))
              ;; Loop through the rest
              (while (and this-word
                          (member (downcase this-word)
                                  (mapcar #'car-safe next-words)))
                (capitalize-word 1)
                (skip-syntax-forward "^w" end)
                (setq this-word (current-word)
                      next-words (mapcar #'cdr-safe next-words)))
              (unless (seq-some #'null next-words)
                ;; If it's not a phrasal verb, bail --- but still
                ;; capitalize the first word!
                (downcase-region bail-pt (point))
                (goto-char bail-pt)
                (capitalize-word 1))))
           ;; Force capitalization if this is the first word.
           ((eq begin (point))
            (capitalize-word 1))
           ;; AP capitalizes /all/ words longer than 3 letters.
           ((and (memq style titlecase-styles-capitalize-non-short-words)
                 (> (length this-word) titlecase-short-word-length))
            (capitalize-word 1))
           ;; Skip the next word if:
           ((or
             ;; Sentence style just capitalizes the first word.  Since we
             ;; can't be sure how the user has already capitalized
             ;; anything, we just skip the current word.
             (eq style 'sentence)
             ;; None of the styles require a capital letter after an
             ;; apostrophe.
             (memq (char-before (point)) '(?' ?’))
             ;; FIXME: Hyphens are a completely different story with
             ;; capitalization.
             (eq (char-before (point)) ?-))
            (forward-word 1))
           ;; Down-case words that should be.
           ((member (downcase this-word) downcase-word-list)
            (downcase-word 1))
           ;; Otherwise, do the default function on the word.
           (t
            (funcall titlecase-default-case-function 1))))

        ;; Step over the loop.
        (unless (= end (point))
          (skip-syntax-forward "^w" end)))
      ;; Capitalize the last word, only in some styles and some conditions.
      (when (and (memq style titlecase-styles-capitalize-last-word))
        (save-excursion
          (backward-word 1)
          (when (and (>= (point) begin)
                     (not (seq-some (lambda (r) (looking-at r))
                                    titlecase-skip-words-regexps)))
            (capitalize-word 1)))))))

(defun titlecase--region-with-style (begin end style)
  "Title-case the region of English text from BEGIN to END, using STYLE."
  ;; It doesn't makes sense for this function to be interactive;
  ;; `titlecase-region' can now specify a style interactively.
  (save-excursion
    (save-match-data
      (while (< begin end)
        (goto-char begin)
        (let ((end-step
               (if (re-search-forward titlecase-force-cap-after-punc
                                      end :noerror)
                   (point)
                 end)))
          (if (memq (titlecase--region-with-style-impl begin end-step style)
                    '(skipped))
              (setq begin (point))
            (setq begin end-step)))))))

(defun titlecase--read-style ()
  "Read which title-case style to use from the minibuffer."
  (let ((choice (completing-read
                 "Title-case style: "
                 (mapcar #'cdr titlecase-styles)
                 nil t nil nil
                 (alist-get titlecase-style titlecase-styles))))
    (cl-loop for (s . n) in titlecase-styles
             if (equal n choice) return s)))

(defun titlecase--arg (style interactivep)
  "Process arguments to titlecase functions.
If STYLE is passed to a function in any way, use it; otherwise,
if INTERACTIVEP, prompt the user for a style to use.  As a
fall-back, use `titlecase-style'."
  (or style
      (and interactivep (titlecase--read-style))
      titlecase-style))

(defun titlecase--string (str style)
  "Run `titlecase-command' on STR with STYLE and return the result.
See the docstring for `titlecase-command' for its possible
values."
  (let (;; Remember the existing newlines
        (str-ending-newlines (replace-regexp-in-string
                              "\\`\\([^z-a]*?\\)\n*\\'" "" str nil nil 1)))
    (with-temp-buffer
      (insert str)
      (cond
       ((stringp titlecase-command)
        (call-process-region (point-min) (point-max) titlecase-command t t nil))
       ((listp titlecase-command)
        (apply #'call-process-region (point-min) (point-max)
               (car titlecase-command) t t nil
               (mapcar (lambda (s)
                         (format "%s" (if (eq s 'style) titlecase-style s)))
                       (cdr titlecase-command))))
       ((functionp titlecase-command)
        (funcall titlecase-command (point-min) (point-max) style)))
      ;; Ensure that the string has no extra trailing whitespace.
      (goto-char (point-max))            ; Go to the end of the buffer
      (newline)                          ; Ensure at least one newline
      (delete-blank-lines)               ; Delete all but the last newline
      (insert str-ending-newlines)       ; Replace the pre-existing newlines
      ;; Delete the extra newline and return the buffer as a string
      (buffer-substring (point-min) (1- (point-max))))))

(defun titlecase--lowercase-all-caps (begin end _style)
  "If the text from BEGIN to END is all-caps, downcase it."
  (goto-char begin)
  (unless (re-search-forward "[[:lower:]]" end :noerror)
    (downcase-region begin end)))

;;;###autoload
(defun titlecase-region (begin end &optional style interactivep)
  "Title-case the region of English text from BEGIN to END.
Uses the style provided in `titlecase-style', unless optional
STYLE is provided.

When called interactively , or when INTERACTIVEP is non-nil,
\\[universal-argument] \\[titlecase-region] will prompt the user
for the style to use."
  (interactive "*r\ni\nP")
  (atomic-change-group
    (let ((pt (point))
          (style (titlecase--arg style interactivep)))
      (insert (titlecase--string (delete-and-extract-region begin end) style))
      (goto-char pt))))

;;;###autoload
(defun titlecase-line (&optional point style interactivep)
  "Title-case the line at POINT.
Uses the style provided in `titlecase-style', unless optional
STYLE is provided.

When called interactively , or when INTERACTIVEP is non-nil,
POINT is the current point, and calling with
\\[universal-argument] \\[titlecase-line] will prompt the user
for the style to use."
  (interactive "d\ni\nP")
  (goto-char point)
  (let ((style (titlecase--arg style interactivep))
        (thing (bounds-of-thing-at-point 'line)))
    (titlecase-region (car thing) (cdr thing) style)
    (goto-char (1- (cdr thing)))))

;;;###autoload
(defun titlecase-sentence (&optional point style interactivep)
  "Title-case the sentence at POINT.
Uses the style provided in `titlecase-style', unless optional
STYLE is provided.

When called interactively , or when INTERACTIVEP is non-nil,
POINT is the current point, and calling with
\\[universal-argument] \\[titlecase-sentence] will prompt the
user for the style to use."
  (interactive "d\ni\nP")
  (goto-char point)
  (let ((style (titlecase--arg style interactivep))
        (thing (bounds-of-thing-at-point 'sentence)))
    (titlecase-region (car thing) (cdr thing) style)
    (goto-char (cdr thing))))

;;;###autoload
(defun titlecase-dwim (&optional style interactivep)
  "Title-case either the region, if active, or the current line.
Uses the style provided in `titlecase-style', unless optional
STYLE is provided.

When called interactively with \\[universal-argument]
\\[titlecase-dwim], or when INTERACTIVEP is non-nil, prompt the
user for the style to use."
  (interactive "i\nP")
  (let ((style (titlecase--arg style interactivep)))
    (if (region-active-p)
        (titlecase-region (region-beginning) (region-end) style)
      (funcall titlecase-dwim-non-region-function (point) style))))

(provide 'titlecase)
;;; titlecase.el ends here
