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

;;; Code:

(require 'cl-lib)
(require 'seq)
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

(defcustom titlecase-style 'wikipedia
  "Which style to use when title-casing."
  :type (cons 'choice (cl-loop
                       for style in titlecase-styles
                       collect (list 'const :tag (cdr style) (car style))
                       into choices
                       finally return choices)))

(defcustom titlecase-force-cap-after-punc "[.?!\\/;\\:\n\r]"
  "Regexp to force the next word capitalized."
  :type 'regexp)

(defcustom titlecase-dwim-non-region-function #'titlecase-line
  "What to do with `titlecase-dwim' when region isn't active.
Recommended: `titlecase-line' or `titlecase-sentence'."
  :type 'function)

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

    ;; If the region is in ALL-CAPS, normalize it first.
    (unless (re-search-forward "[[:lower:]]" end :noerror)
      (downcase-region begin end))

    ;; Skip blank lines & white-space (where `current-word' would return nil).
    ;; It's important this uses the same logic that `current-word' uses to scan
    ;; for words, or this may be nil when it's not expected. See #11.
    (goto-char begin)
    (skip-syntax-forward "^w" end)
    (setq begin (point))

    ;; And loop over the rest.
    (while (< (point) end)
      (let ((this-word (current-word)))
        (cond
         ;; Skip ALL-CAPS words.
         ((string-match-p "^[[:upper:]]+$" this-word)
          (forward-word 1))
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
            (while (and this-word (member (downcase this-word)
                                          (mapcar #'car-safe next-words)))
              (capitalize-word 1)
              (skip-syntax-forward "^w" end)
              (setq this-word (current-word)
                    next-words (mapcar #'cdr-safe next-words)))
            (unless (seq-some 'null next-words)
              ;; If it's not a phrasal verb, bail --- but still capitalize the
              ;; first word!
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
           ;; Sentence style just capitalizes the first word.  Since we can't
           ;; be sure how the user has already capitalized anything, we just
           ;; skip the current word.
           (eq style 'sentence)
           ;; None of the styles require a capital letter after an apostrophe.
           (eq (char-before (point)) ?')
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
      (skip-syntax-forward "^w" end))

    ;; Capitalize the last word, only in some styles.
    (when (memq style titlecase-styles-capitalize-last-word)
      (backward-word 1)
      (when (>= (point) begin)
        (capitalize-word 1)))))

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
          (titlecase--region-with-style-impl begin end-step style)
          (setq begin end-step))))))

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

;;;###autoload
(defun titlecase-region (begin end &optional style interactivep)
  "Title-case the region of English text from BEGIN to END.
Uses the style provided in `titlecase-style', unless optional
STYLE is provided.

When called interactively, \\[universal-argument] \\[titlecase-region]
will prompt the user for the style to use."
  (interactive "*r\ni\nP")
  (let ((style (titlecase--arg style interactivep)))
    (titlecase--region-with-style begin end style)))

;;;###autoload
(defun titlecase-line (&optional point style interactivep)
  "Title-case the line at POINT.
Uses the style provided in `titlecase-style', unless optional
STYLE is provided.

When called interactively, POINT is the current point, and
calling with \\[universal-argument] \\[titlecase-line] will
prompt the user for the style to use."
  (interactive "d\ni\nP")
  (let ((style (titlecase--arg style interactivep)))
    (save-excursion
      (goto-char point)
      (titlecase-region (line-beginning-position) (line-end-position) style))))

;;;###autoload
(defun titlecase-sentence (&optional point style interactivep)
  "Title-case the sentence at POINT.
Uses the style provided in `titlecase-style', unless optional
STYLE is provided.

When called interactively, POINT is the current point, and
calling with \\[universal-argument] \\[titlecase-sentence] will
prompt the user for the style to use."
  (interactive "d\ni\nP")
  (let ((style (titlecase--arg style interactivep)))
    (save-excursion
      (goto-char point)
      (titlecase-region (progn (backward-sentence)
                               (point))
                        (progn (forward-sentence)
                               (point))
                        style))))

;;;###autoload
(defun titlecase-dwim (&optional style interactive)
  "Title-case either the region, if active, or the current line.
Uses the style provided in `titlecase-style', unless optional
STYLE is provided.

When called interactively with \\[universal-argument] \\[titlecase-dwim],
prompt the user for the style to use."
  (interactive "i\nP")
  (let ((style (titlecase--arg style interactivep)))
    (if (region-active-p)
              (titlecase-region (region-beginning) (region-end) style)
      (funcall titlecase-dwim-non-region-function (point) style))))

(provide 'titlecase)
;;; titlecase.el ends here
