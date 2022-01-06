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

;; It turns out, capitalizing titles is Hard.  This library only does it in
;; English, and even then, it's pretty jankily put-together.  It's due, mostly,
;; to the fact that many words can mean many different things, depending, and I
;; don't want to put a whole dictionary in this package just to make
;; capitalizing headings a little easier.  I'll expound more on this in the USE
;; section, below.

;; Prior art:

;; - https://emacs.stackexchange.com/questions/66361/#66362
;; - https://github.com/novoid/title-capitalization.el
;; - https://hungyi.net/posts/programmers-way-to-title-case/

;; Rules:

;; - https://capitalizemytitle.com/#capitalizationrules
;; - https://titlecaseconverter.com/rules/

;; INSTALLATION:

;; Make sure both titlecase.el and titlecase-data.el are in your `load-path',
;; and `require' as per usual.

;; USE:

;; The only setting you really should need to set is `titlecase-style', which
;; see.  Each of these styles has a different set of rules regarding which words
;; to capitalize in a title.  After you've set `titlecase-style', you can bind
;; the command `titlecase-dwim' to a key, or call it using M-x, and it will
;; either title-case your region (if it's active) or the current line.

;; The tricky part is figuring out what words to capitalize in the title.

;; Articles ("a", "an", "the") are down-cased.

;; The first word of a title and all "important words" (generally nouns,
;; pronouns, adjectives, verbs, and adverbs) are capitalized.  The last word of
;; a title is always capitalized, but only in Chicago, AP, Bluebook, AMA, NY
;; Times, and Wikipedia.

;; /All/ prepositions are down-cased in Chicago, MLA, AP, NY Times, and
;; Wikipedia, regardless of length; for APA, Bluebook, AMA, and Wikipedia, only
;; prepositions shorter than 5 letters are (presumably, capitalize those longer
;; than 5 letters, however only Wikipedia was clear on that point).

;; Coordinating conjunctions are capitalized in Chicago and APA (presumably),
;; but down-cased in MLA, AP, Bluebook, AMA, NY Times, and Wikipedia.

;; Hyphenated words are tricky: I could possibly figure out a way to have lookup
;; tables to determine when to capitalize the second part of a hyphenated word,
;; but I haven't implemented them yet.  At any rate, the rules tend to be vague
;; enough that it's hard to program anyway: For example, Chicago, APA, MLA, and
;; AP lowercase the second word "after a hyphenated prefix (e.g., Mid-, Anti-,
;; Super, etc.) in compound modifiers," but MLA and APA capitalize the second
;; part of "hyphenated major words (e.g., Self-Report not Self-report).

;; Perhaps unsurprisingly, the AMA (American Medical Association, used in the
;; scientific community) has the most comprehensive capitalization rules around
;; hyphenated words.  I'll just copy-paste the bullet points here:

;; - Lowercase the second word in a hyphenated compound when it is a prefix or
;;   suffix (e.g., "Anti-itch","world-wide") or part of a single word.
;; - Capitalize the second word in a hyphenated compound if both words are equal
;;   and not suffices or prefixes (e.g., "Cost-Benefit")
;; - Capitalize the first non-Greek letter after a lowercase Greek letter (e.g.,
;;   "ω-Bromohexanoic")
;; - Lowercase the first non-Greek letter after a capital Greek letter (e.g.,
;;   "Δ-9-tetrahydrocannabinol")

;; (The AMA also has a rule about capitalizing the genus but not species
;; epithet, but the lookup on that would be wild as hell,
;; so I trust you all to know on that one.)

;; "To" as an infinitive is down-cased in all /except/ AP.  This is a rule I
;; simply cannot implement without knowing whether the /next/ word is a verb,
;; which would require expensive lookups, which even then wouldn't be foolproof.

;; Now that I'm thinking about it, most styles count phrasal verbs (like "play
;; with") as important enough to capitalize, when "with" would usually /not/ be
;; capitalized, but again, open categories like phrasal verbs simply do not work
;; in a package like this.

;; ALL OF THIS IS TO SAY that title-case offers a best-effort attempt to
;; title-case a line or region of text, but you should absolutely
;; double-triple-check against the style guide you're writing for if you're
;; trying for publication or something like that.

;; FURTHER CUSTOMIZATION

;; While `titlecase-style' /should/ be the only customization option you need to
;; tweak, it wouldn't be Emacs if there weren't many more.

;; Most of these style guides fail to specify what to do when a
;; word does /not/ fit one of the criteria listed.  The customization option
;; `titlecase-default-case-function' controls what to do there.

;; None of the styles specify what to do after certain punctuation marks like
;; . : ? etc.  Some do specify "capitalize the first word of ... any
;; subtitle/subheading," which might refer to a word after :, but I'm not sure.
;; Anyway, you can customize `titlecase-force-cap-after-punc' to a regexp which,
;; if it matches after a word, will force the next word to be capitalized.

;; For even more customization, of course, you can tweak this file's `defvar'
;; values, but There Be Monsters.

;; Finally, there's the `titlecase-style' `sentence' if you want some
;; long-asked-for sanity in your titles.

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

(defcustom titlecase-style 'chicago
  "Which style to use when title-casing."
  :type (cons 'choice (cl-loop
                       for style in titlecase-styles
                       collect (list 'const :tag (cdr style) (car style))
                       into choices
                       finally return choices)))

(defcustom titlecase-default-case-function #'capitalize-word
  "What to do to a word when a style doesn't specify what to do."
  :type 'function)

(defcustom titlecase-force-cap-after-punc "[.?!\\/;\\:\n\r]"
  "Regexp to force the next word capitalized."
  :type 'regexp)

(defun titlecase--region-with-style-impl (begin end style)
  "Title-case implementation.
`titlecase-force-cap-after-punc' must be handled by the caller.
This is expected to have run in a block that uses `save-excursion' and
`save-match-data'.  See documentation for `titlecase-region-with-style'
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

(defun titlecase-region-with-style (begin end style)
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

;;;###autoload
(defun titlecase-region (begin end &optional style interactivep)
  "Title-case the region of English text from BEGIN to END.
Uses the style provided in `titlecase-style', unless optional
STYLE is provided.

When called interactively, \\[universal-argument] \\[titlecase-region]
will prompt the user for the style to use."
  (interactive "*r\ni\nP")
  (let ((style (or style
                   (and interactivep (titlecase--read-style))
                   titlecase-style)))
    (titlecase-region-with-style begin end style)))

;;;###autoload
(defun titlecase-dwim ()
  "Title-case either the region, if active, or the current line."
  (interactive)
  (if (region-active-p)
      (titlecase-region (region-beginning) (region-end))
    (titlecase-region (point-at-bol) (point-at-eol))))

;;;###autoload
(defun titlecase-dwim (&optional style interactive)
  "Title-case either the region, if active, or the current line.
Uses the style provided in `titlecase-style', unless optional
STYLE is provided.

When called interactively with \\[universal-argument] \\[titlecase-dwim],
prompt the user for the style to use."
  (interactive "i\nP")
  (let ((style (or style
                   (and interactivep (titlecase--read-style))
                   titlecase-style)))
    (if (region-active-p)
              (titlecase-region (region-beginning) (region-end) style)
            (titlecase-line style))))

(provide 'titlecase)
;;; titlecase.el ends here
