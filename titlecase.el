;;; titlecase.el --- Title-case phrases -*- lexical-binding: t; -*-

;; Author: Case Duckworth <acdw@acdw.net>
;; Maintainer: Case Duckworth <acdw@acdw.net>
;; Version: 0.2.0
;; URL: https://github.com/duckwork/titlecase.el
;; Package-Requires: ((emacs "25.1"))

;;; License:

;; Everyone is permitted to do whatever they like with this software
;; without limitation.  This software comes without any warranty
;; whatsoever, but with two pieces of advice:
;; - Be kind to yourself.
;; - Make good choices.

;;; Commentary:

;; It turns out, capitalizing titles is Hard.  This library only does it in
;; English, and even then, it's pretty jankily put-together.  It's due, mostly,
;; to the fact that many words can mean many different things, depending, and I
;; don't want to put a whole dictionary in this package just to make
;; capitalizing headings a little easier.  I'll expound more on this in the USE
;; section, below.

;; Adapted from https://hungyi.net/posts/programmers-way-to-title-case/ and
;; https://github.com/novoid/title-capitalization.el, and with rules from
;; https://capitalizemytitle.com/#capitalizationrules

;; More prior art:

;; - https://emacs.stackexchange.com/questions/66361/#66362

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

;; Articles ("a", "an", "the") are downcased.

;; The first word of a title and all "important words" (generally nouns,
;; pronouns, adjectives, verbs, and adverbs) are capitalized.  The last word of
;; a title is always capitalized, but only in Chicago, AP, Bluebook, AMA, NY
;; Times, and Wikipedia.

;; /All/ prepositions are downcased in Chicago, MLA, AP, NY Times, and
;; Wikipedia, regardless of length; for APA, Bluebook, AMA, and Wikipedia, only
;; prepositions shorter than 5 letters are (presumably, capitalize those longer
;; than 5 letters, however only Wikipedia was clear on that point).

;; Coordinating conjunctions are capitalized in Chicago and APA (presumably),
;; but downcased in MLA, AP, Bluebook, AMA, NY Times, and Wikipedia.

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

;; (The AMA also has a rule about capitilizing the genus but not species
;; epithet, but the lookup on that would be wild as hell, so I trust yall to
;; know on that one.)

;; "To" as an infinitive is downcased in all /except/ AP.  This is a rule I
;; simply cannot implement without knowing whether the /next/ word is a verb,
;; which would require expensive lookups, which even then wouldn't be foolproof.

;; Now that I'm thinking about it, most styles count phrasal verbs (like "play
;; with") as important enough to capitalize, when "with" would usually /not/ be
;; capitalized, but again, open categories like phrasal verbs simply do not work
;; in a package like this.

;; ALL OF THIS IS TO SAY that titlecase offers a best-effort attempt to
;; titlecase a line or region of text, but you should absolutely
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

;; Finally, there's the titlecase-style `sentence' if you want some
;; long-asked-for sanity in your titles.

;;; Code:

(require 'seq)
(require 'titlecase-data)

(defgroup titlecase nil
  "Customizations for titlecasing phrases."
  :prefix "titlecase-"
  :group 'text)

(defcustom titlecase-style 'chicago
  "Which style to use when titlecasing."
  :type '(choice (const :tag "Chicago Style" chicago)
                 (const :tag "APA Style" apa)
                 (const :tag "MLA Style" mla)
                 (const :tag "AP Style" ap)
                 (const :tag "Bluebook Style" bluebook)
                 (const :tag "AMA Style" ama)
                 (const :tag "New York Times Style" nyt)
                 (const :tag "Wikipedia Style" wikipedia)
                 (const :tag "Sentence style" sentence)))

(defcustom titlecase-default-case-function #'capitalize-word
  "What to do to a word when a style doesn't specify what to do."
  :type 'function)

(defcustom titlecase-force-cap-after-punc "[:.?;\n\r]"
  "Regexp to force the next word capitalized."
  :type 'regexp)

(defun titlecase-region-with-style (begin end style)
  "Titlecase the region of English text from BEGIN to END, using STYLE."
  (interactive "*r")
  (save-excursion
    (save-match-data
      (goto-char begin)
      (let (;; Constants during this function's runtime
            (case-fold-search nil)
            (downcase-word-list (symbol-value
                                 (intern (format "titlecase-lowercase-%s"
                                                 style))))
            ;; State variables
            (this-word (current-word))
            (force-capitalize t))
        ;; If the region is in ALL-CAPS, normalize it first
        (unless (re-search-forward "[[:lower:]]" end :noerror)
          (downcase-region begin end))
        (goto-char begin)                   ; gotta go back to the beginning
        ;; And loop over the rest
        (while (< (point) end)
          (setq this-word (current-word))
          (cond
           ;; Skip ALL-CAPS words
           ((string-match "^[[:upper:]]+$" this-word)
            (forward-word 1))
           ;; Phrasal verbs!
           ((and (memq style titlecase-styles-capitalize-phrasal-verbs)
                 (member (downcase this-word)
                         (mapcar #'car titlecase-phrasal-verbs)))
            ;; We need to do a little state machine thingy here
            (let ((next-words (assoc this-word titlecase-phrasal-verbs))
                  (bail-pt (point)))
              ;; Take care of the first word --- this is inelegant.
              (capitalize-word 1)
              (skip-syntax-forward "^w" end)
              (setq this-word (current-word))
              ;; Loop through the rest
              (while (member (downcase this-word)
                             (mapcar #'car-safe next-words))
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
           ;; Force capitalization if `force-capitalize' is t
           (force-capitalize
            (capitalize-word 1))
           ;; AP capitalizes /all/ words longer than 3 letters
           ((and (memq style titlecase-styles-capitalize-non-short-words)
                 (> (length this-word) titlecase-short-word-length))
            (capitalize-word 1))
           ;; Skip the next word if ...
           ((or
             ;; Sentence style just capitalizes the first word.  Since we can't
             ;; be sure how the user has already capitalized anything, we just
             ;; skip the current word.
             (eq style 'sentence)
             ;; None of the styles require a capital letter after an
             ;; apostrophe.
             (eq (char-before (point)) ?')
             ;; FIXME: Hyphens are a completely different story with
             ;; capitalization.
             (eq (char-before (point)) ?-))
            (forward-word 1))
           ;; Downcase words that should be
           ((member (downcase this-word) downcase-word-list)
            (downcase-word 1))
           ;; Otherwise, do the default function on the word
           (t (funcall titlecase-default-case-function 1)))
          ;; If the word ends with a :, ., ?, newline, or carriage-return,
          ;; force the next word to be capitalized.
          (setq force-capitalize (looking-at titlecase-force-cap-after-punc))
          (skip-syntax-forward "^w" end))
        ;; Capitalize the last word, only in some styles
        (when (memq style titlecase-styles-capitalize-last-word)
          (backward-word 1)
          (when (and (>= (point) begin))
            (capitalize-word 1)))))))

;;;###autoload
(defun titlecase-region (begin end)
  "Titlecase the region of English text from BEGIN to END.
Uses the style provided in `titlecase-style'."
  (interactive "*r")
  (titlecase-region-with-style begin end titlecase-style))

;;;###autoload
(defun titlecase-dwim ()
  "Titlecase either the region, if active, or the current line."
  (interactive)
  (if (region-active-p)
      (titlecase-region (region-beginning) (region-end))
    (titlecase-region (point-at-bol) (point-at-eol))))

(provide 'titlecase)
;;; titlecase.el ends here
