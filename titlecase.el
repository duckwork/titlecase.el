;;; titlecase.el --- Title-case phrases -*- lexical-binding: t; -*-

;; Author: Case Duckworth <acdw@acdw.net>
;; Version: 0.1.0
;; URL: https://github.com/duckwork/titlecase.el
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Adapted from https://hungyi.net/posts/programmers-way-to-title-case/ and
;; https://github.com/novoid/title-capitalization.el, and with rules from
;; https://capitalizemytitle.com/#capitalizationrules

;; It turns out, capitalizing titles is Hard.  This library only does it in
;; English, and even then, it's pretty jankily put-together.  It's due, mostly,
;; to the fact that many words can mean many different things, depending, and I
;; don't want to put a whole dictionary in this package just to make
;; capitalizing headings a little easier.  I'll expound more on this in the USE
;; section, below.

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

(defgroup titlecase nil
  "Customizations for titlecasing phrases."
  :prefix "titlecase-"
  :group 'text)

;;; Lists of words /never/ to capitalize

(defvar titlecase-prepositions
  ;; This was pulled from Wikipedia, and so is somewhat weird.
  ;; https://en.wikipedia.org/wiki/List_of_English_prepositions
  '("'thout" "'tween" "aboard" "about" "above"
    "abreast" "absent" "abt."  "across" "after" "against" "ago" "aloft" "along"
    "alongside" "amid" "amidst" "among" "amongst" "anti" "apart" "apropos"
    "around" "as" "aside" "aslant" "astride" "at" "atop" "away" "before"
    "behind" "below" "beneath" "beside" "besides" "between" "beyond" "but" "by"
    "c." "ca." "circa" "come" "concerning" "contra" "counting" "cum" "despite"
    "down" "during" "effective" "ere" "except" "excepting" "excluding" "failing"
    "following" "for" "from" "hence" "in" "including" "inside" "into" "less"
    "like" "mid" "midst" "minus" "mod" "modulo" "near" "nearer" "nearest"
    "neath" "next" "notwithstanding" "o'" "o'er" "of" "off" "on"
    "onto" "ontop" "opposite" "out" "outside" "over" "pace" "past" "pending"
    "per" "plus" "post" "pre" "pro" "qua" "re" "regarding" "respecting" "round"
    "sans" "save" "saving" "short" "since" "sub" "t'" "than" "through"
    "throughout" "thru" "thruout" "till" "times" "to" "toward" "towards" "under"
    "underneath" "unlike" "until" "unto" "up" "upon" "v." "versus" "via"
    "vis-à-vis" "vs." "w."  "w/" "w/i" "w/o" "wanting" "with" "within"
    "without")
  "List of prepositions in English.
This list is, by necessity, incomplete, even though prepositions
are a closed lexical group in the English language.  This list
was pulled and culled from
https://en.wikipedia.org/wiki/List_of_English_prepositions.")

(defvar titlecase-articles '("a" "an" "the")
  "List of articles in English.")

(defvar titlecase-coordinating-conjunctions '("for" "and" "nor" "but" "or"
                                              "yet" "so")
  "List of coordinating conjunctions in English.")

(defvar titlecase-lowercase-chicago (append titlecase-articles
                                            titlecase-prepositions
                                            titlecase-coordinating-conjunctions)
  "Words to lowercase in Chicago Style.
Include: articles, coordinating conjunctions, prepositions, and
\"to\" in an infinitive (though that's caught as a preposition).")

(defvar titlecase-lowercase-apa (append titlecase-articles
                                        (seq-filter (lambda (p)
                                                      (< (length p) 4))
                                                    titlecase-prepositions))
  "Words to lowercase in APA Style.")

(defvar titlecase-lowercase-mla (append titlecase-articles
                                        titlecase-prepositions
                                        titlecase-coordinating-conjunctions)
  "Words to lowercase in MLA Style.")

(defvar titlecase-lowercase-ap (append titlecase-articles
                                       (seq-filter (lambda (p)
                                                     (< (length p) 4))
                                                   titlecase-prepositions)
                                       (seq-filter
                                        (lambda (p)
                                          (< (length p) 4))
                                        titlecase-coordinating-conjunctions))
  "Words to lowercase in AP Style.")

(defvar titlecase-lowercase-bluebook (append titlecase-articles
                                             titlecase-coordinating-conjunctions
                                             (seq-filter
                                              (lambda (p)
                                                (< (length p) 4))
                                              titlecase-prepositions))
  "Words to lowercase in Bluebook Style.")

(defvar titlecase-lowercase-ama (append titlecase-articles
                                        titlecase-coordinating-conjunctions
                                        (seq-filter (lambda (p)
                                                      (< (length p) 4))
                                                    titlecase-prepositions))
  "Words to lowercase in AMA Style.")

(defvar titlecase-lowercase-nyt (append titlecase-articles
                                        titlecase-prepositions
                                        titlecase-coordinating-conjunctions)
  "Words to lowercase in New York Times Style.")

(defvar titlecase-lowercase-wikipedia
  (append titlecase-articles
          (seq-filter (lambda (p) (< (length p) 5)) titlecase-prepositions)
          titlecase-coordinating-conjunctions)
  "Words to lowercase in Wikipedia Style.")

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
    (goto-char begin)
    ;; If the region is in ALL-CAPS, normalize it first
    (unless (re-search-forward "[a-z]" end :noerror)
      (downcase-region begin end))
    (goto-char begin)                   ; gotta go back to the beginning
    (let (;; Constants during this function's runtime
          (case-fold-search nil)
          (downcase-word-list (symbol-value
                               (intern (format "titlecase-lowercase-%s"
                                               style))))
          ;; State variables
          (this-word (current-word))
          (force-capitalize t))
      ;; And loop over the rest
      (while (< (point) end)
        (setq this-word (current-word))
        (cond
         ;; Skip ALL-CAPS words
         ((string-match "^[A-Z]+$" this-word) (forward-word 1))
         ;; Force capitalization if `force-capitalize' is t
         (force-capitalize (progn (setq force-capitalize nil)
                                  (capitalize-word 1)))
         ;; Special rules for different styles
         ((and (eq style 'ap)
               (> (length this-word) 3))
          (capitalize-word 1))
         ((eq style 'sentence)
          ;; Sentence style just capitalizes the first word.  Since we can't be
          ;; sure how the user has already capitalized anything, we just skip
          ;; the current word.
          (forward-word 1))
         ;; Downcase words that should be
         ((member (downcase this-word) downcase-word-list)
          (downcase-word 1))
         ;; Otherwise, do the default function on the word
         (t (funcall titlecase-default-case-function 1)))
        ;; If the word ends with a :, ., ?, newline, or carriage-return, force
        ;; the next word to be capitalized.
        (when (looking-at titlecase-force-cap-after-punc)
          (setq force-capitalize t))
        (skip-syntax-forward "^w" end))
      ;; Capitalize the last word, only in some styles
      (when (memq style '(chicago ap bluebook ama nyt wikipedia))
        (backward-word 1)
        (when (and (>= (point) begin))
          (capitalize-word 1))))))

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
