;;; titlecase-data.el --- Data for titlecase.el -*- lexical-binding: t; -*-

;; Author: Case Duckworth <acdw@acdw.net>
;; Maintainer: Case Duckworth <acdw@acdw.net>
;; URL: https://github.com/duckwork/titlecase.el

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

;; Since the `titlecase' package requires a lot of data, that data lives here so
;; as to not clog up the main package.

;; Since [[https://github.com/duckwork/titlecase.el/issues/23][Issue #23]] makes
;; a good point that I should like, make more sense in the commentary and README
;; of this repository.  At the same time, those couple of comments I wrote in
;; there I don't want to just /delete/, so until I write this up in a proper
;; blog post, I've included it here, in the data file, because this is where
;; these implementation notes will be of most interest.

;; The only setting you really should need to set is =titlecase-style=, which
;; see.  Each of these styles has a different set of rules regarding which words
;; to capitalize in a title.  After you've set =titlecase-style=, you can bind
;; the command =titlecase-dwim= to a key, or call it using M-x, and it will
;; either title-case your region (if it's active) or the current line.

;; The tricky part is figuring out what words to capitalize in the title.

;; Articles (~a~, ~an~, ~the~) are downcased.

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

;; - Lowercase the second word in a hyphenated compound when it is
;; a prefix or suffix (e.g., "Anti-itch","world-wide") or part of a single word.
;; - Capitalize the second word in a hyphenated compound if both words are equal
;; and not suffices or prefixes (e.g., "Cost-Benefit")
;; - Capitalize the first non-Greek letter after a lowercase Greek letter (e.g.,
;; "ω-Bromohexanoic")
;; - Lowercase the first non-Greek letter after a capital Greek letter (e.g.,
;; "Δ-9-tetrahydrocannabinol") #+end_quote

;; (The AMA also has a rule about capitilizing the genus but not species
;; epithet, but the lookup on that would be wild as hell, so I trust yall to
;; know on that one.)

;; ~To~ as an infinitive is downcased in all /except/ AP.  This is a rule I
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

;; SEE ALSO:

;; Prior art:

;; - https://emacs.stackexchange.com/questions/66361/#66362
;; - https://github.com/novoid/title-capitalization.el
;; - https://hungyi.net/posts/programmers-way-to-title-case/

;; Rules:

;; - https://capitalizemytitle.com/#capitalizationrules
;; - https://titlecaseconverter.com/rules/

;;; Code:

(require 'seq)

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

(defvar titlecase-phrasal-verbs
  (let (verbs)
    (dolist
        (phrase
         '(("account" "for") ("ache" "for") ("act" "on")
           ("act" "out") ("act" "up") ("add" "on") ("add" "up")
           ("add" "up" "to") ("aim" "at") ("allow" "for") ("angle" "for")
           ("answer" "back") ("answer" "for") ("argue" "out") ("ask" "after")
           ("ask" "around") ("ask" "for") ("ask" "in") ("ask" "out")
           ("ask" "over") ("ask" "round") ("auction" "off") ("back" "away")
           ("back" "down") ("back" "off") ("back" "out") ("back" "out" "of")
           ("back" "up") ("bag" "out") ("bail" "out") ("bail" "out" "on")
           ("bail" "up") ("ball" "up") ("balls" "up") ("bang" "about")
           ("bang" "around") ("bang" "on" "about") ("bang" "out") ("bang" "up")
           ("bank" "on") ("barge" "in") ("barge" "into") ("bash" "about")
           ("bash" "in") ("bash" "out") ("bash" "up") ("bawl" "out")
           ("be" "after") ("be" "along") ("be" "away") ("be" "cut" "out" "for")
           ("be" "cut" "up") ("be" "down") ("be" "down" "with")
           ("be" "fed" "up") ("be" "in") ("be" "in" "on") ("be" "not" "on")
           ("be" "off") ("be" "on") ("be" "on" "about") ("be" "onto")
           ("be" "out") ("be" "out" "of") ("be" "out" "to")
           ("be" "snowed" "under") ("be" "taken" "aback") ("be" "taken" "with")
           ("be" "up") ("be" "up" "to") ("bear" "down" "on") ("bear" "on")
           ("bear" "out") ("bear" "up") ("bear" "up" "under") ("bear" "with")
           ("beat" "down") ("beat" "up") ("beaver" "away")
           ("beaver" "away" "at") ("bed" "down") ("bed" "out") ("beef" "up")
           ("belt" "out") ("belt" "up") ("bend" "down") ("bend" "over")
           ("bend" "over" "backwards") ("black" "out") ("blank" "out")
           ("blare" "out") ("blast" "off") ("blaze" "away") ("bliss" "out")
           ("block" "in") ("block" "off") ("block" "out") ("block" "up")
           ("blow" "away") ("blow" "down") ("blow" "off") ("blow" "out")
           ("blow" "over") ("blow" "up") ("blurt" "out") ("bog" "down")
           ("bog" "in") ("bog" "into") ("bog" "off") ("boil" "down")
           ("boil" "down" "to") ("boil" "over") ("boil" "up")
           ("bone" "up" "on") ("book" "in") ("book" "into") ("book" "up")
           ("boot" "up") ("border" "on") ("boss" "about") ("boss" "around")
           ("botch" "up") ("bottle" "away") ("bottle" "out") ("bottle" "up")
           ("bottom" "out") ("bounce" "into") ("bounce" "off") ("bowl" "over")
           ("box" "in") ("box" "up") ("branch" "out") ("break" "away")
           ("break" "down") ("break" "in") ("break" "off") ("break" "out" "of")
           ("break" "through") ("break" "up") ("breeze" "along")
           ("breeze" "in") ("breeze" "into") ("breeze" "through")
           ("brighten" "up") ("bring" "about") ("bring" "along")
           ("bring" "around") ("bring" "back") ("bring" "down")
           ("bring" "forth") ("bring" "forward") ("bring" "in") ("bring" "on")
           ("bring" "out") ("bring" "out" "in") ("bring" "round")
           ("bring" "up") ("brush" "off") ("brush" "up") ("bubble" "over")
           ("bucket" "down") ("budge" "up") ("buff" "up") ("buff" "up" "on")
           ("bug" "off") ("bug" "out") ("build" "up") ("bulk" "out")
           ("bump" "into") ("bump" "off") ("bump" "up") ("bundle" "off")
           ("bundle" "out") ("bundle" "up") ("bunk" "off") ("buoy" "up")
           ("burn" "down") ("burn" "off") ("burn" "out") ("burst" "into")
           ("butt" "in") ("butt" "out") ("butter" "up") ("buy" "in")
           ("buy" "into") ("buy" "off") ("buy" "out") ("buy" "up")
           ("buzz" "around") ("buzz" "off") ("buzz" "off") ("call" "after")
           ("call" "around") ("call" "back") ("call" "for") ("call" "forth")
           ("call" "in") ("call" "off") ("call" "on") ("call" "round")
           ("call" "up") ("calm" "down") ("cancel" "out") ("care" "for")
           ("carried" "away") ("carry" "forward") ("carry" "off")
           ("carry" "on") ("carry" "on" "with") ("carry" "out")
           ("carry" "over") ("carry" "through") ("cart" "off") ("cash" "in")
           ("cash" "in" "on") ("cash" "out") ("cash" "up") ("catch" "at")
           ("catch" "on") ("catch" "out") ("catch" "up") ("catch" "up" "on")
           ("catch" "up" "with") ("cater" "for") ("cater" "to") ("cave" "in")
           ("chalk" "out") ("chalk" "up") ("chalk" "up" "to") ("chance" "upon")
           ("change" "over") ("charge" "up") ("charge" "with") ("chase" "down")
           ("chase" "off") ("chase" "up") ("chat" "up") ("cheat" "on")
           ("cheat" "out" "of") ("check" "by") ("check" "in") ("check" "into")
           ("check" "off") ("check" "out") ("check" "out" "of")
           ("check" "over") ("cheer" "on") ("cheer" "up") ("chew" "on")
           ("chew" "out") ("chew" "over") ("chew" "up") ("chicken" "out")
           ("chill" "out") ("chime" "in") ("chip" "away" "at") ("chip" "in")
           ("choose" "up") ("chop" "down") ("chop" "up") ("chuck" "away")
           ("chuck" "in") ("chuck" "out") ("chuck" "up") ("churn" "out")
           ("clag" "up") ("clam" "up") ("clamp" "down" "on") ("claw" "back")
           ("clean" "out") ("clean" "up") ("clear" "away") ("clear" "off")
           ("clear" "out") ("clear" "up") ("click" "through") ("climb" "down")
           ("clog" "up") ("close" "down") ("close" "in") ("close" "in" "on")
           ("close" "in" "upon") ("close" "off") ("close" "on") ("close" "out")
           ("close" "up") ("cloud" "over") ("clown" "about") ("clown" "around")
           ("cock" "up") ("color" "up") ("colour" "up") ("come" "about")
           ("come" "across") ("come" "apart") ("come" "before") ("come" "by")
           ("come" "down") ("come" "down" "on") ("come" "down" "with")
           ("come" "forth") ("come" "forth" "with") ("come" "from")
           ("come" "in") ("come" "into") ("come" "into" "use") ("come" "off")
           ("come" "off" "it") ("come" "on") ("come" "out") ("come" "out" "in")
           ("come" "out" "with") ("come" "over") ("come" "round")
           ("come" "through") ("come" "through" "with") ("come" "to")
           ("come" "up") ("come" "up" "against") ("come" "up" "with")
           ("come" "upon") ("conjure" "up") ("conk" "out") ("contract" "in")
           ("contract" "out") ("contract" "out" "of") ("cool" "down")
           ("coop" "up") ("cop" "it") ("cop" "off") ("cop" "out")
           ("cotton" "on") ("could" "do" "with") ("count" "in") ("count" "on")
           ("count" "out") ("count" "up") ("cozy" "up") ("cozy" "up" "to")
           ("crack" "down" "on") ("crack" "on") ("crack" "up") ("crank" "out")
           ("crank" "up") ("crash" "out") ("creep" "in") ("creep" "into")
           ("creep" "out") ("creep" "out" "on") ("creep" "over")
           ("creep" "up" "on") ("crop" "up") ("cross" "off") ("cross" "out")
           ("crumb" "down") ("cry" "off") ("cry" "out") ("cut" "across")
           ("cut" "back") ("cut" "back" "on") ("cut" "down")
           ("cut" "down" "on") ("cut" "in") ("cut" "it" "out") ("cut" "off")
           ("cut" "out") ("cut" "out" "on") ("cut" "up") ("dash" "off")
           ("dawn" "on") ("die" "away") ("die" "down") ("die" "for")
           ("die" "off") ("die" "out") ("dig" "in") ("dig" "into") ("dig" "up")
           ("dine" "out") ("dine" "out" "on") ("dip" "in") ("dip" "into")
           ("dip" "out") ("disagree" "with") ("dish" "out") ("dish" "up")
           ("dive" "in") ("dive" "into") ("divvy" "out") ("divvy" "up")
           ("do" "away" "with") ("do" "out" "of") ("do" "up") ("do" "without")
           ("doss" "about") ("doss" "around") ("doss" "down") ("doze" "off")
           ("drag" "on") ("draw" "back") ("draw" "down") ("draw" "in")
           ("draw" "into") ("draw" "on") ("draw" "out") ("draw" "up")
           ("dream" "of") ("dream" "up") ("dredge" "up") ("dress" "down")
           ("dress" "up") ("drift" "apart") ("drift" "off") ("drill" "down")
           ("drill" "down" "through") ("drill" "into") ("drink" "up")
           ("drive" "away") ("drive" "back") ("drive" "by") ("drive" "off")
           ("drive" "out") ("drive" "up") ("drone" "on") ("drop" "around")
           ("drop" "away") ("drop" "back") ("drop" "by") ("drop" "in")
           ("drop" "off") ("drop" "out") ("drop" "over") ("drop" "round")
           ("drop" "someone" "in" "it") ("drop" "through") ("drown" "in")
           ("drown" "out") ("drum" "into") ("drum" "out") ("drum" "up")
           ("duck" "out" "of") ("duff" "up") ("dumb" "down") ("dwell" "on")
           ("dwell" "upon") ("ease" "off") ("ease" "up") ("eat" "away")
           ("eat" "in") ("eat" "into") ("eat" "out") ("eat" "up")
           ("ebb" "away") ("edge" "out") ("edge" "up") ("egg" "on")
           ("eke" "out") ("embark" "on") ("embark" "upon") ("empty" "out")
           ("end" "in") ("end" "up") ("end" "up" "with") ("enter" "for")
           ("enter" "into") ("eye" "up") ("face" "off") ("face" "up" "to")
           ("faff" "about") ("faff" "around") ("fall" "about") ("fall" "apart")
           ("fall" "back") ("fall" "back" "on") ("fall" "behind")
           ("fall" "down") ("fall" "for") ("fall" "in") ("fall" "into")
           ("fall" "off") ("fall" "out") ("fall" "over") ("fall" "through")
           ("fall" "under") ("farm" "out") ("fart" "about") ("fart" "around")
           ("fasten" "down") ("fasten" "on") ("fasten" "onto") ("fasten" "up")
           ("fathom" "out") ("fatten" "up") ("fawn" "on") ("fawn" "over")
           ("feed" "off") ("feed" "on") ("feed" "up") ("feel" "up")
           ("feel" "up" "to") ("fend" "for") ("fend" "off") ("ferret" "out")
           ("fess" "up") ("fess" "up" "to") ("fiddle" "about")
           ("fiddle" "around") ("fiddle" "away") ("fight" "back")
           ("fight" "it" "out") ("fight" "off") ("figure" "on")
           ("figure" "out") ("file" "away") ("fill" "in") ("fill" "in" "for")
           ("fill" "in" "on") ("fill" "out") ("fill" "up") ("filter" "in")
           ("filter" "out") ("find" "out") ("finish" "off") ("fink" "on")
           ("fink" "out") ("fire" "away") ("fire" "off") ("fire" "up")
           ("firm" "up") ("fish" "for") ("fish" "out") ("fit" "in")
           ("fit" "into") ("fit" "up") ("fix" "up") ("fizzle" "out")
           ("flag" "down") ("flag" "up") ("flake" "out") ("flame" "out")
           ("flame" "up") ("flare" "out") ("flare" "up") ("flesh" "out")
           ("flick" "over") ("flick" "through") ("flip" "off") ("flip" "out")
           ("flip" "through") ("flog" "off") ("floor" "it") ("flounce" "off")
           ("flounce" "out") ("fly" "about") ("fly" "around") ("fly" "at")
           ("fly" "by") ("fly" "into") ("fob" "off") ("fob" "off" "on")
           ("fob" "off" "onto") ("fob" "off" "with") ("focus" "on")
           ("fold" "up") ("fool" "around") ("forge" "ahead") ("freak" "out")
           ("freeze" "out") ("freeze" "over") ("freeze" "up") ("freshen" "up")
           ("front" "for") ("front" "off") ("front" "onto") ("front" "out")
           ("front" "up") ("frown" "on") ("gad" "about") ("gad" "around")
           ("gag" "for") ("gang" "up") ("gang" "up" "on") ("gear" "up")
           ("geek" "out") ("get" "about") ("get" "above") ("get" "across")
           ("get" "across" "to") ("get" "after") ("get" "ahead")
           ("get" "ahead" "of") ("get" "along") ("get" "along" "in")
           ("get" "along" "with") ("get" "around") ("get" "around")
           ("get" "around" "to") ("get" "around" "to") ("get" "at")
           ("get" "away") ("get" "away" "from") ("get" "away" "with")
           ("get" "back") ("get" "back" "at") ("get" "back" "into")
           ("get" "back" "to") ("get" "back" "together") ("get" "behind")
           ("get" "behind" "with") ("get" "by") ("get" "by" "on")
           ("get" "by" "with") ("get" "down") ("get" "down" "on")
           ("get" "down" "to") ("get" "in") ("get" "in" "on")
           ("get" "in" "with") ("get" "into") ("get" "it") ("get" "it" "off")
           ("get" "it" "off" "with") ("get" "it" "on") ("get" "it" "on" "with")
           ("get" "it" "together") ("get" "it" "up") ("get" "off")
           ("get" "off" "it") ("get" "off" "on") ("get" "off" "with")
           ("get" "on") ("get" "on" "at") ("get" "on" "for") ("get" "on" "to")
           ("get" "on" "with") ("get" "onto") ("get" "out") ("get" "out" "of")
           ("get" "over") ("get" "over" "with") ("get" "round") ("get" "round")
           ("get" "round" "to") ("get" "through") ("get" "through" "to")
           ("get" "to") ("get" "together") ("get" "up") ("get" "up" "to")
           ("give") ("give" "away") ("give" "back") ("give" "in")
           ("give" "in" "to") ("give" "it" "to") ("give" "it" "up" "for")
           ("give" "it" "up" "to") ("give" "of") ("give" "off") ("give" "onto")
           ("give" "out") ("give" "over") ("give" "over" "to") ("give" "up")
           ("give" "up") ("give" "up" "on") ("give" "up" "to") ("give" "way")
           ("give" "way" "to") ("give" "yourself" "up")
           ("give" "yourself" "up" "to") ("gloss" "over") ("gnaw" "at")
           ("gnaw" "away" "at") ("go" "about") ("go" "across") ("go" "after")
           ("go" "against") ("go" "ahead") ("go" "ahead" "with")
           ("go" "along" "with") ("go" "around") ("go" "at") ("go" "away")
           ("go" "back") ("go" "back" "on") ("go" "before") ("go" "below")
           ("go" "by") ("go" "down") ("go" "down" "on") ("go" "down" "to")
           ("go" "down" "with") ("go" "for") ("go" "forth") ("go" "forward")
           ("go" "in") ("go" "in" "for") ("go" "in" "with") ("go" "into")
           ("go" "it") ("go" "it" "alone") ("go" "off") ("go" "off" "with")
           ("go" "on") ("go" "on" "about") ("go" "on" "at") ("go" "on" "to")
           ("go" "on" "with") ("go" "out") ("go" "out" "for") ("go" "out" "to")
           ("go" "out" "with") ("go" "over") ("go" "over" "to") ("go" "past")
           ("go" "round") ("go" "through") ("go" "through" "with")
           ("go" "together") ("go" "towards") ("go" "under") ("go" "up")
           ("go" "up") ("go" "up" "to") ("go" "with") ("go" "without")
           ("goof" "around") ("goof" "off") ("goof" "up") ("grass" "on")
           ("grass" "up") ("grey" "out") ("grind" "away") ("grind" "down")
           ("grind" "into") ("grind" "on") ("grind" "out") ("grind" "up")
           ("grow" "apart") ("grow" "away" "from") ("grow" "back")
           ("grow" "from") ("grow" "into") ("grow" "on") ("grow" "out")
           ("grow" "out" "of") ("grow" "to") ("grow" "together") ("grow" "up")
           ("grow" "up" "on") ("grow" "upon") ("gun" "for") ("hack" "around")
           ("hack" "into") ("hack" "off") ("ham" "up") ("hammer" "away" "at")
           ("hammer" "into") ("hammer" "out") ("hand" "back") ("hand" "down")
           ("hand" "in") ("hand" "on") ("hand" "out") ("hand" "over")
           ("hang" "about") ("hang" "about") ("hang" "around") ("hang" "back")
           ("hang" "back" "from") ("hang" "in" "there") ("hang" "on")
           ("hang" "onto") ("hang" "out") ("hang" "out" "for") ("hang" "over")
           ("hang" "together") ("hang" "up") ("hang" "up" "on") ("hang" "with")
           ("hanker" "after") ("hanker" "for") ("harp" "on") ("have" "against")
           ("have" "around") ("have" "down" "as") ("have" "in")
           ("have" "it" "away") ("have" "it" "in" "for") ("have" "it" "off")
           ("have" "it" "out" "with") ("have" "off") ("have" "on")
           ("have" "over") ("have" "round") ("have" "up") ("head" "for")
           ("head" "off") ("head" "out") ("head" "up") ("heat" "up")
           ("help" "out") ("hit" "back") ("hit" "for") ("hit" "it" "off")
           ("hit" "it" "off" "with") ("hit" "on") ("hit" "out" "at")
           ("hit" "up") ("hit" "up" "on") ("hit" "upon") ("hit" "with")
           ("hold" "against") ("hold" "back") ("hold" "back" "from")
           ("hold" "down") ("hold" "forth") ("hold" "off") ("hold" "on")
           ("hold" "on" "to") ("hold" "onto") ("hold" "out")
           ("hold" "out" "against") ("hold" "out" "for") ("hold" "out" "on")
           ("hold" "over") ("hold" "together") ("hold" "up") ("hold" "with")
           ("home" "in" "on") ("hone" "in" "on") ("hook" "up")
           ("hook" "up" "to") ("hoon" "around") ("horse" "around")
           ("hound" "out") ("hunker" "down") ("hunt" "down") ("hunt" "out")
           ("hunt" "up") ("hush" "up") ("iron" "out") ("issue" "forth")
           ("jack" "around") ("jack" "in") ("jack" "up") ("jam" "on")
           ("jaw" "away") ("jazz" "up") ("joke" "around") ("jot" "down")
           ("juice" "up") ("jump" "at") ("jump" "in") ("jump" "on")
           ("keel" "over") ("keep" "around") ("keep" "at") ("keep" "away")
           ("keep" "back") ("keep" "down") ("keep" "from") ("keep" "in")
           ("keep" "off") ("keep" "on") ("keep" "out") ("keep" "to")
           ("keep" "up") ("keep" "up" "at") ("keep" "up" "with") ("key" "in")
           ("kick" "about") ("kick" "around") ("kick" "around" "with")
           ("kick" "back") ("kick" "down") ("kick" "in") ("kick" "off")
           ("kick" "out") ("kick" "up") ("kiss" "off") ("kiss" "up" "to")
           ("knock" "about") ("knock" "around") ("knock" "back")
           ("knock" "down") ("knock" "it" "off") ("knock" "off")
           ("knock" "out") ("knock" "together") ("knock" "up")
           ("knuckle" "down") ("knuckle" "under") ("lap" "up")
           ("large" "it" "up") ("lark" "about") ("lark" "around")
           ("lark" "it" "up") ("lash" "down") ("lash" "into") ("lash" "out")
           ("lash" "out" "against") ("lash" "out" "at") ("lash" "out" "on")
           ("latch" "on") ("latch" "on" "to") ("latch" "onto") ("lay" "down")
           ("lay" "into") ("lay" "off") ("lay" "on") ("lay" "out")
           ("lead" "to") ("leak" "out") ("lean" "on") ("leave" "on")
           ("leave" "out") ("let" "down") ("let" "in") ("let" "off")
           ("let" "on") ("let" "out") ("lie" "down") ("lie" "with")
           ("lift" "off") ("light" "up") ("lighten" "up") ("limber" "up")
           ("limber" "up" "for") ("line" "up") ("link" "up")
           ("link" "up" "with") ("listen" "out" "for") ("live" "by")
           ("live" "down") ("live" "for") ("live" "in") ("live" "it" "up")
           ("live" "off") ("live" "out") ("live" "through") ("live" "together")
           ("live" "up" "to") ("live" "with") ("load" "down") ("load" "up")
           ("load" "up" "on") ("lock" "away") ("lock" "down") ("lock" "in")
           ("lock" "onto") ("lock" "out") ("lock" "up")
           ("lock" "yourself" "away") ("log" "in") ("log" "into") ("log" "off")
           ("log" "on") ("log" "out") ("look" "after") ("look" "back")
           ("look" "down" "on") ("look" "for") ("look" "forward" "to")
           ("look" "in") ("look" "in" "on") ("look" "into") ("look" "like")
           ("look" "on") ("look" "on" "as") ("look" "out") ("look" "over")
           ("look" "round") ("look" "to") ("look" "up") ("look" "up" "to")
           ("look" "upon" "as") ("lord" "it" "over") ("lose" "out")
           ("lose" "out" "on") ("lose" "out" "to") ("luck" "out")
           ("magic" "away") ("make" "do" "with") ("make" "for") ("make" "into")
           ("make" "it") ("make" "it" "up" "to") ("make" "off" "with")
           ("make" "out") ("make" "over") ("make" "up") ("make" "up" "for")
           ("make" "up" "to") ("make" "with") ("mark" "down")
           ("mark" "down" "as") ("mark" "off") ("mark" "out")
           ("mark" "out" "for") ("mark" "out" "from") ("mark" "up")
           ("marry" "in") ("marry" "out") ("mash" "up") ("max" "out")
           ("measure" "against") ("measure" "off") ("measure" "out")
           ("measure" "up") ("meet" "with") ("mess" "about")
           ("mess" "about" "with") ("mess" "around") ("mess" "around" "with")
           ("mess" "over") ("mess" "up") ("mess" "with") ("mill" "around")
           ("miss" "out") ("miss" "out" "on") ("mix" "up") ("monkey" "around")
           ("mooch" "about") ("mooch" "around") ("mop" "up") ("mope" "about")
           ("mope" "around") ("mount" "up") ("mouth" "off") ("move" "on")
           ("mug" "up") ("mug" "up" "on") ("mull" "over") ("muscle" "in")
           ("muscle" "in" "on") ("muscle" "into") ("muscle" "out")
           ("naff" "off") ("nag" "at") ("nail" "down") ("name" "after")
           ("nip" "off") ("nip" "out") ("nod" "off") ("nose" "about")
           ("nose" "around") ("note" "down") ("nut" "out") ("occur" "to")
           ("open" "up") ("opt" "for") ("opt" "out") ("owe" "to") ("own" "up")
           ("pack" "away") ("pack" "in") ("pack" "it" "in") ("pack" "off")
           ("pack" "out") ("pack" "up") ("pad" "down") ("pad" "out")
           ("pair" "off") ("pair" "off" "with") ("pair" "up") ("palm" "off")
           ("pan" "out") ("paper" "over") ("pare" "back") ("pare" "down")
           ("pass" "around") ("pass" "as") ("pass" "away") ("pass" "by")
           ("pass" "for") ("pass" "off") ("pass" "on") ("pass" "out")
           ("pass" "over") ("pass" "through") ("pass" "to") ("pass" "up")
           ("patch" "up") ("pay" "back") ("pay" "for") ("pay" "into")
           ("pay" "off") ("peck" "at") ("peg" "away") ("peg" "down")
           ("peg" "it") ("peg" "out") ("pencil" "in") ("perk" "up")
           ("peter" "out") ("phase" "in") ("phase" "out") ("phrasal" "verb")
           ("phrasal" "verbs") ("pick" "at") ("pick" "off") ("pick" "on")
           ("pick" "out") ("pick" "through") ("pick" "up")
           ("pick" "up" "after") ("pick" "up" "on") ("pick" "yourself" "up")
           ("pig" "off") ("pig" "out") ("pile" "up") ("pin" "down")
           ("pin" "on") ("pin" "up") ("pine" "away") ("pipe" "down")
           ("pipe" "up") ("pit" "against") ("pit" "out") ("pitch" "for")
           ("pitch" "in") ("pitch" "into") ("play" "along") ("play" "around")
           ("play" "at") ("play" "away") ("play" "back") ("play" "down")
           ("play" "off") ("play" "on") ("play" "out") ("play" "up")
           ("play" "up" "to") ("play" "upon") ("play" "with") ("plead" "out")
           ("plough" "back") ("plough" "into") ("plough" "on")
           ("plough" "through") ("plough" "up") ("plow" "back") ("plow" "into")
           ("plow" "on") ("plow" "through") ("plow" "up") ("plug" "in")
           ("plump" "for") ("point" "out") ("polish" "off") ("polish" "up")
           ("pony" "up") ("poop" "out") ("poop" "out" "on") ("pop" "in")
           ("pop" "off") ("pop" "out") ("pop" "up") ("potter" "about")
           ("potter" "around") ("pour" "down") ("pour" "forth")
           ("prattle" "on") ("press" "ahead") ("press" "on") ("price" "up")
           ("print" "out") ("prop" "up") ("psych" "out") ("psych" "up")
           ("pull" "ahead") ("pull" "apart") ("pull" "away") ("pull" "back")
           ("pull" "down") ("pull" "for") ("pull" "in") ("pull" "off")
           ("pull" "on") ("pull" "out") ("pull" "over") ("pull" "through")
           ("pull" "to") ("pull" "together") ("pull" "up")
           ("pull" "yourself" "together") ("push" "in") ("put" "across")
           ("put" "away") ("put" "back") ("put" "by") ("put" "down")
           ("put" "down" "for") ("put" "down" "to") ("put" "in")
           ("put" "in" "for") ("put" "off") ("put" "on") ("put" "out")
           ("put" "through") ("put" "towards") ("put" "up") ("put" "up" "with")
           ("quieten" "down") ("rack" "up") ("rain" "down" "on")
           ("rake" "it" "in") ("rake" "up") ("ramp" "up") ("rat" "on")
           ("rat" "out") ("rat" "through") ("ratchet" "up") ("rattle" "off")
           ("reach" "out") ("reach" "out" "for") ("reach" "out" "to")
           ("read" "off") ("read" "out") ("read" "up" "on") ("reckon" "on")
           ("reel" "in") ("reel" "off") ("reel" "out") ("rein" "in")
           ("ride" "off") ("ride" "on") ("ride" "out") ("ride" "up")
           ("ring" "back") ("ring" "off") ("ring" "up") ("rip" "off")
           ("rode" "off") ("roll" "back") ("roll" "in") ("roll" "on")
           ("roll" "out") ("roll" "up") ("romp" "in") ("romp" "through")
           ("room" "in") ("root" "about") ("root" "around") ("root" "for")
           ("root" "out") ("root" "up") ("rope" "in") ("rough" "up")
           ("round" "off") ("row" "back") ("rub" "along") ("rub" "down")
           ("rub" "in") ("rub" "it" "in") ("rub" "off" "on") ("rub" "out")
           ("rub" "up" "against") ("rub" "up" "on") ("rule" "out")
           ("run" "across") ("run" "away") ("run" "down") ("run" "for")
           ("run" "in") ("run" "into") ("run" "off") ("run" "on")
           ("run" "out" "of") ("run" "over") ("run" "through") ("run" "to")
           ("run" "up") ("run" "up" "against") ("run" "up" "on") ("run" "with")
           ("rush" "into") ("sag" "off") ("sail" "into") ("sail" "through")
           ("sally" "forth") ("sally" "out") ("salt" "away") ("save" "on")
           ("save" "up") ("scare" "away") ("scare" "off") ("scout" "about")
           ("scout" "around") ("scout" "out") ("scout" "round") ("scout" "up")
           ("scrape" "along") ("scrape" "by") ("scrape" "in") ("scrape" "into")
           ("scrape" "through") ("scrape" "together") ("scrape" "up")
           ("screen" "off") ("screen" "out") ("screw" "around") ("screw" "up")
           ("see" "about") ("see" "into") ("see" "off") ("see" "out")
           ("see" "through") ("see" "to") ("sell" "off") ("sell" "on")
           ("sell" "out") ("sell" "up") ("send" "back") ("send" "for")
           ("send" "in") ("send" "off") ("send" "off" "for") ("send" "out")
           ("send" "out" "for") ("send" "up") ("set" "about") ("set" "aside")
           ("set" "back") ("set" "forth") ("set" "in") ("set" "off")
           ("set" "out") ("set" "up") ("settle" "down") ("settle" "for")
           ("settle" "in") ("settle" "on") ("settle" "up") ("sex" "up")
           ("shack" "up") ("shake" "down") ("shake" "off") ("shape" "up")
           ("shave" "off") ("shell" "out") ("ship" "off") ("ship" "out")
           ("shoot" "away") ("shoot" "back") ("shoot" "off") ("shoot" "out")
           ("shoot" "up") ("shop" "around") ("show" "around") ("show" "in")
           ("show" "off") ("show" "out") ("show" "over") ("show" "round")
           ("show" "through") ("show" "up") ("shrug" "off") ("shut" "away")
           ("shut" "down") ("shut" "in") ("shut" "off") ("shut" "out")
           ("shut" "out" "of") ("shut" "up") ("shut" "yourself" "away")
           ("shy" "away" "from") ("side" "with") ("sift" "through")
           ("sign" "away") ("sign" "for") ("sign" "in") ("sign" "into")
           ("sign" "off") ("sign" "on") ("sign" "on" "with") ("sign" "out")
           ("sign" "out" "of") ("sign" "up") ("sign" "with") ("simmer" "down")
           ("sink" "in") ("sit" "about") ("sit" "around") ("sit" "back")
           ("sit" "by") ("sit" "down") ("sit" "for") ("sit" "in")
           ("sit" "in" "for") ("sit" "in" "on") ("sit" "on") ("sit" "out")
           ("sit" "over") ("sit" "through") ("sit" "with") ("size" "up")
           ("skive" "off") ("slack" "off") ("slag" "off") ("sleep" "off")
           ("sleep" "on") ("sleep" "over") ("sleep" "through") ("slip" "out")
           ("slip" "up") ("slob" "about") ("slob" "around") ("slope" "off")
           ("slow" "down") ("slow" "up") ("smack" "of") ("smash" "down")
           ("smash" "in") ("smash" "up") ("snap" "off") ("snap" "out" "of")
           ("snap" "to" "it") ("snap" "up") ("sniff" "around") ("sniff" "at")
           ("sniff" "out") ("sober" "up") ("soldier" "on") ("sort" "out")
           ("sound" "off") ("sound" "out") ("spark" "off") ("spark" "up")
           ("speak" "out") ("speak" "up") ("spell" "out") ("spit" "it" "out")
           ("spit" "out") ("split" "up") ("spoil" "for") ("spur" "on")
           ("square" "away") ("square" "off") ("square" "off" "against")
           ("square" "up") ("square" "up" "to") ("square" "with")
           ("squeeze" "up") ("stack" "up") ("stack" "up" "against")
           ("staff" "up") ("stamp" "out") ("stand" "about") ("stand" "around")
           ("stand" "aside") ("stand" "back") ("stand" "by") ("stand" "down")
           ("stand" "for") ("stand" "in" "for") ("stand" "out") ("stand" "up")
           ("stand" "up" "for") ("stand" "up" "to") ("start" "off")
           ("start" "off" "on") ("start" "on") ("start" "on" "at")
           ("start" "out") ("start" "out" "as") ("start" "out" "to")
           ("start" "over") ("start" "up") ("stash" "away") ("stay" "away")
           ("stay" "away" "from") ("stay" "in") ("stay" "on") ("stay" "out")
           ("stay" "over") ("stay" "up") ("steer" "clear" "of") ("stem" "from")
           ("step" "aside") ("step" "back") ("step" "down") ("step" "forward")
           ("step" "in") ("step" "on" "it") ("step" "out") ("step" "to")
           ("step" "up") ("stick" "around") ("stick" "at") ("stick" "by")
           ("stick" "down") ("stick" "it" "to") ("stick" "out")
           ("stick" "out" "for") ("stick" "to") ("stick" "together")
           ("stick" "up") ("stick" "up" "for") ("stick" "with") ("stir" "up")
           ("stitch" "up") ("stop" "around") ("stop" "back") ("stop" "behind")
           ("stop" "by") ("stop" "doing") ("stop" "in") ("stop" "off")
           ("stop" "out") ("stop" "over") ("stop" "round") ("stop" "up")
           ("storm" "off") ("storm" "out") ("stow" "away") ("straighten" "out")
           ("straighten" "up") ("strike" "back") ("strike" "down")
           ("strike" "off") ("strike" "on") ("strike" "out") ("strike" "up")
           ("strike" "upon") ("string" "along") ("string" "out")
           ("string" "together") ("string" "up") ("stub" "out")
           ("stumble" "across") ("stumble" "upon") ("stump" "up") ("suck" "in")
           ("suck" "into") ("suck" "up" "to") ("sum" "up") ("summon" "up")
           ("suss" "out") ("swan" "about") ("swan" "around") ("swan" "in")
           ("swan" "off") ("swear" "by") ("swear" "down") ("sweep" "through")
           ("swing" "around") ("swing" "at") ("swing" "by") ("swing" "round")
           ("syphon" "off") ("tack" "on") ("tack" "onto") ("tag" "along")
           ("tag" "on") ("tag" "onto") ("tag" "with") ("tail" "away")
           ("tail" "back") ("tail" "off") ("take" "after") ("take" "apart")
           ("take" "aside") ("take" "away") ("take" "back") ("take" "down")
           ("take" "in") ("take" "it") ("take" "it" "out" "on")
           ("take" "it" "upon" "yourself") ("take" "off") ("take" "on")
           ("take" "out") ("take" "over") ("take" "to") ("take" "up")
           ("talk" "back") ("talk" "down") ("talk" "down" "to") ("talk" "into")
           ("talk" "out" "of") ("talk" "over") ("talk" "through") ("talk" "up")
           ("tap" "for") ("tap" "into") ("tap" "off" "with") ("tap" "out")
           ("tap" "up") ("team" "up") ("tear" "apart") ("tear" "at")
           ("tear" "away") ("tear" "down") ("tear" "into") ("tear" "off")
           ("tear" "up") ("tee" "off") ("tee" "off" "on") ("tee" "up")
           ("tell" "apart") ("tell" "off") ("tell" "on") ("think" "over")
           ("think" "through") ("think" "up") ("throw" "away") ("throw" "out")
           ("throw" "up") ("tick" "away") ("tick" "by") ("tick" "off")
           ("tick" "over") ("tide" "over") ("tidy" "up") ("tie" "back")
           ("tie" "down") ("tie" "in") ("tie" "in" "with") ("tie" "up")
           ("tighten" "up") ("tip" "off") ("tip" "over") ("tire" "of")
           ("tire" "out") ("toddle" "off") ("tone" "down") ("tool" "up")
           ("top" "off") ("top" "out") ("top" "up") ("touch" "down")
           ("touch" "for") ("touch" "off") ("touch" "on") ("touch" "up")
           ("touch" "upon") ("toy" "at") ("toy" "over") ("toy" "with")
           ("track" "down") ("trade" "down") ("trade" "in") ("trade" "off")
           ("trade" "on") ("trade" "up") ("trade" "upon") ("trickle" "down")
           ("trip" "over") ("trip" "up") ("trot" "out") ("trump" "up")
           ("try" "back") ("try" "for") ("try" "it" "on") ("try" "on")
           ("try" "out") ("try" "out" "for") ("tuck" "away") ("tuck" "in")
           ("tuck" "into") ("tuck" "up") ("tune" "in") ("tune" "in" "to")
           ("tune" "out") ("tune" "up") ("turn" "against") ("turn" "away")
           ("turn" "down") ("turn" "in") ("turn" "into") ("turn" "off")
           ("turn" "on") ("turn" "out") ("turn" "over") ("turn" "to")
           ("turn" "up") ("type" "up") ("use" "up") ("veg" "out")
           ("venture" "forth") ("wade" "in") ("wade" "into") ("wade" "through")
           ("wait" "about") ("wait" "around") ("wait" "behind") ("wait" "in")
           ("wait" "on") ("wait" "out") ("wait" "up") ("wait" "upon")
           ("wake" "up") ("walk" "away" "from") ("walk" "away" "with")
           ("walk" "in" "on") ("walk" "into") ("walk" "off")
           ("walk" "off" "with") ("walk" "on") ("walk" "out")
           ("walk" "out" "on") ("walk" "up") ("want" "out") ("warm" "up")
           ("wash" "away") ("wash" "down") ("wash" "out") ("wash" "over")
           ("wash" "up") ("watch" "out") ("watch" "out" "for") ("watch" "over")
           ("water" "down") ("wean" "off") ("wear" "away") ("wear" "down")
           ("wear" "off") ("wear" "out") ("weed" "out") ("weigh" "in")
           ("weigh" "up") ("went" "up") ("while" "away") ("whip" "out")
           ("whip" "through") ("whip" "up") ("whisk" "away") ("white" "out")
           ("wig" "out") ("wimp" "out") ("wind" "down") ("wind" "on")
           ("wind" "up") ("winkle" "out") ("wipe" "out") ("wire" "up")
           ("wise" "up") ("word" "up") ("work" "off") ("work" "on")
           ("work" "out") ("wrap" "up") ("wriggle" "out" "of") ("write" "down")
           ("write" "in") ("write" "off") ("write" "out") ("write" "up")
           ("yack" "on") ("yammer" "on") ("yield" "to") ("zero" "in" "on")
           ("zero" "out") ("zip" "around") ("zip" "by") ("zip" "up")
           ("zone" "in") ("zone" "in" "on") ("zone" "out") ("zonk" "out")
           ("zoom" "in") ("zoom" "in" "on") ("zoom" "off") ("zoom" "out")))
      (setf (alist-get (car phrase) verbs nil nil 'string-equal)
            (cons (cdr phrase)
                  (alist-get (car phrase) verbs nil nil 'string-equal))))
    verbs)
  "List of phrasal verbs in English.
This list is (a) very long and (b) probably incomplete.  It's
been pulled from
https://capitalizemytitle.com/what-is-a-phrasal-verb/.")

(defvar titlecase-articles '("a" "an" "the")
  "List of articles in English.")

(defvar titlecase-coordinating-conjunctions '("for" "and" "nor" "but" "or"
                                              "yet" "so")
  "List of coordinating conjunctions in English.")


(defvar titlecase-styles-capitalize-phrasal-verbs
  '(chicago apa mla ap bluebook ama nyt wikipedia)
  "Styles in which to capitalize phrasal verbs.")

(defvar titlecase-styles-capitalize-last-word
  '(chicago ap bluebook ama nyt wikipedia)
  "Styles in which to capitalize the last word of a title.")

(defvar titlecase-styles-capitalize-non-short-words
  '(ap)
  "Styles in which to capitalize all non-short words.
See `titlecase-short-word-length'.")

(defvar titlecase-short-word-length 3
  "Maximum length of a short word.
See `titlecase-styles-capitalize-non-short-words'.")

(defvar titlecase-lowercase-chicago (append titlecase-articles
                                            titlecase-prepositions
                                            '("as" "and" "but" "for" "nor"
                                              "or"))
  "Words to lowercase in Chicago Style.
Include: articles, and, but, for, nor, or, prepositions, and
\"to\" in an infinitive (though that's caught as a preposition).")

(defvar titlecase-lowercase-apa (append titlecase-articles
                                        (seq-filter (lambda (p)
                                                      (< (length p) 4))
                                                    titlecase-prepositions)
                                        '("as" "if"))
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
                                        titlecase-coordinating-conjunctions)
                                       '("as" "if"))
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

(defvar titlecase-lowercase-nyt '("as" "and" "as" "at" "but" "by" "en" "for"
                                  "in" "if" "of" "on" "or" "the" "to" "v." "vs."
                                  "via")
  "Words to lowercase in New York Times Style.
NYT is unique in explicitly listing all words to lowercase.
Thanks, NYT!  Of course, these should be capitalized when used as
adverbs, and \"for\" should be capitalized \"if it takes the
place of a verb meaning 'support' or 'advocate.'\" In addition,
\"in\" and \"on\" are generally capitalized when used as
adjectives in actual NYT headlines.")

(defvar titlecase-lowercase-imdb '("a" "an" "and" "as" "at" "by" "for"
                                   "from" "in" "of" "on" "or" "the" "to"
                                   "with")
  "Words to lowercase in IMDB style.
IMDB is another style guide that's explicit in which words to
lowercase.  Shout-out to IMDB!")

(defvar titlecase-lowercase-wikipedia
  (append titlecase-articles
          (seq-filter (lambda (p) (< (length p) 5)) titlecase-prepositions)
          titlecase-coordinating-conjunctions)
  "Words to lowercase in Wikipedia Style.")

(defvar titlecase-lowercase-sentence nil
  "Words to titlecase in Sentence style.
This is nil, since `titlecase--region-with-style-impl' deals with
the logic here (and besides, this list would be /quite/ long when
enumerated!)")

(provide 'titlecase-data)
;;; titlecase-data.el ends here
