;;; soccer.el --- Fixtures, results, table etc for soccer  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/soccer
;; Package-Requires: ((emacs "29.1"))
;; Version: 2.0.0
;; Keywords: games, soccer, football

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
;;
;; This package brings soccer (football) fixtures, results and league
;; tables into Emacs, for every competition the source site covers.
;;
;; The entry point is `soccer', a transient menu from which every view is
;; reachable.  Views open in a `soccer-mode' buffer which has its own
;; transient bound to "?", so a league table is one keystroke away from the
;; fixtures of the club under point.
;;
;; Kick off times are absolute instants and are shown in your own time
;; zone automatically; set `soccer-timezone' to override that.
;;
;; The individual commands are still available directly:
;;
;; Function                     Action
;; soccer                       Transient menu with everything
;; soccer-fixtures-next         Fixture for the next match
;; soccer-fixtures-next-5       Fixtures of the next 5 matches
;; soccer-fixtures-full-in-org  Full fixtures saved in an org file
;; soccer-fixtures-all-clubs    Fixtures for all clubs in a league
;; soccer-results-last          Result of the last match
;; soccer-results-last-5        Results of the last 5 matches
;; soccer-results-full-in-org   Full list of results in an org file
;; soccer-results-all-clubs     Results for all clubs in a league
;; soccer-table                 Full ranking table
;; soccer-table-top-4           Ranking table with top 4 teams
;; soccer-table-bottom-4        Ranking table with bottom 4 teams
;; soccer-scorecard             Scorecard of a match

;;; Code:

(require 'soccer-source)
(require 'soccer-leagues)
(require 'org)
(require 'dom)
(require 'json)
(require 'iso8601)
(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'transient)

;;;; Customization

(defgroup soccer nil
  "Soccer fixtures, results and tables inside Emacs."
  :group 'games
  :link '(url-link :tag "Homepage" "https://github.com/md-arif-shaikh/soccer"))

(defcustom soccer-timezone nil
  "Time zone used to display kick off times.
Any value accepted as the ZONE argument of `format-time-string' will do,
for instance \"Europe/London\".  When nil, your system time zone is used,
which is almost always what you want: the source publishes absolute UTC
instants, so Emacs can place them in your local time by itself."
  :type '(choice (const :tag "System time zone" nil) string)
  :group 'soccer)

(defcustom soccer-team-column-width 22
  "Width in characters reserved for each club name in match listings."
  :type 'integer
  :group 'soccer)

;;;; Faces

;; The faces inherit from the standard semantic faces so that they follow
;; whichever theme is loaded, and only fall back to explicit colours on
;; displays where those faces are not distinguishable.

(defface soccer-face-win
  '((t :inherit success :weight bold))
  "Face for a win."
  :group 'soccer)

(defface soccer-face-loss
  '((t :inherit error :weight bold))
  "Face for a loss."
  :group 'soccer)

(defface soccer-face-draw
  '((t :inherit shadow :weight bold))
  "Face for a draw."
  :group 'soccer)

(defface soccer-face-fixtures
  '((t :inherit default))
  "Face for a club name in a fixture."
  :group 'soccer)

(defface soccer-face-local-time
  '((t :inherit font-lock-keyword-face))
  "Face for the kick off time."
  :group 'soccer)

(defface soccer-face-time-to-kickoff
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for the time remaining until kick off."
  :group 'soccer)

(defface soccer-face-scorecard-header
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for a scorecard title."
  :group 'soccer)

(defface soccer-face-date
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for the date heading separating matches."
  :group 'soccer)

(defface soccer-face-heading
  '((t :inherit font-lock-doc-face))
  "Face for the column heading of the league table."
  :group 'soccer)

(defface soccer-face-score
  '((t :inherit default :weight bold))
  "Face for a full time score."
  :group 'soccer)

(defface soccer-face-live
  '((t :inherit warning :weight bold))
  "Face for a match that is being played right now."
  :group 'soccer)

(defface soccer-face-rank
  '((t :inherit shadow))
  "Face for a position in the league table."
  :group 'soccer)

(defface soccer-face-competition
  '((t :inherit font-lock-type-face))
  "Face for a competition name."
  :group 'soccer)

;;;; Utilities

(defun soccer--get-league-names ()
  "Extract the list of known competition names."
  (mapcar #'car (soccer-leagues--leagues)))

(defun soccer--get-league-url (league)
  "Get url of a LEAGUE."
  (soccer-leagues--get-base-league-url league))

(defun soccer--read-league ()
  "Prompt for a competition name."
  (completing-read "League: " (soccer--get-league-names) nil t))

(defun soccer--read-club (league)
  "Prompt for a club taking part in LEAGUE."
  (completing-read "Club: " (soccer-leagues--get-club-names league) nil t))

(defun soccer--read-league-and-club ()
  "Prompt for a competition and then for one of its clubs."
  (let ((league (soccer--read-league)))
    (list league (soccer--read-club league))))

(defun soccer--parse-timestamp (string)
  "Parse STRING, an ISO 8601 timestamp, into an Emacs time value."
  (when (and string (not (string-empty-p string)))
    (ignore-errors
      (encode-time (iso8601-parse string)))))

(defun soccer--format-time (time format)
  "Format TIME with FORMAT in `soccer-timezone'."
  (and time (format-time-string format time (or soccer-timezone nil))))

(defun soccer--humanize-delta (time)
  "Describe how far away TIME is from now, in words."
  (when time
    (let* ((seconds (round (float-time (time-subtract time (current-time)))))
           (future (>= seconds 0))
           (s (abs seconds))
           (days (floor s 86400))
           (hours (floor (mod s 86400) 3600))
           (mins (floor (mod s 3600) 60))
           (amount (cond ((> days 0) (format "%d day%s" days (if (= days 1) "" "s")))
                         ((> hours 0) (format "%d hour%s" hours (if (= hours 1) "" "s")))
                         (t (format "%d min%s" mins (if (= mins 1) "" "s"))))))
      (cond
       (future (format "in %s" amount))
       ;; A match lasts a bit over an hour and a half including the break.
       ((< s 6600) "live now")
       (t (format "%s ago" amount))))))

(defun soccer--pad (string width &optional right)
  "Pad STRING to WIDTH, on the left unless RIGHT is non-nil.
Padding is added around the string without disturbing its text
properties, and over-long strings are truncated."
  (let* ((string (or string ""))
         (len (string-width string)))
    (cond
     ((> len width) (truncate-string-to-width string width nil nil t))
     (right (concat string (make-string (- width len) ?\s)))
     (t (concat (make-string (- width len) ?\s) string)))))

;;;; Parsing the match lists

(defun soccer--top-level-spans (node)
  "Return the span elements directly inside NODE.
Match rows are sometimes wrapped in a link, which is looked through."
  (let* ((children (seq-filter #'consp (dom-children node)))
         (anchor (car (seq-filter (lambda (c) (eq (dom-tag c) 'a)) children)))
         (children (if anchor
                       (seq-filter #'consp (dom-children anchor))
                     children)))
    (seq-filter (lambda (c) (eq (dom-tag c) 'span)) children)))

(defun soccer--score-pair (li)
  "Return (HOME . AWAY) scores found in match row LI, or nil.
The score sits in a span holding one span per side; it is located by that
shape rather than by class name."
  (cl-loop for span in (soccer--top-level-spans li)
           for parts = (cl-loop for inner in (dom-by-tag span 'span)
                                for text = (soccer-source-text inner)
                                when (string-match-p "\\`[0-9]+\\'" text)
                                collect text)
           when (>= (length parts) 2)
           return (cons (car parts) (car (last parts)))))

(defun soccer--status-text (li)
  "Return the status label of match row LI, such as \"FT\" or \"HT\"."
  (let* ((spans (soccer--top-level-spans li))
         (label (cl-loop for span in spans
                         for text = (soccer-source-text span)
                         ;; Skip the score span and the bare "v" separator.
                         unless (or (string-match-p "\\`[0-9 ]+\\'" text)
                                    (string-empty-p text)
                                    (string-equal text "v"))
                         return text)))
    label))

(defun soccer--parse-match (li date competition)
  "Turn match row LI into a plist, tagged with DATE and COMPETITION."
  (let* ((teams (seq-filter (lambda (d) (dom-by-tag d 'picture))
                            (dom-by-tag li 'div)))
         (home (soccer-source-text (nth 0 teams)))
         (away (soccer-source-text (nth 1 teams)))
         (time-node (car (dom-by-tag li 'time)))
         (kickoff (soccer--parse-timestamp
                   (and time-node (dom-attr time-node 'datetime))))
         (scores (soccer--score-pair li))
         (url (dom-attr (car (dom-by-tag li 'a)) 'href)))
    (when (and home away (not (string-empty-p home)) (not (string-empty-p away)))
      (list :home home
            :away away
            :time kickoff
            :date date
            :competition competition
            :home-score (car scores)
            :away-score (cdr scores)
            :status (soccer--status-text li)
            :url url))))

(defun soccer--parse-date-heading (string)
  "Parse STRING, a date heading such as \"Monday, 10 August 2026\", into a time."
  (when string
    (ignore-errors
      (pcase-let ((`(,_ ,_ ,_ ,day ,month ,year . ,_)
                   (parse-time-string string)))
        (when (and day month year)
          (encode-time (list 0 0 12 day month year nil -1 nil)))))))

(defun soccer--parse-match-sections (dom)
  "Extract every match listed in DOM as a plist.
The page groups matches under a section per day, each introduced by a
date heading and a competition heading; results rows carry no timestamp
of their own, so the date has to come from that heading."
  (cl-loop for section in (dom-by-tag dom 'section)
           for items = (soccer-leagues--match-list-items section)
           when items
           append
           (let* ((heading (soccer-source-text (car (dom-by-tag section 'h2))))
                  (competition (soccer-source-text (car (dom-by-tag section 'h3))))
                  (date (soccer--parse-date-heading heading)))
             (cl-loop for li in items
                      for match = (soccer--parse-match li date competition)
                      when match collect match))))

(defun soccer--matches (league data-type &optional club force)
  "Return the matches of DATA-TYPE for LEAGUE as a list of plists.
DATA-TYPE is the string \"fixtures\" or \"results\".  When CLUB is given
only that club's matches are kept.  FORCE bypasses the page cache."
  (let* ((url (concat (soccer--get-league-url league) "/" (downcase data-type)))
         (matches (soccer--parse-match-sections (soccer-source-dom url force))))
    (if club
        (seq-filter (lambda (m)
                      (or (string-equal club (plist-get m :home))
                          (string-equal club (plist-get m :away))))
                    matches)
      matches)))

;;;; Parsing the league table

(defun soccer--row-cells (row)
  "Return the cells of table ROW in document order."
  (seq-filter (lambda (c) (memq (dom-tag c) '(th td)))
              (seq-filter #'consp (dom-children row))))

(defun soccer--form-letters (cell)
  "Return the recent form recorded in table CELL as a list of plists.
Each entry carries the letter to display and the full description that
the source supplies as a tooltip."
  (cl-loop for span in (dom-by-tag cell 'span)
           for title = (dom-attr span 'title)
           when title
           collect (list :letter (cond ((string-prefix-p "Won" title) "W")
                                       ((string-prefix-p "Lost" title) "L")
                                       ((string-prefix-p "Drew" title) "D")
                                       (t "-"))
                         :description title)))

(defun soccer--table-rows (league &optional force)
  "Return the league table of LEAGUE as a list of plists.
FORCE bypasses the page cache.  Columns are looked up by their heading
rather than by position, so the ordering of the source table may change
without breaking this."
  (let* ((url (concat (soccer--get-league-url league) "/table"))
         (dom (soccer-source-dom url force))
         (table (car (dom-by-tag dom 'table))))
    (unless table
      (user-error "Soccer: no league table published for %s" league))
    (let* ((rows (dom-by-tag table 'tr))
           (headings (mapcar #'soccer-source-text (soccer--row-cells (car rows))))
           (index (lambda (name)
                    (seq-position headings name #'string-equal-ignore-case))))
      (cl-loop for row in (cdr rows)
               for cells = (soccer--row-cells row)
               for get = (lambda (name)
                           (let ((i (funcall index name)))
                             (and i (nth i cells)
                                  (soccer-source-text (nth i cells)))))
               for num = (lambda (name)
                           (string-to-number (or (funcall get name) "0")))
               for form-cell = (let ((i (funcall index "Form")))
                                 (and i (nth i cells)))
               when (funcall get "Team")
               collect (list :rank (funcall num "P")
                             :team (funcall get "Team")
                             :played (funcall num "GP")
                             :won (funcall num "W")
                             :drawn (funcall num "D")
                             :lost (funcall num "L")
                             :goals-for (funcall num "F")
                             :goals-against (funcall num "A")
                             :goal-difference (funcall num "GD")
                             :points (funcall num "Pts")
                             :form (and form-cell (soccer--form-letters form-cell)))))))

;;;; Rendering

(defvar-local soccer--view nil
  "What the current buffer is showing: `fixtures', `results' or `table'.")
(defvar-local soccer--league nil
  "Competition shown in the current buffer.")
(defvar-local soccer--club nil
  "Club the current buffer is filtered by, if any.")
(defvar-local soccer--limit nil
  "Maximum number of matches shown in the current buffer, if any.")
(defvar-local soccer--updated nil
  "When the current buffer was last refreshed.")

(defun soccer--result-faces (home-score away-score)
  "Return the faces for the home and away side given HOME-SCORE, AWAY-SCORE."
  (let ((h (string-to-number (or home-score "0")))
        (a (string-to-number (or away-score "0"))))
    (cond ((> h a) (cons 'soccer-face-win 'soccer-face-loss))
          ((< h a) (cons 'soccer-face-loss 'soccer-face-win))
          (t (cons 'soccer-face-draw 'soccer-face-draw)))))

(defun soccer--insert-match (match)
  "Insert a single MATCH into the current buffer."
  (let* ((home (plist-get match :home))
         (away (plist-get match :away))
         (home-score (plist-get match :home-score))
         (away-score (plist-get match :away-score))
         (played (and home-score away-score))
         (faces (and played (soccer--result-faces home-score away-score)))
         (time (plist-get match :time))
         (status (plist-get match :status))
         (live (and status (string-match-p "\\`[0-9]+'\\|HT" status)))
         (lead (cond (live (propertize (soccer--pad status 7 t) 'face 'soccer-face-live))
                     (played (propertize (soccer--pad (or status "FT") 7 t)
                                         'face 'soccer-face-draw))
                     (time (propertize (soccer--pad (soccer--format-time time "%H:%M") 7 t)
                                       'face 'soccer-face-local-time))
                     (t (make-string 7 ?\s))))
         (middle (if played
                     (propertize (format "%s - %s" home-score away-score)
                                 'face 'soccer-face-score)
                   (propertize "  v  " 'face 'soccer-face-draw)))
         (trailer (if played
                      ""
                    (or (soccer--humanize-delta time) ""))))
    (insert
     (string-trim-right
      (concat
       "  " lead "  "
       (soccer--pad (propertize home 'face (if played (car faces) 'soccer-face-fixtures))
                    soccer-team-column-width)
       "  " (soccer--pad middle 5) "  "
       (soccer--pad (propertize away 'face (if played (cdr faces) 'soccer-face-fixtures))
                    soccer-team-column-width t)
       (if (string-empty-p trailer)
           ""
         (concat "  " (propertize trailer 'face 'soccer-face-time-to-kickoff))))))
    ;; Carry the match on the line so the keymap can act on it.
    (put-text-property (line-beginning-position) (point) 'soccer-match match)
    (insert "\n")))

(defun soccer--insert-matches (matches)
  "Insert MATCHES grouped under a heading per day."
  (if (null matches)
      (insert (propertize "  Nothing to show.\n" 'face 'soccer-face-draw))
    (let (last-date last-competition)
      (dolist (match matches)
        (let* ((time (or (plist-get match :time) (plist-get match :date)))
               (date (and time (soccer--format-time time "%A, %e %B %Y")))
               (competition (plist-get match :competition)))
          (unless (equal date last-date)
            (when last-date (insert "\n"))
            (insert (propertize (format "  %s\n" (or date "Date unknown"))
                                'face 'soccer-face-date))
            (setq last-date date last-competition nil))
          ;; Only worth naming the competition when a day mixes several.
          (when (and competition
                     (not (string-empty-p competition))
                     (not (equal competition last-competition))
                     (not (equal competition soccer--league)))
            (insert (propertize (format "  %s\n" competition)
                                'face 'soccer-face-competition))
            (setq last-competition competition))
          (soccer--insert-match match))))))

(defun soccer--insert-table (rows)
  "Insert league table ROWS."
  (insert (propertize
           (format "  %-3s %-24s %4s %4s %4s %4s %5s %5s %5s %5s   %s\n"
                   "#" "Team" "P" "W" "D" "L" "GF" "GA" "GD" "Pts" "Form")
           'face 'soccer-face-heading))
  (let ((total (length rows)))
    (dolist (row rows)
      (let* ((rank (plist-get row :rank))
             (points-face (cond ((<= rank 4) 'soccer-face-win)
                                ((> rank (- total 3)) 'soccer-face-loss)
                                (t 'default)))
             (form (mapconcat
                    (lambda (f)
                      (let ((letter (plist-get f :letter)))
                        (propertize letter
                                    'face (cond ((equal letter "W") 'soccer-face-win)
                                                ((equal letter "L") 'soccer-face-loss)
                                                (t 'soccer-face-draw))
                                    'help-echo (plist-get f :description))))
                    (plist-get row :form) " ")))
        (insert
         (string-trim-right
          (concat
           "  "
           (propertize (format "%-3d" rank) 'face 'soccer-face-rank)
           " "
           (soccer--pad (plist-get row :team) 24 t)
           (format " %4d" (plist-get row :played))
           (propertize (format " %4d" (plist-get row :won)) 'face 'soccer-face-win)
           (propertize (format " %4d" (plist-get row :drawn)) 'face 'soccer-face-draw)
           (propertize (format " %4d" (plist-get row :lost)) 'face 'soccer-face-loss)
           (format " %5d %5d %5d" (plist-get row :goals-for)
                   (plist-get row :goals-against) (plist-get row :goal-difference))
           (propertize (format " %5d" (plist-get row :points)) 'face points-face)
           "   " form)))
        (put-text-property (line-beginning-position) (point) 'soccer-team
                           (plist-get row :team))
        (insert "\n")))))

(defun soccer--header-line ()
  "Build the header line that describes the current buffer."
  (concat
   " "
   (propertize (or soccer--league "soccer") 'face 'bold)
   (if soccer--club (concat " · " soccer--club) "")
   " · "
   (symbol-name (or soccer--view 'fixtures))
   (if soccer--updated
       (concat " · updated " (format-time-string "%H:%M" soccer--updated))
     "")
   "   ? menu  g refresh  q quit"))

(defun soccer--render ()
  "Draw the current view into the current buffer."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (pcase soccer--view
      ('table (soccer--insert-table (soccer--table-rows soccer--league)))
      (_ (let ((matches (soccer--matches soccer--league
                                         (symbol-name soccer--view)
                                         soccer--club)))
           ;; Both pages already arrive in the order we want to show them:
           ;; fixtures soonest first, results most recent first.
           (when soccer--limit
             (setq matches (seq-take matches soccer--limit)))
           (soccer--insert-matches matches))))
    (setq soccer--updated (current-time))
    (setq header-line-format (soccer--header-line))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun soccer--buffer-name (league view club)
  "Name the buffer showing VIEW of LEAGUE, optionally filtered by CLUB."
  (format "*soccer: %s%s · %s*" league (if club (concat " " club) "") view))

(defun soccer--show (league view &optional club limit)
  "Display VIEW of LEAGUE in a `soccer-mode' buffer.
CLUB filters the matches and LIMIT caps how many are shown."
  (let ((buffer (get-buffer-create (soccer--buffer-name league view club))))
    (with-current-buffer buffer
      (soccer-mode)
      (setq soccer--league league
            soccer--view view
            soccer--club club
            soccer--limit limit)
      (soccer--render))
    (pop-to-buffer buffer)
    buffer))

;;;; Major mode

(defun soccer-refresh ()
  "Refetch and redraw the current view."
  (interactive)
  (unless soccer--view (user-error "Not in a soccer buffer"))
  (soccer-source-clear-cache)
  (soccer--render)
  (message "soccer: refreshed"))

(defun soccer-match-at-point ()
  "Return the match described by the line at point."
  (get-text-property (line-beginning-position) 'soccer-match))

(defun soccer-team-at-point ()
  "Return the club named on the line at point, in any view."
  (or (get-text-property (line-beginning-position) 'soccer-team)
      (let ((match (soccer-match-at-point)))
        (and match
             (completing-read "Club: " (list (plist-get match :home)
                                             (plist-get match :away))
                              nil t)))))

(defun soccer-browse-match ()
  "Open the match at point on the source site."
  (interactive)
  (let ((match (soccer-match-at-point)))
    (unless (and match (plist-get match :url))
      (user-error "No match at point"))
    (browse-url (plist-get match :url))))

(defun soccer-show-fixtures-at-point ()
  "Show the fixtures of the club named at point."
  (interactive)
  (let ((team (soccer-team-at-point)))
    (unless team (user-error "No club at point"))
    (soccer--show soccer--league 'fixtures team)))

(defun soccer-show-results-at-point ()
  "Show the results of the club named at point."
  (interactive)
  (let ((team (soccer-team-at-point)))
    (unless team (user-error "No club at point"))
    (soccer--show soccer--league 'results team)))

(defun soccer-show-fixtures ()
  "Show the fixtures of the competition in this buffer."
  (interactive)
  (soccer--show soccer--league 'fixtures soccer--club))

(defun soccer-show-results ()
  "Show the results of the competition in this buffer."
  (interactive)
  (soccer--show soccer--league 'results soccer--club))

(defun soccer-show-table ()
  "Show the league table of the competition in this buffer."
  (interactive)
  (soccer--show soccer--league 'table))

(defun soccer-clear-club-filter ()
  "Drop the club filter and show the whole competition."
  (interactive)
  (soccer--show soccer--league (or soccer--view 'fixtures) nil))

(defun soccer-switch-league (league)
  "Show the current view for another LEAGUE."
  (interactive (list (soccer--read-league)))
  (soccer--show league (or soccer--view 'fixtures) nil))

(defun soccer-filter-by-club (club)
  "Filter the current view by CLUB."
  (interactive (list (soccer--read-club soccer--league)))
  (soccer--show soccer--league (or soccer--view 'fixtures) club))

(defvar soccer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "?") #'soccer-buffer-menu)
    (define-key map (kbd "g") #'soccer-refresh)
    (define-key map (kbd "f") #'soccer-show-fixtures)
    (define-key map (kbd "r") #'soccer-show-results)
    (define-key map (kbd "t") #'soccer-show-table)
    (define-key map (kbd "l") #'soccer-switch-league)
    (define-key map (kbd "c") #'soccer-filter-by-club)
    (define-key map (kbd "a") #'soccer-clear-club-filter)
    (define-key map (kbd "F") #'soccer-show-fixtures-at-point)
    (define-key map (kbd "R") #'soccer-show-results-at-point)
    (define-key map (kbd "s") #'soccer-scorecard-at-point)
    (define-key map (kbd "w") #'soccer-browse-match)
    (define-key map (kbd "RET") #'soccer-dwim-at-point)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map)
  "Keymap for `soccer-mode'.")

(define-derived-mode soccer-mode special-mode "Soccer"
  "Major mode for browsing soccer fixtures, results and tables.

\\{soccer-mode-map}"
  (setq truncate-lines t)
  (setq-local cursor-type 'box)
  (buffer-disable-undo)
  (hl-line-mode 1))

(defun soccer-dwim-at-point ()
  "Do the obvious thing with the line at point.
On a finished match that means its scorecard, on a table row the club's
fixtures."
  (interactive)
  (let ((match (soccer-match-at-point)))
    (cond
     ((and match (plist-get match :home-score)) (soccer-scorecard-at-point))
     (match (soccer-browse-match))
     ((get-text-property (line-beginning-position) 'soccer-team)
      (soccer-show-fixtures-at-point))
     (t (user-error "Nothing to open here")))))

;;;; Scorecard

(defun soccer--match-details (url)
  "Return the detail of the match at URL as an alist.
The match page embeds its data as JSON in an island prop, which is far
steadier than reading it back out of the rendered markup."
  (let* ((dom (soccer-source-dom url))
         (island (car (dom-elements dom 'name "\\`FootballMatchHeaderWrapper\\'")))
         (props (and island (dom-attr island 'props))))
    (unless props
      (user-error "Soccer: could not read the match detail"))
    (let-alist (json-parse-string props :object-type 'alist)
      .initialData.match)))

(defun soccer--format-scorecard (match league)
  "Format the detail alist MATCH of LEAGUE as a string."
  (let-alist match
    (let* ((home .homeTeam.name)
           (away .awayTeam.name)
           (home-goals (or .homeTeam.score 0))
           (away-goals (or .awayTeam.score 0))
           (faces (soccer--result-faces (number-to-string home-goals)
                                        (number-to-string away-goals)))
           (kickoff (soccer--parse-timestamp .kickOff))
           (scorers (lambda (list)
                      (if (seq-empty-p list)
                          "—"
                        (string-join (append list nil) ", ")))))
      (string-join
       (delq nil
             (list
              (propertize (format "%s %d - %d %s" home home-goals away-goals away)
                          'face 'soccer-face-scorecard-header)
              (propertize
               (string-join
                (delq nil (list (or league .leagueName)
                                (and (stringp .venue) .venue)
                                (soccer--format-time kickoff "%a %e %b %Y, %H:%M")))
                " · ")
               'face 'soccer-face-time-to-kickoff)
              (format "%s  %s"
                      (propertize (soccer--pad home soccer-team-column-width t)
                                  'face (car faces))
                      (funcall scorers .homeTeam.scorers))
              (format "%s  %s"
                      (propertize (soccer--pad away soccer-team-column-width t)
                                  'face (cdr faces))
                      (funcall scorers .awayTeam.scorers))))
       "\n"))))

(defun soccer-scorecard-at-point ()
  "Show the scorecard of the match at point."
  (interactive)
  (let* ((match (soccer-match-at-point))
         (url (and match (plist-get match :url))))
    (unless url (user-error "No match at point"))
    (message "%s" (soccer--format-scorecard (soccer--match-details url)
                                            (plist-get match :competition)))))

;;;###autoload
(defun soccer-scorecard (date home away)
  "Get the scorecard for the match between HOME and AWAY on DATE.
DATE is in YYYY-MM-DD format, as produced by `org-read-date'."
  (interactive
   (let* ((league (soccer--read-league))
          (clubs (soccer-leagues--get-club-names league)))
     (list (org-read-date nil nil nil "Date of the match: ")
           (completing-read "Home club: " clubs nil t)
           (completing-read "Away club: " clubs nil t))))
  (let* ((wanted (soccer--parse-date-heading date))
         (match (seq-find
                 (lambda (m)
                   (and (equal home (plist-get m :home))
                        (equal away (plist-get m :away))
                        (let ((time (or (plist-get m :time) (plist-get m :date))))
                          (and time wanted
                               (equal (soccer--format-time time "%F")
                                      (format-time-string "%F" wanted))))))
                 (soccer--parse-match-sections
                  (soccer-source-dom
                   (concat (soccer-leagues--get-base-url) "results"))))))
    (if (and match (plist-get match :url))
        (message "%s" (soccer--format-scorecard
                       (soccer--match-details (plist-get match :url))
                       (plist-get match :competition)))
      (message "Result not found.  Wrong date or club name?\n\
Only recent matches are listed; use `soccer-results-last' to check."))))

;;;; Message based commands

(defun soccer--match-line (match)
  "Format MATCH as a single line for the echo area."
  (with-temp-buffer
    (soccer--insert-match match)
    (string-trim-right (buffer-string))))

(defun soccer--message-matches (league data-type limit club)
  "Show LIMIT matches of DATA-TYPE for CLUB in LEAGUE in the echo area."
  (let ((matches (soccer--matches league data-type club)))
    (setq matches (seq-take matches (or limit (length matches))))
    (if (null matches)
        (message "soccer: nothing to show for %s" (or club league))
      (message "%s" (mapconcat #'soccer--match-line matches "\n")))))

;;;###autoload
(defun soccer-fixtures-next (league club)
  "The next match of CLUB of LEAGUE."
  (interactive (soccer--read-league-and-club))
  (soccer--message-matches league "fixtures" 1 club))

;;;###autoload
(defun soccer-fixtures-next-5 (league club)
  "The next 5 matches in fixtures of CLUB of LEAGUE."
  (interactive (soccer--read-league-and-club))
  (soccer--message-matches league "fixtures" 5 club))

;;;###autoload
(defun soccer-fixtures-all-clubs (league)
  "The next matches in a LEAGUE."
  (interactive (list (soccer--read-league)))
  (soccer--show league 'fixtures))

;;;###autoload
(defun soccer-results-last (league club)
  "The last result of CLUB of LEAGUE."
  (interactive (soccer--read-league-and-club))
  (soccer--message-matches league "results" 1 club))

;;;###autoload
(defun soccer-results-last-5 (league club)
  "The last 5 results of CLUB of LEAGUE."
  (interactive (soccer--read-league-and-club))
  (soccer--message-matches league "results" 5 club))

;;;###autoload
(defun soccer-results-all-clubs (league)
  "All the latest results in a LEAGUE."
  (interactive (list (soccer--read-league)))
  (soccer--show league 'results))

;;;###autoload
(defun soccer-table (league)
  "Get full rank table of a LEAGUE."
  (interactive (list (soccer--read-league)))
  (soccer--show league 'table))

;;;###autoload
(defun soccer-table-top-4 (league)
  "Get table for LEAGUE with top 4 teams."
  (interactive (list (soccer--read-league)))
  (message "%s" (soccer--table-extract league 4 "top")))

;;;###autoload
(defun soccer-table-bottom-4 (league)
  "Get table for LEAGUE with bottom 4 teams."
  (interactive (list (soccer--read-league)))
  (message "%s" (soccer--table-extract league 4 "bottom")))

(defun soccer--table-extract (league num top/bottom)
  "Render NUM rows from the TOP/BOTTOM of the table of LEAGUE."
  (let* ((rows (soccer--table-rows league))
         (num (min num (length rows)))
         (subset (if (string-equal top/bottom "top")
                     (seq-take rows num)
                   (seq-drop rows (- (length rows) num)))))
    (with-temp-buffer
      (let ((soccer--league league))
        (soccer--insert-table subset))
      (string-trim-right (buffer-string)))))

;;;; Org export

(defun soccer--org-file-name (club data-type)
  "Name of the org file holding DATA-TYPE for CLUB."
  (expand-file-name (format "soccer_%s_%s.org"
                            (string-replace " " "_" (or club "all"))
                            data-type)
                    (temporary-file-directory)))

(defun soccer--export-org (league data-type club)
  "Write the DATA-TYPE of CLUB in LEAGUE to an org file and open it."
  (let ((matches (soccer--matches league data-type club))
        (file (soccer--org-file-name club data-type))
        (results-p (string-equal data-type "results")))
    (with-temp-buffer
      (insert (format "* %s for %s\n\n" (capitalize data-type) (or club league)))
      (insert (format "|Date|Time|Home|Away|%s\n" (if results-p "Score|" "")))
      (insert "|-\n")
      (dolist (match matches)
        (let ((time (or (plist-get match :time) (plist-get match :date))))
          (insert (format "|%s|%s|%s|%s|%s\n"
                          (or (soccer--format-time time "%F") "")
                          (if (plist-get match :time)
                              (soccer--format-time time "%H:%M")
                            "")
                          (plist-get match :home)
                          (plist-get match :away)
                          (if results-p
                              (format "%s-%s|"
                                      (or (plist-get match :home-score) "")
                                      (or (plist-get match :away-score) ""))
                            "")))))
      (goto-char (point-min))
      (forward-line 2)
      (org-table-align)
      (write-region (point-min) (point-max) file))
    (find-file file)))

;;;###autoload
(defun soccer-fixtures-full-in-org (league club)
  "Full fixtures of CLUB of LEAGUE saved in an org file."
  (interactive (soccer--read-league-and-club))
  (soccer--export-org league "fixtures" club))

;;;###autoload
(defun soccer-results-full-in-org (league club)
  "Full results of CLUB of LEAGUE saved in an org file."
  (interactive (soccer--read-league-and-club))
  (soccer--export-org league "results" club))

;;;; Schedules in the org agenda

(defcustom soccer-schedule-dir (expand-file-name "~/Dropbox/org/")
  "Directory to store soccer schedules.
Remember to add this in the list of agenda files if it is not already."
  :type 'directory
  :group 'soccer)

(defcustom soccer-schedule-keyword "FOOTBALL"
  "Keyword to represent fixture schedule in agenda view."
  :type 'string
  :group 'soccer)

(defun soccer--get-schedule-file-name (league)
  "Get the file name based on LEAGUE."
  (file-name-concat soccer-schedule-dir
                    (format "%s.org" (string-replace " " "-" league))))

(defun soccer--create-initial-file (league)
  "Create a file for LEAGUE to store schedules."
  (let ((file-name (soccer--get-schedule-file-name league)))
    (make-directory (file-name-directory file-name) t)
    (with-temp-buffer
      (insert (format "#+TITLE: Schedules for %s\n\n" league))
      (append-to-file (point-min) (point-max) file-name))))

(defun soccer--schedule-existsp (header file-name)
  "Check if a schedule HEADER already exists inside a file of FILE-NAME."
  (let ((content (with-temp-buffer
                   (insert-file-contents file-name)
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (string-match-p (regexp-quote header) content)))

(defun soccer--get-schedule-data (league team num-weeks)
  "Create the schedule for TEAM in a LEAGUE in the next NUM-WEEKS."
  (let ((horizon (time-add (current-time) (* num-weeks 7 24 60 60))))
    (cl-loop for match in (soccer--matches league "fixtures" team)
             for time = (plist-get match :time)
             when (and time (time-less-p time horizon))
             collect (list (format "%s: %s vs %s" league
                                   (plist-get match :home)
                                   (plist-get match :away))
                           (format "SCHEDULED: <%s>"
                                   (soccer--format-time time "%F %a %H:%M"))))))

;;;###autoload
(defun soccer-schedule (league team num-weeks)
  "Add the schedules of TEAM in LEAGUE for the next NUM-WEEKS."
  (interactive
   (let* ((league (soccer--read-league))
          (team (soccer--read-club league))
          (weeks (read-number "For number of weeks: " 4)))
     (list league team weeks)))
  (let* ((file-name (soccer--get-schedule-file-name league))
         (data (soccer--get-schedule-data league team num-weeks))
         (added 0))
    (unless (file-exists-p file-name)
      (soccer--create-initial-file league))
    (with-temp-buffer
      (cl-loop for (header timestamp) in data
               unless (soccer--schedule-existsp header file-name)
               do (progn
                    (cl-incf added)
                    (insert (format "* %s %s\n%s\n\n"
                                    soccer-schedule-keyword header timestamp))))
      (append-to-file (point-min) (point-max) file-name))
    (message "soccer: added %d fixture%s for %s"
             added (if (= added 1) "" "s") team)))

;;;###autoload
(defun soccer-schedule-league (league num-weeks)
  "Schedule fixtures of all clubs for a LEAGUE for next NUM-WEEKS."
  (interactive
   (list (soccer--read-league) (read-number "For number of weeks: " 4)))
  (dolist (team (soccer-leagues--get-club-names league))
    (soccer-schedule league team num-weeks)))

;;;###autoload
(defun soccer-schedule-remove-past-fixtures (league)
  "Remove past fixtures of a LEAGUE."
  (interactive (list (soccer--read-league)))
  (let ((file-name (soccer--get-schedule-file-name league)))
    (unless (file-exists-p file-name)
      (user-error "No schedule file for %s exists!" league))
    (with-current-buffer (find-file-noselect file-name)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (thing-at-point 'line t))
               (start (and line (string-match "<" line))))
          (if (and start
                   (let* ((end (string-match ">" line))
                          (stamp (substring line (1+ start) end)))
                     (time-less-p (org-time-string-to-time stamp) (current-time))))
              (progn
                (forward-line -1)
                (let ((beg (line-beginning-position)))
                  (forward-line 2)
                  (delete-region beg (+ (line-end-position) (if (eobp) 0 1)))))
            (forward-line))))
      (write-file file-name))))

;;;###autoload
(defun soccer-schedule-remove-league (league)
  "Remove all fixtures of a LEAGUE.
This will remove the org file for the given league."
  (interactive (list (soccer--read-league)))
  (let ((file-name (soccer--get-schedule-file-name league)))
    (unless (file-exists-p file-name)
      (user-error "No schedule file for %s exists!" league))
    (delete-file file-name)))

;;;; Transient menus

(defun soccer--transient-description ()
  "Describe the buffer the transient was invoked from."
  (if soccer--league
      (format "%s%s" soccer--league (if soccer--club (concat " · " soccer--club) ""))
    "soccer"))

;;;###autoload (autoload 'soccer "soccer" nil t)
(transient-define-prefix soccer ()
  "Browse soccer fixtures, results and tables."
  [:description
   (lambda () (propertize "soccer" 'face 'transient-heading))
   ["Browse"
    ("f" "Fixtures" soccer-fixtures-all-clubs)
    ("r" "Results" soccer-results-all-clubs)
    ("t" "Table" soccer-table)]
   ["A club"
    ("n" "Next match" soccer-fixtures-next)
    ("N" "Next 5 matches" soccer-fixtures-next-5)
    ("l" "Last result" soccer-results-last)
    ("L" "Last 5 results" soccer-results-last-5)]
   ["Table"
    ("4" "Top 4" soccer-table-top-4)
    ("b" "Bottom 4" soccer-table-bottom-4)
    ("s" "Scorecard" soccer-scorecard)]]
  [["Org"
    ("of" "Fixtures to org" soccer-fixtures-full-in-org)
    ("or" "Results to org" soccer-results-full-in-org)]
   ["Agenda"
    ("aa" "Schedule a club" soccer-schedule)
    ("al" "Schedule a league" soccer-schedule-league)
    ("ap" "Drop past fixtures" soccer-schedule-remove-past-fixtures)
    ("ax" "Drop a league" soccer-schedule-remove-league)]
   ["Cache"
    ("g" "Clear cache" soccer-leagues-refresh)]])

(transient-define-prefix soccer-buffer-menu ()
  "Act on the current soccer buffer."
  [:description soccer--transient-description
   ["View"
    ("f" "Fixtures" soccer-show-fixtures)
    ("r" "Results" soccer-show-results)
    ("t" "Table" soccer-show-table)]
   ["Filter"
    ("l" "Other league" soccer-switch-league)
    ("c" "Filter by club" soccer-filter-by-club)
    ("a" "All clubs" soccer-clear-club-filter)]
   ["At point"
    ("F" "Club fixtures" soccer-show-fixtures-at-point)
    ("R" "Club results" soccer-show-results-at-point)
    ("s" "Scorecard" soccer-scorecard-at-point)
    ("w" "Open in browser" soccer-browse-match)]
   ["Buffer"
    ("g" "Refresh" soccer-refresh)
    ("q" "Quit" quit-window)]])

(provide 'soccer)
;;; soccer.el ends here
