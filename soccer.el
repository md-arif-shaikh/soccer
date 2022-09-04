;;; soccer.el --- Fixtures, results, table etc for soccer  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/soccer
;; Package-Requires: ((emacs "26.1") (dash "2.19.1"))
;; Version: 1.2.0
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
;; This package brings soccer (football) fixtures, results, table in your Emacs.
;; Currently it works for
;;
;; 1. Premier League (England)
;; 2. La Liga (Spain)
;; 3. Ligue 1 (France)
;;
;; Other leagues could be easily included and would be in future.
;;
;; To get the time of kick off in your local time, you may want to set the
;; following value accordingly
;;
;; (setq soccer-time-local-time-utc-offset "+0530") ;; this should be changed to your local one
;;
;; Common Functions:
;;
;; This package comes with handy interactive functions to get useful information in your minibuffer.
;; To invoke a function use "M-x Function" where Function could be any of the following functions
;;
;; Functions	                Actions
;; soccer-fixtures-next	Fixture for the Next match
;; soccer-fixtures-next-5	Fixtures of the Next 5 matches
;; soccer-fixtures-full-in-org	Full fixtures saved in org file
;; soccer-fixtures-all-clubs    Fixtures for all clubs in a league
;; soccer-results-last	        Result of the last match
;; soccer-results-last-5	Results of the last 5 matches
;; soccer-results-full-in-org	Full list of results in org file
;; soccer-results-all-clubs     Results for all clubs in a league
;; soccer-table                 Full Ranking table
;; soccer-table-top-4           Ranking table with top 4 teams
;; soccer-table-bottom-4        Ranking table with bottom 4 teams
;; soccer-scorecard             Scorecard of a match

;;; Code:

(require 'soccer-leagues)
(require 'soccer-time)
(require 'org)
(require 'dom)
(require 'dash)

(defvar soccer-color--win "#98C379"
  "Color to indicate a win.")
(defvar soccer-color--loss "#BE5046"
  "Color to indicate a loss.")
(defvar soccer-color--draw "#56B6C2"
  "Color to indicate a draw.")
(defvar soccer-color--fixtures "#C678DD"
  "Color to indicate a fixture.")
(defvar soccer-color--time-to-kickoff "#E5C07B"
  "Color to indicate time to kickoff.")
(defvar soccer-color--scorecard-header "#E5C07B"
  "Color for scorecard title.")
(defvar soccer-color--local-time "#E5C07B"
  "Color for local time.")

(defface soccer-face-local-time
  `((t :foreground ,soccer-color--local-time
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for local time."
  :group 'soccer-face)

(defface soccer-face-win
  `((t :foreground ,soccer-color--win
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for win."
  :group 'soccer-face)

(defface soccer-face-loss
  `((t :foreground ,soccer-color--loss
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for loss."
  :group 'soccer-face)

(defface soccer-face-draw
  `((t :foreground ,soccer-color--draw
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for draw."
  :group 'soccer-face)

(defface soccer-face-fixtures
  `((t :foreground ,soccer-color--fixtures
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for fixtures."
  :group 'soccer-face)

(defface soccer-face-time-to-kickoff
  `((t :foreground ,soccer-color--time-to-kickoff
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for time to kickoff."
  :group 'soccer-face)

(defface soccer-face-scorecard-header
  `((t :foreground ,soccer-color--scorecard-header
       :weight extra-bold
       :box nil
       :underline t))
  "Face for soccer scorecard title."
  :group 'soccer-face)

(defun soccer--get-league-names ()
  "Extract league names from `soccer-leagues--leagues-alist'."
  (mapcar 'car soccer-leagues--leagues-alist))

(defun soccer--get-league-url (league)
  "Get url of a LEAGUE."
  (cdr (assoc league soccer-leagues--leagues-alist)))

(defun soccer--get-date-from-datetime (datetime)
  "Get date from DATETIME string."
  (if datetime
      (nth 0 (split-string datetime "T"))
    ""))

(defun soccer--get-time-and-offset-from-datetime (datetime)
  "Get time from DATETIME string."
  (if datetime
      (let* ((timestring (nth 1 (split-string datetime "T"))))
	(list (substring timestring 0 -8) (substring timestring -5)))
    ""))

(defun soccer--get-league-data-alist (league data-type &optional club)
  "Get data of DATA-TYPE for a LEAGUE.  Optionally filter by CLUB."
  (let* ((url (concat (soccer--get-league-url league) "/" (downcase data-type))))
    (with-current-buffer (url-retrieve-synchronously url)
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	     (datetimes-dom (dom-by-tag dom 'time))
	     (homes-dom (dom-by-class dom "football-match__team football-match__team--home football-team"))
	     (aways-dom (dom-by-class dom "football-match__team football-match__team--away football-team"))
	     datetimes
	     dates
	     times
	     utc-offsets
	     homes
	     aways
	     results
	     league-data)
	(when (string-equal data-type "fixtures")
	  (setq datetimes (cl-loop for datetime in datetimes-dom
				   collect (dom-attr datetime 'datetime)))
	  (setq dates (cl-loop for datetime in datetimes
			       collect (soccer--get-date-from-datetime datetime)))
	  (setq times (cl-loop for datetime in datetimes
			       collect (nth 0 (soccer--get-time-and-offset-from-datetime datetime))))
	  (setq utc-offsets (cl-loop for datetime in datetimes
				     collect (nth 1 (soccer--get-time-and-offset-from-datetime datetime)))))
	(setq homes (cl-loop for d in homes-dom
			     collect (s-trim (dom-texts (dom-by-class d "team-name__long")))))
	(setq aways (cl-loop for d in aways-dom
			     collect (s-trim (dom-texts (dom-by-class d "team-name__long")))))
	(when (string-equal data-type "results")
	  (setq results (cl-loop for h-d in homes-dom
				 for a-d in aways-dom
				 collect (concat (dom-texts (dom-by-class h-d "football-team__score"))
						 "-"
						 (dom-texts (dom-by-class a-d "football-team__score")))))
	  (setq dates (let* ((dates-dom (dom-by-class dom "football-matches__day")))
			(-flatten (-concat (cl-loop for date-dom in dates-dom
						    collect (let* ((results-dom (dom-by-class date-dom "football-match__team football-match__team--home football-team"))
								   (day (dom-strings (dom-by-class dates-dom "date-divider"))))
							      (cl-loop for d in results-dom
								       collect day))))))))
	(setq league-data `(("date" . ,dates)
			    ("time" . ,times)
			    ("source-time-utc-offset" . ,utc-offsets)
			    ("home" . ,homes)
			    ("away" . ,aways)
			    ("result" . ,results)))
	(if club
	    (soccer--filter-league-data-alist-by-club club league-data)
	  league-data)))))

(soccer--get-league-data-alist "Premier League" "results")

(defun soccer--filter-league-data-alist-by-club (club league-data-alist)
  "Filter the LEAGUE-DATA-ALIST by CLUB."
  (let* ((indices1 (-elem-indices club (cdr (assoc "home" league-data-alist))))
	 (indices2 (-elem-indices club (cdr (assoc "away" league-data-alist))))
	 (indices (-sort '< (-concat indices1 indices2)))
	 (items (mapcar 'car league-data-alist)))
    (cl-loop for item in items
	     collect (cons item (cl-loop for index in indices
					 collect (nth index (cdr (assoc item league-data-alist))))))))

(defun soccer--get-league-data-fixture-strings (dates times homes aways source-time-utc-offsets n)
  "Get the fixtures strings to show in buffer for given DATES, TIMES, HOMES, AWAYS, SOURCE-TIME-UTC-OFFSETS and N, where n is the nth in the results."
  (let* ((date (nth n dates))
	 (time (nth n times))
	 (source-time-utc-offset (nth n source-time-utc-offsets))
	 (local-time-list (soccer-time--get-local-time-list time date "-" 2 1 0 source-time-utc-offset soccer-time-local-time-utc-offset))
	 (local-min (nth 0 local-time-list))
	 (local-hour (nth 1 local-time-list))
	 (local-A/P (nth 2 local-time-list))
	 (match-day-local (nth 3 local-time-list))
	 (match-day-num-local (nth 4 local-time-list))
	 (match-month-local (nth 5 local-time-list))
	 (match-year-local (nth 6 local-time-list))
	 (home (nth n homes))
	 (away (nth n aways))
	 (time-till-kickoff-list (soccer-time--get-time-till-kick-off time date "-" 2 1 0 source-time-utc-offset soccer-time-local-time-utc-offset))
	 (days-remain (nth 0 time-till-kickoff-list))
	 (hours-remain (nth 1 time-till-kickoff-list))
	 (mins-remain (nth 2 time-till-kickoff-list))
	 time-till-kickoff-string
	 local-time-string)
    (setq local-time-string (format "%s %s %s %s %s:%s %s" match-year-local match-month-local (format "%02d" match-day-num-local) match-day-local (format "%02d" local-hour) (format "%02d" local-min) local-A/P))
    (setq time-till-kickoff-string (format "⟶ %s %s %s"
					   (if (> days-remain 0) (format "%2s days" days-remain) "")
					   (if (> hours-remain 0) (format "%2s hours" hours-remain) "")
					   (cond ((> mins-remain 0) (format "%2s mins untill kickoff" mins-remain))
						 ((and (< mins-remain 0) (> mins-remain (- 95))) "match is live now.")
						 (t "match has finished"))))
    (format "%s %s  LT: %s %40s - %-40s %s" date time (propertize local-time-string 'face 'soccer-face-local-time) (propertize home 'face 'soccer-face-fixtures) (propertize away 'face 'soccer-face-fixtures) (propertize time-till-kickoff-string 'face 'soccer-face-time-to-kickoff))))

(defun soccer--get-league-data-results-strings (dates homes aways results n)
  "Get the fixtures stings to show in buffer for given DATES, HOMES, AWAYS, RESULTS and N, where is the nth in the results."
  (let* ((date (nth n dates))
	 (home (nth n homes))
	 (away (nth n aways))
	 (result (nth n results))
	 (home-goals (substring result 0 1))
	 (away-goals (substring result 2)))
    (format "%s %s" date
	    (cond ((> (string-to-number home-goals) (string-to-number away-goals))
		   (format "%s - %s" (propertize (concat home " " home-goals) 'face 'soccer-face-win) (propertize (concat away-goals " " away) 'face 'soccer-face-loss)))
		  ((< (string-to-number home-goals) (string-to-number away-goals))
		   (format "%s - %s" (propertize (concat home " " home-goals) 'face 'soccer-face-loss) (propertize (concat away-goals " " away) 'face 'soccer-face-win)))
		  ((= (string-to-number home-goals) (string-to-number away-goals))
		   (format "%s - %s" (propertize (concat home " " home-goals) 'face 'soccer-face-draw) (propertize (concat away-goals " " away) 'face 'soccer-face-draw)))))))

(defun soccer--get-league-data (league data-type num-of-results &optional club)
  "Get NUM-OF-RESULT number of DATA-TYPE for a LEAGUE.  Optionally filter by CLUB."
  (let* ((league-data (soccer--get-league-data-alist league data-type club))
	 (dates (cdr (assoc "date" league-data)))
	 (times (cdr (assoc "time" league-data)))
	 (homes (cdr (assoc "home" league-data)))
	 (aways (cdr (assoc "away" league-data)))
	 (source-time-utc-offsets (cdr (assoc "source-time-utc-offset" league-data)))
	 results
	 msg-str
	 (number-of-results (length dates)))
    (when (string-equal data-type "results")
      (setq results (cdr (assoc "result" league-data))))
    (if num-of-results
	(setq num-of-results (min number-of-results num-of-results))
      (setq num-of-results number-of-results))
    (setq msg-str (cl-loop for n from 0 to (1- num-of-results)
			   collect (cond ((string-equal data-type "fixtures") (soccer--get-league-data-fixture-strings dates times homes aways source-time-utc-offsets n))
					 ((string-equal data-type "results") (soccer--get-league-data-results-strings dates homes aways results n)))))
    (message "%s" (string-join msg-str "\n"))))

(defun soccer--get-league-data-in-org (league data-type num-of-results &optional club)
  "Get NUM-OF-RESULT number of DATA-TYPE for a CLUB (optional) of a LEAGUE."
  (let* ((league-data (soccer--get-league-data-alist league data-type club))
	 (dates (cdr (assoc "date" league-data)))
	 (times (cdr (assoc "time" league-data)))
	 (homes (cdr (assoc "home" league-data)))
	 (aways (cdr (assoc "away" league-data)))
	 results
	 (file-name (format "%ssoccer_%s_%s.org" (temporary-file-directory) club data-type))
	 (number-of-results (length dates)))
    (when (string-equal data-type "results")
      (setq results (cdr (assoc "result" league-data))))
    (if num-of-results
	(setq num-of-results (min number-of-results num-of-results))
      (setq num-of-results number-of-results))
    (with-temp-buffer
      (insert (format "* %s for %s\n\n" data-type club))
      (insert "|--|--|--|--|--|\n")
      (insert (format "|Date |Time | Home | Away | %s \n" (if (string-equal data-type "results") "score |" "")))
      (insert "|--|--|--|--|\n")
      (dolist (n (number-sequence 0 (1- num-of-results)))
	(let* ((date (nth n dates))
	       (time (if times (nth n times) ""))
	       (home (nth n homes))
	       (away (nth n aways))
	       result)
	  (when (string-equal data-type "results")
	    (setq result (nth n results)))
	  (insert (format "|%s|%s|%s|%s|%s\n" date time home away (if (string-equal data-type "results") (format "%s|" result) "")))
	  (insert "|--|--|--|--|\n")))
      (forward-line (* -1 num-of-results))
      (org-table-align)
      (write-region (point-min) (point-max) file-name)
      (find-file file-name))))

(defun soccer--get-league-table-data-alist (league)
  "Get data alist for standing table for a LEAGUE."
  (let* ((url (concat (soccer--get-league-url league) "/table")))
    (with-current-buffer (url-retrieve-synchronously url)
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	     (dom-strings-list (-remove #'string-blank-p (dom-strings (nth 0 (dom-by-tag dom 'table)))))
	     (results-strings-list (-partition 15 (-drop 12 dom-strings-list)))
	     teams
	     played
	     wins
	     losses
	     goal-for
	     goal-against
	     current-form-list)
	(setq teams-full-name (cl-loop for result in results-strings-list
				       collect (string-trim (nth 1 result))))
	(setq teams (cl-loop for result in results-strings-list
			     collect ""))
	(setq played (cl-loop for result in results-strings-list
			      collect (nth 2 result)))
	(setq wins (cl-loop for result in results-strings-list
			    collect (nth 3 result)))
	(setq losses (cl-loop for result in results-strings-list
			      collect (nth 5 result)))
	(setq goal-for (cl-loop for result in results-strings-list
				collect (nth 6 result)))
	(setq goal-against (cl-loop for result in results-strings-list
				    collect (nth 7 result)))
	(setq current-form-list (cl-loop for result in results-strings-list
					 collect (-take-last 5 result)))
        `(("teams-full-name" . ,teams-full-name)
	  ("teams" . ,teams)
	  ("played" . ,played)
	  ("wins" . ,wins)
	  ("losses" . ,losses)
	  ("goal-for" . ,goal-for)
	  ("goal-against" . ,goal-against)
	  ("current-form-list" . ,current-form-list))))))

(defun soccer--table-data (league num top/bottom)
  "Get table for LEAGUE with TOP/BOTTOM NUM teams."
  (let* ((data-alist (soccer--get-league-table-data-alist league))
	 (team-names (cdr (assoc "teams-full-name" data-alist)))
	 (teams (cdr (assoc "teams" data-alist)))
	 (played (cdr (assoc "played" data-alist)))
	 (wins (cdr (assoc "wins" data-alist)))
	 (losses (cdr (assoc "losses" data-alist)))
	 (goal-for (cdr (assoc "goal-for" data-alist)))
	 (goal-against (cdr (assoc "goal-against" data-alist)))
	 (current-form-list (cdr (assoc "current-form-list" data-alist)))
	 (num-result (min num (length teams)))
	 (start (if (string-equal top/bottom "top") 0 (- (length teams) num-result)))
	 (stop (if (string-equal top/bottom "top") (1- num-result) (1- (length teams))))
	 (header-string (format "Rank Team %-20s Played Won Lost Drawn  GF  GA  GD  Points  Form" "Name"))
	 table-string)
    (setq table-string (cl-loop for n from start to stop
				collect (let ((team (nth n teams))
					      (team-name (nth n team-names))
					      (matches (nth n played))
					      (win (string-to-number (nth n wins)))
					      (loss (string-to-number (nth n losses)))
					      (goalF (string-to-number (nth n goal-for)))
					      (goalA (string-to-number (nth n goal-against)))
					      (current-form (nth n current-form-list))
					      current-form-string-list
					      draw)
					  (setq draw (- (string-to-number matches) win loss))
					  (setq current-form-string-list (cl-loop for form-string-full in current-form
										  collect (let ((form-string (upcase (substring form-string-full 0 1))))
											    (cond ((string-equal form-string "W") (propertize form-string 'face 'soccer-face-win))
												  ((string-equal form-string "L") (propertize form-string 'face 'soccer-face-loss))
												  ((string-equal form-string "D") (propertize form-string 'face 'soccer-face-draw))))))
					(format "%3s %4s %-20s %5s %4s %4s %5s %4s %3s %3s %5s %8s" (1+ n) team team-name matches (propertize (format "%s" win) 'face 'soccer-face-win) (propertize (format "%s" loss) 'face 'soccer-face-loss) (propertize (format "%s" draw) 'face 'soccer-face-draw) goalF goalA (- goalF goalA) (+ (* 3 win) draw) (string-join current-form-string-list)))))
    (string-join (-insert-at 0 header-string table-string) "\n")))

(defun soccer-table-top-4 (league)
  "Get table for LEAGUE with top 4 teams."
  (interactive
   (list (completing-read "league: " (soccer--get-league-names))))
  (message (soccer--table-data league 4 "top")))

(defun soccer-table-bottom-4 (league)
  "Get table for LEAGUE with bottom 4 teams."
  (interactive
   (list (completing-read "league: " (soccer--get-league-names))))
  (message (soccer--table-data league 4 "bottom")))

(defun soccer-table (league)
  "Get full rank table of a LEAGUE."
  (interactive
   (list (completing-read "league: " (soccer--get-league-names))))
  (let ((buffer-name (concat "*soccer-rank-table-" league "*")))
    (generate-new-buffer buffer-name)
    (with-current-buffer buffer-name
      (insert (soccer--table-data league 30 "top")))
    (switch-to-buffer-other-window buffer-name)))

(defun soccer-fixtures-full-in-org (league club)
  "Full fixtures of CLUB of LEAGUE saved in a org file."
  (interactive
   (let* ((league-name (completing-read "league: " (soccer--get-league-names)))
	  (club-name (completing-read "club: " (mapcar 'car (soccer-leagues--get-club-names-and-urls league-name)))))
     (list league-name club-name)))
  (soccer--get-league-data-in-org league "fixtures" nil club))

(defun soccer-fixtures-next (league club)
  "The next match of CLUB of LEAGUE."
  (interactive
   (let* ((league-name (completing-read "league: " (soccer--get-league-names)))
	  (club-name (completing-read "club: " (mapcar 'car (soccer-leagues--get-club-names-and-urls league-name)))))
     (list league-name club-name)))
  (soccer--get-league-data league "fixtures" 1 club))

(defun soccer-fixtures-all-clubs (league)
  "The next matches in a LEAGUE."
  (interactive
   (list (completing-read "league: " (soccer--get-league-names))))
  (soccer--get-league-data league "fixtures" 10))

(defun soccer-fixtures-next-5 (league club)
  "The next 5 matches in fixtures of CLUB of LEAGUE."
  (interactive
   (let* ((league-name (completing-read "league: " (soccer--get-league-names)))
	  (club-name (completing-read "club: " (mapcar 'car (soccer-leagues--get-club-names-and-urls league-name)))))
     (list league-name club-name)))
     (soccer--get-league-data league "fixtures" 5 club))


(defun soccer-results-full-in-org (league club)
  "Full results of CLUB of LEAGUE saved in a org file."
  (interactive
   (let* ((league-name (completing-read "league: " (soccer--league-names)))
	  (club-name (completing-read "club: " (mapcar 'car (soccer-leagues--get-club-names-and-urls league-name)))))
     (list league-name club-name)))
  (soccer--get-league-data-in-org league club "results" nil))

(defun soccer-results-last (league club)
  "The last result of CLUB of LEAGUE."
  (interactive
   (let* ((league-name (completing-read "league: " (soccer--get-league-names)))
	  (club-name (completing-read "club: " (mapcar 'car (soccer-leagues--get-club-names-and-urls league-name)))))
     (list league-name club-name)))
  (soccer--get-league-data league "results" 1 club))

(defun soccer-results-last-5 (league club)
  "The last 5 results of CLUB of LEAGUE."
  (interactive
   (let* ((league-name (completing-read "league: " (soccer--get-league-names)))
	  (club-name (completing-read "club: " (mapcar 'car (soccer-leagues--get-club-names-and-urls league-name)))))
     (list league-name club-name)))
  (soccer--get-league-data league "results" 5 club))

(defun soccer-results-all-clubs (league)
  "All the latest results in a LEAGUE."
  (interactive
   (list (completing-read "league: " (soccer--get-league-names))))
  (soccer--get-league-data league "results" 10))

;; score card
(defun soccer--get-scorecard-alist (date home away)
  "Get the live scorecard for a match between HOME and AWAY on DATE.  DATE should in the format DD-MM-YYYY."
  (let* ((live-match-url (downcase (replace-regexp-in-string " " "-" (concat "https://www.scorespro.com/soccer/livescore/" home "-vs-" away "/" date "/")))))
    (with-current-buffer (url-retrieve-synchronously live-match-url)
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	     (title (dom-strings (dom-by-tag dom 'title)))
	     (home-goal-scorers (dom-strings (dom-by-class dom "sp-goalscorers__home-team")))
	     (away-goal-scorers (dom-strings (dom-by-class dom "sp-goalscorers__away-team")))
	     (home-goals (/ (length home-goal-scorers) 2))
	     (away-goals (/ (length away-goal-scorers) 2))
	     (home-scorecard-strings-list (delete-dups (-partition 2 home-goal-scorers)))
	     (away-scorecard-strings-list (delete-dups (-partition 2 away-goal-scorers))))
	(when title
	  `(("title" . ,title)
	    ("home" . ,home)
	    ("away" . ,away)
	    ("home-goals" . ,home-goals)
	    ("away-goals" . ,away-goals)
	    ("home-scorecard-list" . ,home-scorecard-strings-list)
	    ("away-scorecard-list" . ,away-scorecard-strings-list)))))))

(defun soccer--all-clubs ()
  "Get all club names."
  (delete-dups (-flatten (cl-loop for leagues in (soccer--league-names)
				  collect (mapcar 'car (cdr (assoc leagues soccer-leagues--leagues-alist)))))))

(defun soccer-scorecard (date home away)
  "Get the socrecard for a match between HOME and AWAY on a DATE.  Enter DATE in YYYY-MM-DD format if entering it manually.  If the input is from `org-read-date' calendar popup then it is in YYYY-MM-DD format by default."
  (interactive
   (list (org-read-date nil nil nil "Date of the match: ") (completing-read "home/away club/nation: " (soccer--all-clubs)) (completing-read "away/home club/nation: " (soccer--all-clubs))))
  (let* ((day-moth-year (soccer-time--get-day-month-year-number-from-date date "-" 2 1 0))
	 (dd (-first-item day-moth-year))
	 (mm (-second-item day-moth-year))
	 (yyyy (-third-item day-moth-year))
	 (new-date (format "%02d-%02d-%04d" dd mm yyyy))
	 (scorecard-alist  (or (soccer--get-scorecard-alist new-date home away) (soccer--get-scorecard-alist new-date away home)))
	 (title (cdr (assoc "title" scorecard-alist))))
    (if title
	(let* ((home-goals (cdr (assoc "home-goals" scorecard-alist)))
	       (away-goals (cdr (assoc "away-goals" scorecard-alist)))
	       (home-team (cdr (assoc "home" scorecard-alist)))
	       (away-team (cdr (assoc "away" scorecard-alist)))
	       (home-scorecard-list (cdr (assoc "home-scorecard-list" scorecard-alist)))
	       (away-scorecard-list (cdr (assoc "away-scorecard-list" scorecard-alist)))
	       (home-scorecard-goals (delete-dups (cl-loop for g in home-scorecard-list
							   collect (format "%s(%s)" (-second-item g) (-first-item g)))))
	       (away-scorecard-goals (delete-dups (cl-loop for g in away-scorecard-list
							   collect (format "%s(%s)" (-first-item g) (-second-item g)))))
	       (home-scorecard (format "%-20s %s: %s" home-team (length home-scorecard-goals) (string-join home-scorecard-goals ", ")))
	       (away-scorecard (format "%-20s %s: %s" away-team (length away-scorecard-goals) (string-join away-scorecard-goals  ", "))))
	  (message (format "%s\n%s\n%s" (propertize (-first-item title) 'face 'soccer-face-scorecard-header)
			   (propertize home-scorecard 'face (cond ((> home-goals away-goals) 'soccer-face-win)
								  ((= home-goals away-goals) 'soccer-face-draw)
								  ((< home-goals away-goals) 'soccer-face-loss)))
			   (propertize away-scorecard 'face (cond ((> home-goals away-goals) 'soccer-face-loss)
								  ((= home-goals away-goals) 'soccer-face-draw)
								  ((< home-goals away-goals) 'soccer-face-win))))))
      (message "Result not found. Probably entered wrong date/name?\nTry `soccer-fixtures-next` to get the correct match info."))))

(provide 'soccer)
;;; soccer.el ends here
