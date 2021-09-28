;;; soccer.el --- fixtures, results, etc for soccer  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Keywords: 

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

;;; Code:
(require 'soccer-leagues)
(require 'org)
(require 'dom)
(require 'convert-time)

(defcustom soccer--local-time-zone-abrv "IST"
  "Abbreviation for local time zone."
  :type 'string
  :group 'soccer)

(defcustom soccer--source-time-zone-abrv "CEST"
  "Abbreviation for source time zone."
  :type 'string
  :group 'soccer)

(defface soccer-face--win
  '((t :foreground "green"
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for win."
  :group 'soccer-face)

(defface soccer-face--loss
  '((t :foreground "red"
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for loss."
  :group 'soccer-face)

(defface soccer-face--draw
  '((t :foreground "cyan"
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for draw."
  :group 'soccer-face)

(defvar soccer--league-names)
(setq soccer--league-names (mapcar 'car soccer--leagues))

(defun soccer--get-league-url (league club)
  "Get the url for a CLUB of LEAGUE."
  (let* ((url (cdr (assoc club (cdr (assoc league soccer--leagues))))))
    url))

(defun soccer--get-league-data-alist (league club data-type)
  "Get data for a CLUB of a LEAGUE."
  (let* ((url (concat (soccer--get-league-url league club) (format "/%s/" data-type))))
    (with-current-buffer (url-retrieve-synchronously url)
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	     (dates-dom (dom-by-class dom "kick_t_dt"))
	     (times-dom (dom-by-class dom "kick_t_ko"))
	     (homes-dom (dom-by-class dom "home_o"))
	     (awayes-dom (dom-by-class dom "away_o"))
	     results-dom
	     (file-name (format "/tmp/soccer_%s_%s.org" club data-type))
	     (number-of-results (length dates-dom))
	     dates
	     times
	     homes
	     awayes
	     results)
	(when (string-equal data-type "results")
	  (setq results-dom (dom-by-class dom "score cshas_ended"))
	  (setq results (cl-loop for n from 0 to number-of-results
			       collect (dom-texts (nth n results-dom)))))
	(setq dates (cl-loop for n from 0 to number-of-results
			     collect (dom-texts (nth n dates-dom))))
	(setq times (cl-loop for n from 0 to number-of-results
			     collect (dom-texts (nth n times-dom))))
	(setq homes (cl-loop for n from 0 to number-of-results
			     collect (dom-texts (nth n homes-dom))))
	(setq awayes (cl-loop for n from 0 to number-of-results
			      collect (dom-texts (nth n awayes-dom))))
	`(("date" . ,dates)
	  ("time" . ,times)
	  ("home" . ,homes)
	  ("away" . ,awayes)
	  ("result" . ,results))))))

(defun soccer--prepend-zero (num)
  "Prepend zero to NUM."
  (if (< num 10)
      (concat "0" (format "%s" num))
    num))

(defun soccer--get-league-data (league club data-type num-of-results)
  "Get NUM-OF-RESULT number of DATA-TYPE for a CLUB of a LEAGUE."
  (let* ((league-data (soccer--get-league-data-alist league club data-type))
	 (dates (cdr (assoc "date" league-data)))
	 (times (cdr (assoc "time" league-data)))
	 (homes (cdr (assoc "home" league-data)))
	 (awayes (cdr (assoc "away" league-data)))
	 results
	 msg-str
	 (number-of-results (length dates)))
    (when (string-equal data-type "results")
      (setq results (cdr (assoc "result" league-data))))
    (if num-of-results
	(setq num-of-results (min number-of-results num-of-results))
      (setq num-of-results number-of-results))
    (setq msg-str (cl-loop for n from 0 to (1- num-of-results)
			   collect (let* ((date (nth n dates))
					  (time (nth n times))
					  (local-time-list (convert-time--get-converted-time time soccer--source-time-zone-abrv soccer--local-time-zone-abrv))
					  (local-min (nth 0 local-time-list))
					  (local-hour (nth 1 local-time-list))
					  (local-A/P (nth 2 local-time-list))
					  (local-day (nth 3 local-time-list))
					  (home (nth n homes))
					  (away (nth n awayes))
					  (local-date (cond ((string-equal local-day "+0d") date)
							    ((string-equal local-day "+1d") (convert-time--get-next-or-previous-date date "\\." 0 1 2 "++1"))
							    ((string-equal local-day "-1d") (convert-time--get-next-or-previous-date date "\\." 0 1 2 "--1"))))
					  (match-day-local (convert-time--get-day-name-from-date local-date "\\." 0 1 2))
					  (match-day-num-local (convert-time--get-day-from-date local-date "\\." 0))
					  (match-month-local (convert-time--get-month-name-from-date local-date "\\." 1))
					  (match-year-local (convert-time--get-year-from-date local-date "\\." 2))
					  result
					  home-goals
					  away-goals
					  )
				     (if (< match-year-local 2000)
					 (setq match-year-local (+ 2000 match-year-local)))
				     (when (string-equal data-type "results")
				       (setq result (split-string (nth n results)))
				       (setq home-goals (car result))
				       (setq away-goals (nth 0 (last result))))
				     (if (string-equal data-type "results")
					 (format "%s %s  Local Time: %s %s %s %s %s:%s %s %s" date time match-year-local match-month-local (soccer--prepend-zero match-day-num-local) match-day-local (soccer--prepend-zero local-hour) (soccer--prepend-zero local-min) local-A/P  
						 (cond ((> (string-to-number home-goals) (string-to-number away-goals))
							(format "%s - %s" (propertize (concat home " " home-goals) 'face 'soccer-face--win) (propertize (concat away-goals " " away) 'face 'soccer-face--loss)))
						       ((< (string-to-number home-goals) (string-to-number away-goals))
							(format "%s - %s" (propertize (concat home " " home-goals) 'face 'soccer-face--loss) (propertize (concat away-goals " " away) 'face 'soccer-face--win)))
						       ((= (string-to-number home-goals) (string-to-number away-goals))
							(format "%s - %s" (propertize (concat home " " home-goals) 'face 'soccer-face--draw) (propertize (concat away-goals " " away) 'face 'soccer-face--draw)))))
				       (format "%s %s  Local Time: %s %s %s %s %s:%s %s %s - %s" date time match-year-local match-month-local (soccer--prepend-zero match-day-num-local) match-day-local (soccer--prepend-zero local-hour) (soccer--prepend-zero local-min) local-A/P home away)))))
    (message "%s" (string-join msg-str "\n"))))

(defun soccer--get-league-data-in-org (league club data-type num-of-results)
  "Get NUM-OF-RESULT number of DATA-TYPE for a CLUB of a LEAGUE."
  (let* ((league-data (soccer--get-league-data-alist league club data-type))
	 (dates (cdr (assoc "date" league-data)))
	 (times (cdr (assoc "time" league-data)))
	 (homes (cdr (assoc "home" league-data)))
	 (awayes (cdr (assoc "away" league-data)))
	 results
	 (file-name (format "/tmp/soccer_%s_%s.org" club data-type))
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
	       (time (nth n times))
	       (home (nth n homes))
	       (away (nth n awayes))
	       result)
	  (when (string-equal data-type "results")
	    (setq result (nth n results)))
	  (insert (format "|%s|%s|%s|%s|%s\n" date time home away (if (string-equal data-type "results") (format "%s|" result) "")))
	  (insert "|--|--|--|--|\n")))
      (forward-line (* -1 num-of-results))
      (org-table-align)
      (write-region (point-min) (point-max) file-name)
      (find-file file-name))))

(defun soccer--fixtures-full-in-org (league club)
  "Fixtures of CLUB of LEAGUE."
  (interactive
   (let* ((league-name (completing-read "league: " soccer--league-names))
	  (club-name (completing-read "club: " (mapcar 'car (cdr (assoc league-name soccer--leagues))))))
     (list league-name club-name)))
  (soccer--get-league-data-in-org league club "fixtures" nil))

(defun soccer--fixtures-next (league club)
  "Fixtures of CLUB of LEAGUE."
  (interactive
   (let* ((league-name (completing-read "league: " soccer--league-names))
	  (club-name (completing-read "club: " (mapcar 'car (cdr (assoc league-name soccer--leagues))))))
     (list league-name club-name)))
  (soccer--get-league-data league club "fixtures" 1))

(defun soccer--fixtures-next-5 (league club)
  "Fixtures of CLUB of LEAGUE."
  (interactive
   (let* ((league-name (completing-read "league: " soccer--league-names))
	  (club-name (completing-read "club: " (mapcar 'car (cdr (assoc league-name soccer--leagues))))))
     (list league-name club-name)))
  (soccer--get-league-data league club "fixtures" 5))


(defun soccer--results-full-in-org (league club)
  "Fixtures of CLUB of LEAGUE."
  (interactive
   (let* ((league-name (completing-read "league: " soccer--league-names))
	  (club-name (completing-read "club: " (mapcar 'car (cdr (assoc league-name soccer--leagues))))))
     (list league-name club-name)))
  (soccer--get-league-data-in-org league club "results" nil))

(defun soccer--results-last (league club)
  "Fixtures of CLUB of LEAGUE."
  (interactive
   (let* ((league-name (completing-read "league: " soccer--league-names))
	  (club-name (completing-read "club: " (mapcar 'car (cdr (assoc league-name soccer--leagues))))))
     (list league-name club-name)))
  (soccer--get-league-data league club "results" 1))

(defun soccer--results-last-5 (league club)
  "Fixtures of CLUB of LEAGUE."
  (interactive
   (let* ((league-name (completing-read "league: " soccer--league-names))
	  (club-name (completing-read "club: " (mapcar 'car (cdr (assoc league-name soccer--leagues))))))
     (list league-name club-name)))
  (soccer--get-league-data league club "results" 5))

(provide 'soccer)
;;; soccer.el ends here
