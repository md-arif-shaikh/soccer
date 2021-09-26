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
(setq soccer--league-names (mapcar 'car soccer--leagues))

(defun soccer--get-league-url (league club)
  "Get the url for a CLUB of LEAGUE."
  (let* ((url (cdr (assoc club (cdr (assoc league soccer--leagues))))))
    url))

(defun soccer--get-league-data (league club data-type)
  "Get data for a CLUB of a LEAGUE."
  (let* ((url (concat (soccer--get-league-url league club) (format "/%s/" data-type))))
    (with-current-buffer (url-retrieve-synchronously url)
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	     (dates (dom-by-class dom "kick_t_dt"))
	     (times (dom-by-class dom "kick_t_ko"))
	     (homes (dom-by-class dom "home_o"))
	     (awayes (dom-by-class dom "away_o"))
	     results
	     (file-name (format "/tmp/soccer_%s_%s.org" club data-type))
	     (number-of-results (length dates)))
	(when (string-equal data-type "results")
	  (setq results (dom-by-class dom "score cshas_ended")))
	(with-temp-buffer
	  (insert (format "* %s for %s\n\n" data-type club))
	  (insert "|--|--|--|--|--|\n")
	  (insert (format "|Date |Time | Home | Away | %s \n" (if (string-equal data-type "results") "score |" "")))
	  (insert "|--|--|--|--|\n")
	  (dolist (n (number-sequence 0 (1- number-of-results)))
	    (let* ((date (dom-texts (nth n dates)))
		   (time (dom-texts (nth n times)))
		   (home (dom-texts (nth n homes)))
		   (away (dom-texts (nth n awayes)))
		   result)
	      (when (string-equal data-type "results")
		(setq result (dom-texts (nth n results))))
	      (insert (format "|%s|%s|%s|%s|%s\n" date time home away (if (string-equal data-type "results") (format "%s|" result) "")))
	      (insert "|--|--|--|--|\n")))
	  (forward-line (* -1 number-of-results))
	  (org-table-align)
	  (write-region (point-min) (point-max) file-name)
	  (find-file file-name))))))

(defun soccer--fixtures (league club)
  "Fixtures of CLUB of LEAGUE."
  (interactive
   (let* ((league-name (completing-read "league: " soccer--league-names))
	  (club-name (completing-read "club: " (mapcar 'car (cdr (assoc league-name soccer--leagues))))))
     (list league-name club-name)))
  (soccer--get-league-data league club "fixtures"))

(defun soccer--results (league club)
  "Fixtures of CLUB of LEAGUE."
  (interactive
   (let* ((league-name (completing-read "league: " soccer--league-names))
	  (club-name (completing-read "club: " (mapcar 'car (cdr (assoc league-name soccer--leagues))))))
     (list league-name club-name)))
  (soccer--get-league-data league club "results"))

(provide 'soccer)
;;; soccer.el ends here
