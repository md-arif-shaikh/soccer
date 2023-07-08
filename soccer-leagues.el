;;; soccer-leagues.el --- Part of soccer.el, data for soccer leagues  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/soccer
;; Package-Requires: ((emacs "25.1"))
;; Keywords: games
;; Version: 1.2.0

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

;; Local variables:
;; package-lint-main-file: "soccer.el"
;; end:

;;; Commentary:
;; This contains list of urls for different clubs to get the relevant data.
;;

;;; Code:

(require 'dom)
(require 's)

(defun soccer-leagues--get-base-url ()
  "Get the base url of the website to get data from."
  "https://www.theguardian.com/football/")

(defun soccer-leagues--get-base-league-url (league)
  "Get the base url for a LEAGUE."
  (if (member league (mapcar #'car soccer-leagues--leagues-alist))
      (cdr (assoc league soccer-leagues--leagues-alist))
    (user-error "Unknown league %s" league)))

(defun soccer-leagues--get-club-names-and-urls (league)
  "Get the team names and the corresponding urls for a LEAGUE."
  (let* ((url (concat (soccer-leagues--get-base-league-url league)  "/table")))
    (with-current-buffer (url-retrieve-synchronously url)
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	     (data-dom (dom-by-class dom "team-name__long")))
	(cl-loop for d in data-dom
		 collect (cons (s-trim (-first-item (dom-strings d))) (dom-attr d 'href)))))))

;;; This function is no longer used. Instead we use competition tab
(defun soccer-leagues--get-league-names-and-urls ()
  "Get the team names and the corresponding urls."
  (let* ((url "https://www.theguardian.com/football/teams"))
    (with-current-buffer (url-retrieve-synchronously url)
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	     (data-dom (dom-by-class dom "fc-container__header__title"))
	     (url-list (cl-loop for d in data-dom
				collect (cons (car (dom-strings (dom-by-class d "fc-container__title__text"))) (dom-attr (dom-by-tag d 'a) 'href)))))
        url-list))))

(defun soccer-leagues--get-competition-names-and-urls ()
  "Get the competition names and the corresponding urls."
  (let* ((url "https://www.theguardian.com/football/competitions"))
    (with-current-buffer (url-retrieve-synchronously url)
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	     (data-dom (dom-by-class dom "fc-item fc-item--list-compact"))
	     (url-list (cl-loop for d in data-dom
				collect (cons (car (dom-strings d)) (dom-attr (dom-by-tag d 'a) 'href)))))
        url-list))))

(defgroup soccer-leagues nil
  "Customization group for soccer leagues."
  :group 'soccer
  :link '(url-link :tag "Homepage" "https://github.com/md-arif-shaikh/soccer"))

;;; The functions to fetch league and urls is not working anymore. Hardcoding some of these.
;;;(setq soccer-leagues--leagues-alist (soccer-leagues--get-competition-names-and-urls))
(defcustom soccer-leagues-leagues-alist '(("Premier League" . "https://www.theguardian.com/football/premierleague")
					  ("La Liga" . "https://www.theguardian.com/football/laligafootball")
					  ("Serie A" . "https://www.theguardian.com/football/serieafootball")
					  ("Bundeshliga" . "https://www.theguardian.com/football/bundesligafootball")
					  ("Ligue 1" . "https://www.theguardian.com/football/ligue1football")
					  ("Champions League" . "https://www.theguardian.com/football/championsleague")
					  ("FA Cup" . "https://www.theguardian.com/football/fa-cup")
					  ("Carabao Cup" . "https://www.theguardian.com/football/carabao-cup")
					  ("Championship" . "https://www.theguardian.com/football/championship")
					  ("Europa League" . "https://www.theguardian.com/football/uefa-europa-league")
					  ("Conference League" . "https://www.theguardian.com/football/europa-conference-league")
					  ("MLS" . "https://www.theguardian.com/football/mls")
					  ("Women's Super League" . "https://www.theguardian.com/football/womens-super-league"))
  "Alist of league and urls."
  :type '(alist :value-type (group integer))
  :group 'soccer)

(provide 'soccer-leagues)

;;; soccer-leagues.el ends here
