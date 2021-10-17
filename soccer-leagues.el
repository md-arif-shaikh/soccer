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

(defun soccer-leagues--get-club-names-and-urls (country league)
  "Get the team names and the corresponding urls for a LEAGUE in a COUNTRY."
  (let* ((url (string-replace " " "-" (format "https://www.scorespro.com/soccer/%s/%s/teams/" (downcase country) (downcase league)))))
    (with-current-buffer (url-retrieve-synchronously url)
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	     (data-dom (dom-by-class dom "team st-br uc"))
	     (url-list (cl-loop for d in data-dom
				collect (cons (car (dom-strings (dom-by-tag d 'a))) (concat "https://www.scorespro.com" (dom-attr (dom-by-tag d 'a) 'href))))))
        url-list))))

(defgroup soccer-leagues nil
  "Customization group for soccer leagues."
  :group 'soccer)

(defcustom soccer-leagues-alist '(("England" . "Premier League")
				  ("Spain" . "Laliga")
				  ("France" . "Ligue 1")
				  ("Italy" . "Serie A")
				  ("Germany" . "Bundesliga")
				  ("England" . "Championship"))
  "Each element is a cons-cell (CONTRY . LEAGUE)."
  :type '(repeat (cons :tag "COUNTRY"
		       (choice
			(string :tag "Name")
			(const "Table"))
		       (string :tag "CLUB")))
  :group 'soccer-leagues)

(defvar soccer-leagues--leagues-alist)
(setq soccer-leagues--leagues-alist
      (let* ((country-list (mapcar 'car soccer-leagues-alist))
	     (league-list (mapcar 'cdr soccer-leagues-alist)))
	(cl-loop for n from 0 to (1- (length country-list))
		 collect (let* ((country (nth n country-list))
				(league (nth n league-list))
				(name (format "%s: %s" country league))
				(urls (soccer-leagues--get-club-names-and-urls country league)))
			   (cons name urls)))))

(provide 'soccer-leagues)

;;; soccer-leagues.el ends here

