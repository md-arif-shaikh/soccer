;;; soccer-leagues.el --- Part of soccer.el, data for soccer leagues  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/soccer
;; Package-Requires: ((emacs "25.1"))
;; Keywords: games
;; Version: 1.1.0

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

(defgroup soccer-leagues nil "Customization group for soccer leagues.")

(defcustom soccer-leagues--leagues-alist
  `(("England: Premier League" . ,(soccer-leagues--get-club-names-and-urls "England" "Premier League"))
    ("Spain: LaLiga" . ,(soccer-leagues--get-club-names-and-urls "Spain" "LaLiga"))
    ("France: Ligue 1" . ,(soccer-leagues--get-club-names-and-urls "France" "Ligue 1"))
    ("Italy: Serie A" . ,(soccer-leagues--get-club-names-and-urls "Italy" "Serie A"))
    ("Germany: Bundesliga" . ,(soccer-leagues--get-club-names-and-urls "Germany" "Bundesliga"))
    ("England: Championship" . ,(soccer-leagues--get-club-names-and-urls "England" "Championship")))
  "Alist of soccer league data."
  ;; Specifying the table is a little klunky.  We want one table per
  ;; league.
  :type '(repeat (cons :tag "League"
                       (string :tag "League Name")
                       (repeat :tag "Teams"
                               (cons :tag "Team/Table"
                                     (choice
                                      (string :tag "Name")
                                      (const "Table"))
                                     (string :tag "URL")))))
  :group 'soccer-leagues)

(provide 'soccer-leagues)

;;; soccer-leagues.el ends here
