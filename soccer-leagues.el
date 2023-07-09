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
  (let* ((base-url "https://www.theguardian.com") (url (concat base-url "/football/competitions")))
    (with-current-buffer (url-retrieve-synchronously url)
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
             (sections (dom-elements (dom-by-id dom "maincontent") 'data-container-name "^nav/list$"))
             (url-list (cl-loop for section in sections
                                append (cl-loop for item in (dom-by-tag section 'li)
                                                for name = (car (dom-strings item))
                                                for url = (concat base-url (dom-attr (dom-by-tag item 'a) 'href))
                                                collect (cons name url)))))
        url-list))))

(defgroup soccer-leagues nil
  "Customization group for soccer leagues."
  :group 'soccer
  :link '(url-link :tag "Homepage" "https://github.com/md-arif-shaikh/soccer"))

(defvar soccer-leagues--leagues-alist)
(setq soccer-leagues--leagues-alist (soccer-leagues--get-competition-names-and-urls))

(provide 'soccer-leagues)

;;; soccer-leagues.el ends here
