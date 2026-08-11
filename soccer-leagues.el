;;; soccer-leagues.el --- Part of soccer.el, data for soccer leagues  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/soccer
;; Package-Requires: ((emacs "29.1"))
;; Keywords: games
;; Version: 2.0.0

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

;; Discovery of the available competitions and of the clubs taking part in
;; them.  Both lists are scraped lazily, on first use, and then cached: the
;; competition list used to be fetched while this file was being loaded,
;; which blocked Emacs startup and broke outright when offline.

;;; Code:

(require 'dom)
(require 'cl-lib)
(require 'soccer-source)

(defgroup soccer-leagues nil
  "Customization group for soccer leagues."
  :group 'soccer
  :link '(url-link :tag "Homepage" "https://github.com/md-arif-shaikh/soccer"))

(defconst soccer-leagues--base-url "https://www.theguardian.com"
  "Origin of the site the data is scraped from.")

(defun soccer-leagues--get-base-url ()
  "Get the base url of the website to get data from."
  (concat soccer-leagues--base-url "/football/"))

(defvar soccer-leagues--leagues-alist nil
  "Cached alist of (COMPETITION-NAME . URL).
Populated on first use by `soccer-leagues--leagues'.")

(defun soccer-leagues--get-competition-names-and-urls ()
  "Get the competition names and the corresponding urls."
  (let* ((url (concat soccer-leagues--base-url "/football/competitions"))
         (dom (soccer-source-dom url))
         (sections (dom-elements (dom-by-id dom "maincontent")
                                 'data-container-name "^nav/list$")))
    (cl-loop for section in sections
             append (cl-loop for item in (dom-by-tag section 'li)
                             for name = (car (dom-strings item))
                             for href = (dom-attr (car (dom-by-tag item 'a)) 'href)
                             when (and name href)
                             collect (cons (string-trim name)
                                           (concat soccer-leagues--base-url href))))))

(defun soccer-leagues--leagues (&optional force)
  "Return the alist of competitions, fetching it if needed.
With FORCE non-nil, refetch even when a cached list is available."
  (when (or force (null soccer-leagues--leagues-alist))
    (setq soccer-leagues--leagues-alist
          (soccer-leagues--get-competition-names-and-urls)))
  (unless soccer-leagues--leagues-alist
    (user-error "Soccer: could not read the list of competitions"))
  soccer-leagues--leagues-alist)

(defun soccer-leagues--get-base-league-url (league)
  "Get the base url for a LEAGUE."
  (or (cdr (assoc league (soccer-leagues--leagues)))
      (user-error "Unknown league %s" league)))

(defun soccer-leagues--match-list-items (dom)
  "Return the list elements of DOM that describe a single match.
A match row is recognised structurally: it is a list item holding at
least two team containers, each carrying a club crest."
  (cl-loop for li in (dom-by-tag dom 'li)
           when (>= (length (cl-remove-if-not
                             (lambda (d) (dom-by-tag d 'picture))
                             (dom-by-tag li 'div)))
                    2)
           collect li))

(defun soccer-leagues--club-names-from-page (url)
  "Return every club name appearing in the match list at URL."
  (condition-case nil
      (cl-loop for li in (soccer-leagues--match-list-items (soccer-source-dom url))
               append (cl-loop for div in (dom-by-tag li 'div)
                               when (dom-by-tag div 'picture)
                               collect (soccer-source-text div)))
    (error nil)))

(defvar soccer-leagues--clubs-cache (make-hash-table :test #'equal)
  "Cache mapping a league name to its list of club names.")

(defun soccer-leagues--get-club-names (league &optional force)
  "Get the club names taking part in LEAGUE.
With FORCE non-nil, ignore any cached list.

The names are collected from the fixtures and results pages rather than
from the league table on purpose.  The table spells clubs out in full
\(\"AFC Bournemouth\"), while the match lists abbreviate them
\(\"Man Utd\", \"C Palace\"); since these names are what the fixtures and
results are later filtered by, they have to be taken from the same place."
  (let ((cached (gethash league soccer-leagues--clubs-cache)))
    (if (and cached (not force))
        cached
      (let* ((base (soccer-leagues--get-base-league-url league))
             (names (delete-dups
                     (append
                      (soccer-leagues--club-names-from-page (concat base "/fixtures"))
                      (soccer-leagues--club-names-from-page (concat base "/results"))))))
        (setq names (sort (delete "" names) #'string<))
        (unless names
          (user-error "Soccer: no clubs found for %s" league))
        (puthash league names soccer-leagues--clubs-cache)
        names))))

(defun soccer-leagues-refresh ()
  "Discard the cached competition and club lists."
  (interactive)
  (setq soccer-leagues--leagues-alist nil)
  (clrhash soccer-leagues--clubs-cache)
  (soccer-source-clear-cache))

(provide 'soccer-leagues)

;;; soccer-leagues.el ends here
