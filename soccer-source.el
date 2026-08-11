;;; soccer-source.el --- Part of soccer.el, fetching and caching  -*- lexical-binding: t; -*-

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

;; Low level plumbing shared by the rest of soccer.el: retrieving a page
;; over HTTP, parsing it into a DOM and caching the result for a while so
;; that flipping between views does not hammer the source site.
;;
;; The upstream site renders its markup with per-build hashed CSS class
;; names (`dcr-1kxmahn' and friends), so nothing here or in the callers may
;; key on a class name.  Everything is located structurally instead, by tag
;; and by position, which survives a rebuild of the site.

;;; Code:

(require 'dom)
(require 'url)

;; The `soccer' customization group itself is defined in soccer.el, the
;; main file of the package.

(defcustom soccer-source-cache-ttl 300
  "Number of seconds a fetched page stays fresh in the cache.
Set to 0 to disable caching entirely."
  :type 'integer
  :group 'soccer)

(defcustom soccer-source-timeout 20
  "Seconds to wait for the source site before giving up."
  :type 'integer
  :group 'soccer)

(defvar soccer-source--cache (make-hash-table :test #'equal)
  "Cache mapping a URL to a cons cell of (TIMESTAMP . DOM).")

(defun soccer-source-clear-cache ()
  "Forget every cached page so the next request refetches."
  (interactive)
  (clrhash soccer-source--cache)
  (message "soccer: cache cleared"))

(defun soccer-source--fresh-p (entry)
  "Return non-nil when cache ENTRY is still within `soccer-source-cache-ttl'."
  (and entry
       (> soccer-source-cache-ttl 0)
       (< (float-time (time-subtract (current-time) (car entry)))
          soccer-source-cache-ttl)))

(defun soccer-source--retrieve (url)
  "Fetch URL and return its parsed DOM, signalling on failure."
  ;; Some CDNs answer a bare Emacs user agent with a challenge page, so
  ;; present an ordinary browser one.
  (let* ((url-request-extra-headers
          '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) \
AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0 Safari/537.36")
            ("Accept" . "text/html,application/xhtml+xml")))
         (buffer (url-retrieve-synchronously url t t soccer-source-timeout)))
    (unless buffer
      (user-error "Soccer: no response from %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          ;; Skip the HTTP headers before handing the body to libxml.
          (if (re-search-forward "\r?\n\r?\n" nil t)
              (libxml-parse-html-region (point) (point-max))
            (user-error "Soccer: malformed response from %s" url)))
      (kill-buffer buffer))))

(defun soccer-source-dom (url &optional force)
  "Return the parsed DOM for URL, using the cache unless FORCE is non-nil."
  (let ((entry (gethash url soccer-source--cache)))
    (if (and (not force) (soccer-source--fresh-p entry))
        (cdr entry)
      (let ((dom (soccer-source--retrieve url)))
        (puthash url (cons (current-time) dom) soccer-source--cache)
        dom))))

(defun soccer-source-text (node)
  "Return the whitespace collapsed text content of NODE."
  (string-trim
   (replace-regexp-in-string "[ \t\n\r ]+" " " (dom-texts node))))

(provide 'soccer-source)
;;; soccer-source.el ends here
