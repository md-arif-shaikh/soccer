;;; soccer-time.el --- Part of soccer.el, utilities for time conversion  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/soccer
;; Package-Requires: ((emacs "24.4"))
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
;; Utilities for conversion of time to local time and other nice stuffs
;; 

;;; Code:

(require 'org)
(defcustom soccer-time--local-time-utc-offset "+0530"
  "UTC offfset for local time zone."
  :type 'string
  :group 'soccer)

(defcustom soccer-time--source-time-utc-offset "+0200"
  "UTC offfset for source time zone."
  :type 'string
  :group 'soccer)

(defun soccer-time--get-day-name (day month year)
  "Get the day name from DAY MONTH and YEAR."
  (aref calendar-day-abbrev-array (nth 6 (decode-time (encode-time (list 0 0 0 day month year nil nil nil))))))

(defun soccer-time--get-day-month-year-number-from-date (date separator day-position month-position year-position)
  "Get day, month and year numbers from a DATE with SEPARATOR.  DAY-POSITION, MONTH-POSITION and YEAR-POSITION helps to recognize the format of date."
  (let* ((splitted-strings (split-string date separator))
	 (day (string-to-number (nth day-position splitted-strings)))
	 (month (string-to-number (nth month-position splitted-strings)))
	 (year (string-to-number (nth year-position splitted-strings))))
    (list day month year)))

(defun soccer-time--get-day-name-from-date (date separator day-position month-position year-position)
  "Get day name from a DATE with SEPARATOR.  DAY-POSITION, MONTH-POSITION and YEAR-POSITION helps to recognize the format of date."
  (let* ((splitted-strings (split-string date separator))
	 (day (string-to-number (nth day-position splitted-strings)))
	 (month (string-to-number (nth month-position splitted-strings)))
	 (year (string-to-number (nth year-position splitted-strings))))
    (soccer-time--get-day-name day month year)))

(defun soccer-time--get-month-name-from-date (date separator month-position)
  "Get month name from a DATE with SEPARATOR.  MONTH-POSITION is the position of month number in the date."
  (let* ((splitted-strings (split-string date separator))
	 (month (string-to-number (nth month-position splitted-strings))))
    (aref calendar-month-abbrev-array (1- month))))

(defun soccer-time--get-year-from-date (date separator year-position)
  "Get year from a DATE with SEPARATOR.  YEAR-POSITION is the position of month number in the date."
  (let* ((splitted-strings (split-string date separator)))
    (string-to-number (nth year-position splitted-strings))))

(defun soccer-time--get-day-from-date (date separator day-position)
  "Get day number from a DATE with SEPARATOR.  DAY-POSITION is the position of month number in the date."
  (let* ((splitted-strings (split-string date separator)))
    (string-to-number (nth day-position splitted-strings))))

(defun soccer-time--get-next-or-previous-date (date separator day-position month-position year-position delta)
  "Get the next or previous date with DELTA time difference for a DATE with SEPARATOR.  DAY-POSITION, MONTH-POSITION and YEAR-POSITION helps to recognize the format of date."
  (let* ((day-month-year (soccer-time--get-day-month-year-number-from-date date separator day-position month-position year-position))
	 (day (nth 0 day-month-year))
	 (month (nth 1 day-month-year))
	 (year (nth 2 day-month-year))
	 (org-date-string (org-read-date nil nil delta nil (encode-time (list 0 0 0 day month year nil nil nil))))
	 (org-date-strings-list (split-string org-date-string "-"))
	 (org-year (nth 0 org-date-strings-list))
	 (org-month (nth 1 org-date-strings-list))
	 (org-day (nth 2 org-date-strings-list))
	 (next-date-string-list '(0 0 0)))
    (setf (nth day-position next-date-string-list) org-day)
    (setf (nth month-position next-date-string-list) org-month)
    (setf (nth year-position next-date-string-list) org-year)
    (string-join next-date-string-list (if (string-equal separator "\\.") "." separator))))

(defun soccer-time--get-local-time-list (time date separator day-position month-position year-position source-utc-offset local-utc-offset)
  "Get local time in form of list of mins, hours, AM/PM, dayname, daynumber, monthname year for given TIME, DATE, SOURCE-UTC-OFFSET and LOCAL-UTC-OFFSET.  SEPARATOR is the separator in the DATE and DAY-POSITION, MONTH-POSITION and YEAR-POSITION are positions of day, month and year in the date."
  (let* ((source-time-list (parse-time-string (format "%s %s" time source-utc-offset)))
	 (source-min (nth 1 source-time-list))
	 (source-hour (nth 2 source-time-list))
	 (source-offset (nth 8 source-time-list))
	 (local-offset (nth 8 (parse-time-string (format "%s %s" time local-utc-offset))))
	 (seconds-shift (- local-offset source-offset))
	 (hour-shift (/ seconds-shift 3600))
	 (min-shift (/ (% seconds-shift 3600) 60))
	 (local-min (+ source-min min-shift))
	 (local-hour (+ source-hour hour-shift))
	 (day-offset "++0")
	 AM/PM
	 local-date
	 local-dayname
	 local-daynumber
	 local-monthname
	 local-year)
    (cond ((< local-min 0) (progn
			     (setq local-min (+ local-min 60))
			     (setq local-hour (1- local-hour))))
	  ((>= local-min 60) (progn
			       (setq local-min (- local-min 60))
			       (setq local-hour (1+ local-hour)))))
    (cond ((< local-hour 0) (progn
				(setq local-hour (+ local-hour 24))
				(setq day-offset "--1")))
	  ((>= local-hour 24) (progn
				(setq local-hour (- local-hour 24))
				(setq day-offset "++1"))))
    (if (>= local-hour 12)
	(progn
	  (setq local-hour (- local-hour 12))
	  (setq AM/PM "PM"))
      (setq AM/PM "AM"))
    (setq local-date (soccer-time--get-next-or-previous-date date separator day-position month-position year-position day-offset))
    (setq local-dayname (soccer-time--get-day-name-from-date local-date separator day-position month-position year-position))
    (setq local-daynumber (soccer-time--get-day-from-date local-date separator day-position))
    (setq local-monthname (soccer-time--get-month-name-from-date local-date separator month-position))
    (setq local-year (soccer-time--get-year-from-date local-date separator year-position))
    (list local-min local-hour AM/PM local-dayname local-daynumber local-monthname local-year)))

(defun soccer-time--get-time-till-kick-off (time date separator day-position month-position year-position source-utc-offset local-utc-offset)
  "Get get day hour mins till kick-off from given TIME, DATE, SOURCE-UTC-OFFSET and LOCAL-UTC-OFFSET.  SEPARATOR is the separator in the DATE and DAY-POSITION, MONTH-POSITION and YEAR-POSITION are positions of day, month and year in the date."
  (let* ((local-time-list (soccer-time--get-local-time-list time date separator day-position month-position year-position source-utc-offset local-utc-offset))
	 (local-min (nth 0 local-time-list))
	 (local-hour (nth 1 local-time-list))
	 (A/P (nth 2 local-time-list))
	 (local-daynumber (nth 4 local-time-list))
	 (local-monthname (nth 5 local-time-list))
	 (local-year (nth 6 local-time-list))
	 local-time-string
	 time-delta-seconds
	 days-remain
	 hours-remain
	 mins-remain)
    (when (string-equal A/P "PM")
      (setq local-hour (+ 12 local-hour)))
    (setq local-time-string (format "%s %s, %s %s:%s %s" local-monthname local-daynumber local-year local-hour (if (< local-min 10) (concat "0" (format "%s" local-min)) local-min) local-utc-offset))
    (setq time-delta-seconds (time-subtract (date-to-time local-time-string) 
					    (date-to-time (current-time-string))))
    (setq days-remain (/ time-delta-seconds (* 24 60 60)))
    (setq hours-remain (/ (% time-delta-seconds (* 24 60 60)) (* 60 60)))
    (setq mins-remain (/ (% (% time-delta-seconds (* 24 60 60)) (* 60 60)) 60))
    (list days-remain hours-remain mins-remain)))

(provide 'soccer-time)
;;; soccer-time.el ends here
