;; -*- coding: utf-8-unix; lexical-binding: t -*-
;;; briem-joiner.el --- Add joins to text

;; Copyright (C) 2019 Helmut Eller

;; Keywords: handwriting

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Make text look like handwritten cursive Italic by inserting
;; appropriate joins between characters.  This requires the Briem
;; Handwriting font which has glyphs for the joins in its Private Use
;; Area (the fonts directory contains a copy of the fonts from
;; http://66.147.242.192/~operinan/4/4201/4201_4051.html or if the IP
;; address changes: go to briem.net / "the old site" / "The italic
;; project" / "Teaching aids" / "Software").

;; The main commands are `briem-add-joins' and `briem-remove-joins'
;; which insert respectively delete join characters.

(require 'cl-lib)

(defface briem-handwriting-face
  '((t (:family "Briem Handwriting")))
  "Face for displaying cursive italic"
  :group 'briem-faces)

(defconst briem--base (file-name-directory load-file-name))

(defun briem--join-list-filename ()
  (expand-file-name "JoinList.txt" briem--base))

(defun briem--load-join-list ()
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents (briem--join-list-filename)))
    (let ((result '()))
      (while (cond ((eobp) nil)
		   ((looking-at "#") (forward-line) t)
		   (t (push (briem--load-line) result) t)))
      (cl-reduce #'append result :from-end t))))

(defun briem--load-line ()
  (let ((result '()))
    (while (cond ((eolp) (forward-line) nil)
		 ((looking-at "[ \t]") (forward-char) t)
		 (t
		  (push (buffer-substring (point) (+ (point) 3)) result)
		  (forward-char 3)
		  t)))
    (reverse result)))

(defvar briem--join-table% nil)

(defun briem--join-table ()
  (or briem--join-table%
      (let ((tab1 (make-char-table nil)))
	(dolist (string (briem--load-join-list))
	  (let ((char1 (aref string 0))
		(join (aref string 1))
		(char2 (aref string 2)))
	    (let ((tab2 (or (aref tab1 char1)
			    (aset tab1 char1 (make-char-table nil)))))
	      (aset tab2 char2 join))))
	(setq briem--join-table% tab1))))

(defun briem-lookup (char1 char2)
  (let ((tab1 (aref (briem--join-table) char1)))
    (if tab1 (aref tab1 char2))))

(defun briem---add-joins (string)
  (let ((len (length string)))
    (with-output-to-string
      (dotimes (i len)
	(let* ((char1 (aref string i))
	       (j (1+ i))
	       (join (if (< j len)
			 (briem-lookup (aref string i) (aref string j)))))
	  (write-char char1)
	  (when join
	    (write-char join)))))))

(defun briem--add-joins (string)
  (let ((str (briem---add-joins string)))
    (add-text-properties 0 (length str) '(face briem-handwriting-face) str)
    str))

(defun briem-add-joins (start end)
  "Add joins to the region."
  (interactive "r")
  (let ((new-string (briem--add-joins (buffer-substring start end))))
    (save-excursion
      (delete-region start end)
      (goto-char start)
      (insert new-string))))

(defun briem--join-chars ()
  (let ((joins '()))
    (map-char-table (lambda (_ tab2)
		      (map-char-table (lambda (_ join)
					(cl-pushnew join joins))
				      tab2))
		    (briem--join-table))
    joins))

(defvar briem--join-regexp% nil)

(defun briem--join-regexp ()
  (or briem--join-regexp%
      (setq briem--join-regexp% (regexp-opt-charset (briem--join-chars)))))

(defun briem-remove-joins (start end)
  "Remove joins from the region."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward (briem--join-regexp) end t)
      (replace-match ""))))

(provide 'briem-joiner)
