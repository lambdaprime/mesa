;;
;; This source file is a part of mesa project.
;; Description for this project can be found in README.org.
;;
;; Mesa is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Mesa is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with mesa. If not, see <http://www.gnu.org/licenses/>.
;;
;;

;;; Description:

;; Global functions

;;; Code:

(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'utf-8 (lambda ()
  (interactive)
  (revert-buffer-with-coding-system 'utf-8)))

(defun json-pretty-print ()
  (interactive)
  "Formats selected json and makes it look pretty"
  (shell-command-on-region (region-beginning) (region-end)
    (format "java -jar %s -json" pretty-printer-path) (current-buffer) t))

(defun html-pretty-print ()
  (interactive)
  "Formats selected html and makes it look pretty"
  (shell-command-on-region (region-beginning) (region-end)
    (format "java -jar %s -html" pretty-printer-path) (current-buffer) t))

(defun xml-pretty-print ()
  (interactive)
  "Formats selected xml and makes it look pretty"
  (shell-command-on-region (region-beginning) (region-end)
    (format "java -jar %s -xml" pretty-printer-path) (current-buffer) t))

(defun pre ()
  "Wraps selected text into <pre> </pre>"
  (interactive)
  (kill-region (region-beginning) (region-end))
  (insert (format "<pre>%s</pre>" (clipboard-content))))

(defun ul ()
  "Wraps selected text into <pre> </pre>"
  (interactive)
  (fset 'li [home ?< ?l ?i ?> end ?< ?/ ?l ?i ?>])
  (apply-macro-to-region-lines (region-beginning) (region-end) 'li)
  (kill-region (region-beginning) (region-end))
  (insert (format "<ul>\n%s</ul>" (clipboard-content))))

(require 'ido)

(defvar favourites nil)

(defun favourite (fav)
  (interactive
    (list (ido-completing-read "Open: " favourites)))
  (let ((default-directory (cdr (assoc fav favourites))))
    (call-interactively 'ido-find-file)))

(defun delete-blank-lines ()
  "Delete all blank lines in the current buffer"
  (interactive)
  (goto-line 0)
  (while (search-forward "\n\n" nil t)
    (replace-match "\n" nil t)
    (goto-line 0)))

