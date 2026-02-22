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

;; Helper functions definitions.

;;; Code:

(defun buffer-current-line ()
  "Returns number of line where cursor is."
  (let ((l (what-line)))
    (string-match "Line \\(.*\\)" l)
    (string-to-number (match-string 1 l))))

(defun buffer-lines-number ()
  "Returns number of lines in the current buffer."
  (count-lines (point-min) (point-max)))

(defun copy-to-clipboard (str)
  "Copy STR to clipboard."
  (with-temp-buffer 
    (insert str)
    (kill-new str)))

(defun clipboard-content ()
  "Returns clipboard content as a string."
  (with-temp-buffer
    (if (not (zerop (length kill-ring)))
      (yank))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun slash-2-backslash ()
  "Interactive function which reads string from clipboard and 
converts slash symbols in it to back slashes. Saves it back 
to clipboard."
  (interactive)
  (copy-to-clipboard (slash-to-backslash (clipboard-content))))

(defun slash-to-backslash (str)
  "Returns STR where all slash symbols changed to two back slashes"
  (with-temp-buffer 
    (insert str)
    (replace-string "/" "\\" nil (point-min) (point-max)) 
    (buffer-substring-no-properties (point-min) (point-max))))

(defun slash-to-dbl-backslash (str)
  "Returns STR where all slash symbols changed to two back slashes"
  (with-temp-buffer 
    (insert str)
    (replace-string "/" "\\\\" nil (point-min) (point-max)) 
    (buffer-substring-no-properties (point-min) (point-max))))

(defun backslash-to-slash (str)
  "Returns STR where all backslash symbols changed to slashes"
  (with-temp-buffer 
    (insert str)
    (replace-string "\\" "/" nil (point-min) (point-max)) 
    (buffer-substring-no-properties (point-min) (point-max))))

(defun string-starts-with (str1 str2)
  "Returns t if STR1 starts with STR2"
  (if (<= (length str2) (length str1))
   (string= (substring str1 0 (length str2)) str2)))

(defun string-suffix-p (suffix str &optional ignore-case)
  "Returns t if STR ends with SUFFIX"
  (if ignore-case
    (setq suffix (downcase suffix) str (downcase str)))
  (if (<= (length suffix) (length str))
   (string= (substring str (- (length str) (length suffix)) ) suffix)))

(defun get-parent-dir (full-name) 
  (setq len (1- (length full-name)))
  (if (= (elt full-name len) ?/)
    (setq len (1- len)))
  (while (and (>= len 0)
              (not (= (elt full-name len) ?/)))
    (setq len (1- len)))
  (substring full-name 0 (1+ len)))

(defun make-tmp-dir(path) 
  "Creates a temp directory in PATH and returns it"
  (setq path (file-name-as-directory path))
  (let ((dir ".tmp"))
    (while (file-directory-p (concat path dir))
      (setq dir (format ".tmp%s" (random 100000))))
    (setq dir (format "%s%s/" path dir))
    (make-directory dir)
    dir))

(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (save-excursion
     (set-buffer buffer-or-string)
     major-mode))

;(defun \\ ()
;  "Opens windows explorer in current dired buffer directory"
;  (interactive)
;  (if (eq major-mode 'dired-mode)
;    (dired-do-async-shell-command 
;      (concat 
;        "explorer \"" 
;        (slash-to-backslash (dired-current-directory)) "\""))))

(defun win32-path (path)
  "If it is linux then converts PATH to wine windows-like path."
  (if (eq system-type 'gnu/linux)
    (concat "Z:" (slash-to-backslash path))))

(defun cygwin-path (path)
  "Transform Windows path to cygwin's 
d:/Program Files/GnuWin32 => /cygdrive/d/Program Files/GnuWin32"
  (if (> (length path) 2)
    (concat "/cygdrive/" (substring path 0 1) (substring path 2))))

(defun ibuffer-jump-top-line()
  (goto-line 4))

(defun dired-update-dwim-target-buffer(dwim-target)
  (require 'dired-aux)
  (dolist (buffer (dired-buffers-for-dir dwim-target))
    (with-current-buffer buffer (revert-buffer))))

(defun copy-line ()
  (interactive)
  (kill-ring-save (point-at-bol) (point-at-eol))
  (message "1 line copied"))

(defun current-line()
  "Returns text of the current line"
  (beginning-of-line)
  (let ((s (point)))
    (end-of-line)
    (buffer-substring-no-properties s (point))))

(defun read-file (path)
  "Return the contents of PATH as a string, or signal a helpful error."
  (unless (file-exists-p path)
    (error "read-file: file not found: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))
