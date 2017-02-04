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

;; Dired keystrokes

;;; Code:

(require 'dired)

(define-key dired-mode-map [tab] 'other-window)

(define-key dired-mode-map [apps] (lambda ()
  (interactive)
  (w32-context-menu 
    (concat "\"" (dired-get-filename) "\""))))

(defun w32-context-menu (filename)
  (start-process-shell-command 
    "context"
    "*context*"
    "context"
    filename))

(define-key dired-mode-map "X" (lambda ()
  "Executes command in current directory"
  (interactive)
  (call-interactively 'shell-command)))

(define-key dired-mode-map "e" (lambda ()
  "Runs Windows application associated with a
file as new asynchronous subprocess of emacs (when
emacs exits all its subprocesses die). You can select
more than one file in dired buffer and it will
execute all of them one-by-one.

All process output are stored in separate
`#<process_name>#' buffer.

The disadvantage of key `&' is that it stores output
from different subprocesses in one buffer. Moreover,
sometimes it does not allow you to start more than one 
subprocess."
  (interactive)
  (require 'cl)
  (defun my-execute()
    (let (
      (output-buffer-name (concat "#" (dired-get-filename) "#"))
      (cur-buffer (current-buffer)))
      ;; if there is a buffer with such name
      (block loop (dolist (buffer (buffer-list))
        (if (string= 
              (buffer-name buffer)
              output-buffer-name)
          ;; switch to it and rename
          (progn
            (switch-to-buffer buffer)
            (rename-uniquely)
            (switch-to-buffer cur-buffer)
            (return-from loop)))))
      (if (eq system-type 'windows-nt)
        (start-process 
          "dired-process"
          output-buffer-name
          "cmd.exe"
          "/C"
	  (dired-get-filename))
	(start-process 
	  "dired-process"
          output-buffer-name
	  "run-mailcap"
	  (dired-get-filename)))))
   (dired-map-over-marks (my-execute) nil)))

(define-key dired-mode-map "*" (lambda ()
  "Mark/unmark all files in current directory."
  (interactive)
  (if (eq (length (dired-get-marked-files)) 1)
   (dired-mark-files-regexp ".*")
   (dired-unmark-all-marks))))

(define-key dired-mode-map "d" (lambda ()
  "Deletes marked files not only which marked for deletion."
  (interactive)
  (dired-do-delete)))

(define-key dired-mode-map "c" (lambda ()
  "Runs PowerShell in current directory."
  (interactive)
  (start-process 
    "dired-process"
    "*PowerShell*"
    "cmd.exe" "/C start cmd")))

;; open new directories in current buffer
(define-key dired-mode-map "a" 'dired-find-alternate-file)

(define-key dired-mode-map "b" (lambda ()
  (interactive)
  (setq buf (current-buffer))
  (dired-up-directory)
  (if (not (eq buf (current-buffer))) (kill-buffer buf))))

(define-key dired-mode-map "/" (lambda ()
  "Opens windows explorer in current dired buffer directory"
  (interactive)
  (setq path (slash-to-backslash (dired-current-directory)))
  (if (= ?\\ (elt path (- (length path) 1)))
    (setq path (substring path 0 (- (length path) 1))))
  (if (eq major-mode 'dired-mode)
    (start-process 
      "dired-process"
      "*explorer*"
      "explorer.exe"
      path))))

(define-key dired-mode-map (kbd "<prior>") (lambda ()
  "Definition for <PageUp> key in `dired' mode.
Helps to keep your cursor on file names during dired buffers listing."
  (interactive)
  (setq count my-dired-listing-range)
  (while (and (> (buffer-current-line) 2)
              (> count 0))
    (dired-previous-line 1)
    (setq count (1- count)))
  (while (and (> (buffer-current-line) 2)
              (string= (dired-get-filename) ""))
    (dired-previous-line 1))
  (if (= (buffer-current-line) 2)
    (dired-next-line 1))
  (if (string= (dired-get-filename) ".")
    (dired-next-line 1))))

(define-key dired-mode-map (kbd "<next>") (lambda ()
  "Definition for <PageDown> key in `dired' mode.
Helps to keep your cursor on file names during dired buffers listing."
  (interactive)
  (setq count my-dired-listing-range)
  (while (and (< (buffer-current-line) (buffer-lines-number))
              (> count 0))
    (dired-next-line 1)
    (setq count (1- count)))
  (while (and (< (buffer-current-line) (buffer-lines-number))
              (string= (dired-get-filename) ""))
    (dired-next-line 1))))

(define-key dired-mode-map [home] (lambda ()
  "Definition for <HOME> key in `dired' mode.
Move your cursor not to the top of the line, but to the 
first file name in current dired listing"
  (interactive)
  (if (not (string= (thing-at-point 'filename) ".."))
    (if (not (search-backward "..\n" nil t))
      (if (search-backward "  total used" nil t)
        (progn 
          (dired-next-line 1)
          (if (string= (thing-at-point 'filename) ".")
            (dired-next-line 1)))
        (if (not (search-forward ".." nil t))
          (dired-jump)
          (backward-char 2)))))))

(define-key dired-mode-map [end] (lambda () 
  "Definition for <END> key in `dired' mode.
Move your cursor not to the end of the line, but to the 
last file name in current dired listing"
  (interactive)
  (let ((l (count-lines-page))
        count)
    (string-match "+ \\(.*\\))" l)
    (setq count (string-to-number (match-string 1 l)))
    (if (= count 0)
      (dired-previous-line 1)
      (dired-next-line (1- count)))
    (if (< (buffer-current-line) (buffer-lines-number))
      (dired-previous-line 2)))))

(define-key dired-mode-map [M-return] (lambda () 
  "Shows window shell `File Properties' dialog for selected
file in dired"
  (interactive)
  (start-process 
    "dired-process" 
    "dired-process-output" 
   ;; @TODO fix this path names
    "FilePropertiesDialog.exe" 
    (slash-to-backslash (dired-get-filename)))))

(define-key dired-mode-map "=" (lambda () 
  "Compare two marked files in dired using external programm"
  (require 'dired-aux) 
  (require 'cl)
  (interactive)
  (if (>= (length (dired-get-marked-files)) 2)
    (setq files-to-compare (subseq (dired-get-marked-files) 0 2))
    (if (= (length (dired-get-marked-files)) 1)
      (setq files-to-compare (list (car (dired-get-marked-files))))
      (setq files-to-compare (list (dired-get-filename)))))
  (if (= (length files-to-compare) 1)
    (progn 
      (setq target 
        (read-file-name 
          (format "Diff %s with: " (car files-to-compare))
          (dired-dwim-target-directory)))
      (setq target (expand-file-name target))
      (setq files-to-compare (append files-to-compare (list target)))))
  (switch-to-buffer-other-frame (current-buffer))
  (if (file-directory-p (elt files-to-compare 0))
    (ediff-directories (elt files-to-compare 0) (elt files-to-compare 1) nil)
    (ediff (elt files-to-compare 0) (elt files-to-compare 1) nil))))

(defun select-drive-and-open (cur-drive-name) 
  "Shows hard-drives listing menu and lets user to select one of them. 
Opens selected drive in dired."
  (interactive)
  (setq new-drive-name "")
  (with-temp-buffer
    (call-process "HardDrivesMenu" nil t nil (format "[-%s-]" cur-drive-name))
    (setq new-drive-name 
          (buffer-substring (point-min) (point-max))))
  (if (not (string= new-drive-name ""))
    (find-file new-drive-name)))

(define-key dired-mode-map [M-f1] (lambda () 
  "Shows hard-drives listing menu and lets user to select one of them. 
Opens selected drive in dired."
  (interactive)
  (let ((cur-drive-name
          (char-to-string 
            (elt (dired-current-directory) 0))))
  (if (eq system-type 'windows-nt)
    (select-drive-and-open cur-drive-name)
    (find-file "/media")))))

(define-key dired-mode-map [M-f2] (lambda () 
  "Shows hard-drives listing menu and lets user to select one of them. 
Opens selected drive in dired in new window."
  (interactive)
  (setq cur-drive-name (char-to-string (elt (dired-current-directory) 0)))
  (if (one-window-p)
    (split-window-horizontally))
  (other-window 1)
  (setq prev-buf-name (buffer-name))
  (if (eq system-type 'windows-nt)
    (select-drive-and-open cur-drive-name)
    (find-file "/media"))
  (if (string= prev-buf-name (buffer-name))
    (other-window -1))))

(define-key dired-mode-map [(control p)] (lambda () 
  "Copy current directory name to clipboard."
  (interactive)
  (copy-to-clipboard (dired-current-directory))))

(define-key dired-mode-map [(control P)] (lambda () 
  "Copy full path to currently selected file to clipboard."
  (interactive)
  (copy-to-clipboard (dired-get-filename))))

(define-key dired-mode-map [(control home)] (lambda () 
  (interactive)
  (beginning-of-line)))

(define-key dired-mode-map [(control shift home)] (lambda () 
  (interactive)
  (set-mark (point))
  (beginning-of-line)))

(define-key dired-mode-map [(control end)] (lambda () 
  (interactive)
  (end-of-line)))

(define-key dired-mode-map [(control shift end)] (lambda () 
  (interactive)
  (set-mark (point))
  (end-of-line)))

(define-key dired-mode-map [M-f7] (lambda ()
  (interactive)
  (setq file-mask (read-string "File mask: "))
  (setq find-expression (read-string "Find expression: "))
  (if (string= find-expression "")
    (find-dired "." (format "-name \"%s\"" file-mask))
    (if (string= file-mask "")
      (grep-find (format "grep --exclude=\"*\.svn*\" -r -n \"%s\" -s *" find-expression ))
      (grep-find 
        (format 
          "find -type f -name \"%s\" -exec grep -r -H -n \"%s\" {} \";\""
          file-mask 
          find-expression))))))

(define-key dired-mode-map "\C-\\" (lambda () 
  "Jump to the root directory keystroke"
  (interactive)
  (setq prev-dir (dired-current-directory))
  (dired-jump)
  (while (not (equal prev-dir (dired-current-directory)))
    (kill-buffer (other-buffer))
    (setq prev-dir (dired-current-directory))
    (dired-jump))))

(define-key dired-mode-map [M-f5] (lambda ()
  "Compress files using zip keystroke"
  (interactive)
  (defun my-dired-do-compress ()
    (cd (dired-current-directory))
    ;; if wse use async call here we may fail to zip all files
    (dired-run-shell-command
      (format 
        "zip%s -r9 %s %s"
        (if (eq system-type 'windows-nt) ".exe" "")
        (shell-quote-argument archive-file-name)
        (shell-quote-argument (dired-get-filename 'no-dir)))))
  (require 'dired-aux)
  (setq buffer (current-buffer))
  (if (> (length (dired-get-marked-files)) 1)
    (setq archive-name
      (concat 
        (dired-dwim-target-directory)
        (remove ?:
          (nth 1 
            (reverse (split-string (dired-current-directory) "/"))))
        ".zip"))
    (setq archive-name 
      (concat 
        (dired-dwim-target-directory)
        (substring
          (car (dired-get-marked-files))
          (length (dired-current-directory)))
        ".zip")))
  (setq archive-file-name 
    (expand-file-name (read-file-name "Archive: " archive-name)))
  (let ((dired-no-confirm t)) 
    (dired-map-over-marks-check 
      (function my-dired-do-compress) nil 'compress t))
  (switch-to-buffer buffer)
  (dired-unmark-all-marks)
  (revert-buffer)))

(define-key dired-mode-map "C" (lambda () 
  "Async copy with progress bar in *rsync* buffer"
  (interactive)
  (defun my-dired-do-copy ()
    (setq source-list 
      (concat 
        source-list 
        " \"" 
        (if (eq system-type 'windows-nt) 
          (cygwin-path (dired-get-filename))
          (dired-get-filename))
        "\"")))
  (require 'dired-aux)
  (setq source-list "")
  (if (eq (length (dired-get-marked-files)) 1)
    (my-dired-do-copy)
    (let ((dired-no-confirm t)) 
      (dired-map-over-marks-check 
        (function my-dired-do-copy) nil 'copy t)))
  (setq dired-dst
    (expand-file-name 
      (read-file-name "Copy to: " (dired-dwim-target-directory))))
  (setq dst
    (if (eq system-type 'windows-nt) 
      (cygwin-path dired-dst)
      dired-dst))
  (set-process-sentinel
    (start-process-shell-command
      "rsync"
      "*rsync*"
      (format "rsync -rPp %s %s" source-list 
        (shell-quote-argument dst)))
    (lambda (p e) (when (not (equal (process-status p) 'run)) (progn
      (dired-unmark-all-marks)
      (dired-update-dwim-target-buffer dired-dst)))))))

