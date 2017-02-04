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

;; Advice functions goes here

;;; Code:

(defadvice ls-lisp-format (around my-ls-lisp-format 
  (file-name file-attr file-size switches time-index &rest now))
  "Advice definition which removes unnecessary information
during file listing in dired. For such purposes 
`ls-lisp-verbosity' customized variable can be used, but 
even if it is equal to nil dired will display file 
permissions field like \"drwxrwxrwx\".\. So here we just 
get full control to what dired shows and leave only those 
fields which we need."
  (progn
    ad-do-it
    (setq ad-return-value (concat 
      (substring ad-return-value 0 1)
      (substring ad-return-value 10)))))
(ad-activate 'ls-lisp-format t)

(defadvice ediff-buffers (around my-ediff-buffers (arg1 arg2))
  (switch-to-buffer-other-frame (current-buffer))
  ad-do-it)
(ad-activate 'ediff-buffers t)

(defadvice kill-word (around my-kill-word (arg))
  "Do not overwrite clipboard content on word deletion"
  (let (old-val)
    (setq old-val (clipboard-content))
    ad-do-it
    (copy-to-clipboard old-val)))
(ad-activate 'kill-word t)

(defadvice kill-whole-line (around my-kill-whole-line (arg))
  "Do not overwrite clipboard content on line deletion"
  (let (old-val)
    (setq old-val (clipboard-content))
    ad-do-it
    (copy-to-clipboard old-val)))
(ad-activate 'kill-whole-line t)

(defadvice kill-line (around my-kill-line (arg))
  "Do not overwrite clipboard content on line deletion"
  (let (old-val)
    (setq old-val (clipboard-content))
    ad-do-it
    (copy-to-clipboard old-val)))
(ad-activate 'kill-line t)

(defadvice backward-kill-word (around my-backward-kill-word (arg))
  "Do not overwrite clipboard content on word deletion."
  (let (old-val)
    (setq old-val (clipboard-content))
    ad-do-it
    (copy-to-clipboard old-val)))
(ad-activate 'backward-kill-word t)

(defadvice find-file-other-window 
  (around my-find-file-other-window (filename &optional wildcards))
  (find-alternate-file filename))

(defadvice dired-other-window 
  (around my-dired-other-window (dirname &optional switches))
  (find-alternate-file dirname))

(defadvice dired-mouse-find-file-other-window 
  (around my-dired-mouse-find-file-other-window (event))
  "When click on file in dired do not open it in other buffer, 
and use current instead."
  (ad-activate 'find-file-other-window t)
  (ad-activate 'dired-other-window t)
  ad-do-it
  (ad-deactivate 'dired-other-window)
  (ad-deactivate 'find-file-other-window))
(ad-activate 'dired-mouse-find-file-other-window t)

(defadvice dired-do-async-shell-command
  (around my-dired-do-async-shell-command 
    (command &optional arg file-list))
  "When you invoking some command through
`dired-do-async-shell-command', it always pop-up the
*Async Shell Command* buffer. This advice helps to
preserve your windows arrangement after calling it."
  (if (one-window-p)
    (progn 
      ad-do-it
      (delete-other-windows))
    (progn
      (other-window 1)
      (let ((cur-buf (buffer-name (current-buffer))))
        (other-window -1)
        ad-do-it
        (other-window 1)
        (switch-to-buffer cur-buf)
        (other-window -1)))))
(ad-activate 'dired-do-async-shell-command t)

