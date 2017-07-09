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

;; Sets different customizable variables. You can change the
;; values of some of them.

;; All customizable variables which are changed by mesa 
;; should be here.

;;; Code:

(setq default-major-mode 'text-mode)

(if (eq system-type 'windows-nt)
  (load "customize-win32.el")
  (load "customize-linux.el"))

;; enabling ls-lisp
(setq ls-lisp-use-insert-directory-program nil)

;; prevent from splitting the windows
(setq split-height-threshold 1200)
(setq split-width-threshold 2000)

(setq semantic-symref-tool 'global)

(require 'ls-lisp)

;; use ibuffer instead of default buffer mode
(defalias 'list-buffers 'ibuffer)

;; use ido for buffer switching
(ido-mode 1)
(setq ido-save-directory-list-file nil)
;; display any item that contains the chars you typed
(setq ido-enable-flex-matching t)

;; auto-save in the file itself rather than into auto-save directory
(setq auto-save-visited-file-name t)

(custom-set-variables
 '(blink-cursor-mode nil)
 ;; Copy/Paste like in Windows
 '(cua-mode t nil (cua-base))
 '(inhibit-startup-screen t)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-verbosity nil)
 '(dired-dwim-target t)
 '(dired-auto-revert-buffer t)
 '(make-backup-files nil)
 '(tooltip-mode nil)
 ;; turn off menu bar at the top
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(org-support-shift-select t)
 '(org-replace-disputed-keys t)
 '(Buffer-menu-mode-width 2)
 '(Buffer-menu-buffer+size-width 15))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(default 
    ((t 
      (:inherit nil
        :stipple nil
        :background "#A9A28F"
        :foreground "black"
        :inverse-video nil
        :box nil
        :strike-through nil
        :overline nil
        :underline nil
        :slant normal
        :weight normal
        :height 100
        :width normal
        :foundry "unknown"
        :family "Monospace"))))
  '(fringe 
    ((
     ((class color) 
      (background light)) 
     (:background "gray20")))))

;; visit file or directory in current dired buffer 
;; instead of creating a new one
;(put 'dired-find-alternate-file 'disabled nil)

(setq 
  ls-lisp-use-localized-time-format t
  ls-lisp-verbosity '()
  ls-lisp-format-time-list
    '("%d-%m-%Y %H:%M" "%d-%m-%Y      "))

;; how many lines to scroll during PgUp/PgDown in dired
(setq my-dired-listing-range 20)

;; set short answers for yes/no questions
(defalias 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode t)
(setq-default indent-tabs-mode nil)

(require 'cc-mode)

(make-variable-buffer-local 'compile-cmd)
(make-variable-buffer-local 'run-cmd)

(defun read-parameters ()
  "Asks for compilation command and a run command. If empty string is 
passed then the current command is preserved."
  (setq input (read-shell-command "Compile command: "))
  (if (> (length input) 0)
    (setq compile-cmd input))
  (setq input (read-shell-command "Run command: "))
  (if (> (length input) 0)
    (setq run-cmd input)))

(defun process (run-anyway)
  "Compiles current file (if it has been modified) and runs."
  (lexical-let ((cmd run-cmd))
    (setq compilation-finish-function (lambda (buffer ret)
      (if (string= ret "finished\n")
        (progn
          (shell-command cmd))))))
  (let ((is-changed (buffer-modified-p)))
    (if is-changed
      (compile compile-cmd)
      (if run-anyway
        (progn
          (message "Runing...")
          (shell-command run-cmd))))))

(define-key c++-mode-map [f5] (lambda ()
  (interactive)
  (read-parameters)))

(define-key c++-mode-map [f6] (lambda ()
  (interactive)
  (process nil)))

(define-key c++-mode-map [f7] (lambda ()
  (interactive)
  (process t)))

(define-key java-mode-map [f5] (lambda ()
  (interactive)
  (read-parameters)))

(define-key java-mode-map [f6] (lambda ()
  (interactive)
  (process nil)))

(define-key java-mode-map [f7] (lambda ()
  (interactive)
  (process t)))

(add-hook 'org-mode-hook 'org-indent-mode)

(require 'epa-file)
(epa-file-enable)
(setq epa-file-name-regexp "\\.\\(gpg\\)$")
(epa-file-name-regexp-update)

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; highlight parenthesis
(show-paren-mode t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
