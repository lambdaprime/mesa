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

;; Start-up actions.

;;; Code:

(dired "~")

(if (eq system-type 'windows-nt)
  (load "start-up-win32.el"))

(defun auto-save-cleanup () 
  (delete-directory "~/.emacs.d/auto-save-list" t nil)
  (make-directory "~/.emacs.d/auto-save-list"))

(add-hook 'kill-emacs-hook 'auto-save-cleanup)
(run-at-time nil 600 'auto-save-cleanup)
