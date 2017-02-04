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

;; Buffer menu keystrokes

;;; Code:

(define-key Buffer-menu-mode-map [home] (lambda () 
  (interactive) 
  (goto-line 1)))

(define-key Buffer-menu-mode-map [end] (lambda ()
  (interactive)
  (goto-char (point-max))(previous-line)))

(define-key Buffer-menu-mode-map [tab] (lambda ()
  (interactive)
  (other-window 0)))

;; keys for ibuffer menu
(require 'ibuffer)
(define-key ibuffer-mode-map [C-home] (lambda () 
  (interactive) 
  (ibuffer-jump-top-line)))

(define-key ibuffer-mode-map [C-end] (lambda ()
  (interactive)
  (goto-char (point-max))
  (previous-line)
  (previous-line)
  (previous-line)
  (beginning-of-line)))

(define-key ibuffer-mode-map [right] (lambda () 
  (interactive)
  (forward-char)))

(define-key ibuffer-mode-map [left] (lambda () 
  (interactive)
  (backward-char)))

