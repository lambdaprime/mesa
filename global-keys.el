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

;; Global keystrokes

;;; Code:

(global-set-key "\C-l" 'goto-line)
(global-set-key [(control shift w)] 'toggle-truncate-lines)
(global-set-key [(control shift a)] 'mark-whole-buffer)
(global-set-key [(control d)] 'kill-whole-line)
(global-set-key [(delete)] 'delete-char)

(add-hook 'java-mode-hook (lambda ()
  (define-key java-mode-map "\C-d"
    'kill-whole-line)))

(fset 'h1
   [home ?< ?h ?1 ?> end ?< ?/ ?h ?1 ?>])
(fset 'h2
   [home ?< ?h ?2 ?> end ?< ?/ ?h ?2 ?>])
(fset 'h3
   [home ?< ?h ?3 ?> end ?< ?/ ?h ?3 ?>])
(fset 'h4
   [home ?< ?h ?4 ?> end ?< ?/ ?h ?4 ?>])
(fset 'p
   [home ?< ?p ?> end ?< ?/ ?p ?>])

(global-set-key [f11] (lambda ()
  "Opens current buffer in single window mode"
  (interactive)
  (if (one-window-p)
    (progn 
      (split-window-horizontally)
      (other-window 1)
      (next-buffer)
      (other-window -1))
    (delete-other-windows))))

(global-set-key [escape] (lambda () 
  "Close currently opened buffer"
  (interactive)
  (if (string-starts-with (buffer-name) " *Minibuf")
    (keyboard-escape-quit)
    (kill-buffer (current-buffer)))))

(global-set-key [f9] (lambda ()
  "Moves latest opened dired buffers to the top"
  (interactive)
  (require 'cl)
  (setq first-found nil)
  (block loop (dolist (buffer (buffer-list))
    (if (eq (buffer-mode buffer) 'dired-mode)
      (if (not first-found)
        (progn
          (delete-other-windows)
          (split-window-horizontally)
          (setq first-found t)
          (switch-to-buffer buffer)
          (other-window 1))
        (progn
          (switch-to-buffer buffer)
          (other-window -1)
          (return-from loop))))))))

;; buffer navigation keys
; page up
(global-set-key [M-prior] 'previous-buffer)
; page down
(global-set-key [M-next] 'next-buffer)
(global-set-key "\C-xb" 'ido-switch-buffer)
(global-set-key "\C-xf" 'favourite)
(global-set-key "\C-b" (lambda ()
  (interactive)
  (ibuffer)
  (ibuffer-jump-top-line)))
(global-set-key "\C-\M-b" (lambda ()
  (interactive)
  (when (= (length (window-list)) 1)
    (split-window-horizontally))
  (other-window 1)
  (ibuffer)))

;; window navigation keys
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key [C-tab] 'other-frame)
(global-set-key [(control shift tab)] (lambda ()
  (interactive)
  (other-frame -1)))

;; org keys

(require 'org)
(define-key org-mode-map [M-down] 'windmove-down)
(define-key org-mode-map [M-up] 'windmove-up)
(define-key org-mode-map [M-right] 'windmove-right)
(define-key org-mode-map [M-left] 'windmove-left)

(defun switch-to-window(id)
  (ignore-errors (while (windmove-left)))
  (ignore-errors (while (windmove-up)))
  (when (< id (length (window-list)))
    (let ((dst-win (nth id (window-list))))
      (while (not (eql (selected-window) dst-win))
        (other-window 1) ))))

(global-set-key (kbd "<kp-1>") (lambda ()
  (interactive)
  (switch-to-window 0)))

(global-set-key (kbd "<kp-2>") (lambda ()
  (interactive)
  (switch-to-window 1)))

(global-set-key (kbd "<kp-3>") (lambda ()
  (interactive)
  (switch-to-window 2)))

(global-set-key (kbd "<kp-4>") (lambda ()
  (interactive)
  (switch-to-window 3)))

(global-set-key (kbd "<kp-5>") (lambda ()
  (interactive)
  (switch-to-window 4)))

(global-set-key (kbd "<kp-6>") (lambda ()
  (interactive)
  (switch-to-window 5)))

