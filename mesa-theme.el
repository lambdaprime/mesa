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

;; mesa theme: A dark-themed Emacs configuration with Org-mode and CUA keybindings.

(deftheme mesa
  "Mesa theme")

(custom-theme-set-variables
 'mesa
 ;; Copy/Paste like in Windows
 '(cua-mode t nil (cua-base))
 '(inhibit-startup-screen t)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-verbosity nil)
 '(dired-dwim-target t)
 '(dired-auto-revert-buffer t)
 '(make-backup-files nil)
 '(org-support-shift-select t)
 '(org-replace-disputed-keys t)
 '(org-startup-folded t)
 ;; apply org-block face to org-quote and org-verse
 '(org-fontify-quote-and-verse-blocks t)
 ;; do not put silent newline into files on save
 '(mode-require-final-newline nil)
 '(Buffer-menu-mode-width 2)
 '(Buffer-menu-buffer+size-width 15)
 '(blink-cursor-mode nil)
 '(tooltip-mode nil)
 ;; turn off menu bar at the top
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(ispell-dictionary nil))

(custom-theme-set-faces
 'mesa
 '(default ((t (:inherit nil :stipple nil :background "#A9A28F" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "Monospace"))))
 '(fringe ((((class color) (background light)) (:background "gray20"))))
 ;; Color for blocks in org-mode by default very dark, changing it to be more lighter
 '(org-code ((t (:foreground "dark magenta" :extend t))))
 '(org-block-begin-line ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF" :extend t))))
 '(org-block-end-line ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF" :extend t))))
 '(org-block ((t (:background "#b9b6ac" :extend t)))))

(provide-theme 'mesa)
