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

;; Allows to extract files/dirs in archive-mode to particular location
;; by pressing C on their file names

;;; Code:

(require 'arc-mode)

(define-key archive-mode-map "C" 'extract-to-filesystem)

(defun extract-to-filesystem ()
  (interactive)
  (if
    (or
      (string-suffix-p "zip" (buffer-file-name) t)
      (string-suffix-p "jar" (buffer-file-name) t))
    (unzip-to-filesystem)))

(defun unzip-to-filesystem ()
  (let ((entry (thing-at-point 'filename)))
    (if (not (null entry)) 
      (unzip-to-filesystem-1 entry))))

(defun unzip-to-filesystem-1 (entry)
  (require 'dired-aux) 
  (require 'cl)
  (defun read-dst()
    (expand-file-name 
      (file-name-as-directory
        (read-directory-name "Extract to: " (dired-dwim-target-directory)))))
  (defun top-entry(entry)
    (substring entry 0 (search "/" entry)))
  (defun top-entry-p(entry)
    (and (< (count ?/ entry) 2) (string-suffix-p "/" entry)))
  (defun can-rename(rename-to)
    (if (file-exists-p rename-to)
      (if (= ?y (read-key (format "Overwrite %s ? (y): " rename-to)))
        (progn
          (if (file-directory-p rename-to) 
            (delete-directory rename-to t)
            (delete-file rename-to))
          t))
      t))
  (let 
    ( (dst (read-dst)) 
      tmp-dst 
      (is-dir nil) )
    (setq tmp-dst 
      (if (top-entry-p entry)
        dst
        (make-tmp-dir dst)))
    (when (string-suffix-p "/" entry)
      (setq is-dir t))
    (dired-run-shell-command (format
      "unzip%s -o %s %s -d %s"
      (if (eq system-type 'windows-nt) ".exe" "")
      (shell-quote-argument (buffer-file-name))
      (shell-quote-argument (if is-dir (concat entry "*") entry))
      (shell-quote-argument tmp-dst)))
    (when (not (top-entry-p entry))
      (if is-dir
        (setq entry (substring entry 0 (- (length entry) 1))))
      (let ((rename-to (concat dst (file-name-nondirectory entry))))
        (if (can-rename rename-to)
          (rename-file (concat tmp-dst entry) rename-to))
        (delete-directory tmp-dst t)
        (dired-update-dwim-target-buffer dst))
    )))
