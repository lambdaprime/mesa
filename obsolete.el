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

;; Old code which is not used anymore and kept as for history.

;;; Code:


(defun move-directory-recursively (dir-src dir-dst)
  "Moves directory DIR-SRC to the DIR-DST recursively. 
To move directory dir1 into the directory dir2 you should
call this function like as follows:
  (move-directory-recursively `dir1' `dir2/dir1')
To move content of the directory dir1 into the directory
dir2:
  (move-directory-recursively `dir1' `dir2')
If dir2 does not exist it will be created."
  (let ((queue (list (cons dir-src dir-dst)))
        dir-dst dir-src remove-later)
    (while queue
      (let ((dir-src-dst (car queue)))
        (setq dir-src (car dir-src-dst))
        (setq dir-dst (cdr dir-src-dst)))
      (setq queue (cdr queue))
      ;; if dir-dst is a file signal an error
      (and 
        (file-exists-p dir-dst)
        (not (file-directory-p dir-dst))
        (signal 'file-error 
          (format "Error: file %s exist" dir-dst)))
      ;; if dir-dst does not exist - create it
      (if (not (file-exists-p dir-dst))
        (make-directory dir-dst))
      (dolist (file (directory-files dir-src))
        (and
          (not (string= file "."))
          (not (string= file ".."))
          (let ((path (concat dir-src "/" file)))
            (if (file-directory-p path)
              ;; it is a directory
              (progn
                ;; place it to the queue
                (setq queue 
                  (cons 
                    (cons path (concat dir-dst "/" file))
                    queue))
                ;; and store it path to remove it later
                (push path remove-later))
              ;; not a dir
              (progn
                (message
                  (format "Moving %s to %s" path dir-dst))
                (rename-file path dir-dst)))))))
  ;; after we moved all content we can remove the
  ;; empty directories in dir-src
  (dolist (dir remove-later)
    (condition-case err
      (dired-delete-file dir 'always)
      (error ;; catch errors from failed deletions
        (dired-log "%s\n" err)))))
  (dired-delete-file dir-src 'always))

(defadvice dired-rename-file
  (around my-dired-rename-file
    (file newname ok-if-already-exists))
  "This advice definition helps to deal with
`Renaming: permission denied' error message when moving
directories between different logical disks in dired.
This is a Windows specific problem."
  (condition-case err
    ad-do-it
    (file-error 
      (and 
        (string-starts-with 
          (error-message-string err)
          "Renaming: permission denied")
        (file-directory-p file)
        (move-directory-recursively file newname)))))
(ad-activate 'dired-rename-file t)
