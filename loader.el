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

;; This file responsible for loading all mesa files to the 
;; current Emacs session. 
;;

(let ((mesa-dir (substring (pwd) 10)))
  (add-to-list 'load-path mesa-dir))

(load "utils.el")
(load "customize.el")
(load "start-up.el")
(load "global-keys.el")
(load "dired-keys.el")
(load "global-funcs.el")
(load "bmenu-keys.el")
(load "advices.el")
(load "arc-keys.el")
(load "favourites.el")

