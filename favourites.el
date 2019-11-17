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

(setq home (if (eq system-type 'windows-nt)
  (concat "C:/Users/" (getenv "USERNAME"))
  "~"))

(setq favourites '())
(add-to-list 'favourites (cons "download" (concat home "/Downloads/")))
(add-to-list 'favourites (cons "workspace" (concat home "/workspace/")))
(add-to-list 'favourites (cons "tmp" (concat home "/tmp/")))
(add-to-list 'favourites (cons "src" (concat home "/src/")))
(add-to-list 'favourites (cons "storage" (concat home "/storage/")))
(add-to-list 'favourites (cons "materials" (concat home "/materials/")))
