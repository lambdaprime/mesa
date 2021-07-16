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

;; Sets Windows customizable variables. You can change the
;; values of some of them.

;; All Windows specific customizable variables which are changed by mesa 
;; should be here.

;;; Code:

(setq pretty-printer-path (concat (getenv "UserProfile") "/opt/pretty-printer.jar"))

; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

;; On Windows org-mode behaves differently from Linux and
;; it expands everything when file is opened.
;; Adding a hook which will collapse everything back.
(add-hook 'org-mode-hook 'org-overview)
