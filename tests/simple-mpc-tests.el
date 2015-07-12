;;; simple-mpc-tests.el --- tests for simple-mpc
;;
;; Copyright (C) 2015

;; Author: Joren Van Onder <joren.vanonder@gmail.com>
;; Maintainer: Joren Van Onder <joren.vanonder@gmail.com>
;; Keywords: multimedia, mpd, mpc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'cl-extra)
(require 'simple-mpc-vars)
(require 'simple-mpc)

(ert-deftest simple-mpc-test-quit ()
  "Tests that `simple-mpc-quit' cleans up as it should."
  (simple-mpc-quit)
  (should-not (cl-some (lambda (buffer-name) (get-buffer buffer-name))
                       (list simple-mpc-main-buffer-name
                             simple-mpc-current-playlist-buffer-name
                             simple-mpc-query-buffer-name))))

(ert-deftest simple-mpc-test-main-screen ()
  "Tests that the main 'splash' screen is displayed after running
`simple-mpc'."
  (save-window-excursion
    (simple-mpc)
    (should (get-buffer simple-mpc-main-buffer-name))))

(provide 'simple-mpc-tests)
;;; simple-mpc-tests.el ends here
