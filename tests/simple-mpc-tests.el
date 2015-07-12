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
(require 'simple-mpc-query)
(require 'simple-mpc)

(defun simple-mpc-test-dead ()
  "Tests that all simple-mpc buffers are cleaned up."
  (should-not (cl-some (lambda (buffer-name) (get-buffer buffer-name))
                       (list simple-mpc-main-buffer-name
                             simple-mpc-current-playlist-buffer-name
                             simple-mpc-query-buffer-name))))

(ert-deftest simple-mpc-test-quit ()
  "Tests that `simple-mpc-quit' cleans up as it should."
  (simple-mpc-quit)
  (simple-mpc-test-dead))

(ert-deftest simple-mpc-test-main-screen ()
  "Tests that the main 'splash' screen is displayed after running
`simple-mpc'."
  (save-window-excursion
    (simple-mpc)
    (should (get-buffer simple-mpc-main-buffer-name))))

(ert-deftest simple-mpc-test-direct-query-quit ()
  "Tests that after calling `simple-mpc-query' directly quitting
does not go to main screen."
  (save-window-excursion
    (simple-mpc-query "artist" "")
    (simple-mpc-query-quit)
    (simple-mpc-test-dead)))

(ert-deftest simple-mpc-test-direct-playlist-quit ()
  "Tests that after calling `simple-mpc-view-current-playlist'
directly quitting does not go to main screen."
  (save-window-excursion
    (simple-mpc-view-current-playlist)
    (simple-mpc-current-playlist-quit)
    (simple-mpc-test-dead)))

(provide 'simple-mpc-tests)
;;; simple-mpc-tests.el ends here
