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

(defvar simple-mpc-test-mpd-absolute-path "~/simple_mpc_test_mpd/"
  "The test directory for our test mpd server.")

(defun simple-mpc-test-find-project-root (project-name)
  "Will return the absolute path to the PROJECT-NAME directory in
the pwd."
  (let ((match-index (string-match project-name default-directory)))
    (substring default-directory 0 (+ match-index (length project-name)))))

(defun simple-mpc-test-mpd-setup ()
  "Setup a test mpd server."
  (setq simple-mpc-test-mpd-absolute-path (expand-file-name simple-mpc-test-mpd-absolute-path))
  (let* ((mpd-process (apply-partially 'start-process
                                       "simple-mpc-test-mpd" "simple-mpc-test-mpd-buffer"
                                       "mpd" "--no-daemon" "--verbose"))
         (project-root-absolute-path (simple-mpc-test-find-project-root "simple-mpc"))
         (project-env-absolute-path (concat project-root-absolute-path "/tests/env/")))
    (make-directory simple-mpc-test-mpd-absolute-path)
    (make-directory (concat simple-mpc-test-mpd-absolute-path "music/"))
    (make-directory (concat simple-mpc-test-mpd-absolute-path "playlists/"))
    (mapc (lambda (file)
            (copy-file (concat project-env-absolute-path file)
                       (concat simple-mpc-test-mpd-absolute-path "music/" file) t))
          (directory-files project-env-absolute-path nil ".*\.ogg" t))
    (setq simple-mpc-arguments (concat "--host " simple-mpc-test-mpd-absolute-path "mpd_socket"))
    (funcall mpd-process (concat project-root-absolute-path "/tests/env/mpd.conf"))
    (simple-mpc-call-mpc nil '("--wait" "update"))
    (sleep-for .5))) ;; todo why do we need this now?

(defun simple-mpc-test-mpd-teardown ()
  "Kills the test mpd server."
  (kill-process "simple-mpc-test-mpd")
  (sleep-for .5) ;; todo is kill-process not synchronous?
  (kill-buffer "simple-mpc-test-mpd-buffer")
  (setq simple-mpc-arguments "")
  (delete-directory simple-mpc-test-mpd-absolute-path t))

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

(ert-deftest simple-mpc-test-query-show-all ()
  "Tests that `simple-mpc-query' displays all songs in db."
  (simple-mpc-test-mpd-setup)
  (simple-mpc-query "artist" "")
  (switch-to-buffer simple-mpc-query-buffer-name) ;; todo why do we have to switch manually now?
  (should (eq 3 (count-lines (point-min) (point-max))))
  (simple-mpc-query-quit)
  (simple-mpc-test-mpd-teardown))

(provide 'simple-mpc-tests)
;;; simple-mpc-tests.el ends here
