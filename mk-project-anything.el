;;; mk-project-anything.el --- Anything integration for mk-project

;; Copyright (C) 2008  Matt Keller <mattkeller at gmail dot com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; More information about this library, including the most recent
;; version and a comprehensive README, is available at
;; http://github.com/mattkeller/mk-project

;;; Code:

(require 'mk-project)
(require 'anything)
(require 'anything-config)

;; ---------------------------------------------------------------------
;; Anything Sources
;; ---------------------------------------------------------------------

(defvar anything-c-source-mk-project-projects
  '((name . "mk-project Projects")
    (candidates . (lambda ()
                    (let ((allkeys '()))
                      (maphash (lambda (k v) (setq allkeys (cons k allkeys))) mk-proj-list)
                      allkeys)))
    (action . (lambda (entry)
                (mk-proj-load entry))))
  "All configured mk-project projects.")

(defvar anything-c-source-mk-project-files
  '((name . "Files")
    (candidates . (lambda ()
                     (mapcar (lambda (s)
                               (concat (file-name-as-directory mk-proj-basedir)
                                       (file-name-nondirectory s)))
                             (if mk-proj-patterns-are-regex
                                 (flatten (mapcar 'mk-proj-fib-matches mk-proj-src-patterns))
                               (mk-proj-fib-matches nil)))))
    (match anything-c-match-on-file-name
           anything-c-match-on-directory-name)
    (type . file))
  "All files of the currently active project.")

(defvar anything-c-source-mk-project-open-buffers
  '((name . "Buffers")
    (candidates . (lambda () (mapcar 'buffer-name (mk-proj-buffers))))
    (type . buffer)
    (candidate-transformer anything-c-skip-current-buffer
                           anything-c-highlight-buffers
                           anything-c-skip-boring-buffers)
    (persistent-action . anything-c-mk-project-buffer-persistent-action)
    (persistent-help . "Show this buffer / C-u \\[anything-execute-persistent-action]: Kill this buffer"))
  "All buffers of the currently active project." )

(defun anything-c-mk-project-buffer-persistent-action (name)
  (flet ((kill (item)
               (with-current-buffer item
                 (if (and (buffer-modified-p)
                          (buffer-file-name (current-buffer)))
                     (progn
                       (save-buffer)
                       (kill-buffer item))
                   (kill-buffer item))))
         (goto (item)
               (switch-to-buffer item)))
    (if current-prefix-arg
        (progn
          (kill name)
          (anything-delete-current-selection))
      (goto name))))

(defvar mk-proj-anything-sources
  '(anything-c-source-mk-project-open-buffers
    anything-c-source-mk-project-files)
  "The `anything-sources' that `project-find-anything' uses.

See also: `anything-c-source-mk-project-open-buffers',
          `anything-c-source-mk-project-files'")

;; ---------------------------------------------------------------------
;; Anything Utilities
;; ---------------------------------------------------------------------

(defun* project-find-anything ()
  "Find files, buffers and interesting items with 'anything'.

The behaviour of this command is modified with
`mk-proj-anything-sources'."

  (interactive)
  (mk-proj-assert-proj)
  (unless (get-buffer mk-proj-fib-name)
    (message "Please use project-index to create the index before running project-find-anything")
    (return-from "project-find-anything" nil))
  (anything-other-buffer mk-proj-anything-sources
            "*anything mk-project*"))

(provide 'mk-project-anything)