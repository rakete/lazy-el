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

(defvar mk-proj-friends nil)

(defvar mk-proj-open-friends-cache nil)

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

;; ---------------------------------------------------------------------
;; Anything Sources
;; ---------------------------------------------------------------------

(defvar anything-c-source-mk-project-projects
  '((name . "mk-project Projects")
    (candidates . (lambda ()
                    (let ((ps '()))
                      (maphash (lambda (k p)
                                 (progn
                                   ;;(print (assoc 'org-header p))
                                   (let ((h (cadr (assoc 'org-header p))))
                                     (if h
                                         (setq ps (cons `(,h ,k) ps))
                                       (setq ps (cons k ps))))
                                   ))
                               mk-proj-list)
                      ps)))
    (action . (lambda (entry)
                (if (listp entry)
                    (mk-proj-load (car entry))
                  (mk-proj-load entry)))))
  "All configured mk-project projects.")

(defvar anything-c-source-mk-project-files
  '((name . "Files")
    (candidates . (lambda ()
                    (condition-case nil
                        (mapcar (lambda (s)
                                  (replace-regexp-in-string "/\\./" "/" (concat (file-name-as-directory mk-proj-basedir) s)))
                                (if mk-proj-patterns-are-regex
                                    (mk-proj-fib-matches mk-proj-src-patterns)
                                  (mk-proj-fib-matches nil))) (error nil))))
    (match anything-c-match-on-file-name
           anything-c-match-on-directory-name)
    (type . file))
  "All files of the currently active project.")

(defvar anything-c-source-mk-project-open-buffers
  '((name . "Mk-Project buffers")
    (candidates . (lambda () (mapcar 'buffer-name (condition-case nil
                                                      (remove-if (lambda (buf) (string-match "\*[^\*]\*" (buffer-name buf))) (mk-proj-buffers))
                                                    (error nil)))))
    (type . buffer)
    (candidate-transformer anything-c-skip-current-buffer
                           anything-c-highlight-buffers
                           anything-c-skip-boring-buffers)
    (persistent-action . anything-c-mk-project-buffer-persistent-action)
    (persistent-help . "Show this buffer / C-u \\[anything-execute-persistent-action]: Kill this buffer"))
  "All buffers of the currently active project." )

(defvar anything-c-source-mk-friendly-files
  '((name . "Friendly files")
    (candidates . (lambda () (condition-case nil (mk-proj-fib-friend-matches) (error nil))))
    (match anything-c-match-on-file-name
           anything-c-match-on-directory-name)
    (type . file))
  "All files of projects which are friends of this project.")

(defvar anything-c-source-mk-project-open-friendly-buffers
  '((name . "Mk-Project friendly buffers")
    (candidates . (lambda () (mapcar 'buffer-name (condition-case nil
                                                      (mk-proj-friendly-buffers t)
                                                    (error nil)))))
    (type . buffer)
    (candidate-transformer anything-c-skip-current-buffer
                           anything-c-highlight-buffers
                           anything-c-skip-boring-buffers)
    (persistent-action . anything-c-mk-project-buffer-persistent-action)
    (persistent-help . "Show this buffer / C-u \\[anything-execute-persistent-action]: Kill this buffer"))
  "All friendly buffers of the currently active project." )

(defvar anything-c-source-mk-project-open-special-buffers
  '((name . "Mk-Project special buffers")
    (candidates . (lambda () (mapcar 'buffer-name (condition-case nil
                                                      (mk-proj-special-buffers)
                                                    (error nil)))))
    (type . buffer)
    (candidate-transformer anything-c-skip-current-buffer
                           anything-c-highlight-buffers)
    (persistent-action . anything-c-mk-project-buffer-persistent-action)
    (persistent-help . "Show this buffer / C-u \\[anything-execute-persistent-action]: Kill this buffer"))
  "All special buffers of the currently active project." )

(defvar mk-proj-anything-sources
  '(anything-c-source-mk-project-open-buffers
    anything-c-source-mk-project-open-friendly-buffers
    anything-c-source-mk-project-open-special-buffers
    anything-c-source-mk-project-files
    anything-c-source-mk-friendly-files
    anything-c-source-mk-project-projects)
  "The `anything-sources' that `project-find-anything' uses.

See also: `anything-c-source-mk-project-open-buffers',
          `anything-c-source-mk-project-files'")

(provide 'mk-project-anything)
