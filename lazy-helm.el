;;; lazy-helm.el --- Emacs helm integration for lazy

;; Copyright (C) 2011-2017 Andreas Raster <lazor at affenbande dot org>
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

(require 'lazy)

(require 'helm-mode)
(require 'helm-config)
(require 'helm-locate)
(require 'helm-buffers)
(require 'helm-files)
(require 'helm-ag)

(defvar helm-source-lazy-projects
  '((name . "Lazy projects")
    (candidates . (lambda ()
                    (sort (lazy-filter (lambda (title)
                                            (not (string-match (concat "^[^:]*:\\(.*\\)$") title)))
                                          (lazy-project-names))
                          'string-lessp)))
    (action ("Load project" . (lambda (entry)
                                (lazy-load (car (helm-marked-candidates)))))
            ;; evil hack, if I use helm inside helm, the topmost action gets called for every
            ;; candidate of the 'inside' helm, so this lambda checks if it is called with something
            ;; that looks like a todo, either loads it or calls another helm instance showing a
            ;; selection of todos
            ("Load todo" . (lambda (entry)
                             (if (string-match (concat "^[^:]*:\\(.*\\)$") entry)
                                 (lazy-load entry)
                               (let ((todos (when (functionp 'lazy-org-project-todos)
                                              (lazy-org-project-todos (car (helm-marked-candidates))))))
                                 (if todos
                                     (helm :sources `((name . "Todos")
                                                      (candidates . ,todos)))
                                   (message "No todos!"))))))))
  "All configured lazy projects.")

(defvar helm-source-lazy-todos
  '((name . "Lazy todos")
    (candidates . (lambda () (when (functionp 'lazy-org-project-todos)
                               (lazy-org-project-todos))))
    (action . (lambda (entry)
                (lazy-load (car (helm-marked-candidates))))))
  "Current projects todos")

(defvar helm-source-lazy-files nil
  "All files of the currently active project.")

(setq helm-source-lazy-files
      (helm-build-sync-source "Lazy files"
        :candidates (lambda () (lazy-files))
        :keymap helm-find-files-map
        :action helm-find-files-actions
        :mode-line (list "File(s)" helm-mode-line-string)))

(defvar helm-source-lazy-open-buffers nil
  "All buffers of the currently active project.")

(setq helm-source-lazy-open-buffers
      (helm-build-sync-source "Lazy buffers"
        :candidates (lambda ()
                      (mapcar 'buffer-name
                              (condition-case nil
                                  (cl-remove-if (lambda (buf) (string-match "\*[^\*]\*" (buffer-name buf))) (lazy-buffers))
                                (error nil))))
        :keymap helm-buffer-map
        :action helm-type-buffer-actions))

(defvar helm-source-lazy-friendly-files nil
  "All files of projects which are friends of this project.")

(setq helm-source-lazy-friendly-files
      (helm-build-sync-source "Lazy friendly files"
        :candidates (lambda ()
                      (lazy-friendly-files))
        :keymap helm-find-files-map
        :action helm-type-file-actions
        :mode-line (list "File(s)" helm-mode-line-string)))

(defun lazy-helm ()
  (interactive)
  (helm :sources '(helm-source-lazy-files
                   helm-source-lazy-friendly-files
                   helm-source-lazy-todos
                   helm-source-lazy-projects
                   helm-source-buffers-list
                   )
        :buffer "*helm lazy*"
        :history 'helm-file-name-history))

(defun lazy-helm-do-ag (&optional arg)
  (interactive "P")
  (require 'helm-mode)
  (setq helm-ag--original-window (selected-window))
  (setq helm-ag--last-default-directory nil)
  (let* ((helm-ag--default-target (cond ((and lazy-name (not arg))
                                         (list (condition-case nil (lazy-get-config-val 'basedir) (error default-directory))))
                                        ((not arg)
                                         (list default-directory))
                                        (arg
                                         (helm-read-file-name
                                          "Search in file(s): "
                                          :default default-directory
                                          :marked-candidates t :must-match t))))
         (helm-ag--default-directory (car-safe helm-ag--default-target))
         (helm-do-ag--extensions (when helm-ag--default-target
                                   (helm-ag--do-ag-searched-extensions))))
    (helm-ag--set-do-ag-option)
    (helm-ag--set-command-features)
    (helm-ag--save-current-context)
    (helm-attrset 'name (helm-ag--helm-header helm-ag--default-directory)
                  helm-source-do-ag)
    (helm :sources '(helm-source-do-ag) :buffer "*helm-ag*"
          :input (helm-ag--insert-thing-at-point helm-ag-insert-at-point)
          :keymap helm-do-ag-map)))

(provide 'lazy-helm)
