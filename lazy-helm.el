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

(require 'cl-lib)
(require 'lazy)

(require 'helm-mode)
;;(require 'helm-config)
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


(defun lazy-helm-build-files-source (&optional cached)
  (helm-build-sync-source "Lazy files"
    :candidates (if cached (lazy-sort-files-recentf (lazy-files)) #'lazy-files)
    :keymap helm-find-files-map
    :action helm-find-files-actions
    :mode-line (list "File(s)" helm-mode-line-string)))

(defvar helm-source-lazy-files nil
  "All files of the currently active project.")

(setq helm-source-lazy-files (lazy-helm-build-files-source))

(defun lazy-helm-build-friendly-files-source (&optional cached)
  (helm-build-sync-source "Lazy friendly files"
    :candidates (if cached (lazy-sort-files-recentf (lazy-friendly-files)) #'lazy-friendly-files)
    :keymap helm-find-files-map
    :action helm-type-file-actions
    :mode-line (list "File(s)" helm-mode-line-string)))

(defvar helm-source-lazy-friendly-files nil
  "All files of projects which are friends of this project.")

(setq helm-source-lazy-friendly-files (lazy-helm-build-friendly-files-source))

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

(defface lazy-helm-symbol-definition-face
  '((((background dark)) :foreground "LightSkyBlue")
    (((background light)) :foreground "NavyBlue"))
  "Face used for symbol definitions in lazy-helm.")

(defface lazy-helm-symbol-line-numbers-face
  '((((background dark)) :foreground "MediumSpringGreen")
    (((background light)) :foreground "DarkGreen"))
  "Face used for line numbers in lazy-helm.")

(defun lazy-helm-jump-definition (&optional proj-name invoke-window word)
  (interactive)
  (unless proj-name
    (let ((guessed-name (cadr (assoc 'name (lazy-guess-alist)))))
      (setq proj-name (or guessed-name lazy-name))))
  (unless proj-name
    (lazy-assert-proj))
  (unless invoke-window
    (setq invoke-window (get-buffer-window (current-buffer))))
  (unless word
    (setq word (substring-no-properties (or (thing-at-point lazy-thing-selector) ""))))
  (unless (gethash proj-name lazy-project-symbols)
    (lazy-update-symbols proj-name))
  (let ((project-symbols )
        (sources '())
        (one-symbol-match nil))
    (when (hash-table-p project-symbols)
      (if (> (length sources) 0)
          (helm :sources sources
                :truncate-lines t
                :input (when one-symbol-match word))
        (message "No symbols found!")))))

(require 'helm-ag)

(defvar lazy-helm-do-ag-per-project-settings (make-hash-table :test 'equal))

(defvar lazy-helm-source-do-ag
  (helm-build-async-source "The Silver Searcher"
    :init 'helm-ag--init
    :real-to-display 'helm-ag--candidate-transformer
    :persistent-action 'helm-ag--persistent-action
    :fuzzy-match helm-ag-fuzzy-match
    :action helm-ag--actions
    :candidate-number-limit 9999
    :keymap helm-ag-map
    :follow (and helm-follow-mode-persistent 1)))

(defun lazy-helm-do-ag (&optional basedir targets default-input)
  (interactive)
  (require 'helm-mode)
  (helm-ag--init-state)
  (let* ((cached-settings (when lazy-name (gethash lazy-name lazy-helm-do-ag-per-project-settings)))
         (helm-ag--default-directory (or basedir default-directory))
         (helm-ag--default-target (cond ((and lazy-name (buffer-file-name (current-buffer)) (not (or (lazy-buffer-p (current-buffer)) (lazy-friendly-buffer-p (current-buffer)))))
                                         (list default-directory))
                                        ((and lazy-name cached-settings)
                                         (car-safe cached-settings))
                                        ((and lazy-name)
                                         (list (condition-case nil (lazy-get-config-val 'basedir) (error default-directory))))
                                        (targets targets)
                                        ((and (helm-ag--windows-p) basedir)
                                         (list basedir))
                                        (t
                                         (progn
                                           (when cached-settings
                                             (remhash lazy-name lazy-helm-do-ag-per-project-settings))
                                           (helm-read-file-name
                                            "Search in file(s): "
                                            :default default-directory
                                            :marked-candidates t :must-match t)))))
         (helm-do-ag--extensions (when helm-ag--default-target
                                   (helm-ag--do-ag-searched-extensions)))
         (one-directory-p (helm-do-ag--target-one-directory-p
                           helm-ag--default-target))
         (search-this-file (and (= (length helm-ag--default-target) 1)
                                (not (file-directory-p (car helm-ag--default-target)))
                                (car helm-ag--default-target))))
    (helm-ag--set-do-ag-option)
    (helm-ag--set-command-features)
    (helm-ag--save-current-context)
    (if (or (helm-ag--windows-p) (not one-directory-p)) ;; Path argument must be specified on Windows
        (helm-do-ag--helm default-input search-this-file)
      (print helm-ag--default-target)
      (let* ((helm-ag--default-directory
              (file-name-as-directory (car helm-ag--default-target)))
             (helm-ag--default-target nil))
        (print helm-ag--default-directory)
        (helm-do-ag--helm default-input search-this-file)))))

;; (defun lazy-helm-do-ag (&optional arg)
;;   (interactive "P")
;;   (require 'helm-mode)
;;   (setq helm-ag--original-window (selected-window))
;;   (setq helm-ag--last-default-directory nil)
;;   (let* ((cached-settings (when lazy-name (gethash lazy-name lazy-helm-do-ag-per-project-settings)))
;;          (helm-ag--default-target (cond ((and lazy-name (not arg) (buffer-file-name (current-buffer)) (not (or (lazy-buffer-p (current-buffer)) (lazy-friendly-buffer-p (current-buffer)))))
;;                                          (list default-directory))
;;                                         ((and lazy-name (not arg) cached-settings)
;;                                          (car-safe cached-settings))
;;                                         ((and lazy-name (not arg))
;;                                          (list (condition-case nil (lazy-get-config-val 'basedir) (error default-directory))))
;;                                         ((not arg)
;;                                          (list default-directory))
;;                                         (arg
;;                                          (progn
;;                                            (when cached-settings
;;                                              (remhash lazy-name lazy-helm-do-ag-per-project-settings))
;;                                            (helm-read-file-name
;;                                             "Search in file(s): "
;;                                             :default default-directory
;;                                             :marked-candidates t :must-match t)))))
;;          (helm-do-ag--extensions (when helm-ag--default-target
;;                                    (if (and lazy-name (not arg) cached-settings (or (lazy-buffer-p (current-buffer)) (lazy-friendly-buffer-p (current-buffer))))
;;                                        (cdr cached-settings)
;;                                      (helm-ag--do-ag-searched-extensions))))
;;          (helm-ag--default-directory nil)
;;          (helm-ag-ignore-patterns (lazy-get-config-val 'ignore-ag))
;;          (helm-ag-command-option "--all-types"))
;;     (when lazy-name
;;       (if (and arg helm-ag--default-target (or (lazy-buffer-p (current-buffer)) (lazy-friendly-buffer-p (current-buffer))))
;;           (puthash lazy-name (cons helm-ag--default-target helm-do-ag--extensions) lazy-helm-do-ag-per-project-settings)
;;         (setq helm-ag--default-target (cond ((and (not arg) (lazy-friendly-buffer-p (current-buffer)))
;;                                              (list (directory-file-name (lazy-get-config-val 'basedir nil t (lazy-guess-alist nil nil)))))
;;                                             ((and (not arg) (lazy-get-config-val 'basedir lazy-name t))
;;                                              (list (directory-file-name (lazy-get-config-val 'basedir lazy-name t))))
;;                                             (arg
;;                                              helm-ag--default-target)))))
;;     (setq helm-ag--default-directory (car-safe helm-ag--default-target))
;;     (helm-ag--init-state)
;;     (helm-ag--set-do-ag-option)
;;     (helm-ag--set-command-features)
;;     (helm-ag--save-current-context)
;;     (helm-set-attr 'name (helm-ag--helm-header helm-ag--default-directory)
;;                    lazy-helm-source-do-ag)
;;     (helm :sources '(lazy-helm-source-do-ag)
;;           :input (helm-ag--insert-thing-at-point helm-ag-insert-at-point)
;;           :keymap helm-do-ag-map
;;           :buffer (concat "*helm ag*"
;;                           (concat " " helm-ag--default-directory
;;                                   (when helm-do-ag--extensions
;;                                     (concat "/" (prin1-to-string helm-do-ag--extensions))))))))

(provide 'lazy-helm)
