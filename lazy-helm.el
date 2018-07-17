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


(defun lazy-helm-build-files-source (&optional cached)
  (helm-build-sync-source "Lazy files"
    :candidates (if cached (lazy-files) #'lazy-files)
    :keymap helm-find-files-map
    :action helm-find-files-actions
    :mode-line (list "File(s)" helm-mode-line-string)))

(defvar helm-source-lazy-files nil
  "All files of the currently active project.")

(setq helm-source-lazy-files (lazy-helm-build-files-source))

(defun lazy-helm-build-friendly-files-source (&optional cached)
  (helm-build-sync-source "Lazy friendly files"
    :candidates (if cached (lazy-friendly-files) #'lazy-friendly-files)
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

;; (defun lazy-helm-symbols (&optional proj-name invoke-window word)
;;   (interactive)
;;   (unless proj-name
;;     (let ((guessed-name (cadr (assoc 'name (lazy-guess-alist)))))
;;       (setq proj-name (or guessed-name lazy-name))))
;;   (unless proj-name
;;     (lazy-assert-proj))
;;   (unless invoke-window
;;     (setq invoke-window (get-buffer-window (current-buffer))))
;;   (unless word
;;     (setq word (substring-no-properties (or (thing-at-point lazy-thing-selector) ""))))
;;   (unless (gethash proj-name lazy-project-symbols)
;;     (lazy-update-symbols proj-name))
;;   (let ((project-symbols (gethash proj-name lazy-project-symbols))
;;         (candidates-by-path (make-hash-table :test 'equal :size 100))
;;         (sources '())
;;         (max-symbol-length (gethash proj-name lazy-project-symbols-max-symbol-length 50))
;;         (one-symbol-match (not word))
;;         (sorted-paths '())
;;         (basedir (lazy-get-config-val 'basedir proj-name))
;;         (buffer-file-name (file-truename (or (buffer-file-name (helm--current-buffer)) "/"))))
;;     (when (hash-table-p project-symbols)
;;       (maphash (lambda (path symbols-by-path)
;;                  (let ((candidates '()))
;;                    (maphash (lambda (symbol symbol-locations)
;;                               (when (and (not one-symbol-match)
;;                                          (string-match (regexp-quote word) symbol))
;;                                 (setq one-symbol-match t))
;;                               (let ((symbol-display (if (> (length symbol) max-symbol-length)
;;                                                         (concat (substring symbol 0 (- max-symbol-length 3)) "...")
;;                                                       (format (concat "%-" (prin1-to-string max-symbol-length) "s") symbol))))
;;                                 (cl-dolist (location symbol-locations)
;;                                   (let* ((line-number (nth 0 location))
;;                                          (definition (nth 1 location))
;;                                          (display (concat symbol-display
;;                                                           " "
;;                                                           (propertize (format "%8s" line-number) 'face 'lazy-helm-symbol-line-numbers-face)
;;                                                           " "
;;                                                           (propertize definition 'face 'lazy-helm-symbol-definition-face)
;;                                                           ))
;;                                          (candidate (cons display (list symbol line-number path))))
;;                                     (setq candidates (append candidates (list candidate)))))))
;;                             symbols-by-path)
;;                    (when (> (length candidates) 0)
;;                      (puthash path candidates candidates-by-path)
;;                      (let ((score -1))
;;                        (when (string-equal buffer-file-name path)
;;                          (setq score (+ score 10)))
;;                        (when (and basedir (string-equal basedir (file-name-directory path)))
;;                          (setq score (+ score 10)))
;;                        (when (and basedir (string-match (concat "^" (regexp-quote basedir) "\\(.*\\)") (file-name-directory path)))
;;                          (setq score (+ score 10))
;;                          (let ((rest-length (length (split-string (match-string 1 (file-name-directory path)) "/"))))
;;                            (when (< rest-length 10)
;;                              (setq score (+ score (- 10 (- rest-length 1)))))))
;;                        (setq sorted-paths (append sorted-paths (list (cons score path))))))))
;;                project-symbols)
;;       (setq sorted-paths (sort sorted-paths
;;                                (lambda (a b)
;;                                  (if (eq (car a) (car b))
;;                                      (string-lessp (cdr a) (cdr b))
;;                                    (> (car a) (car b))
;;                                    ))))
;;       (cl-dolist (path sorted-paths)
;;         (let* ((candidates (gethash (cdr path) candidates-by-path))
;;                (source (helm-build-sync-source (cdr path)
;;                          :candidates candidates
;;                          :candidate-number-limit most-positive-fixnum
;;                          :action (lambda (candidate)
;;                                    (let* ((symbol-name (nth 0 candidate))
;;                                           (line-number (nth 1 candidate))
;;                                           (absolute-path (nth 2 candidate)))
;;                                      (with-current-buffer (helm--current-buffer)
;;                                        (ring-insert xref--marker-ring (point-marker))
;;                                        (lazy-jump-highlight (find-buffer-visiting absolute-path)
;;                                                             (lazy-jump invoke-window absolute-path line-number symbol-name)))))
;;                          )))
;;           (setq sources (append sources (list source)))))
;;       (if (> (length sources) 0)
;;           (helm :sources sources
;;                 :truncate-lines t
;;                 :input (when one-symbol-match word))
;;         (message "No symbols found!")))))

(defvar lazy-helm-do-ag-per-project-settings (make-hash-table :test 'equal))

(defun lazy-helm-do-ag (&optional arg)
  (interactive "P")
  (require 'helm-mode)
  (setq helm-ag--original-window (selected-window))
  (setq helm-ag--last-default-directory nil)
  (let* ((cached-settings (when lazy-name (gethash lazy-name lazy-helm-do-ag-per-project-settings)))
         (helm-ag--default-target (cond ((and lazy-name (not arg) (buffer-file-name (current-buffer)) (not (or (lazy-buffer-p (current-buffer)) (lazy-friendly-buffer-p (current-buffer)))))
                                         (list default-directory))
                                        ((and lazy-name (not arg) cached-settings)
                                         (car-safe cached-settings))
                                        ((and lazy-name (not arg))
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
                                   (if (and lazy-name (not arg) cached-settings (or (lazy-buffer-p (current-buffer)) (lazy-friendly-buffer-p (current-buffer))))
                                       (cdr cached-settings)
                                     (helm-ag--do-ag-searched-extensions)))))
    (when (and lazy-name arg)
      (puthash lazy-name (cons helm-ag--default-target helm-do-ag--extensions) lazy-helm-do-ag-per-project-settings))
    (helm-ag--set-do-ag-option)
    (helm-ag--set-command-features)
    (helm-ag--save-current-context)
    (helm-attrset 'name (helm-ag--helm-header helm-ag--default-directory)
                  helm-source-do-ag)
    (helm :sources '(helm-source-do-ag)
          :input (helm-ag--insert-thing-at-point helm-ag-insert-at-point)
          :keymap helm-do-ag-map
          :buffer (concat "*helm ag*"
                          (when (and (or arg cached-settings)
                                     (or (lazy-buffer-p (current-buffer)) (lazy-friendly-buffer-p (current-buffer)))
                                     (or (not (lazy-path-equal helm-ag--default-directory (print default-directory)))
                                         (not (string-equal (prin1-to-string helm-do-ag--extensions) "(\"*\")"))))
                            (concat " " helm-ag--default-directory
                                    (when helm-do-ag--extensions
                                      (concat "/" (prin1-to-string helm-do-ag--extensions)))))))))

(provide 'lazy-helm)
