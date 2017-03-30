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

(defvar helm-c-source-lazy-projects
  '((name . "Lazy projects")
    (candidates . (lambda ()
                    (sort (lazy-filter (lambda (title)
                                            (not (string-match (concat "^[^:]*:\\(.*\\)$") title)))
                                          (lazy-names))
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

(defvar helm-c-source-lazy-todos
  '((name . "Lazy todos")
    (candidates . (lambda () (when (functionp 'lazy-org-project-todos)
                               (lazy-org-project-todos))))
    (action . (lambda (entry)
                (lazy-load (car (helm-marked-candidates))))))
  "Current projects todos")

(defun mk-helm-relative-call (fun entry)
  (lazy-with-directory (lazy-get-config-val 'basedir)
                          (if (file-name-absolute-p entry)
                              (funcall fun entry)
                            (funcall fun (expand-file-name entry (lazy-get-config-val 'basedir))))))

(defun mk-helm-relative-transformer (files)
  (helm-transform-mapcar
   (lambda (file)
     (unless (file-name-absolute-p file)
      (expand-file-name file (lazy-get-config-val 'basedir)))) files))

(defvar helm-c-source-lazy-files
  `((name . "Lazy files")
    (init . (lambda ()
              (with-current-buffer (helm-candidate-buffer 'global)
                ;; (unless (get-buffer (lazy-fib-name))
                ;;   (lazy-fib-init))
                ;;(insert-buffer (lazy-fib-name))
                (loop for filename in (reverse (lazy-files))
                      do (insert (concat (expand-file-name filename) "\n"))))))
    (candidates-in-buffer)
    (candidate-number-limit . 300)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (type . file))
  "All files of the currently active project.")

(defvar lazy-helm-open-buffers-cache nil)

(defvar helm-c-source-lazy-open-buffers
  `((name . "Lazy buffers")
    (init . (lambda ()
              (let ((lazy-buffers (lazy-buffers)))
                (setq lazy-helm-open-buffers-cache
                      (mapcar 'buffer-name
                              (condition-case nil
                                  (remove-if (lambda (buf) (string-match "\*[^\*]\*" (buffer-name buf))) lazy-buffers)
                                (error nil)))))
              (unless helm-buffer-max-length
                (setq helm-buffer-max-length 40))
              (unless helm-buffer-max-len-mode
                (setq helm-buffer-max-len-mode 40))
              ))
    (candidates . lazy-helm-open-buffers-cache)
    (no-matchplugin)
    (volatile)
    (type . buffer)
    ;;(match helm-c-buffer-match-major-mode)
    (persistent-action . helm-c-buffers-list-persistent-action)
    (keymap . ,helm-buffer-map)
    (mode-line . helm-buffer-mode-line-string)
    (persistent-help
     . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer"))
  "All buffers of the currently active project.")

(defvar helm-c-source-lazy-friendly-files
  `((name . "Lazy friendly files")
    (init . (lambda ()
              (with-current-buffer (helm-candidate-buffer 'global)
                (dolist (friend (lazy-get-config-val 'friends nil t))
                  (if (file-exists-p (expand-file-name friend))
                      (insert (concat (expand-file-name friend) "\n"))
                    (when (gethash friend lazy-project-list)
                      (unless (get-buffer (lazy-fib-name friend))
                        (lazy-fib-init friend))
                      (if (with-current-buffer (lazy-fib-name friend)
                            (save-excursion
                              (goto-char (point-min))
                              (file-name-absolute-p (buffer-substring (point-at-bol) (point-at-eol)))))
                          (insert-buffer (lazy-fib-name friend))
                        (mapc (lambda (line)
                                (insert (concat (expand-file-name line (lazy-get-config-val 'basedir friend t)) "\n")))
                              (lazy-fib-matches nil friend))))))
                (goto-char (point-min))
                (let ((inhibit-field-text-motion t)
                      (recentf-table (make-hash-table :test 'equal)))
                  (dolist (recentf-item recentf-list)
                    (puthash recentf-item t recentf-table))
                  (sort-subr nil 'forward-line 'end-of-line nil nil
                             (lambda (a b)
                               (let ((s1 (buffer-substring-no-properties (car a) (cdr a)))
                                     (s2 (buffer-substring-no-properties (car b) (cdr b))))
                                 (cond ((and (gethash s1 recentf-table)
                                             (not (gethash s2 recentf-table)))
                                        t)
                                       ((and (not (gethash s1 recentf-table))
                                             (gethash s2 recentf-table))
                                        nil)
                                       (t
                                        (string> s1 s2))))))))))
    (candidates-in-buffer)
    (candidate-number-limit . 300)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (type . file))
  "All files of projects which are friends of this project.")

(defvar lazy-helm-open-friendly-buffers-cache nil)

(defvar helm-c-source-lazy-open-friendly-buffers
  `((name . "Lazy friendly buffers")
    (init . (lambda ()
              (setq lazy-helm-open-friendly-buffers-cache
                    (mapcar 'buffer-name
                            (condition-case nil
                                (lazy-friendly-buffers nil)
                              (error nil))))
              (let ((result (cl-loop for b in lazy-helm-open-buffers-cache maximize
                                     (length b)
                                     into len-buf maximize
                                     (length
                                      (with-current-buffer b
                                        (symbol-name major-mode)))
                                     into len-mode finally return
                                     (cons len-buf len-mode))))
                (unless helm-buffer-max-length
                  (setq helm-buffer-max-length
                        (car result)))
                (unless helm-buffer-max-len-mode
                  (setq helm-buffer-max-len-mode
                        (cdr result))))))
    (candidates . lazy-helm-open-friendly-buffers-cache)
    (no-matchplugin)
    (volatile)
    (type . buffer)
    ;;(match helm-c-buffer-match-major-mode)
    (persistent-action . helm-c-buffers-list-persistent-action)
    (keymap . ,helm-buffer-map)
    (mode-line . helm-buffer-mode-line-string)
    (persistent-help
     . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer"))
  "All friendly buffers of the currently active project." )

(defvar helm-c-source-lazy-open-special-buffers
  `((name . "Lazy special buffers")
    (candidates . (lambda () (mapcar 'buffer-name (condition-case nil
                                                      (lazy-special-buffers)
                                                    (error nil)))))
    (type . buffer)
    ;;(match helm-c-buffer-match-major-mode)
    (persistent-action . helm-c-buffers-list-persistent-action)
    (keymap . ,helm-buffer-map)
    (volatile)
    (mode-line . helm-buffer-mode-line-string)
    (persistent-help
     . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer"))
  "All special buffers of the currently active project." )

(defun lazy-helm ()
  (interactive)
  (helm :sources '(helm-c-source-lazy-files
                   helm-c-source-lazy-friendly-files
                   helm-c-source-lazy-open-buffers
                   helm-c-source-lazy-open-friendly-buffers
                   helm-c-source-lazy-open-special-buffers
                   helm-c-source-lazy-todos
                   )
        :buffer "*helm lazy*"
        :history 'helm-file-name-history))

(defvar lazy-helm-ag-basedir (make-hash-table :test 'equal))

(defun lazy-helm-do-ag (&optional basedir)
  (interactive "P")
  (require 'helm-mode)
  (setq helm-ag--original-window (selected-window))
  (helm-ag--clear-variables)
  (let* ((helm-ag--default-directory default-directory)
         (helm-do-ag--default-target (or (when (and lazy-name basedir (listp basedir) (eq (car-safe basedir) 4))
                                           (puthash lazy-name
                                                    (helm-read-file-name
                                                     "Search in file(s): "
                                                     :default default-directory
                                                     :marked-candidates t :must-match t)
                                                    lazy-helm-ag-basedir)
                                           (gethash lazy-name lazy-helm-ag-basedir))
                                         (and lazy-name (gethash lazy-name lazy-helm-ag-basedir))
                                         basedir
                                         (condition-case nil (lazy-get-config-val 'basedir) (error nil))
                                         (cadr (assoc 'basedir (lazy-guess-alist)))
                                         default-directory))
         (helm-do-ag--extensions (helm-ag--do-ag-searched-extensions)))
    (helm-ag--set-do-ag-option)
    (helm-ag--save-current-context)
    (helm-attrset 'name (helm-ag--helm-header helm-ag--default-directory)
                  helm-source-do-ag)
    (helm :sources '(helm-source-do-ag) :buffer "*helm-ag*"
          :input (helm-ag--insert-thing-at-point helm-ag-insert-at-point)
          :keymap helm-do-ag-map)))

(provide 'lazy-helm)
