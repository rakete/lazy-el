;;; mk-project-helm.el --- Emacs helm integration for mk-project

;; Copyright (C) 2008  Andreas Raster <lazor at affenbande dot org>
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

(require 'helm-config)
(require 'helm-locate)
(require 'helm-buffers)
(require 'helm-files)
(require 'helm-ag)

(defvar helm-c-source-mk-project-projects
  '((name . "Mk-Project projects")
    (candidates . (lambda ()
                    (sort (mk-proj-filter (lambda (title)
                                            (not (string-match (concat "^[^:]*:\\(.*\\)$") title)))
                                          (mk-proj-names))
                          'string-lessp)))
    (action ("Load project" . (lambda (entry)
                                (mk-proj-load (car (helm-marked-candidates)))))
            ;; evil hack, if I use helm inside helm, the topmost action gets called for every
            ;; candidate of the 'inside' helm, so this lambda checks if it is called with something
            ;; that looks like a todo, either loads it or calls another helm instance showing a
            ;; selection of todos
            ("Load todo" . (lambda (entry)
                             (if (string-match (concat "^[^:]*:\\(.*\\)$") entry)
                                 (mk-proj-load entry)
                               (let ((todos (when (functionp 'mk-org-project-todos)
                                              (mk-org-project-todos (car (helm-marked-candidates))))))
                                 (if todos
                                     (helm :sources `((name . "Todos")
                                                      (candidates . ,todos)))
                                   (message "No todos!"))))))))
  "All configured mk-project projects.")

(defvar helm-c-source-mk-project-todos
  '((name . "Mk-Project todos")
    (candidates . (lambda () (when (functionp 'mk-org-project-todos)
                               (mk-org-project-todos))))
    (action . (lambda (entry)
                (mk-proj-load (car (helm-marked-candidates))))))
  "Current projects todos")

(defun mk-helm-relative-call (fun entry)
  (mk-proj-with-directory (mk-proj-get-config-val 'basedir)
                          (if (file-name-absolute-p entry)
                              (funcall fun entry)
                            (funcall fun (expand-file-name entry (mk-proj-get-config-val 'basedir))))))

(defun mk-helm-relative-transformer (files)
  (helm-transform-mapcar
   (lambda (file)
     (unless (file-name-absolute-p file)
      (expand-file-name file (mk-proj-get-config-val 'basedir)))) files))

(defvar helm-c-source-mk-project-files
  `((name . "Mk-Project files")
    (init . (lambda ()
              (with-current-buffer (helm-candidate-buffer 'global)
                ;; (unless (get-buffer (mk-proj-fib-name))
                ;;   (mk-proj-fib-init))
                ;;(insert-buffer (mk-proj-fib-name))
                (loop for filename in (reverse (mk-proj-files))
                      do (insert (concat (expand-file-name filename) "\n"))))))
    (candidates-in-buffer)
    (candidate-number-limit . 300)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (type . file))
  "All files of the currently active project.")

(defvar mk-project-helm-open-buffers-cache nil)

(defvar helm-c-source-mk-project-open-buffers
  `((name . "Mk-Project buffers")
    (init . (lambda ()
              (let ((project-buffers (mk-proj-buffers)))
                (setq mk-project-helm-open-buffers-cache
                      (mapcar 'buffer-name
                              (condition-case nil
                                  (remove-if (lambda (buf) (string-match "\*[^\*]\*" (buffer-name buf))) project-buffers)
                                (error nil)))))
              (unless helm-buffer-max-length
                (setq helm-buffer-max-length 40))
              (unless helm-buffer-max-len-mode
                (setq helm-buffer-max-len-mode 40))
              ))
    (candidates . mk-project-helm-open-buffers-cache)
    (no-matchplugin)
    (volatile)
    (type . buffer)
    ;;(match helm-c-buffer-match-major-mode)
    (persistent-action . helm-c-buffers-list-persistent-action)
    (keymap . ,helm-c-buffer-map)
    (mode-line . helm-buffer-mode-line-string)
    (persistent-help
     . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer"))
  "All buffers of the currently active project.")

(defvar helm-c-source-mk-project-friendly-files
  `((name . "Mk-Project friendly files")
    (init . (lambda ()
              (with-current-buffer (helm-candidate-buffer 'global)
                (dolist (friend (mk-proj-get-config-val 'friends nil t))
                  (if (file-exists-p (expand-file-name friend))
                      (insert (concat (expand-file-name friend) "\n"))
                    (when (gethash friend mk-proj-list)
                      (unless (get-buffer (mk-proj-fib-name friend))
                        (mk-proj-fib-init friend))
                      (if (with-current-buffer (mk-proj-fib-name friend)
                            (save-excursion
                              (goto-char (point-min))
                              (file-name-absolute-p (buffer-substring (point-at-bol) (point-at-eol)))))
                          (insert-buffer (mk-proj-fib-name friend))
                        (mapc (lambda (line)
                                (insert (concat (expand-file-name line (mk-proj-get-config-val 'basedir friend t)) "\n")))
                              (mk-proj-fib-matches nil friend))))))
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

(defvar mk-project-helm-open-friendly-buffers-cache nil)

(defvar helm-c-source-mk-project-open-friendly-buffers
  `((name . "Mk-Project friendly buffers")
    (init . (lambda ()
              (setq mk-project-helm-open-friendly-buffers-cache
                    (mapcar 'buffer-name
                            (condition-case nil
                                (mk-proj-friendly-buffers nil)
                              (error nil))))
              (let ((result (cl-loop for b in mk-project-helm-open-buffers-cache maximize
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
    (candidates . mk-project-helm-open-friendly-buffers-cache)
    (no-matchplugin)
    (volatile)
    (type . buffer)
    ;;(match helm-c-buffer-match-major-mode)
    (persistent-action . helm-c-buffers-list-persistent-action)
    (keymap . ,helm-c-buffer-map)
    (mode-line . helm-buffer-mode-line-string)
    (persistent-help
     . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer"))
  "All friendly buffers of the currently active project." )

(defvar helm-c-source-mk-project-open-special-buffers
  `((name . "Mk-Project special buffers")
    (candidates . (lambda () (mapcar 'buffer-name (condition-case nil
                                                      (mk-proj-special-buffers)
                                                    (error nil)))))
    (type . buffer)
    ;;(match helm-c-buffer-match-major-mode)
    (persistent-action . helm-c-buffers-list-persistent-action)
    (keymap . ,helm-c-buffer-map)
    (volatile)
    (mode-line . helm-buffer-mode-line-string)
    (persistent-help
     . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer"))
  "All special buffers of the currently active project." )

(defun project-helm ()
  (interactive)
  (helm :sources '(helm-c-source-mk-project-files
                   helm-c-source-mk-project-friendly-files
                   helm-c-source-mk-project-open-buffers
                   helm-c-source-mk-project-open-friendly-buffers
                   helm-c-source-mk-project-open-special-buffers
                   helm-c-source-mk-project-todos
                   )
        :buffer "*helm mk-project*"
        :history 'helm-file-name-history))

(defvar mk-proj-helm-ag-basedir (make-hash-table :test 'equal))

(defun project-helm-do-ag (&optional basedir)
  (interactive "P")
  (require 'helm-mode)
  (setq helm-ag--original-window (selected-window))
  (helm-ag--clear-variables)
  (let* ((helm-ag--default-directory default-directory)
         (helm-do-ag--default-target (or (when (and mk-proj-name basedir (listp basedir) (eq (car-safe basedir) 4))
                                           (puthash mk-proj-name
                                                    (helm-read-file-name
                                                     "Search in file(s): "
                                                     :default default-directory
                                                     :marked-candidates t :must-match t)
                                                    mk-proj-helm-ag-basedir)
                                           (gethash mk-proj-name mk-proj-helm-ag-basedir))
                                         (and mk-proj-name (gethash mk-proj-name mk-proj-helm-ag-basedir))
                                         basedir
                                         (condition-case nil (mk-proj-get-config-val 'basedir) (error nil))
                                         (cadr (assoc 'basedir (mk-proj-guess-alist)))
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
