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

(require 'mk-project)

(require 'helm-config)
(require 'helm-locate)
(require 'helm-buffers)
(require 'helm-files)

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
              (with-current-buffer (helm-candidate-buffer 'project-files)
                (unless (get-buffer (mk-proj-fib-name))
                  (mk-proj-fib-init))
                (insert-buffer (mk-proj-fib-name)))))
    (candidates-in-buffer)
    (candidate-number-limit . 300)
    (action ("Find file" . (lambda (entry) (mk-helm-relative-call 'helm-find-many-files entry)))
            ("Find file as root" . (lambda (entry) (mk-helm-relative-call 'helm-find-file-as-root entry)))
            ("Find file other window" . (lambda (entry) (mk-helm-relative-call 'find-file-other-window entry)))
            ("Find file other frame" . (lambda (entry) (mk-helm-relative-call 'find-file-other-frame entry)))
            ("Open dired in file's directory" . (lambda (entry) (mk-helm-relative-call 'helm-c-open-dired entry)))
            ("Grep File(s) `C-u recurse'" . (lambda (entry) (mk-helm-relative-call 'helm-find-files-grep entry)))
            ("Zgrep File(s) `C-u Recurse'" . (lambda (entry) (mk-helm-relative-call 'helm-ff-zgrep entry)))
            ("Pdfgrep File(s)" . (lambda (entry) (mk-helm-relative-call 'helm-ff-pdfgrep entry)))
            ("Checksum File" . (lambda (entry) (mk-helm-relative-call 'helm-ff-checksum entry)))
            ("Ediff File" . (lambda (entry) (mk-helm-relative-call 'helm-find-files-ediff-files entry)))
            ("Ediff Merge File" . (lambda (entry) (mk-helm-relative-call 'helm-find-files-ediff-merge-files entry)))
            ("View file" . (lambda (entry) (mk-helm-relative-call 'view-file entry)))
            ("Insert file" . (lambda (entry) (mk-helm-relative-call 'insert-file entry)))
            ("Delete file(s)" . (lambda (entry) (mk-helm-relative-call 'helm-delete-marked-files entry)))
            ("Open file externally (C-u to choose)" . (lambda (entry) (mk-helm-relative-call 'helm-c-open-file-externally entry)))
            ("Open file with default tool" . (lambda (entry) (mk-helm-relative-call 'helm-c-open-file-with-default-tool entry)))
            ("Find file in hex dump" . (lambda (entry) (mk-helm-relative-call 'hexl-find-file entry))))
    (persistent-help . "Show this file")
    (action-transformer helm-c-transform-file-load-el
                        helm-c-transform-file-browse-url)
    (candidate-transformer helm-c-highlight-files
                           helm-c-w32-pathname-transformer
                           helm-c-shorten-home-path
                           mk-helm-relative-transformer)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (match helm-c-match-on-file-name))
  "All files of the currently active project.")

(defvar helm-c-source-mk-project-open-buffers
  `((name . "Mk-Project buffers")
    (candidates . (lambda () (mapcar 'buffer-name (condition-case nil
                                                      (remove-if (lambda (buf) (string-match "\*[^\*]\*" (buffer-name buf))) (mk-proj-buffers))
                                                    (error nil)))))
    (type . buffer)
    (match helm-c-buffer-match-major-mode)
    (persistent-action . helm-c-buffers-list-persistent-action)
    (keymap . ,helm-c-buffer-map)
    (mode-line . helm-buffer-mode-line-string)
    (persistent-help
     . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer"))
  "All buffers of the currently active project.")

(defvar helm-c-source-mk-project-friendly-files
  `((name . "Mk-Project friendly files")
    (init . (lambda ()
              (with-current-buffer (helm-candidate-buffer 'friendly-files)
                (dolist (friend (mk-proj-get-config-val 'friends))
                  (if (file-exists-p (expand-file-name friend))
                      (insert (concat (expand-file-name friend) "\n"))
                    (unless (get-buffer (mk-proj-fib-name friend))
                      (mk-proj-fib-init friend))
                    (if (with-current-buffer (mk-proj-fib-name friend)
                          (save-excursion
                            (goto-char (point-min))
                            (file-name-absolute-p (buffer-substring (point-at-bol) (point-at-eol)))))
                        (insert-buffer (mk-proj-fib-name friend))
                      (mapc (lambda (line)
                              (insert (concat line "\n"))) (mk-proj-fib-friend-matches nil friend))))))))
    (candidates-in-buffer)
    (candidate-number-limit . 300)
    (candidate-transformer helm-c-shorten-home-path)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (type . file))
  "All files of projects which are friends of this project.")

(defvar helm-c-source-mk-project-open-friendly-buffers
  `((name . "Mk-Project friendly buffers")
    (candidates . (lambda () (mapcar 'buffer-name (condition-case nil
                                                      (mk-proj-friendly-buffers t)
                                                    (error nil)))))
    (type . buffer)
    (match helm-c-buffer-match-major-mode)
    (persistent-action . helm-c-buffers-list-persistent-action)
    (keymap . ,helm-c-buffer-map)
    (volatile)
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
    (match helm-c-buffer-match-major-mode)
    (persistent-action . helm-c-buffers-list-persistent-action)
    (keymap . ,helm-c-buffer-map)
    (volatile)
    (mode-line . helm-buffer-mode-line-string)
    (persistent-help
     . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer"))
  "All special buffers of the currently active project." )


(defun helm-mkproject ()
  (interactive)
  (helm :sources '(helm-c-source-mk-project-todos
                   helm-c-source-mk-project-open-buffers
                   helm-c-source-mk-project-open-friendly-buffers
                   helm-c-source-mk-project-open-special-buffers
                   helm-c-source-mk-project-files
                   helm-c-source-mk-project-friendly-files)
        :buffer "*helm mk-project*"
        :history 'helm-file-name-history))

(provide 'mk-project-helm)
