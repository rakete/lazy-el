;;; lazy-orgmode.el ---  Org-Mode integration for lazy-el

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
(require 'lazy-sourcemarker)

(require 'cl-lib)
(require 'etags-table)

(require 'org-install)
(require 'org-protocol)
(require 'org-agenda)
(require 'org-clock)
(require 'ob)

(defvar lazy-org-project-search-files nil
  "List of .org files that lazy-org searches for project definitions.")

(defvar lazy-org-todo-keywords '("TODO")
  "Todo keywords that lazy-org will recognize as tasks. Every org entry that
does not have any of those keywords as todo will not be visited by `lazy-org-map-entries'")

(defvar lazy-org-ignore-todos '("DONE"))

(defvar lazy-org-active-todo-keyword nil
  "Active keyword is a special keyword that subtrees can have to specify a task
that has lots of other tasks as children but is not a complete project itself.

So far this is only used when creating project-todos, to put the todo under
the active subtree, instead of the parent subtree.")

(defvar lazy-org-config-save-location t
  "Where to store project org trees. Can be either a directory name to use
one org file per project stored in a single directory, can be a filename
to use a single org file for all projects, for every other non-nil value
a single org file is stored in the projects basedir.")

(defvar lazy-org-config-save-section "Projects"
  "A headline under which to store project org trees.")

(defun lazy-org-after-save-search ()
  (when lazy-name
    (unless (or (string-match ".*recentf.*" (buffer-name (current-buffer)))
                (string-match ".*file-list-cache.*" (buffer-name (current-buffer)))
                (string-match ".*sourcemarker-db.*" (buffer-name (current-buffer)))
                (string-match ".*continue-db.*" (buffer-name (current-buffer))))
      (let ((filename (buffer-file-name (current-buffer))))
        (when (and filename
                   (eq major-mode 'org-mode)
                   (lazy-get-config-val 'org-file)
                   (string-equal (file-truename filename)
                                 (file-truename (lazy-get-config-val 'org-file))))
          (lazy-org-search-projects filename))))))

(defun lazy-org-concatl (&rest sequences-sequence)
  (let ((r '()))
    (mapc (lambda (l1)
           (mapc (lambda (l2)
                (mapc (lambda (x)
                        (add-to-list 'r x t)) l2))
                 l1))
         sequences-sequence)
   r))

(defun lazy-org-files-containing-projects ()
  "Searches all defined projects and returns a list of all .org files
in which projects have been defined as well as the files specified by
`lazy-org-project-search-files'.
Also tries to find org files with projects in files from `recentf-list'."
  (let ((org-files '()))
    (maphash (lambda (k p)
               ;; - first check for org-file config option
               ;; - second check if a file named $projectname.org is in $basedir
               (cond ((cdr (assoc 'org-file p))
                      (add-to-list 'org-files (cadr (assoc 'org-file p))))
                     ((let ((lazy-org-filename (concat (expand-file-name (cadr (assoc 'basedir p))) (cadr (assoc 'name p)) ".org")))
                        (when (file-exists-p lazy-org-filename)
                          (add-to-list 'org-files lazy-org-filename))))))
             lazy-list)
    (when (boundp 'recentf-list)
      (dolist (recent-file recentf-list)
        (when (and (file-exists-p recent-file)
                   (string-match ".*\\.org" recent-file)
                   (= (call-process "bash" nil nil nil "-c" (concat "grep \"MKP_NAME\\|mkp_name\" \"" recent-file "\"")) 0))
          (add-to-list 'org-files recent-file 'string-equal))))
    (let* ((find-cmd (concat (if (eq system-type 'windows-nt) "gfind" "find") " -iname open-files-cache"))
           (default-directory lazy-global-cache-root)
           (directory-table (make-hash-table :test 'equal)))
      (with-temp-buffer
        (call-process-shell-command find-cmd nil t)
        (loop for cache in (split-string (buffer-string) "\n" t)
              do (with-temp-buffer
                   (insert-file-contents cache)
                   (loop for line in (split-string (buffer-string) "\n" t)
                         do (unless (gethash (lazy-dirname line) directory-table)
                              (puthash (lazy-dirname line) t directory-table)
                              (dolist (org-file (directory-files (lazy-dirname line) t ".*\.org$"))
                                (add-to-list 'org-files org-file))))))))
    (cl-remove-if (lambda (x)
                 (or (not (file-exists-p x))
                     (eq x nil)))
               (cl-remove-duplicates
                (append org-files (lazy-org-concatl (mapcar (lambda (path)
                                                            (let ((path (file-name-as-directory path)))
                                                              (cond ((condition-case nil (directory-files path) (error nil))
                                                                     (let ((currentdir default-directory))
                                                                       (cd path)
                                                                       (let ((result (split-string (shell-command-to-string "grep -ls \"MKP_NAME\\|mkp_name\" *.org") "\n" t)))
                                                                         (cd currentdir)
                                                                         (mapcar (lambda (f) (expand-file-name (concat path f))) result))))
                                                                    ((file-exists-p path)
                                                                     `(,(expand-file-name path)))
                                                                    (t (file-expand-wildcards path)))))
                                                          ;; - lazy-org-project-search-files can contain directories, is appending here the right thing to do? have
                                                          ;; I coded this so that somewhere down the line I check for directories? I hope so...
                                                          (append lazy-org-project-search-files
                                                                  (let ((xs '()))
                                                                    (maphash (lambda (k v)
                                                                               (let ((path (cadr (assoc 'basedir v))))
                                                                                 (when path
                                                                                   (add-to-list 'xs path))))
                                                                             lazy-list)
                                                                    xs)))))
                :test #'string-equal))))

;; (setq lazy-org-project-search-files '("~/org/" "~/org/projects.org"))
;; (lazy-org-files-containing-projects)

;; (defun lazy-org-find-recent-org-files ()
;;   (let ((lazy-files (lazy-org-files-containing-projects)))
;;     (remove-if (lambda (recent-file)
;;                  (or (not (string-equal (file-name-extension recent-file) "org"))
;;                      (cl-some (lambda (proj-file)
;;                              (string-equal (file-name-nondirectory recent-file)
;;                                            (file-name-nondirectory proj-file)))
;;                            lazy-files)))
;;                recentf-list)))

(defun lazy-org-symbol-table (&optional symbol)
  "Creates an alist of (symbol . org-property-string) that can be used
to look up the org property which represents a lazy config symbol.

Optional argument SYMBOL can be specified if you just want to look up
a single symbol and don't need the whole alist."
  (let* ((proj-vars (append lazy-required-vars lazy-optional-vars))
         (props '())
         (table (progn
                  (dolist (varchecks proj-vars props)
                    (add-to-list 'props `(,(car varchecks) . ,(concat "mkp_" (replace-regexp-in-string "-" "_" (downcase (symbol-name (car varchecks))))))))
                  )))
    (if symbol
        (cdr (assoc symbol table))
      table)))

;; (lazy-org-symbol-table 'name)

(defun lazy-org-assert-org (&optional proj-name try-guessing)
  "Same as `lazy-assert-proj' but makes sure that the project has
an associated org file.

Optionally PROJ-NAME can be specified to test a specific project other
than the current one."
  (let ((guessed-alist nil))
    (cond
     ;; 1. project loaded, org -> nothing
     ((and (not (condition-case nil (lazy-assert-proj) (error t)))
           (lazy-get-config-val 'org-file lazy-name))
      nil)
     ;; 2. project loaded, not org -> convert and load lazy-name
     ((and try-guessing
           (not (condition-case nil (lazy-assert-proj) (error t)))
           (not (lazy-get-config-val 'org-file lazy-name))
           (y-or-n-p (concat "Create .org file for " lazy-name "? ")))
      (lazy-org-config-save lazy-name (lazy-find-alist lazy-name t))
      (lazy-load lazy-name)
      nil)
     ;; 3. project not loaded, proj-name and org -> load proj-name
     ((and try-guessing
           (condition-case nil (lazy-assert-proj) (error t))
           proj-name
           (lazy-get-config-val 'org-file proj-name)
           (y-or-n-p (concat "Load " proj-name "? ")))
      (lazy-load proj-name)
      nil)
     ;; 4. project not loaded, proj-name not org -> convert and load proj-name
     ((and try-guessing
           (condition-case nil (lazy-assert-proj) (error t))
           proj-name
           (not (lazy-get-config-val 'org-file proj-name))
           (y-or-n-p (concat "Create .org file for " proj-name "? ")))
      (lazy-org-config-save lazy-name (lazy-find-alist proj-name t))
      (lazy-load proj-name)
      nil)
     ;; 5. guessed exists, org -> load guessed
     ((and try-guessing
           (setq guessed-alist (lazy-guess-alist))
           (assoc 'org-file (lazy-find-alist (cadr (assoc 'name guessed-alist)) t))
           (y-or-n-p (concat "Load " (cadr (assoc 'name guessed-alist)) "? ")))
      (lazy-load (cadr (assoc 'name guessed-alist)))
      nil)
     ;; 6. try-guessing t -> create, convert and load guessed
     ((and try-guessing
           guessed-alist
           (or (not lazy-name)
               (not (string-equal lazy-name
                                  (cadr (assoc 'name guessed-alist)))))
           (y-or-n-p (concat "Create .org file for guessed project *" (cadr (assoc 'name guessed-alist)) "? ")))
      (lazy-org-config-save (cadr (assoc 'name guessed-alist)) guessed-alist)
      (lazy-load (cadr (assoc 'name guessed-alist)))
      nil)
     (t
      (error (format "lazy-org: Project %s has no associated org file!" lazy-name))))))

(defun lazy-org-forward-same-level ()
  (interactive)
  (let ((l (save-excursion
             (beginning-of-line)
             (org-outline-level))))
    (outline-next-heading)
    (while (and (> (org-outline-level) l)
                (not (eobp)))
      (outline-next-heading))))










(defvar lazy-org-map-entry-name nil)
(defvar lazy-org-map-entry-level nil)
(defvar lazy-org-map-entry-point nil)
(defvar lazy-org-map-parent-name nil)
(defvar lazy-org-map-parent-level nil)
(defvar lazy-org-map-parent-point nil)
(defvar lazy-org-map-project-recursion nil)
(defun* lazy-org-map-entries (&key function
                                   (file nil)
                                   (match nil)
                                   (scope 'project-tree)
                                   (widen nil)
                                   (close-files nil)) ;; dangerous
  "Map over org entries. Similar to `org-map-entries', but implements
scopes better suited to map other org entries that are projects and accepts
a wider variety of matches.

FUNCTION is a function that is called for every match found by searching
for MATCH in sources specified by FILE.

FILE can be either just a filename, a list of files or a buffer.

MATCH can be a marker or sourcemarker, a single number representing a point or
a project name. FUNCTION is called with point on everything MATCH finds.

Furthermore MATCH can be:
a exact headline: '(headline \"A headline\")
a regular expression: '(re \"expression\")
a org property: '(property \"NAME\" \"VALUE\")
a function with arguments '(func args) (cl-some leftover experiment, use at own risk)

SCOPE can be either project-tree to map of a whole tree of projects, including
all child projects, project-single to map over a single project tree excluding
child projects and project-headline to map over just the headline(s) of projects.

If scope is nil or one of the `org-map-entries' scope symbols, `org-map-entries'
will be used internally. You can specify a MATCH to be used in that case with:
'(org \"tags/property/todo match\")"
  (let ((results '())
        (next-point nil)
        (opened-files nil)
        (enable-local-variables :safe))
    (dolist (project-file (setq opened-files (cond ((and (markerp match) (marker-buffer match))
                                                    `(,(marker-buffer match)))
                                                   ((and (functionp 'continue-sourcemarker-p)
                                                         (continue-sourcemarker-p match))
                                                    `(,(marker-buffer (continue-sourcemarker-restore match))))
                                                   ((or (buffer-live-p file) (and (char-or-string-p file) (file-exists-p file)))
                                                    `(,file))
                                                   ((and file (listp file))
                                                    file)
                                                   ((or (eq file nil) (eq file 'current-file))
                                                    (progn (lazy-org-assert-org) `(,(lazy-get-config-val 'org-file))))
                                                   (t
                                                    (lazy-org-files-containing-projects)))))
      (with-current-buffer (or (when (buffer-live-p project-file) project-file)
                               (org-find-base-buffer-visiting project-file)
                               (if (file-exists-p project-file)
                                   (find-file-noselect project-file)
                                 (progn
                                   (message (format "lazy-org: No such file %s" project-file))
                                   (return-from "lazy-org-map-entries" nil))))
        ;; - call set-auto-mode when buffer not in org-mode so that org-complex-heading-regexp-format
        ;; gets defined
        (unless (eq major-mode 'org-mode)
          (set-auto-mode))
        (save-excursion
          (save-restriction
            (when widen (widen))
            (goto-char (point-min))
            (let* ((project-file (if (buffer-live-p project-file)
                                     (buffer-file-name project-file)
                                   project-file))
                   (re (cond ((and (markerp match) (marker-position match))
                              (marker-position match))
                             ((and (functionp 'continue-sourcemarker-p)
                                   (continue-sourcemarker-p match))
                              (marker-position (continue-sourcemarker-restore match)))
                             ((numberp match)
                              match)
                             ((and match (listp match) (eq (car match) 'headline))
                              (format org-complex-heading-regexp-format (regexp-quote (cadr match))))
                             ((and match (listp match) (eq (car match) 'org))
                              (cadr match))
                             ((and match (listp match) (eq (car match) 're))
                              (regexp-quote (cadr match)))
                             ((and match (listp match) (eq (car match) 'property) (= (cl-list-length match) 3))
                              (concat "^[ \t]*:" (or (second match) "[^:]+") ":[ \t]*\\(" (or (third match) ".*") "\\)"))
                             ((and match (listp match) (functionp (car match)))
                              (funcall (car match) (cadr match)))
                             ((stringp match)
                              (concat "^[ \t]*:\\(MKP_NAME\\|mkp_name\\):[ \t]*\\(\"" match "\"\\)"))
                             (t
                              "^[ \t]*:\\(MKP_NAME\\|mkp_name\\):[ \t]*\\(.*\\)")))
                   (case-fold-search nil)
                   (skip-project-functions nil))
              (while (cond ((numberp re)
                            t)
                           ((stringp re)
                            (re-search-forward re nil t))
                           (t
                            nil))
                (when (numberp re)
                  (goto-char re)
                  (setq re next-point))
                (unless (when skip-project-functions
                          (save-excursion
                            (org-back-to-heading t)
                            (loop for f in skip-project-functions
                                  until (funcall f)
                                  finally return (funcall f))))
                  (let* ((lazy-org-map-entry-name (save-excursion
                                       (org-back-to-heading t)
                                       (beginning-of-line)
                                       (lazy-org-entry-name)))
                         (lazy-org-map-entry-level (save-excursion
                                        (org-back-to-heading t)
                                        (beginning-of-line)
                                        (lazy-org-entry-level)))
                         (lazy-org-map-entry-point (save-excursion
                                        (org-back-to-heading t)
                                        (beginning-of-line)
                                        (point)))
                         (lazy-org-map-parent-name (save-excursion
                                        (org-back-to-heading t)
                                        (beginning-of-line)
                                        (lazy-org-entry-parent-name)))
                         (lazy-org-map-parent-level (save-excursion
                                         (org-back-to-heading t)
                                         (beginning-of-line)
                                         (lazy-org-entry-parent-level)))
                         (lazy-org-map-parent-point (save-excursion
                                         (org-back-to-heading t)
                                         (beginning-of-line)
                                         (lazy-org-entry-parent-point)))
                         (f-closure (lambda ()
                                      (let ((r nil)
                                            (lazy-org-map-entry-name (read (or (org-entry-get (point) (lazy-org-symbol-table 'name))
                                                                  (org-entry-get (point) (upcase (lazy-org-symbol-table 'name)))
                                                                  (concat "\"" lazy-org-map-parent-name ":" (lazy-org-entry-headline) "\"")
                                                                  "nil")))
                                            (lazy-org-map-entry-level (save-excursion
                                                           (beginning-of-line)
                                                           (org-outline-level)))
                                            (lazy-org-map-entry-point (point)))
                                        (setq r (save-excursion
                                                  (org-back-to-heading t)
                                                  (beginning-of-line)
                                                  (funcall function))
                                              results (append results `(,r)))
                                        r)))
                         (lazy-org-map-project-recursion (lambda ()
                                              (funcall f-closure)
                                              (while (and (progn
                                                            (outline-next-heading)
                                                            (> (org-outline-level) lazy-org-map-entry-level))
                                                          (not (eobp)))
                                                (cond ((and (eq scope 'project-tree)
                                                            (or (org-entry-get (point) (lazy-org-symbol-table 'name))
                                                                (org-entry-get (point) (upcase (lazy-org-symbol-table 'name)))))
                                                       (let* ((lazy-org-map-parent-name lazy-org-map-entry-name)
                                                              (lazy-org-map-parent-level lazy-org-map-entry-level)
                                                              (lazy-org-map-parent-point lazy-org-map-entry-point)
                                                              (lazy-org-map-entry-name (read (or (org-entry-get (point) (lazy-org-symbol-table 'name))
                                                                                    (org-entry-get (point) (upcase (lazy-org-symbol-table 'name)))
                                                                                    "nil")))
                                                              (lazy-org-map-entry-level (save-excursion
                                                                             (beginning-of-line)
                                                                             (org-outline-level)))
                                                              (lazy-org-map-entry-point (point)))
                                                         (setq skip-project-functions (append skip-project-functions
                                                                                              `((lambda ()
                                                                                                  (string-equal (read (or (org-entry-get (point) (lazy-org-symbol-table 'name))
                                                                                                                          (org-entry-get (point) (upcase (lazy-org-symbol-table 'name)))
                                                                                                                          "nil"))
                                                                                                                ,(read (or (org-entry-get (point) (lazy-org-symbol-table 'name))
                                                                                                                           (org-entry-get (point) (upcase (lazy-org-symbol-table 'name)))
                                                                                                                           "nil")))))))
                                                         (funcall lazy-org-map-project-recursion)
                                                         (outline-previous-heading)))
                                                      ((and (eq scope 'project-single)
                                                            (or (org-entry-get (point) (lazy-org-symbol-table 'name))
                                                                (org-entry-get (point) (upcase (lazy-org-symbol-table 'name)))))
                                                       (lazy-org-forward-same-level)
                                                       )
                                                      (t
                                                       (let ((lazy-org-map-parent-name lazy-org-map-entry-name)
                                                             (lazy-org-map-parent-level lazy-org-map-entry-level)
                                                             (lazy-org-map-parent-point lazy-org-map-entry-point))
                                                         (funcall f-closure)
                                                         ))))
                                              )))
                    (cond ((eq scope 'project-headline)
                           (save-excursion
                             (org-back-to-heading t)
                             (funcall f-closure)))
                          ((or (eq scope 'project-tree)
                               (eq scope 'project-single))
                           (funcall lazy-org-map-project-recursion))
                          (t
                           (org-map-entries f-closure re scope nil nil)))))))))))
    (dolist (f opened-files)
      (when (and (stringp f) (file-exists-p f))
        (let ((buf (find-buffer-visiting f)))
          (when (and buf (not (buffer-modified-p buf)) (not (lazy-buffer-has-markers-p buf)))
            (kill-buffer buf)))))
    results))

(defun test-lazy-org-map-entries ()
  (interactive)
  (lazy-org-map-entries
   :file (current-buffer)
   :match (point)
   :scope 'project-single
   :function (lambda ()
               (message (format "%s -> %s" (read (prin1-to-string lazy-org-map-parent-name)) lazy-org-map-entry-name))
               )))









(defun lazy-org-entry-name (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (if (and (boundp 'lazy-org-map-entry-name)
            (eq (point) lazy-org-map-entry-point))
       lazy-org-map-entry-name
     (or (read (or (org-entry-get (point) (lazy-org-symbol-table 'name))
                   (org-entry-get (point) (upcase (lazy-org-symbol-table 'name)))
                   "nil"))
         (let ((some-name (or (org-entry-get (point) (lazy-org-symbol-table 'name) t)
                              (org-entry-get (point) (upcase (lazy-org-symbol-table 'name)) t))))
           (when some-name
             (concat (read some-name) ":" (lazy-org-entry-headline))))))))

(defun lazy-org-entry-marker (&optional marker)
  (interactive)
  (if (markerp marker)
      marker
    (save-excursion
      (beginning-of-line)
      (when (looking-at org-complex-heading-regexp)
        (let ((point (point)))
          (with-current-buffer (or (buffer-base-buffer (current-buffer)) (current-buffer))
            (copy-marker (point-marker))))))))

(defun lazy-org-entry-level (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (if (and (boundp 'lazy-org-map-entry-level)
            (numberp lazy-org-map-entry-level)
            (eq (point) lazy-org-map-entry-point))
       lazy-org-map-entry-level
     (save-excursion
       (if (not (looking-at org-complex-heading-regexp)) 0
         (beginning-of-line)
         (org-outline-level))))))

(defun lazy-org-entry-parent-level (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (if (and (boundp 'lazy-org-map-parent-level)
            (numberp lazy-org-map-parent-level)
            (eq (point) lazy-org-map-entry-point))
       lazy-org-map-parent-level
     (save-excursion
       (let ((p (lazy-org-entry-parent-point)))
         (if (eq p nil) (org-outline-level)
           (goto-char p)
           (beginning-of-line)
           (org-outline-level)))))))

(defun lazy-org-entry-parent-name (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (if (and (boundp 'lazy-org-map-parent-name)
            (eq (point) lazy-org-map-entry-point))
       lazy-org-map-parent-name
     (save-excursion
       (let ((p (lazy-org-entry-parent-point)))
         (if (eq p nil) nil
           (goto-char p)
           (read (or (org-entry-get (point) (lazy-org-symbol-table 'name))
                     (org-entry-get (point) (upcase (lazy-org-symbol-table 'name)))))))))))

(defun lazy-org-entry-parent-point (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (if (and (boundp 'lazy-org-map-parent-point)
            (boundp 'lazy-org-map-entry-point)
            (eq (point) lazy-org-map-entry-point))
       lazy-org-map-parent-point
     (save-excursion
       (org-back-to-heading t)
       (let ((lazy-org-map-parent-point nil)
             (lazy-org-map-parent-name (or (org-entry-get (point) (lazy-org-symbol-table 'name) t)
                              (org-entry-get (point) (upcase (lazy-org-symbol-table 'name)) t)))
             (cont nil)
             (is-project (lazy-org-entry-is-project-p)))
         (setq cont (org-up-heading-safe))
         (when is-project
           (setq lazy-org-map-parent-name (or (org-entry-get (point) (lazy-org-symbol-table 'name) t)
                                 (org-entry-get (point) (upcase (lazy-org-symbol-table 'name)) t))))
         (when (and lazy-org-map-parent-name cont)
           (while (and cont
                       (setq lazy-org-map-parent-point (point))
                       (not (string-equal (or (org-entry-get (point) (lazy-org-symbol-table 'name))
                                              (org-entry-get (point) (upcase (lazy-org-symbol-table 'name))))
                                          lazy-org-map-parent-name)))
             (setq cont (org-up-heading-safe))))
         lazy-org-map-parent-point)))))

(defun lazy-org-entry-nearest-active (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (if lazy-org-active-todo-keyword
       (save-excursion
         (org-back-to-heading t)
         (unless (lazy-org-entry-is-project-p)
           (while (and (not (string-equal (org-get-todo-state) lazy-org-active-todo-keyword))
                       (not (lazy-org-entry-is-project-p)))
             (org-up-heading-all 1)))
         (point))
     (lazy-org-entry-parent-point))))

(defun lazy-org-entry-headline (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (save-excursion
     (org-back-to-heading t)
     (beginning-of-line)
     (and (looking-at org-complex-heading-regexp)
          (let* ((headline-raw (org-match-string-no-properties 4)))
            (cond ((string-match org-bracket-link-regexp headline-raw)
                   (match-string 2 headline-raw))
                  ((string-match "^\\([^\\[]+\\)\s+\\[\\([0-9]%\\|[0-9][0-9]%\\)\\]$" headline-raw)
                   (match-string 1 headline-raw))
                  (t
                   headline-raw)))))))

;; (when (string-match "^\\([^\\[]+\\)\s+\\[\\([0-9]%\\|[0-9][0-9]%\\)\\]$" "foobar")
;;   (print (match-string 2 "foobar [19%]")))

(defun lazy-org-entry-is-link-p (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (save-excursion
     (beginning-of-line)
     (and (looking-at org-complex-heading-regexp)
          (let* ((headline-raw (org-match-string-no-properties 4)))
            (string-match org-bracket-link-regexp headline-raw))))))


(defun lazy-org-entry-progress (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (save-excursion
     (beginning-of-line)
     (and (looking-at org-complex-heading-regexp)
          (let* ((headline-raw (org-match-string-no-properties 4)))
            (cond ((string-match "^\\(.*\\)\s+\\[\\([0-9][0-9]%\\)\\]$" headline-raw)
                   (match-string 2 headline-raw))
                  (t
                   "")))))))

(defun lazy-org-entry-is-project-p (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (when (save-excursion
           (beginning-of-line)
           (or (org-entry-get (point) (lazy-org-symbol-table 'name))
               (org-entry-get (point) (upcase (lazy-org-symbol-table 'name)))))
     t)))

(defun lazy-org-entry-is-in-project-p (&optional marker)
  (interactive)
  (with-or-without-marker marker
                          (when (save-excursion
                                  (beginning-of-line)
                                  (or (org-entry-get (point) (lazy-org-symbol-table 'name) t)
                                      (org-entry-get (point) (upcase (lazy-org-symbol-table 'name)) t)))
                            t)))

(defun lazy-org-entry-alist (&optional marker)
  "Get config-alist from org entry properties at point."
  (interactive)
  (with-or-without-marker marker
   (save-excursion
     (org-back-to-heading t)
     (beginning-of-line)
     (unless (and (eq major-mode 'org-mode)
                  (looking-at org-complex-heading-regexp))
       (error "lazy-org: buffer is not in org-mode or point is not a org heading"))
     (let ((entry-config '()))
       (dolist (prop (lazy-org-symbol-table) entry-config)
         (let* ((sym (car prop))
               (propname (cdr prop))
               (value (read (or (org-entry-get (point) propname)
                                (org-entry-get (point) (upcase propname))
                                "nil"))))
           (when value
             (add-to-list 'entry-config `(,sym ,value)))))
       (let ((entry-config (append entry-config `((org-file ,(or (buffer-file-name (current-buffer))
                                                                 (buffer-file-name (buffer-base-buffer (current-buffer)))))
                                                  (org-headline ,(lazy-org-entry-headline))
                                                  (org-level ,(- (lazy-org-entry-level) (lazy-org-entry-parent-level))))))
             (proj-parent (lazy-org-entry-parent-name)))
         (when proj-parent
           (add-to-list 'entry-config `(parent ,proj-parent)))
         entry-config)))))

(defun lazy-org-entry-define-project (&optional marker)
  (when (looking-at org-complex-heading-regexp)
    (let* ((proj-name (lazy-org-entry-name marker))
           (alist (lazy-eval-alist proj-name (lazy-org-entry-alist marker))))
      (when alist
        (puthash proj-name alist lazy-list)
        ;;(message "Defined: %s" proj-name)
        alist))))

(defun lazy-org-entry-undefine-project (&optional marker)
  (interactive)
  (with-or-without-marker marker
                          (save-excursion
                            (org-back-to-heading t)
                            (beginning-of-line)
                            (unless (and (eq major-mode 'org-mode)
                                         (looking-at org-complex-heading-regexp))
                              (error "lazy-org: buffer is not in org-mode or point is not a org heading"))
                            (lazy-undef (lazy-org-entry-name)))))









(defun lazy-org-project-marker (&optional proj-name)
  "Get current or PROJ-NAME projects org entry as marker."
  (interactive)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (lazy-org-assert-org proj-name)
  (let ((org-file (lazy-config-val 'org-file proj-name t))
        (org-headline (lazy-config-val 'org-headline proj-name t))
        (enable-local-variables :safe))
    (with-current-buffer (find-file-noselect org-file)
      (car (lazy-org-map-entries
            :file (current-buffer)
            :match `(headline ,org-headline)
            :scope 'project-headline
            :function (lambda ()
                        (save-excursion
                          (org-back-to-heading t)
                          (beginning-of-line)
                          (lazy-org-entry-marker)
                          )))))))

(defun lazy-org-project-buffer-name (&optional proj-name)
  (let ((proj-name (if proj-name
                  proj-name
                (progn
                  (lazy-assert-proj)
                  lazy-name))))
    (concat "*lazy-org: " (or (lazy-config-val 'parent proj-name) proj-name) "*")))


(defun lazy-org-create-project-buffer (&optional proj-name bufname)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (lazy-org-assert-org proj-name)
  (let* ((marker (lazy-org-project-marker))
         (buffername (or bufname (lazy-org-project-buffer-name proj-name)))
         (headline (lazy-org-entry-headline marker))
         (org-show-hierarchy-above t)
         (org-show-following-heading nil)
         (org-show-siblings nil))
    (let (beg end buf)
      (car (lazy-org-map-entries
            :match marker ;; `(eval (format org-complex-heading-regexp-format ,headline))
            :scope 'project-headline
            :function (lambda ()
                        (progn
                          (org-back-to-heading t)
                          (when (not (lazy-org-entry-is-project-p))
                            (goto-char (lazy-org-entry-parent-point)))
                          (setq beg (point))
                          (org-end-of-subtree t t)
                          (if (org-on-heading-p) (backward-char 1))
                          (setq end (point)
                                buf (or (get-buffer buffername)
                                        (make-indirect-buffer (current-buffer) buffername 'clone)))
                          (with-current-buffer buf
                            (narrow-to-region beg end)
                            (outline-show-all)
                            (goto-char (point-min))
                            (when (lazy-org-entry-is-in-project-p) (org-cycle))
                            (widen)
                            (lazy-org-reveal)
                            (goto-char (marker-position marker))
                            (set-frame-name (lazy-org-entry-headline))
                            (current-buffer)
                            ))))))))

(defun lazy-org-reveal ()
  (interactive)
  ;;(lazy-org-assert-org)
  (lazy-org-map-entries
   :file (current-buffer)
   :match (point)
   :scope 'project-tree
   :function (lambda ()
               (when (or (and (cl-some (lambda (x) (string-equal (org-get-todo-state) x)) lazy-org-todo-keywords)
                              (not (cl-some (lambda (x) (string-equal (org-get-todo-state) x)) lazy-org-ignore-todos)))
                         (lazy-org-entry-is-project-p))
                 (message "Showing entry: %s" (lazy-org-entry-name))
                 (org-show-context)
                 (when (and lazy-name
                            ;;(not (lazy-org-entry-is-project-p))
                            (string-equal (lazy-org-entry-name) lazy-name))
                   (org-show-entry))
                 ))))


(defun lazy-org-get-project-buffer (&optional proj-name bufname)
  (or (get-buffer (or bufname (lazy-org-project-buffer-name proj-name)))
      (lazy-org-create-project-buffer proj-name bufname)))

(defun lazy-org-kill-project-buffer (&optional proj-name bufname)
  (let ((buf (get-buffer (or bufname (lazy-org-project-buffer-name proj-name)))))
    (when buf (kill-buffer buf))))

(defun lazy-org-buffer (&optional proj-name)
  "Open current projects org tree as indirect buffer."
  (interactive)
  (lazy-org-assert-org proj-name t)
  (display-buffer (lazy-org-get-project-buffer proj-name)))















;; (defun lazy-org-yank-below (&optional arg adjust-subtree)
;;   (interactive "P")
;;   (let (p)
;;     (save-excursion
;;       (save-restriction
;;         (org-save-outline-visibility t
;;           (when (looking-at org-complex-heading-regexp)
;;             (show-all)
;;             (let ((level (org-outline-level))
;;                   (auto-adjust (or (org-entry-get (point) (cdr (assoc 'name (lazy-org-symbol-table))))
;;                                    'todo)))
;;               (outline-next-heading)
;;               (while (and (> (org-outline-level) level)
;;                           (not (eobp)))
;;                 (outline-next-heading)
;;                 (when (eq auto-adjust 'todo)
;;                   (setq auto-adjust nil)))
;;               (outline-previous-heading)
;;               (org-end-of-subtree)
;;               (newline)
;;               (let ((org-yank-folded-subtrees t)
;;                     (org-yank-adjusted-subtrees nil))
;;                 (org-yank))
;;               (if (or adjust-subtree auto-adjust)
;;                   (progn
;;                     (outline-previous-heading)
;;                     (org-shiftmetaright))
;;                 (outline-previous-heading))
;;               (setq p (point)))))))
;;     p))

(defun lazy-org-paste-below (&optional arg)
  (interactive "P")
  (save-excursion
    (save-restriction
      (org-save-outline-visibility t
        (when (looking-at org-complex-heading-regexp)
          (outline-show-all)
          (let ((level (org-outline-level)))
            (org-end-of-subtree)
            (org-paste-subtree (+ level 1))))
        (point)))))

(defvar lazy-org-add-todo-mode-map (make-sparse-keymap))

(defvar lazy-org-add-todo-mode-hook nil)

(define-minor-mode lazy-org-add-todo-mode nil nil " AddTodo" lazy-org-add-todo-mode-map
  (run-hooks 'lazy-org-add-todo-mode-hook))

(define-key lazy-org-add-todo-mode-map "\C-c\C-c" 'lazy-org-add-todo-finalize)
(define-key lazy-org-add-todo-mode-map "\C-c\C-k" 'lazy-org-add-todo-abort)

(defvar-local local-prefix-arg nil)
(defun lazy-org-add-todo-finalize (&optional arg)
  (interactive "P")
  (setq arg (or arg local-prefix-arg))
  (lazy-org-assert-org)
  (goto-char (point-min))
  (outline-next-heading)
  (org-copy-subtree 1 t)
  (lazy-org-map-entries
   :file (lazy-org-get-project-buffer)
   :match `(headline ,(lazy-get-config-val 'org-headline))
   :scope 'project-headline
   :function (lambda ()
               (let* ((active (lazy-org-entry-nearest-active))
                      (parent (lazy-org-entry-parent-point))
                      (org-show-hierarchy-above t)
                      (org-show-following-heading nil)
                      (org-show-siblings nil))
                 (if arg
                     (when parent (goto-char parent))
                   (when active (goto-char active)))
                 (save-excursion
                   (goto-char (lazy-org-paste-below arg))
                   (lazy-org-reveal)
                   (lazy-org-entry-define-project)
                   (when parent (goto-char parent))
                   (lazy-org-reveal)))))
  (kill-buffer))

(defun lazy-org-add-todo-abort ()
  (interactive)
  (kill-buffer))

(defun lazy-todo (&optional arg)
  "Pop up a buffer where the user can create a new org entry to be added to the current
project. Will add it to the nearest entry that has a `lazy-org-active-todo-keyword' or to the
parent entry. Adding to the parent can be forced with a prefix argument.

Eventually this could be changed so that the prefix argument asks the user under
which entry (all of them not just the parent or nearest active) to add the todo.

See also `lazy-org-entry-nearest-active'."
  (interactive "P")
  (lazy-org-assert-org)
  (let* ((proj-b (current-buffer))
         (buf (get-buffer-create "*lazy: add todo*"))
         (window (display-buffer buf)))
    (select-window window)
    (set-window-dedicated-p window t)
    (org-mode)
    (lazy-org-add-todo-mode 1)
    (with-current-buffer buf
      (set local-prefix-arg arg)
      (org-insert-heading)
      (org-todo "TODO"))))

(defun lazy-org-project-todos (&optional proj-name)
  "Get all todo titles for a PROJ-NAME or the current project."
  (unless (or proj-name
              (condition-case nil (lazy-org-assert-org) (error t)))
    (setq proj-name (or (lazy-get-config-val 'parent)
                        lazy-name)))
  (when (and proj-name
             (lazy-find-alist proj-name))
    (let ((todos '()))
      (maphash (lambda (title alist)
                 (when (string-match (concat "^" proj-name ":\\(.*\\)$") title)
                   (add-to-list 'todos title)))
               lazy-list)
      todos)))

;; (lazy-org-project-todos "diplom")








(defun lazy-org-find-save-location-marker (&optional proj-name config-alist)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let ((enable-local-variables :safe))
    (cond ;; find parent headline
     ((and (lazy-get-config-val 'parent proj-name t)
           (lazy-get-config-val 'org-file proj-name t)
           (lazy-get-config-val 'org-headline proj-name t))
      (with-current-buffer (find-file-noselect (lazy-get-config-val 'org-file proj-name t))
        (save-excursion
          (if (condition-case nil (goto-char (org-find-exact-headline-in-buffer (lazy-get-config-val 'org-headline proj-name t))) (error nil))
              (point-marker)
            (error "lazy-org: could not find a location to save project %s 1" proj-name)))))
     ;; find existing project headline
     ((and (lazy-get-config-val 'org-file proj-name t)
           (lazy-get-config-val 'org-headline proj-name t))
      (with-current-buffer (find-file-noselect (lazy-get-config-val 'org-file proj-name t))
        (save-excursion
          (if (condition-case nil (goto-char (org-find-exact-headline-in-buffer (lazy-get-config-val 'org-headline proj-name t))) (error nil))
              (point-marker)
            (error "lazy-org: could not find a loctation to save project %s 2" proj-name)))))
     ;; find file in directory named after project
     ((condition-case nil (directory-files (expand-file-name lazy-org-config-save-location)) (error nil))
      (with-current-buffer (find-file-noselect (concat (expand-file-name lazy-org-config-save-location) proj-name ".org"))
        (save-excursion
          (goto-char (point-max))
          (point-marker))))
     ;; find headline (section) to save under in one big org file
     ((and (stringp lazy-org-config-save-location)
           (stringp lazy-org-config-save-section)
           (file-exists-p (expand-file-name lazy-org-config-save-location)))
      (with-current-buffer (find-file-noselect (expand-file-name lazy-org-config-save-location))
        (save-excursion
          (if (condition-case nil (goto-char (org-find-exact-headline-in-buffer lazy-org-config-save-section)) (error nil))
              (point-marker)
            (error "lazy-org: could not find a location to save project %s 3" proj-name)))))
     ;; find file in project basedir
     ((and lazy-org-config-save-location
           (or (lazy-get-config-val 'basedir proj-name t)
               (cadr (assoc 'basedir config-alist))))
      (with-current-buffer (find-file-noselect (concat (or (lazy-get-config-val 'basedir proj-name t)
                                                           (cadr (assoc 'basedir config-alist))) "/" proj-name ".org"))
        (goto-char (point-max))
        (point-marker)))
     ;; no suitable location found
     (t (error "lazy-org: could not find a location to save project %s 4" proj-name)))))







(defun lazy-org-config-insert (proj-name config-alist &optional insert-undefined insert-internal headline)
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "lazy-org: current buffer not in org-mode"))
  (unless proj-name
    (setq proj-name (or (cadr (assoc 'name config-alist)) headline "NewProject")))
  (save-excursion
    (save-restriction
      (org-insert-heading)
      (if headline
          (insert headline)
        (insert proj-name))
      (org-set-property "mkp_name" (prin1-to-string proj-name))
      (loop for k in (append lazy-required-vars lazy-optional-vars)
            if (not (or (eq (car k) 'name)
                        (or (lazy-any (lambda (j) (eq (car k) j)) lazy-internal-vars)
                            insert-internal)))
            do (when (or insert-undefined
                         (assoc (car k) config-alist))
                 (let* ((prop (cdr (assoc (car k) (lazy-org-symbol-table))))
                        (val (cadr (assoc (car k) config-alist))))
                   (org-entry-delete (point) (upcase prop))
                   (org-set-property prop (prin1-to-string val)))))
      (point-marker))))

;; (setq lazy-org-config-save-location "~/org/projects.org")
;; (lazy-org-find-save-location-marker "cl-horde3d")

(defun lazy-org-config-save (proj-name config-alist &optional headline)
  (unless headline
    (setq headline (or (cadr (assoc 'org-headline config-alist)) proj-name)))
  (when lazy-org-config-save-location
    (let ((marker (lazy-org-find-save-location-marker proj-name config-alist)))
      (with-marker
       marker
       (let* ((org-show-hierarchy-above t)
              (org-show-following-heading nil)
              (org-show-siblings nil)
              (mod (buffer-modified-p))
              (saved-marker (save-excursion
                              (cond ((looking-at (format org-complex-heading-regexp-format headline))
                                     (progn
                                       (org-set-property "mkp_name" (prin1-to-string proj-name))
                                       (loop for k in (append lazy-required-vars lazy-optional-vars)
                                             if (not (or (eq (car k) 'name)
                                                         (lazy-any (lambda (j) (eq (car k) j)) lazy-internal-vars)))
                                             do (when (assoc (car k) config-alist)
                                                  (let* ((prop (cdr (assoc (car k) (lazy-org-symbol-table))))
                                                         (val (cadr (assoc (car k) config-alist))))
                                                    (org-entry-delete (point) (upcase prop))
                                                    (org-set-property prop (prin1-to-string val)))))
                                       (point-marker)))
                                    ((looking-at org-complex-heading-regexp)
                                     (org-end-of-subtree)
                                     (lazy-org-config-insert proj-name config-alist nil nil headline))
                                    ((eq major-mode 'org-mode)
                                     (lazy-org-config-insert proj-name config-alist nil nil headline))))))
         (save-buffer)
         (set-buffer-modified-p mod)
         (lazy-org-entry-define-project saved-marker))))))

;;(lazy-org-config-save "sauerbraten" (lazy-find-alist "sauerbraten" t))
;; (lazy-org-find-save-location-marker "sauerbraten")
;;(cadr (assoc 'name (lazy-find-alist "cl-horde3d" t)))

(defun* lazy-org-config-buffer (&optional (state :create) proj-name config-alist)
  (case state
    (:create
     (when (and (boundp 'org-complex-heading-regexp)
                (if (eq major-mode 'org-mode)
                    (save-excursion
                      (org-back-to-heading)
                      (looking-at org-complex-heading-regexp))
                  t))
       (let* ((buf (get-buffer-create "*lazy: new project*"))
              (window (display-buffer buf))
              (config-alist (or config-alist (lazy-guess-alist)))
              (headline (and (eq major-mode 'org-mode)
                             (boundp 'org-complex-heading-regexp)
                             (looking-at org-complex-heading-regexp)
                             (lazy-org-entry-headline))))
         (select-window window)
         (set-window-dedicated-p window t)
         (org-mode)
         (buffer-disable-undo)
         (lazy-org-config-insert (or proj-name
                                   headline
                                   (cadr (assoc 'name config-alist))
                                   "NewProject") config-alist t)
         (goto-char 0)
         (end-of-line)
         (lazy-backend-create-project-mode 'org-mode)
         (buffer-enable-undo))))
    (:edit
     (let* ((buf (lazy-org-get-project-buffer nil "*lazy: edit project*"))
            (window (display-buffer buf)))
       (select-window window)
       (set-window-dedicated-p window t)
       (if (re-search-forward ":PROPERTIES:" (save-excursion (org-end-of-subtree) (point)) t)
           (org-cycle))
       (buffer-disable-undo)
       (lazy-backend-edit-project-mode 'org-mode)
       (buffer-enable-undo)))
    (:finalize-create
     (save-excursion
       (let ((has-error t))
         (goto-char (point-min))
         (outline-next-heading)
         (let* ((headline (lazy-org-entry-headline))
                (proj-name (lazy-org-entry-name))
                (config-alist (lazy-eval-alist proj-name (lazy-org-entry-alist)))
                (marker (lazy-org-find-save-location-marker proj-name config-alist)))
           (org-copy-subtree 1 nil)
           (when marker
             (with-current-buffer (marker-buffer marker)
               (goto-char (marker-position marker))
               (let* ((org-show-hierarchy-above t)
                      (org-show-following-heading nil)
                      (org-show-siblings nil))
                 (save-excursion
                   (org-back-to-heading t)
                   (let ((beg (point-at-bol))
                         (end (org-end-of-subtree)))
                     (delete-region beg end))))
               (org-yank)
               (when (condition-case nil (lazy-org-entry-define-project) (error nil))
                 (progn
                   (print (concat "added: " headline))
                   (lazy-org-reveal)
                   (setq has-error nil))))))
         (unless has-error
           (kill-buffer)))))
    (:finalize-edit
     (let ((marker (org-find-exact-headline-in-buffer (lazy-get-config-val 'org-headline))))
       (when marker
         (goto-char (marker-position marker))
         (unless (eq (condition-case nil (lazy-org-entry-define-project) (error 'error))
                     'error)
           (save-buffer)
           (kill-buffer)))))))














;;(setq lazy-config-function 'lazy-org-config-buffer)

(defun lazy-org-read (cell)
  "Convert the string value of CELL to a number if appropriate.
Otherwise if cell looks like lisp (meaning it starts with a
\"(\" or a \"'\") then read it as lisp, otherwise return it
unmodified as a string.

Also return nil when string equals \"nil\", t when string equals \"t\"

This is taken almost directly from `org-babel-read'."
  (if (and (stringp cell) (not (equal cell "")))
      (or (org-babel-number-p cell)
          (if (or (equal "(" (substring cell 0 1))
                  (equal "'" (substring cell 0 1))
                  (equal "`" (substring cell 0 1))
                  (equal "nil" cell)
                  (equal "t" cell))
              (eval (read cell))
            (progn (set-text-properties 0 (length cell) nil cell) cell)))
    cell))


(defun lazy-org-entry-last-clock-position ()
  (save-excursion
    (save-restriction
      (org-save-outline-visibility t
        (outline-show-all)
        (re-search-forward org-clock-string (save-excursion (org-end-of-subtree)) t)))))

(defun lazy-org-search-projects (&rest search-files)
  (unless (or search-files lazy-org-project-search-files)
    (error "lazy-org: could not find any projects, lazy-org-project-search-files is not set"))
  (let ((buffer-with-projects nil)
        (pre-define-buffer-list (buffer-list))
        (continue-prevent-save t)
        (continue-prevent-restore t)
        (files (or search-files (lazy-org-files-containing-projects)))
        (zeitgeist-prevent-send t))
    (when files
      (lazy-org-map-entries
       :file files
       ;;:match "lazy"
       :scope 'project-tree
       :close-files nil
       :function (lambda ()
                   (when (and (not (lazy-org-entry-is-link-p))
                              (or (and (org-get-todo-state)
                                       (and (cl-some (lambda (x) (string-equal (org-get-todo-state) x)) lazy-org-todo-keywords)
                                            (not (cl-some (lambda (x) (string-equal (org-get-todo-state) x)) lazy-org-ignore-todos))))
                                  (and (lazy-org-entry-is-project-p)
                                       (or (not (org-get-todo-state))
                                           (and (org-get-todo-state)
                                                (cl-some (lambda (x) (string-equal (org-get-todo-state) x)) lazy-org-todo-keywords)
                                                (not (cl-some (lambda (x) (string-equal (org-get-todo-state) x)) lazy-org-ignore-todos)))))))
                     (lazy-org-entry-define-project)
                     (unless (eq (last buffer-with-projects) (current-buffer))
                       (add-to-list 'buffer-with-projects (current-buffer))))))
      (dolist (f files)
        (when (file-exists-p f)
          (let ((buf (find-buffer-visiting f)))
            (when (and buf
                       (not (buffer-modified-p buf))
                       (not (cl-some (lambda (b) (eq b buf)) buffer-with-projects))
                       (not (cl-some (lambda (b) (eq b buf)) pre-define-buffer-list)))
              (kill-buffer buf))))))
    nil))

(defun lazy-org-undefine-entries ()
  (interactive)
  (lazy-org-map-entries
   :file (lazy-org-files-containing-projects)
   :scope 'project-tree
   :function (lambda ()
               (when (not (lazy-org-entry-is-project-p))
                 (lazy-undef lazy-org-map-entry-name)))))

(defun lazy-org-clock-cut ()
  (save-excursion
    (org-save-outline-visibility t
      (when (and (looking-at org-complex-heading-regexp)
                 (lazy-org-entry-last-clock-position))
        (outline-show-all)
        (goto-char (lazy-org-entry-last-clock-position))
        (when (looking-at (concat "\\s-*" org-clock-string))
          (kill-region (point-at-bol) (point-at-bol 2)))))))

(defun lazy-org-clock-yank ()
  (save-excursion
    (org-save-outline-visibility t
      (when (and (looking-at org-complex-heading-regexp)
                 (string-match org-clock-string (car kill-ring)))
        (outline-show-all)
        (org-clock-find-position nil)
        (forward-line)
        (while (looking-at (concat "\\s-*" org-clock-string))
          (forward-line))
        (beginning-of-line)
        (yank)
        (indent-according-to-mode)))))

(defun lazy-org-clock-from-parent-to-todo ()
  (save-excursion
    (let ((todo-name (lazy-org-entry-name)))
      (when (lazy-org-entry-parent-point)
        (goto-char (lazy-org-entry-parent-point))
        (when (and org-clock-marker
                   (string-equal (lazy-org-entry-headline org-clock-marker)
                                 (lazy-org-entry-headline))
                   (y-or-n-p "Move clocked time from %s to %s?" (lazy-org-entry-name org-clock-marker) todo-name))
          (org-clock-out)
          (lazy-org-clock-cut)
          (org-clock-in)))
      (lazy-org-clock-yank))))

(defun lazy-clock-in (&optional proj-name)
  (interactive)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (lazy-org-assert-org proj-name)
  (let ((zeitgeist-prevent-send t)
        (continue-prevent-save t))
    (lazy-org-map-entries
     :file (lazy-get-config-val 'org-file proj-name)
     :match `(headline ,(lazy-get-config-val 'org-headline proj-name))
     :scope 'project-headline
     :function (lambda ()
                 (org-clock-in)
                 ))))

(defun lazy-clock-out (&optional proj-name)
  (interactive)
  (let ((continue-prevent-save t))
    (unless proj-name
      (lazy-assert-proj)
      (setq proj-name lazy-name))
    (when (org-clocking-p)
      (lazy-org-assert-org proj-name)
      (org-clock-out))))

(with-eval-after-load "lazy-orgmode"
  '(progn
     (add-to-list 'lazy-optional-vars '(org-file . (stringp)))
     (add-to-list 'lazy-optional-vars '(org-headline . (stringp)))

     (add-to-list 'lazy-internal-vars 'org-file)
     (add-to-list 'lazy-internal-vars 'org-headline)

     (add-hook 'org-clock-in-hook (lambda ()
                                    (when (and (lazy-org-entry-is-in-project-p)
                                               (or (not (boundp 'lazy-name))
                                                   (and (boundp 'lazy-name)
                                                        lazy-name
                                                        (not (string-equal lazy-name (lazy-org-entry-name))))))
                                      (unless (lazy-find-alist (lazy-org-entry-name))
                                        (lazy-def (lazy-org-entry-name) (lazy-org-entry-alist)))
                                      (lazy-load (lazy-org-entry-name)))))
     (add-hook 'org-after-todo-state-change-hook (lambda ()
                                                   (beginning-of-line)
                                                   (when (cl-some (lambda (x) (string-equal (org-get-todo-state) x)) org-done-keywords)
                                                     (lazy-org-clock-from-parent-to-todo))))
     (add-hook 'org-after-todo-state-change-hook (lambda ()
                                                   (beginning-of-line)
                                                   (when (and (cl-some (lambda (x) (string-equal (org-get-todo-state) x)) lazy-org-todo-keywords)
                                                              (lazy-org-entry-is-in-project-p))
                                                     (lazy-org-entry-define-project))))
     (add-hook 'org-after-todo-state-change-hook (lambda ()
                                                   (beginning-of-line)
                                                   (let ((proj-name (lazy-org-entry-name)))
                                                     (when (and proj-name
                                                                (cl-some (lambda (x) (string-equal (org-get-todo-state) x)) lazy-org-todo-keywords)
                                                                (lazy-org-entry-is-in-project-p)
                                                                (functionp 'lazy-sourcemarker-save-all)
                                                                (not (file-exists-p (lazy-get-config-val 'sourcemarker-db-path proj-name))))
                                                       (lazy-with-current-project proj-name (lazy-sourcemarker-save-all))))))

     (lazy-define-backend 'org-mode
                          :buffer-fun 'lazy-org-config-buffer
                          :save-fun 'lazy-org-config-save
                          :insert-fun 'lazy-org-config-insert
                          :test-fun (lambda (ca)
                                      (assoc 'org-file ca)))

     (add-hook 'lazy-before-load-hook (lambda ()
                                        (unless (lazy-get-config-val 'org-file lazy-name)
                                          (let ((org-file (concat (lazy-get-config-val 'basedir lazy-name) "/" lazy-name ".org")))
                                            (when (file-exists-p org-file)
                                              (lazy-org-search-projects org-file))))))
     (add-hook 'lazy-after-load-hook (lambda ()
                                       (when (lazy-get-config-val 'org-file)
                                         (lazy-clock-in))))
     (add-hook 'lazy-before-unload-hook (lambda ()
                                          (when (lazy-get-config-val 'org-file)
                                            (lazy-clock-out))))
     (add-hook 'after-save-hook 'lazy-org-after-save-search)))

(provide 'lazy-orgmode)
