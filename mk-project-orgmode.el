;;; mk-project-orgmode.el ---  Org-Mode integration for mk-project

;; Copyright (C) 2011  Andreas Raster <lazor at affenbande dot org>
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

(require 'org-install)
(require 'org-protocol)
(require 'org-agenda)
(require 'org-depend)
(require 'ob)

(defvar mk-org-project-files nil
  "List of .org files that mk-org searches for project definitions.")

(defvar mk-org-todo-keywords '("TODO" "ACTIVE" "PLAN" "BUG" "WAITING")
  "Todo keywords that mk-org will recognize as tasks. Every org entry that
does not have any of those keywords as todo will not be visited by `mk-org-map-entries'")

(defvar mk-org-active-todo-keyword "ACTIVE"
  "Active keyword is a special keyword that subtrees can have to specify a task
that has lots of other tasks as children but is not a complete project itself.

So far this is only used when creating project-todos, to put the todo under
the active subtree, instead of the parent subtree.")

(defvar mk-org-default-todo-keyword "TODO"
  "Not used yet. I wanted to have a default todo that gets set when adding a project-todo.")

(defvar mk-proj-org-file nil
  "The current projects .org file.")

(defvar mk-proj-org-marker nil
  "Marker for the current projects org headline." )

(defvar mk-proj-org-level nil
  "The number of stars in front of the projects headline.")

(eval-after-load "mk-project-orgmode"
  '(progn
     (add-to-list 'mk-proj-optional-vars 'org-file)
     (add-to-list 'mk-proj-optional-vars 'org-marker)
     (add-to-list 'mk-proj-optional-vars 'org-level)

     (add-hook 'org-clock-in-hook (lambda ()
                                    (when (mk-org-entry-is-in-project-p)
                                      (mk-proj-load (mk-org-entry-name)))))

     ;; (add-hook 'org-clock-out-hook (lambda ()
     ;;                                 (when (string-equal (mk-org-entry-name) mk-proj-name)
     ;;                                   (if (mk-org-entry-parent-point)
     ;;                                       (progn
     ;;                                         (goto-char (mk-org-entry-parent-point))
     ;;                                         (org-clock-in))
     ;;                                     (project-unload t)))))
     ))








(defun mk-org-files-containing-projects ()
  "Searches all defined projects and returns list of all .org files
in which projects have been defined as well as the files specified by
`mk-org-project-files'"
  (let ((org-files '()))
    (maphash (lambda (k p)
               (when (cdr (assoc 'org-file p))
                 (setq org-files (append (cdr (assoc 'org-file p)) org-files))))
             mk-proj-list)
    (remove-if (lambda (x) (eq x nil)) (remove-duplicates (append org-files mk-org-project-files) :test #'string-equal))))


(defun mk-org-symbol-table (&optional symbol)
  "Creates an alist of (symbol . org-property-string) that can be used
to look up the org property which represents a mk-project config symbol.

Optional argument SYMBOL can be specified if you just want to look up
a single symbol and don't need the whole alist."
  (let* ((proj-vars (append mk-proj-required-vars mk-proj-optional-vars))
         (props '())
         (table (dolist (var proj-vars props)
                  (add-to-list 'props `(,var . ,(concat "MKP_" (replace-regexp-in-string "-" "_" (upcase (symbol-name var)))))))))
    (if symbol
        (cdr (assoc symbol table))
      table)))

(defun mk-org-assert-org (&optional name)
  "Same as `mk-proj-assert-proj' but makes sure that the project has
an associated org file.

Optionally NAME can be specified to test a specific project other
than the current one."
  (mk-proj-assert-proj)
  (if (eq name nil)
      (unless mk-proj-org-file
        (error (format "mk-org: Project %s has no associated org file!" mk-proj-name)))
    (unless (mk-proj-config-val 'org-file name)
      (error (format "mk-org: Project %s has no associated org file!" mk-proj-name)))))

(defun mk-org-forward-same-level ()
  (interactive)
  (let ((p (save-excursion
             (beginning-of-line)
             (point))))
    (condition-case nil (outline-forward-same-level 1) (error nil))
    (when (= (point) p)
      (let ((l (save-excursion
                 (beginning-of-line)
                 (org-outline-level))))
        (outline-next-heading)
        (while (and (> (org-outline-level) l)
                    (not (eobp)))
          (outline-next-heading))))))











(defun* mk-org-map-entries (&key function
                                 (file nil)
                                 (match nil)
                                 (scope 'project-tree)
                                 (widen nil))
  ""
  (let ((results '())
        (next-point nil))
    (dolist (project-file (cond ((and (markerp match) (marker-buffer match))
                                 `(,(marker-buffer match)))
                                ((mk-proj-sourcemarker-p match)
                                 `(,(marker-buffer (mk-proj-sourcemarker-restore match))))
                                ((or (buffer-live-p file) (and (char-or-string-p file) (file-exists-p file)))
                                 `(,file))
                                ((and file (listp file))
                                 file)
                                ((or (eq file nil) (eq file 'current-file))
                                 (progn (mk-org-assert-org) `(,mk-proj-org-file)))
                                (t
                                 (mk-org-files-containing-projects))))
      (print (concat "file: " (prin1-to-string project-file)))
      (with-current-buffer (or (when (buffer-live-p project-file) project-file)
                               (org-find-base-buffer-visiting project-file)
                               (if (file-exists-p project-file)
                                   (find-file-noselect project-file)
                                 (error (format "mk-org: No such file %s" project-file))))
        (save-excursion
          (save-restriction
            (when widen (widen))
            (goto-char (point-min))
            (let* ((project-file (if (buffer-live-p project-file)
                                     (buffer-file-name project-file)
                                   project-file))
                   (re (cond ((and (markerp match) (marker-position match))
                              (marker-position match))
                             ((mk-proj-sourcemarker-p match)
                              (marker-position (mk-proj-sourcemarker-restore match)))
                             ((numberp match)
                              match)
                             ((and match (listp match) (eq (car match) 're))
                              (cadr match))
                             ((and match (listp match) (eq (car match) 'property) (= (list-length match) 3))
                              (concat "^[ \t]*:" (second match) ":[ \t]*\\(" (third match) "\\)"))
                             ((and match (listp match) (functionp (car match)))
                              (funcall (car match) (cadr match)))
                             ((stringp match)
                              (concat "^[ \t]*:MKP_NAME:[ \t]*\\(\"" match "\"\\)"))
                             (t
                              "^[ \t]*:MKP_NAME:[ \t]*\\(.*\\)")))
                   (case-fold-search nil)
                   (skip-project-functions nil))
              (print (concat "re: " (prin1-to-string re)))
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
                  (let* ((entry-name (save-excursion
                                       (org-back-to-heading t)
                                       (beginning-of-line)
                                       (mk-org-entry-name)))
                         (entry-level (save-excursion
                                        (org-back-to-heading t)
                                        (beginning-of-line)
                                        (mk-org-entry-level)))
                         (entry-point (save-excursion
                                        (org-back-to-heading t)
                                        (beginning-of-line)
                                        (point)))
                         (parent-name (save-excursion
                                        (org-back-to-heading t)
                                        (beginning-of-line)
                                        (mk-org-entry-parent-name)))
                         (parent-level (save-excursion
                                         (org-back-to-heading t)
                                         (beginning-of-line)
                                         (mk-org-entry-parent-level)))
                         (parent-point (save-excursion
                                         (org-back-to-heading t)
                                         (beginning-of-line)
                                         (mk-org-entry-parent-point)))
                         (f-closure (lambda ()
                                      (let ((r nil)
                                            (entry-name (read (or (org-entry-get (point) (mk-org-symbol-table 'name))
                                                                  (concat "\"" parent-name ":" (mk-org-entry-headline) "\"")
                                                                  "nil")))
                                            (entry-level (save-excursion
                                                           (beginning-of-line)
                                                           (org-outline-level)))
                                            (entry-point (point)))
                                        (setq r (save-excursion
                                                  (org-back-to-heading t)
                                                  (beginning-of-line)
                                                  (funcall function))
                                              results (append results `(,r)))
                                        r)))
                         (project-recursion (lambda ()
                                              (funcall f-closure)
                                              (while (and (progn
                                                            (outline-next-heading)
                                                            (> (org-outline-level) parent-level))
                                                          (not (eobp)))
                                                (cond ((and (eq scope 'project-tree)
                                                            (org-entry-get (point) (mk-org-symbol-table 'name)))
                                                       (let* ((parent-name entry-name)
                                                              (parent-level entry-level)
                                                              (parent-point entry-point)
                                                              (entry-name (read (or (org-entry-get (point) (mk-org-symbol-table 'name))
                                                                                    "nil")))
                                                              (entry-level (save-excursion
                                                                             (beginning-of-line)
                                                                             (org-outline-level)))
                                                              (entry-point (point))
                                                              )
                                                         (setq skip-project-functions (append skip-project-functions
                                                                                              `((lambda ()
                                                                                                  (string-equal (read (or (org-entry-get (point) (mk-org-symbol-table 'name))
                                                                                                                          "nil"))
                                                                                                                ,(read (or (org-entry-get (point) (mk-org-symbol-table 'name))
                                                                                                                           "nil")))))))
                                                         (funcall project-recursion)))
                                                      ((and (eq scope 'project-single)
                                                            (org-entry-get (point) (mk-org-symbol-table 'name)))
                                                       (progn
                                                         (mk-org-forward-same-level)))
                                                      (t
                                                       (let ((parent-name entry-name)
                                                             (parent-level entry-level)
                                                             (parent-point entry-point)
                                                             )
                                                         (funcall f-closure))))))))
                    (cond ((eq scope 'project-headline)
                           (save-excursion
                             (org-back-to-heading t)
                             (funcall f-closure)))
                          ((or (eq scope 'project-tree)
                               (eq scope 'project-single))
                           (funcall project-recursion))
                          (t
                           (org-map-entries f-closure nil scope nil nil)))))))))))
    results))


(defun test-mk-org-map-entries ()
  (interactive)
  (mk-org-map-entries
   :file (mk-org-files-containing-projects)
   :function (lambda ()
               (message (format "%s -> %s -> %s" (read (prin1-to-string parent-name)) project-name entry-name))
               )))

(defun mk-org-entry-map-ancestry (&key function
                                       (only-projects nil)))








(defmacro with-or-without-marker (marker &rest body)
  `(let ((marker ,marker))
     (if (markerp marker)
         (with-current-buffer (marker-buffer marker)
           (save-excursion
             (goto-char (marker-position marker))
             ,@body))
       ,@body)))

(defun mk-org-entry-name (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (if (and (boundp 'entry-name)
            (eq (point) entry-point))
       entry-name
     (and (org-entry-get (point) (mk-org-symbol-table 'name) t)
          (or (read (or (org-entry-get (point) (mk-org-symbol-table 'name)) "nil"))
              (concat (read (org-entry-get (point) (mk-org-symbol-table 'name) t)) ":" (mk-org-entry-headline)))))))

(defun mk-org-entry-marker (&optional marker)
  (interactive)
  (if (markerp marker)
      marker
    (save-excursion
      (beginning-of-line)
      (when (looking-at org-complex-heading-regexp)
        (copy-marker (point))))))

(defun mk-org-entry-level (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (if (and (boundp 'entry-level)
            (numberp entry-level)
            (eq (point) entry-point))
       entry-level
     (save-excursion
       (if (not (looking-at org-complex-heading-regexp)) 0
         (beginning-of-line)
         (org-outline-level))))))

(defun mk-org-entry-parent-level (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (if (and (boundp 'parent-level)
            (numberp parent-level)
            (eq (point) entry-point))
       parent-level
     (save-excursion
       (let ((p (mk-org-entry-parent-point)))
         (if (eq p nil) (org-outline-level)
           (goto-char p)
           (beginning-of-line)
           (org-outline-level)))))))

(defun mk-org-entry-parent-name (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (if (and (boundp 'parent-name)
            (eq (point) entry-point))
       parent-name
     (save-excursion
       (let ((p (mk-org-entry-parent-point)))
         (if (eq p nil) nil
           (goto-char p)
           (read (org-entry-get (point) (mk-org-symbol-table 'name)))))))))

(defun mk-org-entry-parent-point (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (if (and (boundp 'parent-point)
            (eq (point) entry-point))
       parent-point
     (save-excursion
       (org-back-to-heading t)
       (let ((p nil)
             (n (org-entry-get (point) (mk-org-symbol-table 'name) t))
             (cont nil)
             (stop nil)
             (project-line (mk-org-entry-is-project-p)))
         (if project-line
             (setq cont (org-up-heading-safe)
                   n (org-entry-get (point) (mk-org-symbol-table 'name) t))
           (setq cont t))
         (when (and n cont)
           (while (and (cond (stop
                              nil)
                             ((bobp)
                              (setq stop t))
                             ;; (arg
                             ;;  (org-entry-get (point) (mk-org-symbol-table 'name) t))
                             (t
                              (string-equal (or (org-entry-get (point) (mk-org-symbol-table 'name) t) "nil") n))))
             (setq p (point))
             (unless stop
               (org-up-heading-safe)
               (setq project-line (mk-org-entry-is-project-p)))))
         p)))))

(defun mk-org-entry-nearest-active (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (if mk-org-active-todo-keyword
       (save-excursion
         (org-back-to-heading t)
         (if (mk-org-entry-is-project-p)
             nil
           (while (and (not (string-equal (org-get-todo-state) mk-org-active-todo-keyword))
                       (not (mk-org-entry-is-project-p)))

             (org-up-heading-all 1)))
         (point))
     (mk-org-entry-parent-point))))

(defun mk-org-entry-headline (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (save-excursion
     (org-back-to-heading t)
     (beginning-of-line)
     (and (looking-at org-complex-heading-regexp)
          (let* ((headline-raw (org-match-string-no-properties 4)))
            (cond ((string-match org-bracket-link-regexp headline-raw)
                   (match-string 2 headline-raw))
                  ((string-match "^\\(.*\\)\s+\\[\\([0-9][0-9]%\\)\\]$" headline-raw)
                   (match-string 1 headline-raw))
                  (t
                   headline-raw)))))))

(defun mk-org-entry-is-link-p (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (save-excursion
     (beginning-of-line)
     (and (looking-at org-complex-heading-regexp)e
          (let* ((headline-raw (org-match-string-no-properties 4)))
            (string-match org-bracket-link-regexp headline-raw))))))


(defun mk-org-entry-progress (&optional marker)
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

(defun mk-org-entry-is-project-p (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (when (save-excursion
           (beginning-of-line)
           (org-entry-get (point) (mk-org-symbol-table 'name)))
     t)))

(defun mk-org-entry-is-in-project-p (&optional marker)
  (interactive)
  (with-or-without-marker marker
                          (when (save-excursion
                                  (beginning-of-line)
                                  (org-entry-get (point) (mk-org-symbol-table 'name) t))
                            t)))

(defun mk-org-entry-define-project (&optional marker)
  "Define a project from the org entry at (point)."
  (interactive)
  (with-or-without-marker marker
   (save-excursion
     (org-back-to-heading t)
     (beginning-of-line)
     (unless (and (org-mode-p)
                  (looking-at org-complex-heading-regexp))
       (error "mk-org: buffer is not in org-mode or point is not a org heading"))
     (let* ((entry-properties (org-entry-properties))
            (ks '())
            (props (dolist (p entry-properties ks) (push (car p) ks)))
            (entry-config '()))
       (dolist (propname props entry-config)
         (let ((sym (rassoc propname (mk-org-symbol-table))))
           (if sym
               (let* ((config-propname (cdr sym))
                      (config-item (car sym))
                      (item-value (read (cdr (assoc config-propname entry-properties)))))
                 (add-to-list 'entry-config `(,config-item ,item-value))))))
       (setq entry-config (append entry-config `((org-file ,(or (buffer-file-name (current-buffer))
                                                                (buffer-file-name (buffer-base-buffer (current-buffer)))))
                                                 (org-marker ,(mk-org-entry-marker))
                                                 (org-level ,(- (mk-org-entry-level) (mk-org-entry-parent-level))))))
       (message "%s -> %s : %s" (mk-org-entry-name) (or (mk-org-entry-parent-name) t) (assoc 'org-file entry-config))
       (project-def (mk-org-entry-name) entry-config (or (mk-org-entry-parent-name) t))
       ;; (when (and mk-proj-name (string-equal (mk-org-entry-name) mk-proj-name))
       ;;   (message (format "org-project: loading %s" (mk-org-entry-name)))
       ;;   (mk-proj-load (mk-org-entry-name)))
       ))))

(defun mk-org-entry-undefine-project (&optional marker)
  (interactive)
  (with-or-without-marker marker
   (save-excursion
     (org-back-to-heading t)
     (beginning-of-line)
     (unless (and (org-mode-p)
                  (looking-at org-complex-heading-regexp))
       (error "mk-org: buffer is not in org-mode or point is not a org heading"))
     (project-undef (mk-org-entry-name)))))











(defun mk-org-project-buffer-name (&optional name)
  (let ((name (if name
                  name
                (progn
                  (mk-proj-assert-proj)
                  mk-proj-name))))
    (concat "mk-org: " (or (mk-proj-config-val 'parent name) name))))



(defun mk-org-create-project-buffer (&optional name)
  (unless name
    (mk-proj-assert-proj)
    (setq name mk-proj-name))
  (mk-org-assert-org name)
  (let* ((marker (mk-proj-config-val 'org-marker name))
         (buffername (mk-org-project-buffer-name name))
         (headline (mk-org-entry-headline marker))
         (org-show-hierarchy-above t)
         (org-show-following-heading nil)
         (org-show-siblings nil))
    (let (beg end buf)
      (car (mk-org-map-entries
            :match marker ;; `(eval (format org-complex-heading-regexp-format ,headline))
            :scope 'project-headline
            :function (lambda ()
                        (progn
                          (org-back-to-heading t)
                          (when (not (mk-org-entry-is-project-p))
                            (goto-char (mk-org-entry-parent-point)))
                          (setq beg (point))
                          (org-end-of-subtree t t)
                          (if (org-on-heading-p) (backward-char 1))
                          (setq end (point)
                                buf (or (get-buffer buffername)
                                        (make-indirect-buffer (current-buffer) buffername 'clone)))
                          (with-current-buffer buf
                            (narrow-to-region beg end)
                            (show-all)
                            (beginning-of-buffer)
                            (when (mk-org-entry-is-in-project-p) (org-cycle))
                            (mk-org-reveal)
                            (goto-char (marker-position marker))
                            (set-frame-name (mk-org-entry-headline))
                            (current-buffer)
                            ))))))))

(defun mk-org-reveal ()
  (interactive)
  ;;(mk-org-assert-org)
  (mk-org-map-entries
   :file (current-buffer)
   :match (point)
   :scope 'project-single
   :function (lambda ()
               (when (mk-proj-any (lambda (x) (string-equal (org-get-todo-state) x)) mk-org-todo-keywords)
                 (org-show-context)
                 (when (and mk-proj-name
                            (not (mk-org-entry-is-project-p))
                            (string-equal (mk-org-entry-name) mk-proj-name))
                   (org-show-entry))
                 ))))


(defun mk-org-get-project-buffer (&optional name)
  (or (get-buffer (mk-org-project-buffer-name name))
      (mk-org-create-project-buffer name)))

(defun project-org-buffer (&optional name)
  "Open current projects org tree as indirect buffer."
  (interactive)
  (display-buffer (mk-org-get-project-buffer name)))

;; (defun project-org-marker (&optional name)
;;   "Get current or NAME projects org entry as marker."
;;   (interactive)
;;   (unless name
;;     (mk-proj-assert-proj)
;;     (setq name mk-proj-name))
;;   (mk-org-assert-org name)
;;   (let ((org-file (mk-proj-config-val 'org-file name t))
;;         (org-headline (mk-proj-config-val 'org-headline name t))
;;         )
;;     (car (mk-org-map-entries
;;           :file (mk-org-get-project-buffer) name)
;;           :match `(eval (format org-complex-heading-regexp-format ,org-headline))
;;           :scope 'project-headline
;;           :function (lambda ()
;;                       (save-excursion
;;                         (org-back-to-heading t)
;;                         (beginning-of-line)
;;                         (point-marker)
;;                         )))))

(defun mk-org-yank-below (&optional arg)
  (interactive "P")
  (let (p)
    (save-excursion
      (save-restriction
        (org-save-outline-visibility t
          (when (looking-at org-complex-heading-regexp)
            (show-all)
            (let ((level (org-outline-level))
                  (adjust t))
              (outline-next-heading)
              (while (and (> (org-outline-level) level)
                          (not (eobp)))
                (outline-next-heading)
                (when adjust (setq adjust nil)))
              (outline-previous-heading)
              (org-end-of-subtree)
              (newline)
              (let ((org-yank-folded-subtrees t)
                    (org-yank-adjusted-subtrees t))
                (org-yank))
              (if (and arg adjust)
                  (progn
                    (outline-previous-heading)
                    (org-shiftmetaright))
                (outline-previous-heading))
              (setq p (point)))))))
    p))


(defvar mk-org-add-todo-mode-map (make-sparse-keymap))

(defvar mk-org-add-todo-mode-hook nil)

(define-minor-mode mk-org-add-todo-mode nil nil " AddTodo" mk-org-add-todo-mode-map
  (run-hooks 'mk-org-add-todo-mode-hook))

(define-key mk-org-add-todo-mode-map "\C-c\C-c" 'mk-org-add-todo-finalize)
(define-key mk-org-add-todo-mode-map "\C-c\C-k" 'mk-org-add-todo-abort)

(defun mk-org-add-todo-finalize (&optional arg)
  (interactive "P")
  (setq arg (or arg local-prefix-arg))
  (print arg)
  (mk-org-assert-org)
  (beginning-of-buffer)
  (outline-next-heading)
  (org-copy-subtree 1 t)
  (mk-org-map-entries
   :file (mk-org-get-project-buffer)
   :match mk-proj-org-marker ;;`(re ,(format org-complex-heading-regexp-format mk-proj-org-headline))
   :scope 'project-headline
   :function (lambda ()
               (let* ((active (mk-org-entry-nearest-active))
                      (parent (mk-org-entry-parent-point))
                      (org-show-hierarchy-above t)
                      (org-show-following-heading nil)
                      (org-show-siblings nil))
                 (if arg
                     (goto-char parent)
                   (goto-char active))
                 (save-excursion
                   (goto-char (mk-org-yank-below arg))
                   (print (concat "added: " (mk-org-entry-headline)))
                   (mk-org-entry-define-project)
                   (goto-char parent)
                   (mk-org-reveal)
                   ))))
  (kill-buffer))

(defun mk-org-add-todo-abort ()
  (interactive)
  (kill-buffer))


(defun project-add-todo (&optional arg)
  "Pop up a buffer where the user can create a new org entry to be added to the current
project. Will add it to the nearest entry that has a `mk-org-active-todo-keyword' or to the
parent entry. Adding to the parent can be forced with a prefix argument.

Eventually this could be changed so that the prefix argument asks the user under
which entry (all of them not just the parent or nearest active) to add the todo.

See also `mk-org-entry-nearest-active'."
  (interactive "P")
  (if (and (org-mode-p)
           (looking-at org-complex-heading-regexp)
           (not (gethash (mk-org-entry-name) mk-proj-list)))
      (mk-org-entry-define-project)
    (progn
      (mk-org-assert-org)
      (let* ((proj-b (current-buffer))
             (sm (mk-proj-sourcemarker-create))
             (buf (get-buffer-create "mk-org: add todo"))
             (window (display-buffer buf)))
        (select-window window)
        (set-window-dedicated-p window t)
        (org-mode)
        (mk-org-add-todo-mode 1)
        (with-current-buffer buf
          (set (make-local-variable 'local-prefix-arg) arg)
          (org-insert-heading)
          (when (mk-proj-buffer-p proj-b)
            (org-set-property "MKP_SOURCEMARKER" (concat "'" (prin1-to-string sm)))))))))

(defun project-remove-todo (&optional arg)
  (interactive "P")
  ;; (point) nicht auf org headline und kein arg -> undo letztes todo
  ;; arg gesetzt -> frag nach match und alles was matched
  ;; (point) auf headline und kein arg -> lösche project at (point)
  ;; alle children müssen auch gelöscht werden, nachfragen wären nicht schlecht
  (cond (arg
         (read-string "Match: "))
         )
        (t
         (message "hello interactive")))

(defun project-org-define (&optional arg)
  "Define a project. This should eventually ask the user a couple of questions about
the project, then insert either lisp code into the current buffer or create an org
entry. None of that is implemented so far, but you can use this function to define a
manually constructed project org entry at point (see also `mk-org-entry-define-project'."
  (interactive "P")
  (if (and (org-mode-p)
           (looking-at org-complex-heading-regexp))
      (mk-org-entry-define-project)
    (message "Defining projects via function not implemeted yet. You have to create the properties manually.")))

;; (defvar mk-org-edit-todo-mode-map (make-sparse-keymap))

;; (defvar mk-org-edit-todo-mode-hook nil)

;; (define-minor-mode mk-org-edit-todo-mode nil nil " EditTodo" mk-org-edit-todo-mode-map
;;   (run-hooks 'mk-org-edit-todo-mode-hook))

;; (define-key mk-org-edit-todo-mode-map "\C-c\C-c" 'mk-org-edit-todo-finalize)
;; (define-key mk-org-edit-todo-mode-map "\C-c\C-k" 'mk-org-edit-todo-abort)

;; (defun mk-org-edit-todo-finalize ()
;;   (interactive))

;; (defun project-edit-todo ())











(defun project-insert-org (name config-alist &optional headline)
  (interactive)
  (unless (org-mode-p)
    (error "mk-org: current buffer not in org-mode"))
  (save-excursion
    (save-restriction
      (org-insert-heading-after-current)
      (if headline
          (insert headline)
        (insert name))
      (org-set-property "MKP_NAME" name)
      (loop for v in config-alist
            do (let* ((prop (cdr (assoc (car v) (mk-org-symbol-table))))
                      (val (cdr v)))
                 (org-set-property prop val))))))

(defun mk-org-read (cell)
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


(defun mk-org-define-projects ()
  (interactive)
  (unless mk-org-project-files
    (error "mk-org: could not find any projects, mk-org-project-files is not set"))
  (progn
    (mk-org-map-entries
     :file (mk-org-files-containing-projects)
     ;;:match "mk-project"
     :scope 'project-tree
     :function (lambda ()
                 (when (and (not (mk-org-entry-is-link-p))
                            (or (and (org-get-todo-state) (mk-proj-any (lambda (x) (string-equal (org-get-todo-state) x)) mk-org-todo-keywords))
                                     (mk-org-entry-is-project-p)))
                   (mk-org-entry-define-project))))))

(defun mk-org-undefine-entries ()
  (interactive)
  (mk-org-map-entries
   :file (mk-org-files-containing-projects)
   :scope 'project-tree
   :function (lambda ()
               (when (not (mk-org-entry-is-project-p))
                 (project-undef entry-name)))))


(provide 'mk-project-orgmode)

