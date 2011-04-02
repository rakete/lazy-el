(require 'mk-project)

;; (defconst mk-org-project-properties '(("MKP-NAME" . name)
;;                                       ("MKP-BASEDIR" . basedir)
;;                                       ("MKP-SRC-PATTERNS" . patterns)
;;                                       ("MKP-IGNORE-PATTERNS" . ignore-patterns)
;;                                       ("MKP-ACK-ARGS" . ack-args)
;;                                       ("MKP-VCS" . vcs)
;;                                       ("MKP-TAGS-FILE" . tags-file)
;;                                       ("MKP-COMPILE-CMD" . compile-cmd)
;;                                       ("MKP-FILE-LIST-CACHE" . file-list-cache)
;;                                       ("MKP-OPEN-FILES-CACHE" . open-files-cache)
;;                                       ("MKP-SRC-FIND-CMD" . src-find-cmd)
;;                                       ("MKP-GREP-FIND-CMD" . grep-find-cmd)
;;                                       ("MKP-INDEX-FIND-CMD" . index-find-cmd)
;;                                       ("MKP-ETAGS-CMD" . etags-cmd)
;;                                       ("MKP-PATTERNS-ARE-REGEX" . patterns-are-regex)))

(defconst mk-org-task-properties '(("MKP-TODO-CREATED-BY-MK" . todo-created-by-mk)
                                   ("MKP-TODO-OPEN-FILES" . todo-open-files)
                                   ("MKP-TODO-VIEW-THIS-FILE" . todo-view-this-file)))

(defun mk-org-gen-project-properties ()
  (let* ((proj-vars (append mk-proj-required-vars mk-proj-optional-vars))
         (props '()))
    (dolist (var proj-vars props)
      (add-to-list 'props `(,(concat "MKP-" (upcase (symbol-name var))) . ,var)))))

(defmacro mk-org-do-all-projects (_file &rest body)
  (` (block "mk-org-do-all-projects"
       (let* ((org-startup-folded nil)
              (org-startup-align-all-tables nil)
              (buffer (if (file-exists-p (, _file))
                          (org-get-agenda-file-buffer (, _file))
                        (error (, "No such file %s") (, _file)))))
         (if (not buffer)
             (list (format "MK-PROJECT: No such org-file %s" (, _file)))
           (with-current-buffer buffer
             (unless (org-mode-p)
               (error "File %s is not in `org-mode'" (, _file)))
             (let ((case-fold-search nil))
               (save-excursion
                 (save-restriction
                   (widen)
                   (goto-char (point-min))
                   (let ((re "^[ \t]*:MKP-NAME:[ \t]*\\(\\S-.*\\)"))
                     (while (re-search-forward re nil t)
                       (let ((project-name (org-match-string-no-properties 1)))
                         (save-excursion
                           (save-restriction
                             (outline-previous-heading)
                             (,@ body)))))))))))))))

(defmacro mk-org-do-single-project (name _file &rest body)
  (,@ body))

(defun mk-org-get-project-properties (file name)
  (mk-org-do-all-projects file
                   (if (string-equal project-name name)
                       (return-from "mk-org-do-all-projects" (org-entry-properties)))))

(defun mk-org-get-project-todos (file name))

(defun mk-org-define-projects ()
  (progn
    (dolist (file mk-org-project-files)
      (mk-org-do-all-projects file
                       (let* ((entry-properties (org-entry-properties))
                              (ks '())
                              (props (dolist (p entry-properties ks) (push (car p) ks)))
                              (project-config '()))
                         (progn
                           (dolist (propname props project-config)
                             (let ((sym (assoc propname (mk-org-gen-project-properties))))
                               (if sym
                                   (let* ((config-propname (car sym))
                                          (config-item (cdr sym))
                                          (item-value (cdr (assoc config-propname entry-properties))))
                                     (add-to-list 'project-config `(,config-item ,item-value))))))
                           (project-def project-name project-config t)))))))

(defun mk-org-clock-in-org-hook ())
(defun mk-org-clock-out-org-hook ())

(defun project-undef (name)
  (remhash name mk-proj-list))

(defun project-create-todo ())

(defun project-todos ())

(defvar mk-org-project-files '("/home/lazor/org/projects.org"))


