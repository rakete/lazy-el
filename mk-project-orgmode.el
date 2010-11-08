
(defconst mk-org-project-properties '(("MK-PROJECT-NAME" . name)
                                      ("MK-PROJECT-BASEDIR" . basedir)
                                      ("MK-PROJECT-SRC-PATTERNS" . patterns)
                                      ("MK-PROJECT-IGNORE-PATTERNS" . ignore-patterns)
                                      ("MK-PROJECT-ACK-ARGS" . ack-args)
                                      ("MK-PROJECT-VCS" . vcs)
                                      ("MK-PROJECT-TAGS-FILE" . tags-file)
                                      ("MK-PROJECT-COMPILE-CMD" . compile-cmd)
                                      ("MK-PROJECT-FILE-LIST-CACHE" . file-list-cache)
                                      ("MK-PROJECT-OPEN-FILES-CACHE" . open-files-cache)
                                      ("MK-PROJECT-SRC-FIND-CMD" . src-find-cmd)
                                      ("MK-PROJECT-GREP-FIND-CMD" . grep-find-cmd)
                                      ("MK-PROJECT-INDEX-FIND-CMD" . index-find-cmd)
                                      ("MK-PROJECT-ETAGS-CMD" . etags-cmd)
                                      ("MK-PROJECT-PATTERNS-ARE-REGEX" . patterns-are-regex)))

(defvar mk-org-project-files '("/home/lazor/org/projects.org"))

(defmacro do-org-projects (_file &rest body)
  (` (block "do-org-projects"
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
                   (let ((re "^[ \t]*:MK-PROJECT-NAME:[ \t]*\\(\\S-.*\\)"))
                     (while (re-search-forward re nil t)
                       (let ((project-name (org-match-string-no-properties 1)))
                         (save-excursion
                           (save-restriction
                             (outline-previous-heading)
                             (,@ body)))))))))))))))

(defun get-project-properties (file name)
  (do-org-projects file
                   (if (string-equal project-name name)
                       (return-from "do-org-projects" (org-entry-properties)))))

(defun mk-org-define-projects ()
  (let (all-configs '())
    (progn
      (dolist (file mk-org-project-files all-configs)
        (do-org-projects file
                         (let* ((entry-properties (org-entry-properties))
                                (ks '())
                                (props (dolist (p entry-properties ks) (push (car p) ks)))
                                (project-config '()))
                           (progn
                             (dolist (propname props project-config)
                               (let ((cs (assoc propname mk-org-project-properties)))
                                 (if cs
                                     (let* ((config-propname (car cs))
                                            (config-item (cdr cs))
                                            (item-value (cdr (assoc config-propname entry-properties))))
                                       (push `(,config-item . ,item-value) project-config)))))
                             (push `(,project-name . ,project-config) all-configs)))))
      (dolist (project-config all-configs)
        (let* ((project-name (car project-config))
               (project-alist (cdr project-config)))
          (project-def project-name project-alist))))))

(defun project-undef (name)
  (remhash name mk-proj-list))

(project-undef "BeispielProjekt")