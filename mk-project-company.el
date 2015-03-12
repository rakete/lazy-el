(require 'mk-project)
(require 'company)
(require 'cl-lib)

(defgroup company-project nil
  "Completion back-end for Mk Project."
  :group 'company)

(defvar mk-company-complete-in-projects t)

(defvar mk-company-project-name nil)

(defun mk-company-gtags (prefix)
  (let* ((cmd (concat "global --match-part=first -Gq -c \"" prefix "\""))
         (completions (split-string (condition-case nil (shell-command-to-string cmd) (error nil)) "\n" t)))
    completions))

(defun mk-company-imenu (prefix)
  (let* ((imenu-alist (condition-case nil
                          (if (functionp 'imenu--make-many-index-alist)
                              (imenu--make-many-index-alist)
                            (imenu--make-index-alist))
                        (error nil)))
         (marker-list (append (cdr (assoc "Types" imenu-alist))
                              (cdr (assoc "Variables" imenu-alist))
                              (nthcdr 3 imenu-alist))))
    (let ((case-fold-search nil))
      (remove-duplicates
       (loop for tuple in marker-list
             if (or (string-match (concat "[^ (]* \\(" prefix "[^ ]*\\)[ ]*(" ) (car tuple))
                    (string-match (concat "^\\(" prefix "[^ ]*\\)") (car tuple)))
             collect (match-string 1 (car tuple)))
       :test 'equal))))

(defun mk-company-obarray (prefix)
  (when (derived-mode-p 'emacs-lisp-mode 'inferior-emacs-lisp-mode)
    (let (results)
      (do-all-symbols (sym results)
        (when (or (fboundp sym)
                  (boundp sym))
          (let* ((completion (symbol-name sym))
                 (case-fold-search nil))
            (when (string-match (concat "^" prefix) completion)
              (push completion results))))))))

(defun mk-company-dabbrev-candidates (arg)
  (if mk-company-project-name
      (let ((default-case-fold-search company-dabbrev-code-ignore-case)
            (project-completions (gethash mk-company-project-name mk-proj-completions-cache))
            (dabbrevs (remove-duplicates (company-dabbrev-code 'candidates arg) :test 'equal)))
        (loop for dab in dabbrevs
              if (gethash dab project-completions t)
              collect dab))
    (company-dabbrev-code 'candidates arg)))

;;;###autoload
(defun company-project-runtime (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-project))
    (no-cache nil)
    (sorted nil)
    (duplicates nil)
    (prefix (and (not (company-in-string-or-comment))
                 (buffer-file-name (current-buffer))
                 (or mk-proj-name
                     mk-company-project-name)
                 (or (eq mk-company-complete-in-projects t)
                     (find (or mk-proj-name
                               mk-company-project-name) mk-company-complete-in-projects))
                 (company-grab-symbol)))
    (candidates (append (mk-company-gtags arg)
                        (mk-company-imenu arg)
                        (when (find 'elisp (mk-proj-src-pattern-languages (mk-proj-get-config-val 'src-patterns mk-company-project-name)))
                          (mk-company-obarray arg))
                        (mk-company-dabbrev-candidates arg)))
    (meta (let* ((cache (gethash arg mk-proj-definitions-cache))
                 (definition (plist-get cache :definition))
                 (docstring (plist-get cache :docstring))
                 (meta nil))
            (when docstring
              (push docstring meta))
            (when definition
              (push definition meta))
            (when meta
              (mapconcat 'identity meta ": "))))
    ;;(location (mk-company-get-jump arg 'location))
    (init (setq-local mk-company-project-name (or mk-company-project-name (cadr (assoc 'name (mk-proj-guess-alist))))))))

;;;###autoload
(defun company-project-cached (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-project))
    (no-cache nil)
    (sorted nil)
    (duplicates nil)
    (prefix (and (not (company-in-string-or-comment))
                 (buffer-file-name (current-buffer))
                 (or mk-company-project-name
                     mk-proj-name)
                 (or (eq mk-company-complete-in-projects t)
                     (find (or mk-proj-name
                               mk-company-project-name) mk-company-complete-in-projects))
                 (company-grab-symbol)))
    (candidates (append (mk-proj-completions arg mk-company-project-name)
                        (mk-company-dabbrev-candidates arg)))
    (meta (let* ((cache (gethash arg mk-proj-definitions-cache))
                 (definition (plist-get cache :definition))
                 (docstring (plist-get cache :docstring))
                 (meta nil))
            (when docstring
              (push docstring meta))
            (when definition
              (push definition meta))
            (when meta
              (mapconcat 'identity meta ": "))))
    ;;(location (mk-company-get-jump arg 'location))
    (init (setq-local mk-company-project-name (or mk-company-project-name (cadr (assoc 'name (mk-proj-guess-alist))))))))

(provide 'mk-project-company)

;; mk-project-company.el ends here
