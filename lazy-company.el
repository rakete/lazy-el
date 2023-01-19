(require 'lazy)
(require 'lazy-eldoc)

(require 'company)
(require 'company-dabbrev-code)
(require 'cl-lib)

(defgroup company-project nil
  "Completion back-end for Mk Project."
  :group 'company)

(defvar lazy-company-complete-in-projects t)

(defvar lazy-company-project-name nil)

(defun lazy-company-gtags-candidates (prefix)
  (let* ((cmd (concat "global --match-part=first -Gq -c \"" prefix "\""))
         (completions (split-string (condition-case nil (shell-command-to-string cmd) (error nil)) "\n" t)))
    completions))

(defun lazy-company-imenu-candidates (prefix)
  (let* ((imenu-alist (condition-case nil
                          (if (functionp 'imenu--make-many-index-alist)
                              (imenu--make-many-index-alist)
                            (imenu--make-index-alist))
                        (error nil)))
         (marker-list (append (cdr (assoc "Types" imenu-alist))
                              (cdr (assoc "Variables" imenu-alist))
                              (nthcdr 3 imenu-alist))))
    (let ((case-fold-search nil))
      (cl-remove-duplicates
       (cl-loop for tuple in marker-list
             if (or (string-match (concat "[^ (]* \\(" prefix "[^ ]*\\)[ ]*(" ) (car tuple))
                    (string-match (concat "^\\(" prefix "[^ ]*\\)") (car tuple)))
             collect (match-string 1 (car tuple)))
       :test 'equal))))

(defun lazy-company-obarray-candidates (prefix)
  (when (derived-mode-p 'emacs-lisp-mode 'inferior-emacs-lisp-mode)
    (lazy-completions-for-elisp prefix)))

;;;###autoload
(defun lazy-company-project-runtime (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-project))
    (no-cache nil)
    (sorted nil)
    (duplicates nil)
    (prefix (and (not (company-in-string-or-comment))
                 (buffer-file-name (current-buffer))
                 (or lazy-name
                     lazy-company-project-name)
                 (or (eq lazy-company-complete-in-projects t)
                     (cl-find (or lazy-name
                                  lazy-company-project-name)
                              lazy-company-complete-in-projects))
                 (company-grab-symbol)))
    (candidates (append (lazy-company-gtags-candidates arg)
                        (lazy-company-imenu-candidates arg)
                        (when (cl-find 'elisp (lazy-src-pattern-languages (lazy-get-config-val 'src-patterns lazy-company-project-name)))
                          (lazy-company-obarray-candidates arg))))
    (meta (lazy-eldoc-function-meta arg lazy-company-project-name (lazy-find-alist lazy-company-project-name)))
    ;;doc
    ;;location
    (init (setq-local lazy-company-project-name (or lazy-company-project-name (cadr (assoc 'name (lazy-guess-alist))))))))

(provide 'lazy-company)

;; mk-project-company.el ends here
