(require 'mk-project)
(require 'company)
(require 'cl-lib)

(defgroup company-project nil
  "Completion back-end for Mk Project."
  :group 'company)

(defvar mk-company-complete-in-projects t)

(defvar mk-company-project-name nil)

(defvar mk-company-history (make-hash-table :test 'equal))
(defvar mk-company-history-length 20)

(defun mk-company-add-history (proj-name)
  (let ((completion (substring-no-properties (nth company-selection company-candidates))))
    (let ((history (gethash proj-name mk-company-history)))
      (setq history (remove-if (lambda (last) (string-equal last completion)) history))
      (when (eq (length history) mk-company-history-length)
        (setq history (butlast history 1)))
      (push completion history)
      (puthash proj-name history mk-company-history))))

(defun mk-company-history-candidates (prefix proj-name)
  (remove-if-not 'identity (mapcar (lambda (last) (when (string-match (concat "^" prefix) last) last))
                                   (gethash proj-name mk-company-history))))

(defun mk-company-gtags-candidates (prefix)
  (let* ((cmd (concat "global --match-part=first -Gq -c \"" prefix "\""))
         (completions (split-string (condition-case nil (shell-command-to-string cmd) (error nil)) "\n" t)))
    completions))

(defun mk-company-imenu-candidates (prefix)
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

(defun mk-company-obarray-candidates (prefix)
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


(defun company-project-history (command &optional arg &rest ignored)
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
    (candidates (mk-company-history-candidates arg mk-company-project-name))
    (meta (mk-eldoc-function-meta arg mk-company-project-name (mk-proj-find-alist mk-company-project-name)))
    ;;doc
    ;;location
    (post-completion (mk-company-add-history mk-company-project-name))
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
    (meta (mk-eldoc-function-meta arg mk-company-project-name (mk-proj-find-alist mk-company-project-name)))
    ;;doc
    ;;location
    (post-completion (mk-company-add-history mk-company-project-name))
    (init (setq-local mk-company-project-name (or mk-company-project-name (cadr (assoc 'name (mk-proj-guess-alist))))))))

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
    (candidates (append (mk-company-gtags-candidates arg)
                        (mk-company-imenu-candidates arg)
                        (when (find 'elisp (mk-proj-src-pattern-languages (mk-proj-get-config-val 'src-patterns mk-company-project-name)))
                          (mk-company-obarray-candidates arg))
                        (mk-company-dabbrev-candidates arg)))
    (meta (mk-eldoc-function-meta arg mk-company-project-name (mk-proj-find-alist mk-company-project-name)))
    ;;doc
    ;;location
    (post-completion (mk-company-add-history mk-company-project-name))
    (init (setq-local mk-company-project-name (or mk-company-project-name (cadr (assoc 'name (mk-proj-guess-alist))))))))

(defun mk-company-transform-history-to-front (candidates)
  (if (functionp company-backend)
      candidates
    (let ((low-priority (cdr (memq :with company-backend))))
      (if (null low-priority)
          candidates
        (let ((history (gethash mk-company-project-name mk-company-history))
              (history-candidates)
              (other-candidates))
          (dolist (c (reverse candidates))
            (if (memq c history)
                (push c history-candidates)
              (push c other-candidates)
              ))
          (setq history-candidates (sort history-candidates (lambda (a b) (< (position a history :test 'equal) (position b history :test 'equal)))))
          (append history-candidates other-candidates))))))

(provide 'mk-project-company)

;; mk-project-company.el ends here
