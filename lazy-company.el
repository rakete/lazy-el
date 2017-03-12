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

(defvar lazy-company-history (make-hash-table :test 'equal))
(defvar lazy-company-history-length 20)

(defun lazy-company-add-history (proj-name)
  (let ((completion (substring-no-properties (nth company-selection company-candidates))))
    (let ((history (gethash proj-name lazy-company-history)))
      (setq history (cl-remove-if (lambda (last) (string-equal last completion)) history))
      (when (eq (length history) lazy-company-history-length)
        (setq history (butlast history 1)))
      (push completion history)
      (puthash proj-name history lazy-company-history))))

(defun lazy-company-history-candidates (prefix proj-name)
  (cl-remove-if-not 'identity (mapcar (lambda (last) (when (string-match (concat "^" prefix) last) last))
                                   (gethash proj-name lazy-company-history))))

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
       (loop for tuple in marker-list
             if (or (string-match (concat "[^ (]* \\(" prefix "[^ ]*\\)[ ]*(" ) (car tuple))
                    (string-match (concat "^\\(" prefix "[^ ]*\\)") (car tuple)))
             collect (match-string 1 (car tuple)))
       :test 'equal))))

(defun lazy-company-obarray-candidates (prefix)
  (when (derived-mode-p 'emacs-lisp-mode 'inferior-emacs-lisp-mode)
    (let (results)
      (do-all-symbols (sym results)
        (when (or (fboundp sym)
                  (boundp sym))
          (let* ((completion (symbol-name sym))
                 (case-fold-search nil))
            (when (string-match (concat "^" prefix) completion)
              (push completion results))))))))

(defun lazy-company-dabbrev-candidates (arg)
  (if lazy-company-project-name
      (let ((case-fold-search company-dabbrev-code-ignore-case)
            (project-completions (gethash lazy-company-project-name lazy-completions-cache))
            (dabbrevs (cl-remove-duplicates (company-dabbrev-code 'candidates arg) :test 'equal)))
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
                 (or lazy-name
                     lazy-company-project-name)
                 (or (eq lazy-company-complete-in-projects t)
                     (cl-find (or lazy-name
                               lazy-company-project-name) lazy-company-complete-in-projects))
                 (company-grab-symbol)))
    (candidates (lazy-company-history-candidates arg lazy-company-project-name))
    (meta (lazy-eldoc-function-meta arg lazy-company-project-name (lazy-find-alist lazy-company-project-name)))
    ;;doc
    ;;location
    (post-completion (lazy-company-add-history lazy-company-project-name))
    (init (setq-local lazy-company-project-name (or lazy-company-project-name (cadr (assoc 'name (lazy-guess-alist))))))))

;;;###autoload
(defun lazy-company-project-cached (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-project))
    (no-cache nil)
    (sorted nil)
    (duplicates nil)
    (prefix (and (not (company-in-string-or-comment))
                 (buffer-file-name (current-buffer))
                 (or lazy-company-project-name
                     lazy-name)
                 (or (eq lazy-company-complete-in-projects t)
                     (cl-find (or lazy-name
                               lazy-company-project-name) lazy-company-complete-in-projects))
                 (company-grab-symbol)))
    (candidates (append (lazy-completions arg lazy-company-project-name)
                        (lazy-company-dabbrev-candidates arg)))
    (meta (lazy-eldoc-function-meta arg lazy-company-project-name (lazy-find-alist lazy-company-project-name)))
    ;;doc
    ;;location
    (post-completion (lazy-company-add-history lazy-company-project-name))
    (init (setq-local lazy-company-project-name (or lazy-company-project-name (cadr (assoc 'name (lazy-guess-alist))))))))

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
                               lazy-company-project-name) lazy-company-complete-in-projects))
                 (company-grab-symbol)))
    (candidates (append (lazy-company-gtags-candidates arg)
                        (lazy-company-imenu-candidates arg)
                        (when (cl-find 'elisp (lazy-src-pattern-languages (lazy-get-config-val 'src-patterns lazy-company-project-name)))
                          (lazy-company-obarray-candidates arg))
                        (lazy-company-dabbrev-candidates arg)))
    (meta (lazy-eldoc-function-meta arg lazy-company-project-name (lazy-find-alist lazy-company-project-name)))
    ;;doc
    ;;location
    (post-completion (lazy-company-add-history lazy-company-project-name))
    (init (setq-local lazy-company-project-name (or lazy-company-project-name (cadr (assoc 'name (lazy-guess-alist))))))))

(defun lazy-company-transform-history-to-front (candidates)
  (if (functionp company-backend)
      candidates
    (let ((low-priority (cdr (memq :with company-backend))))
      (if (null low-priority)
          candidates
        (let ((history (gethash lazy-company-project-name lazy-company-history))
              (history-candidates)
              (other-candidates))
          (dolist (c (reverse candidates))
            (if (memq c history)
                (push c history-candidates)
              (push c other-candidates)
              ))
          (setq history-candidates (sort history-candidates (lambda (a b) (< (cl-position a history :test 'equal) (cl-position b history :test 'equal)))))
          (append history-candidates other-candidates))))))

(provide 'lazy-company)

;; mk-project-company.el ends here
