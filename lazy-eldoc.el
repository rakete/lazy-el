(require 'lazy)

(require 'which-func)

(defun lazy-eldoc-cleanup-thing (thing)
  (when thing
    ;; nuke newlines
    (setq thing (replace-regexp-in-string "\n" " " thing))
    ;; nuke comments (note non-greedy *? instead of *)
    (setq thing (replace-regexp-in-string "/\\*.*?\\*/" " " thing))
    ;; (just-one-space)
    (setq thing (replace-regexp-in-string "[ \t]+" " " thing))
    (setq thing (replace-regexp-in-string "[ \t]+$" "" thing)))
  thing)

(defun lazy-eldoc-thing-at-point-function-synopsis ()
  (let ((a (point-at-bol))
        (b (point-at-eol)))
    (save-excursion
      (if (bolp) (forward-line -1) (beginning-of-line))
      (skip-chars-forward "^{;")
      (dotimes (i 3) (backward-sexp))
      (setq b (point))
      (skip-chars-forward "^{;")
      (setq a (point)))
    (buffer-substring a b)))

(defun lazy-eldoc-thing-at-point-function-name ()
  (let ((a (point-at-bol))
        (b (point-at-eol)))
    (condition-case nil
        (when (and ;;(string-match "(\\|)" (buffer-substring a b))
               (not (nth 4 (syntax-ppss))))
          (save-excursion
            (if (bolp) (forward-line -1) (beginning-of-line))
            (re-search-forward ")")
            (skip-chars-backward "^(")
            (skip-chars-backward " ")
            (skip-chars-backward "^ ")
            (setq b (point))
            (skip-chars-forward "^(")
            (setq a (point)))
          (save-excursion
            (re-search-forward "(")
            (skip-chars-backward "^(")
            (skip-chars-backward " ")
            (skip-chars-backward "^ ")
            (when (or (< (point) b))
              (setq b (point))
              (skip-chars-forward "^(")
              (setq a (point))))
          (when (string-equal (buffer-substring a b) "")
            (save-excursion
              (if (bolp) (forward-line -1) (beginning-of-line))
              (re-search-forward "(")
              (skip-chars-backward "^(")
              (skip-chars-backward " ")
              (skip-chars-backward "^ ")
              (setq b (point))
              (skip-chars-forward "^(")
              (setq a (point))))
          (when (> b a)
            (buffer-substring a b)))
      (error nil))))

(defun lazy-eldoc-function-meta (&optional symbol proj-name proj-alist)
  (condition-case nil
      (unless (or (window-minibuffer-p)
                  (string-match "\*.*\*" (buffer-name (current-buffer))))
        (setq symbol (or symbol
                         (or (and (not symbol)
                                  (boundp 'company-candidates)
                                  company-candidates
                                  (car-safe company-candidates))
                             (lazy-eldoc-thing-at-point-function-name))))
        (setq proj-alist (or proj-alist
                             (lazy-find-alist proj-name)
                             (lazy-find-alist lazy-name)
                             (lazy-guess-alist)))
        (setq proj-name (cadr (assoc 'name proj-alist)))
        (unless (and proj-name proj-alist)
          (lazy-assert-proj))
        (when (and symbol (> (length symbol) 0))
          (let* ((gtags-plist (or (car-safe (lazy-find-symbol proj-name proj-alist 'gtags symbol (concat "global -x -d -e \"^" symbol ".*\"")))
                                  (car-safe (lazy-find-symbol proj-name proj-alist 'gtags symbol (concat "global -x -s -e \"^" symbol ".*\"")))))
                 (line-number (plist-get gtags-plist :line-number))
                 (file-path (plist-get gtags-plist :file-path))
                 (definition (plist-get gtags-plist :definition)))
            (when definition
              (setq definition (replace-regexp-in-string " {$" "" definition)))
            (if (and definition (string-match ")[ \t{;]*$" definition))
                definition
              (when (and line-number file-path (file-exists-p file-path))
                (with-temp-buffer
                  (insert-file-contents file-path)
                  (forward-line (1- line-number))
                  (lazy-eldoc-cleanup-thing (lazy-eldoc-thing-at-point-function-synopsis))))))))
    (error nil)))

(defun lazy-eldoc-function-doc ()
  "yoinks!")

;; (defvar lazy-definitions-cache (make-hash-table :test 'equal))

;; (defun lazy-update-obarray-definitions-cache (proj-name)
;;   (do-all-symbols (sym)
;;     (when (or (fboundp sym)
;;               (boundp sym))
;;       (let* ((completion (symbol-name sym))
;;              (doc (condition-case nil
;;                       (if (fboundp sym)
;;                           (documentation sym t)
;;                         (documentation-property sym 'variable-documentation t))
;;                     (error nil)))
;;              (case-fold-search nil)
;;              (docstring (and (stringp doc)
;;                              (string-match ".*$" doc)
;;                              (match-string 0 doc)))
;;              (cached-definition (when docstring (gethash completion lazy-definitions-cache))))
;;         (if (and docstring cached-definition)
;;             (plist-put cached-definition :docstring docstring)
;;           (puthash completion
;;                    (plist-put cached-definition :docstring docstring)
;;                    lazy-definitions-cache))))))

;; (defun lazy-update-gtags-definitions-cache (proj-name)
;;   (let* ((cmd (concat "global --match-part=first -xGq -d \".*\""))
;;          (lines (split-string (condition-case nil (shell-command-to-string cmd) (error nil)) "\n" t))
;;          (case-fold-search nil))
;;     (when lines
;;       (loop for line in lines
;;             do (when (string-match (concat "^"
;;                                            "\\([^ ]*\\)" ;; completion
;;                                            "[ \t]+\\([[:digit:]]+\\)" ;; linum
;;                                            "[ \t]+\\([^ \t]+\\)" ;; file
;;                                            "[ \t]+\\(.*\\)" ;; definition
;;                                            "$")
;;                                    line)
;;                  (let* ((completion (match-string 1 line))
;;                         (cached-definition (gethash completion lazy-definitions-cache)))
;;                    (plist-put cached-definition :line-number (match-string 2 line))
;;                    (plist-put cached-definition :file-path (match-string 3 line))
;;                    (plist-put cached-definition :definition (match-string 4 line))
;;                    (unless cached-definition
;;                      (puthash completion
;;                               cached-definition
;;                               lazy-definitions-cache))))))))

(provide 'lazy-eldoc)
