(require 'mk-project)

(require 'which-func)

(defun mk-eldoc-cleanup-thing (thing)
  (when thing
    ;; nuke newlines
    (setq thing (replace-regexp-in-string "\n" " " thing))
    ;; nuke comments (note non-greedy *? instead of *)
    (setq thing (replace-regexp-in-string "/\\*.*?\\*/" " " thing))
    ;; (just-one-space)
    (setq thing (replace-regexp-in-string "[ \t]+" " " thing))
    (setq thing (replace-regexp-in-string "[ \t]+$" "" thing)))
  thing)

(defun mk-eldoc-thing-at-point-function-synopsis ()
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

(defun mk-eldoc-thing-at-point-function-name ()
  (let ((a (point-at-bol))
        (b (point-at-eol)))
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
      (buffer-substring a b))))

(defun mk-eldoc-function-meta (&optional symbol proj-name proj-alist)
  (unless (or (window-minibuffer-p)
              (string-match "\*.*\*" (buffer-name (current-buffer))))
    (setq symbol (or symbol
                     (or (and (not symbol)
                              (boundp 'company-candidates)
                              company-candidates
                              (car-safe company-candidates))
                         (mk-eldoc-thing-at-point-function-name))))
    (setq proj-alist (or proj-alist
                         (mk-proj-find-alist proj-name)
                         (mk-proj-find-alist mk-proj-name)
                         (mk-proj-guess-alist)))
    (setq proj-name (cadr (assoc 'name proj-alist)))
    (unless (and proj-name proj-alist)
      (mk-proj-assert-proj))
    (when symbol
      (let* ((gtags-plist (car-safe (mk-proj-find-symbol proj-name proj-alist 'gtags symbol (concat "global -x -d -e \"^" symbol ".*\""))))
             (line-number (plist-get gtags-plist :line-number))
             (file-path (plist-get gtags-plist :file-path))
             (definition (plist-get gtags-plist :definition)))
        (when definition
          (setq definition (replace-regexp-in-string " {$" "" definition)))
        (if (and definition (string-match ")[ \t{;]*$" definition))
            definition
          (when (and line-number file-path)
            (with-temp-buffer
              (insert-file-contents file-path)
              (forward-line (1- line-number))
              (mk-eldoc-cleanup-thing (mk-eldoc-thing-at-point-function-synopsis)))))))))

(defun mk-eldoc-function-doc ()
  "yoinks!")

;; (defvar mk-proj-definitions-cache (make-hash-table :test 'equal))

;; (defun mk-proj-update-obarray-definitions-cache (proj-name)
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
;;              (cached-definition (when docstring (gethash completion mk-proj-definitions-cache))))
;;         (if (and docstring cached-definition)
;;             (plist-put cached-definition :docstring docstring)
;;           (puthash completion
;;                    (plist-put cached-definition :docstring docstring)
;;                    mk-proj-definitions-cache))))))

;; (defun mk-proj-update-gtags-definitions-cache (proj-name)
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
;;                         (cached-definition (gethash completion mk-proj-definitions-cache)))
;;                    (plist-put cached-definition :line-number (match-string 2 line))
;;                    (plist-put cached-definition :file-path (match-string 3 line))
;;                    (plist-put cached-definition :definition (match-string 4 line))
;;                    (unless cached-definition
;;                      (puthash completion
;;                               cached-definition
;;                               mk-proj-definitions-cache))))))))

(provide 'mk-project-eldoc)
