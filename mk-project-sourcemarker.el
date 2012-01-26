
(require 'mk-project)
(require 'continue)

(eval-after-load "mk-project-sourcemarker"
  '(progn
     (add-to-list 'mk-proj-optional-vars 'sourcemarker)
     (add-hook 'mk-proj-after-load-hook 'sourcemarker-visit)
     (add-hook 'after-save-hook (lambda ()
                                  (when (and (boundp 'mk-proj-name) mk-proj-name
                                             (or (mk-proj-buffer-p (current-buffer))
                                                 (mk-proj-friendly-buffer-p (current-buffer)))
                                             (file-exists-p (buffer-file-name (current-buffer))))
                                    (sourcemarker-set))))
     (add-hook 'kill-buffer-hook (lambda ()
                                   (when (and (boundp 'mk-proj-name) mk-proj-name
                                              (boundp 'mk-proj-sourcemarker) mk-proj-sourcemarker
                                              (find-buffer-visiting (cdr (assoc :file mk-proj-sourcemarker)))
                                              (eq (current-buffer) (find-buffer-visiting (cdr (assoc :file mk-proj-sourcemarker)))))
                                     (let ((next-buf (some (lambda (buf)
                                                                  (when (and (not (eq buf (current-buffer)))
                                                                             (or (mk-proj-buffer-p buf)
                                                                                 (mk-proj-friendly-buffer-p buf))
                                                                             (file-exists-p (buffer-file-name buf)))
                                                                    buf)) (buffer-list))))
                                       (when next-buf
                                         (with-current-buffer next-buf
                                           (sourcemarker-set)))))))
     ))

;; (kill-local-variable 'kill-buffer-hook)
;; (setq kill-buffer-hook nil)

;; (kill-local-variable 'after-save-hook)
;; (setq after-save-hook nil)

(defun sourcemarker-visit ()
  "Restore project sourcemarker and go there."
  (interactive)
  (when mk-proj-sourcemarker
    (let* ((m (continue-sourcemarker-restore mk-proj-sourcemarker))
           (buf (marker-buffer m))
           (oldframe (current-frame)))
      (when (markerp m)
        (when (get-buffer-window (get-buffer buf) 'visible)
          (select-frame (window-frame (get-buffer-window (get-buffer buf) 'visible))))
        (goto-char (marker-position m))
        (display-buffer buf)
        (with-current-buffer buf
          (sourcemarker-set))))))

(defun sourcemarker-set ()
  "Update a projects sourcemarker."
  (interactive)
  (mk-proj-set-config-val 'sourcemarker (continue-sourcemarker-create)))

(provide 'mk-project-sourcemarker)