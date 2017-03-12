;;; lazy-sourcemarker.el ---  Sourcemarker for lazy-el.

;; Copyright (C) 2011-2017 Andreas Raster <lazor at affenbande dot org>
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

(require 'lazy)
(require 'continue)

(defvar lazy-sourcemarker-per-project-db t)

(defmacro lazy-sourcemarker-with-project-db (&rest body)
  `(if (and lazy-sourcemarker-per-project-db
            (lazy-get-config-val 'sourcemarker-db-path)
            (lazy-get-config-val 'sourcemarker-db-symbol))
       (let ((global-db-symbol continue-db-symbol)
             (global-db-path continue-db-path)
             (continue-db-path (lazy-get-config-val 'sourcemarker-db-path))
             (continue-db-symbol (lazy-get-config-val 'sourcemarker-db-symbol)))
         (condition-case nil
             (symbol-value (intern continue-db-symbol))
           (error (setf (symbol-value (intern continue-db-symbol))
                        (make-hash-table :test 'equal))))
         ,@body)
     ,@body))

(defun lazy-sourcemarker-load-project-db ()
  (interactive)
  (lazy-assert-proj)
  (lazy-sourcemarker-with-project-db
   (message "loading project db: %s" continue-db-symbol)
   (continue-load-db)))

(defun lazy-sourcemarker-write-project-db ()
  (interactive)
  (when lazy-name
    (lazy-sourcemarker-with-project-db
     (continue-write-db))))

(defun lazy-sourcemarker-restore ()
  (interactive)
  (if (or (condition-case nil (lazy-assert-proj) (error t))
          (not (or (lazy-buffer-p (current-buffer))
                   (lazy-friendly-buffer-p (current-buffer)))))
      (continue-restore)
    (if (and (not (condition-case nil (lazy-assert-proj) (error t)))
             (not (lazy-sourcemarker-with-project-db (or (gethash (buffer-file-name (current-buffer)) (symbol-value (intern continue-db-symbol)))
                                                       (gethash (file-truename (buffer-file-name (current-buffer))) (symbol-value (intern continue-db-symbol))))))
             (lazy-get-config-val 'parent))
        (lazy-with-current-project (lazy-get-config-val 'parent)
                                      (lazy-sourcemarker-restore))
      (lazy-sourcemarker-with-project-db
       (when (continue-restore)
         (message "restoring sourcemarker in buffer %s (%s)" (buffer-name (current-buffer)) (buffer-file-name (current-buffer))))))))

(defun lazy-sourcemarker-restore-all ()
  (interactive)
  (lazy-assert-proj)
  (dolist (buf (append (lazy-file-buffers) (lazy-friendly-file-buffers)))
    (with-current-buffer buf
      (lazy-sourcemarker-restore))))

(defun lazy-sourcemarker-save ()
  (interactive)
  (if (or (condition-case nil (lazy-assert-proj) (error t))
          (not (or (lazy-buffer-p (current-buffer))
                   (lazy-friendly-buffer-p (current-buffer)))))
      (continue-save)
    (lazy-sourcemarker-with-project-db
       (continue-save))))

(defun lazy-sourcemarker-save-all ()
  (interactive)
  (let* ((results '())
         (sorted-buffers (dolist (buf (append (lazy-buffers) (lazy-friendly-buffers))
                                      (sort results (lambda (a b) (< (car a) (car b)))))
                           (when (buffer-file-name buf)
                             (when (eq buf (current-buffer))
                               (lazy-sourcemarker-save))
                             (lazy-sourcemarker-with-project-db
                              (let* ((filename (buffer-file-name buf))
                                     (sm (or (gethash filename (symbol-value (intern-soft continue-db-symbol)) nil)
                                             (gethash (file-truename filename) (symbol-value (intern-soft continue-db-symbol)) nil)))
                                     (timestamp (and sm (read (cdr (assoc :timestamp sm))))))
                                (if timestamp
                                    (add-to-list 'results `(,timestamp . ,buf))
                                  (add-to-list 'results `(-1 . ,buf)))))))))
    (dolist (tuple sorted-buffers)
      (with-current-buffer (cdr tuple)
        (lazy-sourcemarker-save)))
    (lazy-sourcemarker-write-project-db)))

(defun lazy-sourcemarker-display-most-recent-buffer ()
  (lazy-assert-proj)
  (let* ((results '())
         (buffer (progn
                   (dolist (buf (append (lazy-buffers)))
                     (when (buffer-file-name buf)
                       (lazy-sourcemarker-with-project-db
                        (let* ((filename (buffer-file-name buf))
                               (sm (or (gethash filename (symbol-value (intern continue-db-symbol)))
                                       (gethash (file-truename filename) (symbol-value (intern continue-db-symbol)))))
                               (timestamp (and sm (read (cdr (assoc :timestamp sm))))))
                          (when timestamp
                            (add-to-list 'results `(,timestamp . ,buf)))))))
                   (cdar (sort results (lambda (a b) (> (car a) (car b))))))))
    (let* ((display-buffer-reuse-frames nil)
           (buffer (or buffer (car (lazy-file-buffers))))
           (window (get-buffer-window buffer 'visible))
           (popwin-close 'popwin:close-popup-window))
      (when buffer
        (with-current-buffer buffer
          (when (functionp popwin-close)
            (funcall popwin-close))
          (if window
              (raise-frame (window-frame window))
            (switch-to-buffer buffer))
          (recenter))))
    ))

;;(sort '((0 . "foo") (1 . "bar")) (lambda (a b) (> (car a) (car b))))

(with-eval-after-load "lazy-sourcemarker"
  '(progn
     (add-to-list 'lazy-optional-vars '(sourcemarker-db-path . (stringp)))
     (add-to-list 'lazy-optional-vars '(sourcemarker-db-symbol . (stringp)))

     (add-to-list 'lazy-var-before-get-functions
                  '(sourcemarker-db-path . (lambda (var val &optional proj-name config-alist)
                                             (if lazy-sourcemarker-per-project-db
                                                 (if val
                                                     (expand-file-name val)
                                                   (lazy-get-cache-file 'sourcemarker-db nil 'copy))
                                               (or (and val (expand-file-name val))
                                                   continue-db-path)))))
     (add-to-list 'lazy-var-before-get-functions
                  '(sourcemarker-db-symbol . (lambda (var val &optional proj-name config-alist)
                                               (let ((proj-name (or proj-name
                                                                    (and (boundp 'lazy-name)
                                                                         lazy-name))))
                                                 (cond (val val)
                                                       ((and proj-name lazy-sourcemarker-per-project-db)
                                                        (concat proj-name "-sourcemarker-db"))
                                                       (t continue-db-symbol))))))

     (remove-hook 'find-file-hook 'continue-restore)
     (add-hook 'find-file-hook 'lazy-sourcemarker-restore)

     (remove-hook 'after-save-hook 'continue-save)
     (add-hook 'after-save-hook 'lazy-sourcemarker-save)
     (add-hook 'kill-buffer-hook 'lazy-sourcemarker-save)
     (run-with-idle-timer 120 t 'lazy-sourcemarker-write-project-db)

     (add-hook 'lazy-before-files-load-hook (lambda ()
                                              (remove-hook 'find-file-hook 'lazy-sourcemarker-restore)
                                              (dolist (proj-name (append (list lazy-name) (lazy-get-config-val 'friends)))
                                                (dolist (buf (append (lazy-file-buffers proj-name)))
                                                  (when buf
                                                    (with-current-buffer buf
                                                      (lazy-sourcemarker-save)))))
                                              (lazy-sourcemarker-load-project-db)
                                              ))
     (add-hook 'lazy-after-load-hook (lambda ()
                                       (lazy-sourcemarker-restore-all)
                                       (add-hook 'find-file-hook 'lazy-sourcemarker-restore)
                                       (lazy-sourcemarker-display-most-recent-buffer)))

     (add-hook 'lazy-before-files-unload-hook (lambda ()
                                                (lazy-sourcemarker-save-all)
                                                (remove-hook 'kill-buffer-hook 'lazy-sourcemarker-save)))
     (add-hook 'lazy-after-unload-hook (lambda ()
                                         (add-hook 'kill-buffer-hook 'lazy-sourcemarker-save)))
     ))

(provide 'lazy-sourcemarker)
