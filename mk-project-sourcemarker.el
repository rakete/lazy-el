;;; mk-project-sourcemarker.el ---  Sourcemarker for mk-project.

;; Copyright (C) 2011  Andreas Raster <lazor at affenbande dot org>
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

(require 'mk-project)
(require 'continue)

(defvar mk-sourcemarker-per-project-db t)

(eval-after-load "mk-project-sourcemarker"
  '(progn
     (add-to-list 'mk-proj-optional-vars '(sourcemarker-db-path . (stringp)))
     (add-to-list 'mk-proj-optional-vars '(sourcemarker-db-symbol . (stringp)))

     (add-to-list 'mk-proj-var-before-get-functions
                  '(sourcemarker-db-path . (lambda (var val &optional proj-name config-alist)
                                             (if mk-sourcemarker-per-project-db
                                                 (if val
                                                     (expand-file-name val)
                                                   (mk-proj-get-cache-file 'sourcemarker-db nil 'copy))
                                               (or (and val (expand-file-name val))
                                                   continue-db-path)))))
     (add-to-list 'mk-proj-var-before-get-functions
                  '(sourcemarker-db-symbol . (lambda (var val &optional proj-name config-alist)
                                               (let ((proj-name (or proj-name
                                                                    (and (boundp 'mk-proj-name)
                                                                         mk-proj-name))))
                                                 (cond (val val)
                                                       ((and proj-name mk-sourcemarker-per-project-db)
                                                        (concat proj-name "-sourcemarker-db"))
                                                       (t continue-db-symbol))))))

     (remove-hook 'find-file-hook 'continue-restore)
     (add-hook 'find-file-hook 'mk-sourcemarker-restore)

     (remove-hook 'after-save-hook 'continue-save)
     (add-hook 'after-save-hook 'mk-sourcemarker-save)
     (add-hook 'kill-buffer-hook 'mk-sourcemarker-save)
     (run-with-idle-timer 120 t 'mk-sourcemarker-write-project-db)

     (add-hook 'mk-proj-before-files-load-hook (lambda ()
                                                 (remove-hook 'find-file-hook 'mk-sourcemarker-restore)
                                                 (dolist (proj-name (append (list mk-proj-name) (mk-proj-get-config-val 'friends)))
                                                   (dolist (buf (append (mk-proj-file-buffers proj-name)))
                                                     (when buf
                                                       (with-current-buffer buf
                                                         (continue-save)))))
                                                 (mk-sourcemarker-load-project-db)
                                                 ))
     (add-hook 'mk-proj-after-load-hook (lambda ()
                                          (mk-sourcemarker-restore-all)
                                          (add-hook 'find-file-hook 'mk-sourcemarker-restore)
                                          (mk-sourcemarker-display-most-recent-buffer)))

     (add-hook 'mk-proj-before-files-unload-hook (lambda ()
                                                   (mk-sourcemarker-save-all)
                                                   (remove-hook 'kill-buffer-hook 'mk-sourcemarker-save)))
     (add-hook 'mk-proj-after-unload-hook (lambda ()
                                            (add-hook 'kill-buffer-hook 'mk-sourcemarker-save)))
     ))

(defmacro mk-sourcemarker-with-project-db (&rest body)
  `(if (and mk-sourcemarker-per-project-db
            (mk-proj-get-config-val 'sourcemarker-db-path)
            (mk-proj-get-config-val 'sourcemarker-db-symbol))
       (let ((global-db-symbol continue-db-symbol)
             (global-db-path continue-db-path)
             (continue-db-path (mk-proj-get-config-val 'sourcemarker-db-path))
             (continue-db-symbol (mk-proj-get-config-val 'sourcemarker-db-symbol)))
         (condition-case nil
             (symbol-value (intern continue-db-symbol))
           (error (setf (symbol-value (intern continue-db-symbol))
                        (make-hash-table :test 'equal))))
         ,@body)
     ,@body))

(defun mk-sourcemarker-load-project-db ()
  (interactive)
  (mk-proj-assert-proj)
  (unless (intern-soft (mk-proj-get-config-val 'sourcemarker-db-symbol))
    (mk-sourcemarker-with-project-db
     (message "loading project db: %s" continue-db-symbol)
     (continue-load-db))))

(defun mk-sourcemarker-write-project-db ()
  (interactive)
  (mk-proj-assert-proj)
  (mk-sourcemarker-with-project-db
   (continue-write-db)))

(defun mk-sourcemarker-restore ()
  (interactive)
  (if (or (condition-case nil (mk-proj-assert-proj) (error t))
          (not (or (mk-proj-buffer-p (current-buffer))
                   (mk-proj-friendly-buffer-p (current-buffer)))))
      (continue-restore)
    (if (and (not (condition-case nil (mk-proj-assert-proj) (error t)))
             (not (mk-sourcemarker-with-project-db (or (gethash (buffer-file-name (current-buffer)) (symbol-value (intern continue-db-symbol)))
                                                       (gethash (mk-proj-file-truename (buffer-file-name (current-buffer))) (symbol-value (intern continue-db-symbol))))))
             (mk-proj-get-config-val 'parent))
        (mk-proj-with-current-project (mk-proj-get-config-val 'parent)
                                      (mk-sourcemarker-restore))
      (mk-sourcemarker-with-project-db
       (message "restoring sourcemarker in buffer %s (%s)" (buffer-name (current-buffer)) (buffer-file-name (current-buffer)))
       (continue-restore)))))

(defun mk-sourcemarker-restore-all ()
  (interactive)
  (mk-proj-assert-proj)
  (dolist (buf (append (mk-proj-file-buffers) (mk-proj-friendly-file-buffers)))
    (with-current-buffer buf
      (mk-sourcemarker-restore))))

(defun mk-sourcemarker-save ()
  (interactive)
  (if (or (condition-case nil (mk-proj-assert-proj) (error t))
          (not (or (mk-proj-buffer-p (current-buffer))
                   (mk-proj-friendly-buffer-p (current-buffer)))))
      (continue-save)
    (mk-sourcemarker-with-project-db
       (continue-save))))

(defun mk-sourcemarker-save-all ()
  (interactive)
  (let* ((results '())
         (sorted-buffers (dolist (buf (append (mk-proj-buffers) (mk-proj-friendly-buffers))
                                      (sort results (lambda (a b) (< (car a) (car b)))))
                           (when (buffer-file-name buf)
                             (when (eq buf (current-buffer))
                               (mk-sourcemarker-save))
                             (mk-sourcemarker-with-project-db
                              (let* ((filename (buffer-file-name buf))
                                     (sm (or (gethash filename (symbol-value (intern-soft continue-db-symbol)) nil)
                                             (gethash (mk-proj-file-truename filename) (symbol-value (intern-soft continue-db-symbol)) nil)))
                                     (timestamp (and sm (read (cdr (assoc :timestamp sm))))))
                                (if timestamp
                                    (add-to-list 'results `(,timestamp . ,buf))
                                  (add-to-list 'results `(-1 . ,buf)))))))))
    (dolist (tuple sorted-buffers)
      (with-current-buffer (cdr tuple)
        (mk-sourcemarker-save)))
    (mk-sourcemarker-write-project-db)))

(defun mk-sourcemarker-display-most-recent-buffer ()
  (mk-proj-assert-proj)
  (let* ((results '())
         (buffer (progn
                   (dolist (buf (append (mk-proj-buffers)))
                     (when (buffer-file-name buf)
                       (mk-sourcemarker-with-project-db
                        (let* ((filename (buffer-file-name buf))
                               (sm (or (gethash filename (symbol-value (intern continue-db-symbol)))
                                       (gethash (mk-proj-file-truename filename) (symbol-value (intern continue-db-symbol)))))
                               (timestamp (and sm (read (cdr (assoc :timestamp sm))))))
                          (when timestamp
                            (add-to-list 'results `(,timestamp . ,buf)))))))
                   (cdar (sort results (lambda (a b) (> (car a) (car b))))))))
    (let ((display-buffer-reuse-frames nil))
      (let ((buffer (or buffer (car (mk-proj-file-buffers))))
            (window (get-buffer-window buffer 'visible)))
        (if window
            (raise-frame (window-frame window))
          (display-buffer buffer))
        (with-current-buffer buffer (recenter))))
    ))

;;(sort '((0 . "foo") (1 . "bar")) (lambda (a b) (> (car a) (car b))))

(provide 'mk-project-sourcemarker)
