;;; mk-project-sourcemarker.el ---  Org-Mode integration for mk-project

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

(defun sourcemarker-setup ()
  (add-to-list 'mk-proj-optional-vars 'sourcemarker)
  (add-to-list 'mk-proj-internal-vars 'sourcemarker)
  (add-hook 'mk-proj-after-load-hook 'sourcemarker-display-most-recent-buffer)
  ;;(add-hook 'mk-proj-before-unload-hook 'sourcemarker-set)
  ;; (add-hook 'after-save-hook (lambda ()
  ;;                              (when (and (boundp 'mk-proj-name) mk-proj-name
  ;;                                         (or (mk-proj-buffer-p (current-buffer))
  ;;                                             (mk-proj-friendly-buffer-p (current-buffer)))
  ;;                                         (buffer-file-name (current-buffer))
  ;;                                         (file-exists-p (buffer-file-name (current-buffer))))
  ;;                                (sourcemarker-set))))
  ;; (add-hook 'kill-buffer-hook (lambda ()
  ;;                               (when (and (boundp 'mk-proj-name) mk-proj-name
  ;;                                          (boundp 'mk-proj-sourcemarker) mk-proj-sourcemarker
  ;;                                          (find-buffer-visiting (cdr (assoc :file mk-proj-sourcemarker)))
  ;;                                          (eq (current-buffer) (find-buffer-visiting (cdr (assoc :file mk-proj-sourcemarker)))))
  ;;                                 (let ((next-buf (some (lambda (buf)
  ;;                                                         (when (and (not (eq buf (current-buffer)))
  ;;                                                                    (or (mk-proj-buffer-p buf)
  ;;                                                                        (mk-proj-friendly-buffer-p buf))
  ;;                                                                    (buffer-file-name buf)
  ;;                                                                    (file-exists-p (buffer-file-name buf)))
  ;;                                                           buf)) (buffer-list))))
  ;;                                   (when next-buf
  ;;                                     (with-current-buffer next-buf
  ;;                                       (sourcemarker-set)))))))
  )

(defun sourcemarker-display-most-recent-buffer ()
  (let ((buffer (cdar (sort (loop for b in (mk-proj-buffers)
                                  if (and (buffer-file-name b)
                                          (assoc :timestamp (gethash (buffer-file-name b) continue-db nil)))
                                  collect `(,(read (cdr (assoc :timestamp (gethash (buffer-file-name b) continue-db nil)))) . ,b))
                            (lambda (a b) (> (car a) (car b)))))))
    (when buffer
      (display-buffer buffer))))

(defun sourcemarker-visit ()
  "Restore project sourcemarker and go there."
  (interactive)
  (when mk-proj-sourcemarker
    (let* ((m (continue-sourcemarker-restore mk-proj-sourcemarker))
           (buf (find-file-noselect (cdr (assoc :file mk-proj-sourcemarker))))
           (oldframe (current-frame)))
      (when m
        (when (get-buffer-window (get-buffer buf) 'visible)
          (select-frame (window-frame (get-buffer-window (get-buffer buf) 'visible))))
        (goto-char m)
        (display-buffer buf)
        (with-current-buffer buf
          (sourcemarker-set))))))

(defun sourcemarker-set ()
  "Set/update a projects sourcemarker."
  (interactive)
  (when (and (buffer-file-name (current-buffer))
             (mk-proj-buffer-p (current-buffer)))
    (mk-proj-set-config-val 'sourcemarker (continue-sourcemarker-create))))

(provide 'mk-project-sourcemarker)