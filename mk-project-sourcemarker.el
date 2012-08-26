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

(defun mk-sourcemarker-setup ()
  (add-hook 'mk-proj-after-load-hook 'mk-sourcemarker-display-most-recent-buffer)
  (add-hook 'mk-proj-before-unload-hook 'mk-sourcemarker-save))

(defun mk-sourcemarker-display-most-recent-buffer ()
  (mk-proj-assert-proj)
  (let ((buffer (cdar (sort (loop for b in (mk-proj-buffers)
                                  if (and (buffer-file-name b)
                                          (assoc :timestamp (gethash (buffer-file-name b) continue-db nil)))
                                  collect `(,(read (cdr (assoc :timestamp (gethash (buffer-file-name b) continue-db nil)))) . ,b))
                            (lambda (a b) (> (car a) (car b)))))))
    (let ((display-buffer-reuse-frames t))
      (unless (or (mk-proj-file-buffer-p (current-buffer))
                  (mk-proj-friendly-file-buffer-p (current-buffer)))
        (if buffer
            (display-buffer buffer)
          (when (car (mk-proj-file-buffers))
            (display-buffer (car (mk-proj-file-buffers)))))))))

(defun mk-sourcemarker-create ()
  (interactive)
  (mk-proj-assert-proj)
  (when (and (buffer-file-name (current-buffer))
             (not (condition-case nil (mk-proj-assert-proj) (error t)))
             (mk-proj-buffer-p (current-buffer)))
    (continue-sourcemarker-create)))

(defun mk-sourcemarker-save ()
  (interactive)
  (mk-proj-assert-proj)
  (dolist (buf (mk-proj-friendly-file-buffers))
    (when (and (buffer-file-name buf)
               (not (condition-case nil (mk-proj-assert-proj) (error t)))
               (mk-proj-buffer-p buf))
      (with-current-buffer buf
        (continue-save))))
  (continue-write-db))

(provide 'mk-project-sourcemarker)
