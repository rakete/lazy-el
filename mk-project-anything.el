;;; mk-project-anything.el --- Anything integration for mk-project

;; Copyright (C) 2008  Matt Keller <mattkeller at gmail dot com>
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

;;; Commentary:

;; More information about this library, including the most recent
;; version and a comprehensive README, is available at
;; http://github.com/mattkeller/mk-project

;;; Code:

(require 'mk-project)
(require 'anything)
(require 'anything-config)

;; ---------------------------------------------------------------------
;; Anything Utilities
;; ---------------------------------------------------------------------

(defun* project-find-anything ()
  "Find files, buffers and interesting items with 'anything'.

The behaviour of this command is modified with
`mk-proj-anything-sources'."

  (interactive)
  (mk-proj-assert-proj)
  (unless (get-buffer mk-proj-fib-name)
    (message "Please use project-index to create the index before running project-find-anything")
    (return-from "project-find-anything" nil))
  (anything-other-buffer mk-proj-anything-sources
            "*anything mk-project*"))

(defun anything-c-mk-project-buffer-persistent-action (name)
  (flet ((kill (item)
               (with-current-buffer item
                 (if (and (buffer-modified-p)
                          (buffer-file-name (current-buffer)))
                     (progn
                       (save-buffer)
                       (kill-buffer item))
                   (kill-buffer item))))
         (goto (item)
               (switch-to-buffer item)))
    (if current-prefix-arg
        (progn
          (kill name)
          (anything-delete-current-selection))
      (goto name))))

;; ---------------------------------------------------------------------
;; Anything Friends
;; ---------------------------------------------------------------------

(defun mk-proj-get-project-files (name &optional regex)
  (let ((files '()))
    (when (mk-proj-find-config name)
      (let* ((friend-config (mk-proj-find-config name))
             (friend-basedir (expand-file-name (car (cdr (assoc 'basedir friend-config)))))
             (friend-file-list-cache (expand-file-name (car (cdr (assoc 'file-list-cache friend-config)))))
             (friend-cache-buffer-name (concat "*" friend-file-list-cache "*")))
        (with-current-buffer (generate-new-buffer friend-cache-buffer-name)
          (with-current-buffer (find-file-noselect-1 friend-cache-buffer-name friend-file-list-cache nil nil nil nil)
            (goto-char (point-min))
            (while 
                (progn
                  (let ((raw-file (buffer-substring (line-beginning-position) (line-end-position))))
                    (when (> (length raw-file) 0)
                      (let ((file (if (file-name-absolute-p raw-file) 
                                      raw-file
                                    (replace-regexp-in-string "/\\./" "/" (concat (file-name-as-directory friend-basedir) raw-file)))))
                        (if regex
                            (when (string-match regex file) (add-to-list 'files file))
                          (add-to-list 'files file)))
                      (= (forward-line) 0))))))
          (kill-buffer))))
    (sort files #'string-lessp)))

(defun mk-proj-friend-matches (&optional regex)
  (let ((files '()))
    (dolist (f mk-proj-friends files)
      (if (file-exists-p (expand-file-name f))
          (if regex
              (when (string-match regex file) (add-to-list 'files f))
            (add-to-list 'files f))
        (setq files (append files (mk-proj-get-project-files f regex)))))))

(defun mk-proj-friendly-buffer-p (buf)
  (let ((file-name (mk-proj-buffer-name buf)))
    (if (and file-name
             (block "friend-loop"
               (dolist (f mk-proj-friends)
                 (if (file-exists-p (expand-file-name f))
                     (when (string-equal f file-name)
                       (return-from "friend-loop" t))
                   (when (mk-proj-find-config f)
                     (let* ((friend-config (mk-proj-find-config f))
                            (friend-basedir (expand-file-name (car (cdr (assoc 'basedir friend-config))))))
                       (when (string-match (concat "^" (regexp-quote friend-basedir)) file-name)
                         (return-from "friend-loop" t))))))))
        t
      nil)))

(defun mk-proj-friendly-buffers ()
  (let ((buffers nil))
    (dolist (b (buffer-list))
      (when (mk-proj-friendly-buffer-p b) (push b buffers)))
    buffers))

(defun mk-proj-save-open-friends-info ()
  (when mk-proj-open-friends-cache
    (with-temp-buffer
      (dolist (f (mapcar (lambda (b) (mk-proj-buffer-name b)) (mk-proj-friendly-buffers)))
        (when f
          (unless (string-equal mk-proj-tags-file f)
            (insert f "\n"))))
      (if (file-writable-p mk-proj-open-friends-cache)
          (progn
            (write-region (point-min)
                          (point-max)
                          mk-proj-open-friends-cache)
            (message "Wrote open friends to %s" mk-proj-open-friends-cache))
        (message "Cannot write to %s" mk-proj-open-friends-cache)))))

(defun mk-proj-visit-saved-open-friends ()
  (when mk-proj-open-friends-cache
    (when (file-readable-p mk-proj-open-friends-cache)
      (message "Reading open friends from %s" mk-proj-open-friends-cache)
      (with-temp-buffer
        (insert-file-contents mk-proj-open-friends-cache)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((start (point)))
            (while (not (eolp)) (forward-char)) ; goto end of line
            (let ((line (buffer-substring start (point))))
              (message "Attempting to open %s" line)
              (find-file-noselect line t)))
          (forward-line))))))

(defun mk-proj-kill-emacs-hook-friends ()
  (when (and mk-proj-name mk-proj-open-friends-cache)
    (mk-proj-save-open-friends-info)))

(defun project-close-friends ()
  (interactive)
  (mk-proj-assert-proj)
  (let ((closed nil)
        (dirty nil)
        (basedir-len (length mk-proj-basedir)))
    (dolist (b (mk-proj-friendly-buffers))
      (cond
       ((buffer-modified-p b)
        (push (buffer-name) dirty))
       (t
        (kill-buffer b)
        (push (buffer-name) closed))))
    (message "Closed %d friendly buffers, %d modified friendly buffers where left open"
             (length closed) (length dirty))))

(defun project-multi-occur-with-friends (regex)
  "Search all open project files (including friends) for 'regex' using `multi-occur'.

If called with prefix arg it will behave just like `project-multi-occur'"
  (interactive "sRegex: ")
  (multi-occur (mk-proj-filter (lambda (b) (if (buffer-file-name b) b nil)) 
                               (if current-prefix-arg
                                   (mk-proj-buffers)
                                 (append (mk-proj-buffers) (mk-proj-friendly-buffers))))
               regex))

(defun mk-proj-friend-basedirs ()
  "Return all friends basedirs. This may also return single filenames instead of a directory."
  (let* ((basedirs '()))
    (dolist (f mk-proj-friends basedirs)
      (if (file-exists-p (expand-file-name f))
          (add-to-list 'basedirs f)
        (add-to-list 'basedirs (mk-proj-config-val f 'basedir))))))

(defun project-ack-with-friends ()
  "Run ack with project's basedir and all friend basedirs as arguments, using the `ack-args' configuration.
With C-u prefix, act like `project-ack'."
  (interactive)
  (mk-proj-assert-proj)
  (if current-prefix-arg
      (project-ack)
    (let* ((wap (word-at-point))
           (regex (if wap (read-string (concat "Ack project for (default \"" wap "\"): ") nil nil wap)
                    (read-string "Ack project for: ")))
           (whole-cmd (concat (mk-proj-ack-cmd regex) " " mk-proj-basedir (let ((s ""))
                                                                            (dolist (d (mk-proj-friend-basedirs) s)
                                                                              (setq s (concat s " " d))))))
           (confirmed-cmd (read-string "Ack command: " whole-cmd nil whole-cmd))
           (default-directory (file-name-as-directory
                               (if (mk-proj-has-univ-arg)
                                   default-directory
                                 mk-proj-basedir))))
      (compilation-start confirmed-cmd 'ack-mode))))

;; ---------------------------------------------------------------------
;; Anything Sources
;; ---------------------------------------------------------------------

(defvar anything-c-source-mk-project-projects
  '((name . "mk-project Projects")
    (candidates . (lambda ()
                    (let ((allkeys '()))
                      (maphash (lambda (k v)
                                 (setq allkeys (cons k allkeys)))
                               mk-proj-list)
                      (mk-proj-filter (lambda (k)
                                        (if (file-exists-p (mk-proj-config-val k 'basedir)) k nil))
                                      allkeys))))
    (action . (lambda (entry)
                (mk-proj-load entry))))
  "All configured mk-project projects.")

(defvar anything-c-source-mk-project-files
  '((name . "Files")
    (candidates . (lambda ()
                    (mapcar (lambda (s)
                              (replace-regexp-in-string "/\\./" "/" (concat (file-name-as-directory mk-proj-basedir) s)))
                            (if mk-proj-patterns-are-regex
                                (flatten (mapcar 'mk-proj-fib-matches mk-proj-src-patterns))
                              (mk-proj-fib-matches nil)))))
    (match anything-c-match-on-file-name
           anything-c-match-on-directory-name)
    (type . file))
  "All files of the currently active project.")

(defvar anything-c-source-mk-project-open-buffers
  '((name . "Buffers")
    (candidates . (lambda () (mapcar 'buffer-name (mk-proj-buffers))))
    (type . buffer)
    (candidate-transformer anything-c-skip-current-buffer
                           anything-c-highlight-buffers
                           anything-c-skip-boring-buffers)
    (persistent-action . anything-c-mk-project-buffer-persistent-action)
    (persistent-help . "Show this buffer / C-u \\[anything-execute-persistent-action]: Kill this buffer"))
  "All buffers of the currently active project." )

(defvar anything-c-source-mk-friendly-files
  '((name . "Friendly files")
    (candidates . (lambda () (mk-proj-friend-matches)))
    (match anything-c-match-on-file-name
           anything-c-match-on-directory-name)
    (type . file))
  "All files and files of projects which are friends of this project.")

(defvar anything-c-source-mk-project-open-friendly-buffers
  '((name . "Friendly buffers")
    (candidates . (lambda () (mapcar 'buffer-name (mk-proj-friendly-buffers))))
    (type . buffer)
    (candidate-transformer anything-c-skip-current-buffer
                           anything-c-highlight-buffers
                           anything-c-skip-boring-buffers)
    (persistent-action . anything-c-mk-project-buffer-persistent-action)
    (persistent-help . "Show this buffer / C-u \\[anything-execute-persistent-action]: Kill this buffer"))
  "All buffers of the currently active project." )

(defvar mk-proj-anything-sources
  '(anything-c-source-mk-project-open-buffers
    anything-c-source-mk-project-open-friendly-buffers
    anything-c-source-mk-project-files
    anything-c-source-mk-friendly-files
    anything-c-source-mk-project-projects)
  "The `anything-sources' that `project-find-anything' uses.

See also: `anything-c-source-mk-project-open-buffers',
          `anything-c-source-mk-project-files'")

;; ---------------------------------------------------------------------
;; Anything Setup
;; ---------------------------------------------------------------------

(eval-after-load "mk-project-anything"
  '(progn
     (add-to-list 'mk-proj-optional-vars 'friends)
     (add-to-list 'mk-proj-optional-vars 'open-friends-cache)

     (add-hook 'kill-emacs-hook 'mk-proj-kill-emacs-hook-friends)
     (add-hook 'mk-proj-project-load-hook 'mk-proj-visit-saved-open-friends)
     (add-hook 'mk-proj-project-unload-hook 'mk-proj-save-open-friends-info)))

(provide 'mk-project-anything)
