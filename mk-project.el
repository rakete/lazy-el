;;; mk-project.el ---  Lightweight project handling

;; Copyright (C) 2010  Matt Keller <mattkeller at gmail dot com>
;;                     Andreas Raster <lazor at affenbande dot org>
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

;; Quickly switch between projects and perform operations on a
;; per-project basis. A 'project' in this sense is a directory of
;; related files, usually a directory of source files. Projects are
;; defined in pure elisp with the 'project-def' function. No
;; mk-project-specific files are required in the project's base
;; directory.

;; More information about this library, including the most recent
;; version and a comprehensive README, is available at
;; http://github.com/mattkeller/mk-project

;;; Code:

(require 'grep)
(require 'thingatpt)
(require 'cl)

(defvar mk-proj-version "1.5.1"
  "As tagged at http://github.com/mattkeller/mk-project/tree/master")

(defvar mk-global-cache-root "~/.mk-project")

;; ---------------------------------------------------------------------
;; Project Variables
;;
;; These variables are set when a project is loaded and nil'd out when
;; unloaded. These symbols are the same as defined in the 2nd parameter
;; to project-def except for their "mk-proj-" prefix.
;; ---------------------------------------------------------------------

(defvar mk-proj-name nil
  "Name of the current project. Required. First argument to project-def.")

(defvar mk-proj-basedir nil
  "Base directory of the current project. Required. Value is expanded with
expand-file-name. Example: ~me/my-proj/.")

(defvar mk-proj-src-patterns nil
  "List of shell patterns to include in the TAGS file. Optional. Example:
'(\"*.java\" \"*.jsp\").

This value is not used when `mk-proj-src-find-cmd' is set.")

(defvar mk-proj-ignore-patterns nil
  "List of shell patterns to avoid searching for with project-find-file and
project-grep. Optional. Example: '(\"*.class\").

This value is not used in indexing when `mk-proj-index-find-cmd'
is set -- or in grepping when `mk-proj-grep-find-cmd' is set.")

(defvar mk-proj-ack-args nil
  "String of arguments to pass to the `ack' command. Optional.
Example: \"--java\".")

(defvar mk-proj-vcs nil
  "When set to one of the VCS types in `mk-proj-vcs-path', grep
and index commands will ignore the VCS's private files (e.g.,
.CVS/). Example: 'git.

This value is not used in indexing when `mk-proj-index-find-cmd'
is set -- or in grepping when `mk-proj-grep-find-cmd' is set.")

; TODO: support multiple tags file via tags-table-list
(defvar mk-proj-tags-file nil
  "Path to the TAGS file for this project. Optional. Use an absolute path,
not one relative to basedir. Value is expanded with expand-file-name.")

(defvar mk-proj-compile-cmd nil
  "Command to build the entire project. Can be either a string specifying
a shell command or the name of a function. Optional. Example: make -k.")

(defvar mk-proj-install-cmd nil)
(defvar mk-proj-run-cmd nil)
;;(defvar mk-proj-syntaxcheck-cmd nil)

(defvar mk-proj-startup-hook nil
  "Hook function to run after the project is loaded. Optional. Project
variables (e.g. mk-proj-basedir) will be set and can be referenced from this
function.")

(defvar mk-proj-shutdown-hook nil
  "Hook function to run after the project is unloaded. Optional.  Project
variables (e.g. mk-proj-basedir) will still be set and can be referenced
from this function.")

(defvar mk-proj-file-list-cache nil
  "Cache *file-index* buffer to this file. Optional. If set, the *file-index*
buffer will take its initial value from this file and updates to the buffer
via 'project-index' will save to this file. Value is expanded with
expand-file-name.")

(defvar mk-proj-open-files-cache nil
  "Cache the names of open project files in this file. Optional. If set,
project-load will open all files listed in this file and project-unload will
write all open project files to this file. Value is expanded with
expand-file-name.")

(defvar mk-proj-src-find-cmd nil
  "Specifies a custom \"find\" command to locate all files to be
included in the TAGS file (see `project-tags'). Optional. The
value is either a string or a function of one argument that
returns a string. The argument to the function will be the symbol
\"'src\".

If non-null (or if the function returns non-null), the custom
find command will be used and the `mk-proj-src-patterns' and
`mk-proj-vcs' settings are ignored when finding files to include
in TAGS.")

(defvar mk-proj-grep-find-cmd nil
  "Specifies a custom \"find\" command to use as the default when
running `project-grep'. Optional. The value is either a string or
a function of one argument that returns a string. The argument to
the function will be the symbol \"'grep\". The string or returned
string MUST use find's \"-print0\" argument as the results of
this command are piped to \"xargs -0 ...\".

If non-null (or if the function returns non-null), the custom
find command will be used and the `mk-proj-ignore-patterns' and
`mk-proj-vcs' settings will not be used in the grep command.

The custom find command should use \".\" (current directory) as
the path that find starts at -- this will allow the C-u argument
to `project-grep' to run the command from the current buffer's
directory.")

(defvar mk-proj-index-find-cmd nil
  "Specifies a custom \"find\" command to use when building an
listing of all files in the project (to be used by
`project-find-file'). Optional. The value is either a string or a
function of one argument that returns a string. The argument to
the function will be the symbol \"'index\".

If non-null (or if the function returns non-null), the custom
find command will be used and the `mk-proj-ignore-patterns' and
`mk-proj-vcs' settings are not used when in the grep command.")

(defun mk-proj-fib-name (&optional proj-name)
  "Buffer name of the file-list cache. This buffer contains a
list of all the files under the project's basedir (minus those
matching ignore-patterns) or, if index-find-cmd is set, the list
of files found by calling the custom find command.  The list is
used by `project-find-file' to quickly locate project files."
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (concat "*" proj-name " file-index*"))

(defconst mk-proj-vcs-path '((git . ".git")
                             (cvs . ".CVS")
                             (svn . ".svn")
                             (bzr . ".bzr")
                             (hg  . ".hg")
                             (darcs . "_darcs"))
  "When `mk-proj-vcs' is one of the VCS types listed here, ignore
the associated paths when greping or indexing the project. This
value is not used if a custom find command is set in
`mk-proj-grep-find-cmd' or `mk-proj-index-find-cmd'")

(defvar mk-proj-required-vars '(name
                                basedir)
  "Project config vars that are required in every project.

See also `mk-proj-optional-vars' `mk-proj-var-functions' `mk-proj-load-vars'")

(defvar mk-proj-optional-vars '(parent ;; parent needs to come first!
                                src-patterns
                                ignore-patterns
                                ack-args
                                vcs
                                tags-file
                                compile-cmd
                                install-cmd
                                run-cmd
                                ;;syntaxcheck-cmd
                                startup-hook
                                shutdown-hook
                                file-list-cache
                                open-files-cache
                                src-find-cmd
                                grep-find-cmd
                                index-find-cmd
                                etags-cmd
                                patterns-are-regex
                                friends
                                open-friends-cache)
  "Project config vars that are optional.

See also `mk-proj-required-vars' `mk-proj-var-functions' `mk-proj-load-vars'")

(defvar mk-proj-internal-vars '(open-files-cache
                                open-friends-cache
                                file-list-cache
                                tags-file))

(defvar mk-proj-var-functions '((basedir . (lambda (var val &optional proj-name)
                                             (when (stringp val)
                                               (expand-file-name val))))
                                (tags-file . (lambda (var val &optional proj-name)
                                               (if val
                                                   (expand-file-name val)
                                                 (mk-proj-get-cache-path var proj-name))))
                                (file-list-cache . (lambda (var val &optional proj-name)
                                                     (if val
                                                         (expand-file-name val)
                                                       (mk-proj-get-cache-path var proj-name))))
                                (open-files-cache . (lambda (var val &optional proj-name)
                                                      (if val
                                                          (expand-file-name val)
                                                        (mk-proj-get-cache-path var proj-name))))
                                (open-friends-cache . (lambda (var val &optional proj-name)
                                                        (if val
                                                            (expand-file-name val)
                                                          (mk-proj-get-cache-path var proj-name)))))
  "Config vars from `mk-proj-required-vars' and `mk-proj-optional-vars' (except 'name')
can be associated with a function in this association list, which will be
applied to the value of the var right after it is taken from the config-alist.

That means when loading a project, or when querying the configuration with
`mk-proj-get-config-val', the var symbol is to look up a function in this list
and, if present, that function is then applied to the var symbol and var value pair
and its result returned as var value.

See also `mk-proj-load-vars',`mk-proj-get-config-val'.")

(defvar mk-proj-ask-functions '((name . (lambda ()
                                          (read-string "Name: " super)))
                                (basedir . (lambda ()
                                             (expand-file-name (concat "~/" (ido-completing-read "Basedir: " (ido-file-name-all-completions "~"))))))
                                (src-patterns . (lambda ()
                                                  (let ((xs '()))
                                                    (loop for p = (read-string "Source pattern regex: " super) then (read-string (concat "Source pattern regex " (prin1-to-string xs) ": "))
                                                          until (string-equal p "")
                                                          if (condition-case nil (listp (read p)) (error nil))
                                                          append (read p) into xs
                                                          else
                                                          collect p into xs
                                                          finally return xs))))
                                (ignore-patterns . (lambda ()
                                                     (let ((xs '()))
                                                       (loop for p = (read-string "Ignore pattern regex: " super) then (read-string (concat "Ignore pattern regex " (prin1-to-string xs) ": "))
                                                             until (string-equal p "")
                                                             if (condition-case nil (listp (read p)) (error nil))
                                                             append (read p) into xs
                                                             else
                                                             collect p into xs
                                                             finally return xs))))
                                (ack-args . (lambda ()
                                              (read-string "Ack arguments: " super)))
                                (vcs . (lambda ()
                                         (loop for v = (read-string "vcs: " super) then (read-string "vcs: " super)
                                               until (mk-proj-any (lambda (x) (eq (car x) (read v))) mk-proj-vcs-path)
                                               finally return (read v))))
                                (compile-cmd . (lambda ()
                                                 (read-string "Compile command: " super)))
                                (patterns-are-regex . (lambda () t))))

(defvar mk-proj-before-load-hook '())
(defvar mk-proj-after-load-hook '())

(defvar mk-proj-before-unload-hook '())
(defvar mk-proj-after-unload-hook '())

(defvar mk-proj-history '())

;; ---------------------------------------------------------------------
;; Customization
;; ---------------------------------------------------------------------

(defgroup mk-project nil
  "A programming project management library."
  :group 'tools)

(defcustom mk-proj-ack-respect-case-fold t
  "If on and case-fold-search is true, project-ack will ignore case by passing \"-i\" to ack."
  :type 'boolean
  :group 'mk-project)

(defcustom mk-proj-use-ido-selection nil
  "If ido-mode is available, use ido selection where appropriate."
  :type 'boolean
  :group 'mk-project)

(defcustom mk-proj-ack-cmd (if (eq system-type 'windows-nt) "ack.pl" "ack-grep")
  "Name of the ack program to run. Defaults to \"ack\" (or \"ack.pl\" on Windows)."
  :type 'string
  :group 'mk-project)

(defcustom mk-proj-file-index-relative-paths t
  "If non-nil, generate relative path names in the file-index buffer"
  :type 'boolean
  :group 'mk-project)

(defcustom mk-proj-menu-on t
  "If non-nil, define the 'mk-project' menu in the menu-bar at
load time. See also `project-menu-remove'."
  :type 'boolean
  :group 'mk-project)

;; ---------------------------------------------------------------------
;; Utils
;; ---------------------------------------------------------------------

(defun mk-proj-zip (&rest lists)
  (let* (;;(lists (append (list a) rest))
         (n (- (length lists) 1))
         (i 0)
         (rs '()))
    (while (some 'identity (mapcar (lambda (l) (> (length l) i)) lists))
      (setq rs (append rs (list (loop for m from 0 to n
                                      collect (nth i (nth m lists))))))
      (setq i (1+ i)))
    rs))

(defun mk-proj-dirname (path)
  (apply #'concat (reverse (mapcar (lambda (s)
                                     (concat s "/"))
                                   (cdr (reverse (split-string path "/")))))))

(defun mk-proj-filename (path)
  (car (reverse (split-string path "/"))))

(defun mk-proj-replace-tail (str tail-str replacement)
  (if (string-match (concat tail-str "$")  str)
    (replace-match replacement t t str)
    str))

(defun mk-proj-assert-proj ()
  (unless mk-proj-name
    (error "No project is set!")))

(defun mk-proj-maybe-kill-buffer (bufname)
  (let ((b (get-buffer bufname)))
    (when b (kill-buffer b))))

(defun mk-proj-get-vcs-path (&optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (if (or mk-proj-vcs (mk-proj-get-config-val 'vcs proj-name t))
      (cdr (assoc (or mk-proj-vcs (mk-proj-get-config-val 'vcs proj-name t)) mk-proj-vcs-path))
    nil))

(defun mk-proj-has-univ-arg ()
  (eql (prefix-numeric-value current-prefix-arg) 4))

(defun mk-proj-names ()
  (let ((names nil))
    (maphash (lambda (k v) (add-to-list 'names k)) mk-proj-list)
    names))

(defun mk-proj-use-ido ()
  (and (boundp 'ido-mode) mk-proj-use-ido-selection))

(defun mk-proj-find-cmd-val (context &optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((cmd (ecase context
               ('src   (mk-proj-get-config-val 'src-find-cmd proj-name))
               ('grep  (mk-proj-get-config-val 'grep-find-cmd proj-name))
               ('index (mk-proj-get-config-val 'index-find-cmd proj-name)))))
    (if cmd
        (cond ((stringp cmd) cmd)
              ((functionp cmd) (funcall cmd context))
              (t (error "find-cmd is neither a string or a function")))
      nil)))

(defun mk-proj-filter (condp lst)
  "Filter LST with CONDP. All elements for which CONDP returns t will be kept,
all others filtered."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun* mk-proj-any (condp lst)
  "Apply CONDP to all elements of LST, return t as soon as CONDP
yields t."
  (dolist (x lst)
    (when (funcall condp x)
      (return-from "mk-proj-any" x))))

(defun* mk-proj-all (condp lst)
  "Apply CONDP to all elements of LST, return nil as soon as
CONDP yields nil."
  (let ((b t))
    (dolist (x lst b)
      (unless b
        (return-from "mk-proj-all" nil))
       (setq b (funcall condp x)))))

(defun mk-proj-flatten (xs)
  "Takes a list XS of lists and returns a list with only the elements of
all lists within the list XS.

\(mk-proj-flatten '(1 (2 (3 4)))) = '(1 2 3 4)"
  (if (listp xs)
      (let ((ret nil))
        (while xs
          (setq ret (append ret (mk-proj-flatten (car xs))))
          (setq xs (cdr xs)))
        ret)
    (list xs)))


(defmacro mk-proj-assoc-pop (key alist)
  "Like `assoc', but remove the (KEY . value) pair from the ALIST."
  `(let ((result (assoc ,key ,alist)))
     (setq ,alist (delete result ,alist))
     result))

(defun mk-proj-alist-union (alist1 alist2)
  "Make a union alist out of ALIST1 and ALIST2. The second alist
is the one that overwrites values in the first alist if they both
contain a similar key."
  (append (mapcar (lambda (c)
                    (or (mk-proj-assoc-pop (car c) alist2) c)) alist1) alist2))

(defun mk-proj-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun* mk-proj-buffer-has-markers-p (&optional buf)
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (forward-char)
        (when (buffer-has-markers-at (point))
          (return-from "mk-proj-buffer-has-markers-p" t)))
      (return-from "mk-proj-buffer-has-markers-p" nil))))






;; ---------------------------------------------------------------------
;; Project Configuration
;; ---------------------------------------------------------------------

(defvar mk-proj-list (make-hash-table :test 'equal))

(defun mk-proj-find-config (proj-name)
  "Get a projects config-alist from the global projects hashmap."
  (gethash proj-name mk-proj-list))

(defun mk-proj-get-config-val (key &optional proj inherit)
  "Finds the value associated with KEY. A project PROJ
can optionally be specified. Either in form of a config-alist or
a name.
If the third argument INHERIT is non-nil, all parents will queried
for the KEY and the first value that is found is returned."
  (unless (and proj (stringp proj))
    (mk-proj-assert-proj)
    (setq proj mk-proj-name))
  (let* ((proj-alist (cond ((listp proj)
                            proj)
                           (t
                            (mk-proj-find-config proj))))
         (val (if (assoc key proj-alist)
                  (let ((v (cdr (assoc key proj-alist))))
                    ;; check for list, (x . y) vs (x y)
                    ;; I got annoyed by making this mistake too often
                    (if (listp v)
                        (car v)
                      v))
                (let ((parent (car (cdr (assoc 'parent proj-alist)))))
                  (when (and inherit parent)
                    (mk-proj-get-config-val key parent t)))))
         (fn (cdr (assoc key mk-proj-var-functions))))
    (if fn (funcall fn key (unless (some (apply-partially 'eq key) mk-proj-internal-vars) val) proj) val)))

;;(mk-proj-get-config-val 'file-list-cache "mk-project:overlay for todos in project buffers" nil)

(defalias 'mk-proj-config-val 'mk-proj-get-config-val
  "Alias for `mk-proj-get-config-val' to ensure backward compatibility.")

(defun mk-proj-set-config-val (key value &optional proj-name)
  "Set the value associated with KEY to VALUE in config of project NAME.

Works only for projects defined in org-mode. See also `mk-proj-config-get-val'"
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((marker (mk-proj-get-config-val 'org-marker proj-name)))
    (if marker
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char (marker-position marker))
            (if (cdr (assoc key (mk-org-symbol-table)))
                (progn
                  (org-set-property (cdr (assoc key (mk-org-symbol-table))) (prin1-to-string value))
                  (setf (symbol-value (intern (concat "mk-proj-" (symbol-name key)))) value)
                  ;; (append `((,name ,key)) (mk-proj-filter (lambda (x)
                  ;;                                           (not (eq (car x) key)))
                  ;;                                         (mk-proj-find-config proj-name)))
                  (project-def proj-name `((,key ,value)) t)
                  (let ((mod (buffer-modified-p)))
                    (save-buffer)
                    (set-buffer-modified-p mod)))
              (error "mk-proj-set-config-val: could not find property string for key"))))
      ;; alternative that finds and modifies a project-def lisp definition
      (when mk-proj-config-file
        (with-current-buffer (find-file-noselect mk-proj-config-file)
          (save-excursion
            (goto-char 0)
            ;; find the project-def call that defines name, abort
            ;; when multiple matches are found
            (let ((re (concat "\\s-*\(project-def.*\"" proj-name "\""))
                  (matches '()))
              (while (re-search-forward re nil t)
                (setq matches (append matches `(,(point)))))
              (when (= (length matches) 1)
                (let ((re2 (concat "\(" (symbol-name key) ".*\)+$")))
                  ;; either change existing (key value) pair or add new one
                  (if (re-search-forward re2 nil t)
                      (let* ((full (match-string 0))
                             (open (apply #'+ (mapcar (lambda (c) (if (eq c ?\() 1 0)) full)))
                             (close (apply #'+ (mapcar (lambda (c) (if (eq c ?\)) 1 0)) full))))
                        ;; check if more open then closing parens, just abort if that is the case
                        ;; most likely we are looking at a multi-line lambda then
                        (unless (< (- close open) 0)
                          (replace-match (concat "(" (symbol-name key) " " (prin1-to-string value) ")"))
                          (insert (make-string (- close open) ?\)))))
                    ;; if re2 is not found no such key exists and we add a new (key value) pair
                    (progn
                      (end-of-sexp)
                      (beginning-of-line)
                      (when (re-search-forward "\\(\(.*\\)\\([\)]++\\)$" nil t)
                        (let ((full (match-string 0))
                              (one (match-string 1))
                              (two (match-string 2)))
                          (replace-match (concat "(" (symbol-name key) " " (prin1-to-string value) ")\n"))
                          (indent-according-to-mode)
                          (insert full))))))))
            (let ((mod (buffer-modified-p)))
              (save-buffer)
              (set-buffer-modified-p mod))))))))

(defun* project-def (proj-name &optional config-alist inherit)
  "Associate the settings in CONFIG-ALIST with project PROJ-NAME.
Specify an optional project to inherit all settings from the
INHERIT argument. If INHERIT is t, then reuse project settings
of already existing project with the same name as the one which is
to be defined.

Values obtained through inheritance are not determined when the
project is defined, they are determined when `mk-proj-get-config-val'
is used to get a config value of a project.

All values within CONFIG-ALIST will be evaluated when they look
like a lisp expression or symbol. So make sure to quote lists!

See also `project-undef'."
  (interactive)
  ;; I changed the behaviour of this function to evaluate the config values (when they look like something
  ;; that could be evaluated)
  ;; thats rather nasty because now lists must be quoted or else project definition will fail
  ;; on the other hand it enables my org-mode integration to have its property values evaluated
  ;; EDIT: I think I made it backwards compatible through a condition-case, lets see if anyone complains...
  ;; (unless proj-name
  ;;   (if (assoc 'name mk-proj-ask-functions)
  ;;       (let ((super (when inherit (mk-proj-get-config-val 'name inherit t))))
  ;;         (setq proj-name (funcall (cdr (assoc 'name mk-proj-ask-functions)))))
  ;;     (error "project-def: no name given!")))
  (let* ((evaluated-config-alist (let ((evaluated-config-alist `((name ,proj-name))))
                                   (dolist (cv config-alist evaluated-config-alist)
                                     (let* ((key (car cv))
                                            ;; super behaves like a keyword that can be used within a configuration
                                            ;; to refer to the parents value (if inherit has been specified)
                                            ;; I haven't tested this, it is a experimental feature
                                            (super (when inherit (mk-proj-get-config-val key (if (stringp inherit) inherit proj-name) t)))
                                            (lisp (car (cdr cv)))
                                            (value (condition-case nil (eval lisp) (error lisp))))
                                       (unless (eq key 'name)
                                         (setq evaluated-config-alist (append `((,key ,value)) evaluated-config-alist)))))))
         (combined-alist (cond ((eq inherit nil)
                                ;; no inherit -> use only the config given (after evaluation)
                                evaluated-config-alist)
                               ((or (eq inherit t) (eq inherit proj-name))
                                ;; inherit == proj-name -> use union of existing config and evaluated-config-alist
                                (mk-proj-alist-union (gethash proj-name mk-proj-list) evaluated-config-alist))
                               ((and inherit (gethash proj-name mk-proj-list))
                                ;; the proj-name exists already -> same as previous case, but append parent
                                (append (mk-proj-alist-union (gethash proj-name mk-proj-list) evaluated-config-alist)
                                        (unless (eq inherit t) `((parent ,inherit)))))
                               (t
                                ;; non of the other cases matched -> use the config-alist and append parent
                                ;; why only append parent and no union in the last two cases? because the actual
                                ;; inherited values are not determined at time of definition but at runtime
                                ;; by mk-proj-get-config-val
                                (append evaluated-config-alist `((parent ,inherit)))))))
    (puthash proj-name combined-alist mk-proj-list)))

(defvar mk-proj-guess-functions '((name . ((('basedir)
                                            '(let ((name (car (reverse (mk-proj-filter #'string-to-list
                                                                                       (split-string basedir) "/")))))
                                               (unless (gethash proj-name mk-proj-list)
                                                 `(100 . ,name))))
                                           (('buffer)
                                            '(progn
                                               (unless buffer
                                                 (setq buffer (current-buffer)))
                                               `(10 . ,(car (split-string (mk-proj-filename (buffer-file-name buffer)) "\\.")))))))
                                  (src-patterns . ((('mode)
                                                    '(progn
                                                       (unless mode
                                                         (setq mode major-mode))
                                                       `(10 . ,(loop for buf in (buffer-list)
                                                                     if (eq (with-current-buffer buf major-mode) mode)
                                                                     append (list (regexp-quote (mk-proj-filename (buffer-file-name buf))))))))))
                                  (basedir . ((('buffer)
                                               `(10 . ,(mk-proj-basedir (buffer-file-name buffer))))))
                                  (vcs . ((('basedir)
                                           '(let ((r nil))
                                              (loop for f in (directory-files (mk-proj-basename (buffer-file-name (current-buffer))))
                                                    until (setq r (mk-proj-any (lambda (y) (string-equal (cdr y) ".git")) mk-proj-vcs-path))
                                                    finally return `(10 . ,(car r)))))))))

;; (loop for buf in (buffer-list)
;;       if (eq (with-current-buffer buf major-mode) major-mode)
;;       append (list (regexp-quote (mk-proj-filename (buffer-file-name buf)))))

(defun mk-proj-config-guess-alist (alist)
  )

(defun mk-proj-config-insert (&optional config-alist proj-name insert-undefined)
  (unless proj-name
    (setq proj-name (or (cadr (assoc 'name config-alist)) "NewProject")))
  (insert (concat "(project-def \"" proj-name "\" '("))
  (loop for k in (append mk-proj-required-vars mk-proj-optional-vars)
        if (not (or (eq k 'name)
                    (mk-proj-any (lambda (j) (eq k j)) mk-proj-internal-vars)))
        do (when (or insert-undefined
                     (assoc k config-alist))
             (insert (concat "(" (symbol-name k) " " (prin1-to-string (cadr (assoc k config-alist))) ")"))
             (unless (eq k (car (last mk-proj-optional-vars)))
               (newline))
             (indent-according-to-mode)))
  (insert "))\n"))

(defvar mk-proj-config-file "/home/lazor/.emacs.d/init-mkproject.el")
(defvar mk-proj-config-section ";; autogenerated projects")

(defun mk-proj-config-save (config-alist &optional proj-name)
  (with-current-buffer (find-file-noselect mk-proj-config-file)
    (goto-char 0)
    (when (or (search-forward mk-proj-config-section nil t)
              (progn
                (end-of-buffer)
                (newline)
                (newline)
                (insert mk-proj-config-section)
                (newline)
                nil))
      (end-of-line)
      (newline))
    (mk-proj-config-insert config-alist proj-name)
    (let ((mod (buffer-modified-p)))
      (save-buffer)
      (set-buffer-modified-p mod))))

(defun* mk-proj-config-buffer (&optional (state :create) config-alist)
  (case state
    (:create
     (let* ((proj-b (current-buffer))
            (buf (get-buffer-create "*mk-proj: new project*"))
            (window (display-buffer buf)))
       (select-window window)
       (set-window-dedicated-p window t)
       (emacs-lisp-mode)
       (buffer-disable-undo)
       (mk-proj-config-insert nil "NewProject" t)
       (goto-char 0)
       (end-of-line)
       (mk-proj-new-project-mode)
       (buffer-enable-undo)))
    (:finalize
     (let ((result nil))
       (while (not (setq result (condition-case nil (eval (read (buffer-string))) (error nil)))))
       (mk-proj-config-save result)
       (kill-buffer (buffer-name))))))

(defvar mk-proj-config-function 'mk-proj-config-buffer)

(defun* project-new ()
  (interactive)
  (funcall mk-proj-config-function))

(defvar mk-proj-new-project-mode-map (make-sparse-keymap))

(defvar mk-proj-new-project-mode-hook nil)

(define-minor-mode mk-proj-new-project-mode nil nil " NewProject" mk-proj-new-project-mode-map
  (run-hooks 'mk-proj-new-project-mode-hook))

(define-key mk-proj-new-project-mode-map "\C-c\C-c"
  (lambda ()
    (interactive)
    (funcall mk-proj-config-function :finalize)))
(define-key mk-proj-new-project-mode-map "\C-c\C-k"
  (lambda ()
    (interactive)
    (kill-buffer (buffer-name))))














(defun project-undef (name)
  "Opposite of `project-define'."
  (remhash proj-name mk-proj-list))

(defun mk-proj-proj-vars ()
  "This returns a list of all proj-vars as symbols.
Replaces the old mk-proj-proj-vars constant."
  (mapcar (lambda (var)
            (intern (concat "mk-proj-" (symbol-name var))))
          (append mk-proj-required-vars mk-proj-optional-vars)))

(defun mk-proj-defaults ()
  "Set all default values for project variables"
    (dolist (var (mk-proj-proj-vars))
      (set var nil)))

(defun mk-proj-load-vars (proj-name proj-alist)
  "Set project variables from proj-alist. A project variable is what
a config variable becomes after loading a project. Essentially
a global lisp symbol with the same name as the config variable
prefixed by 'mk-proj-'. For example, the basedir config var becomes
mk-proj-basedir in global scope.

See also `mk-proj-required-vars' `mk-proj-optional-vars' `mk-proj-var-functions'"
  (catch 'mk-proj-load-vars
    (labels ((maybe-set-var (var)
                            (let ((proj-var (intern (concat "mk-proj-" (symbol-name var))))
                                  (val (mk-proj-get-config-val var proj-name t))
                                  ;;(fn (cdr (assoc var mk-proj-var-functions)))
                                  )
                              (setf (symbol-value proj-var) val))))
      (mk-proj-defaults)
      (let ((required-vars (mk-proj-filter (lambda (s)
                                             (not (string-equal (symbol-name s) "name")))
                                           mk-proj-required-vars)))
        (setq mk-proj-name proj-name)
        (dolist (v required-vars)
          (unless (mk-proj-config-val v proj-name t)
            (mk-proj-defaults)
            (throw 'mk-proj-load-vars v))
          (maybe-set-var v)))
      (dolist (v mk-proj-optional-vars)
        (maybe-set-var v)))))

(defun mk-proj-get-cache-path (symbol &optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((directory (concat mk-global-cache-root
                           (cond ((mk-proj-get-config-val 'parent proj-name)
                                  (let ((a (concat "/" (mk-proj-join "/" (mk-proj-ancestry proj-name)))))
                                    (if (mk-proj-get-config-val 'basedir proj-name) a (concat a "/"))))
                                 (t
                                  (concat "/" proj-name "/")))))
        (file (concat (symbol-name symbol))))
    (make-directory directory t)
    (let ((r (concat directory file)))
      (cond ((file-exists-p r)
             r)
            ((and (mk-proj-get-config-val 'parent proj-name) (file-exists-p (mk-proj-get-config-val 'parent proj-name)))
             (progn
               (copy-file (mk-proj-get-cache-path symbol (mk-proj-get-config-val 'parent proj-name))
                          r)
               r))
            (t
             r)))))

(defun mk-proj-join (delimiter strings)
  (reduce (lambda (a b)
            (concatenate 'string a delimiter b))
          strings))

(defun mk-proj-ancestry (&optional proj-name)
  (let* ((current (or proj-name
                      (progn
                        (mk-proj-assert-proj)
                        mk-proj-name)))
         (ancestry `(,current)))
    (while (mk-proj-config-val 'parent current)
      (setq ancestry (cons (mk-proj-config-val 'parent current) ancestry)
            current (mk-proj-config-val 'parent current)))
    ancestry))

(defun mk-proj-load (proj-name)
  (interactive)
  (let ((oldname mk-proj-name))
    (unless proj-name
      (error "mk-proj-load: proj-name should not be nil"))
    (run-hooks 'mk-proj-before-load-hook)
    (let ((proj-config (mk-proj-find-config proj-name)))
      (unless (or (string= oldname proj-name) (eq proj-config nil))
        (project-unload))
      (if proj-config
          (let ((v (mk-proj-load-vars proj-name proj-config)))
            (when v
              (error "Required config value '%s' missing in %s!" (symbol-name v) proj-name)))
        (error "Project %s does not exist!" proj-name)))
    (when (not (file-directory-p mk-proj-basedir))
      (error "Base directory %s does not exist!" mk-proj-basedir))
    (when (and mk-proj-vcs (not (mk-proj-get-vcs-path)))
      (error "Invalid VCS setting!"))
    (message "Loading project %s ..." proj-name)
    (cd mk-proj-basedir)
    (mk-proj-tags-load)
    (mk-proj-fib-init)
    (add-hook 'kill-emacs-hook 'mk-proj-kill-emacs-hook)
    (when mk-proj-startup-hook
      (run-hooks 'mk-proj-startup-hook))
    (mk-proj-visit-saved-open-files)
    (mk-proj-visit-saved-open-friends)
    (run-hooks 'mk-proj-after-load-hook)
    (message "Loading project %s done" proj-name)))

(defun project-load (&optional proj-name)
  "Load a project's settings."
  (interactive)
  (let ((name (or proj-name
                  (if (mk-proj-use-ido)
                      (ido-completing-read "Project Name (ido): " (mk-proj-names))
                    (completing-read "Project Name: " (mk-proj-names))))))
    (mk-proj-load name)))

(defun mk-proj-kill-emacs-hook ()
  "Ensure we save the open-files-cache info on emacs exit"
  (when (and mk-proj-name mk-proj-open-files-cache)
    (mk-proj-save-open-file-info))
  (when (and mk-proj-friends mk-proj-open-friends-cache)
    (mk-proj-save-open-friends-info)))

(defun mk-proj-unload-vars ()
  (let ((vars (append mk-proj-required-vars mk-proj-optional-vars))
        (last-alist '()))
    (dolist (var vars)
      (let ((sym (intern-soft (concat "mk-proj-" (symbol-name var)))))
        (when (and (boundp sym)
                   (eval sym))
          (add-to-list 'last-alist `(,sym ,(eval sym))))))
    (when last-alist
      (add-to-list 'mk-proj-history last-alist))
    (mk-proj-defaults)))

(defun project-unload (&optional arg)
  "Unload the current project's settings after running the shutdown hook."
  (interactive "P")
  (when mk-proj-name
    (condition-case nil
        (progn
          (message "Unloading project %s" mk-proj-name)
          (run-hooks 'mk-proj-before-unload-hook)
          (mk-proj-tags-clear)
          (mk-proj-maybe-kill-buffer (mk-proj-fib-name))
          (mk-proj-save-open-file-info)
          (mk-proj-save-open-friends-info)
          (and (or (mk-proj-buffers) (mk-proj-friendly-buffers))
               (not arg)
               (y-or-n-p (concat "Close all " mk-proj-name " project files? "))
               (project-close-files)
               (project-close-friends))
          (when mk-proj-shutdown-hook (run-hooks 'mk-proj-shutdown-hook))
          (run-hooks 'mk-proj-project-unload-hook)
          (run-hooks 'mk-proj-after-unload-hook))
      (error nil)))
  (mk-proj-unload-vars)
  (when (buffer-file-name (current-buffer))
    (cd (mk-proj-dirname (buffer-file-name (current-buffer)))))
  (message "Project settings have been cleared"))

(defun project-close-files ()
  "Close all unmodified files that reside in the project's basedir"
  (interactive)
  (mk-proj-assert-proj)
  (let ((closed nil)
        (dirty nil)
        (basedir-len (length mk-proj-basedir)))
    (dolist (b (mk-proj-buffers))
      (cond
       ((or (buffer-modified-p b) (string-equal (buffer-name b) "*scratch*"))
        (push (buffer-name b) dirty))
       (t
        (push (buffer-name b) closed)
        (kill-buffer b))))
    (message "Closed %d buffers, %d modified buffers where left open"
             (length closed) (length dirty))))

(defun mk-proj-buffer-name (buf)
  "Return buffer's name based on filename or dired's location"
  (let ((file-name (or (buffer-file-name buf)
                       (with-current-buffer buf list-buffers-directory))))
    (if file-name
        (expand-file-name file-name)
      nil)))

(defun mk-proj-buffer-p (buf)
  "Is the given buffer in our project, is a file opened? Also detects dired buffers open to basedir/*"
  (let ((file-name (mk-proj-buffer-name buf)))
    (if (and file-name
             (file-exists-p file-name)
             (string-match (concat "^" (regexp-quote mk-proj-basedir)) file-name))
        t
      nil)))

(defun mk-proj-buffers ()
  "Get a list of buffers that reside in this project's basedir"
  (mk-proj-assert-proj)
  (let ((buffers nil))
    (dolist (b (buffer-list))
      (when (mk-proj-buffer-p b) (push b buffers)))
    buffers))

(defun mk-proj-special-buffers ()
  (mk-proj-assert-proj)
  (append (remove-if (lambda (buf) (not (string-match "\*[^\*]\*" (buffer-name buf)))) (mk-proj-buffers))
          (remove-if (lambda (buf) (or (and (symbolp 'mk-org-project-buffer-name)
                                            (not (string-equal (mk-org-project-buffer-name) (buffer-name buf))))
                                       (compilation-buffer-p buf)))
                     (buffer-list))))

(defun project-status (&optional proj-name)
  "View project's variables."
  (interactive)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (if mk-proj-basedir
      (let ((b (get-buffer-create "*mk-proj: project-status*")))
        (with-current-buffer b
          (kill-region (point-min) (point-max))
          (dolist (v (append mk-proj-required-vars mk-proj-optional-vars))
            (insert (format "%-24s = %s\n" (symbol-name v) (mk-proj-config-val v proj-name t)))))
        (when (not (eq b (current-buffer)))
          (switch-to-buffer-other-window b)))
    (message "No project loaded.")))

(defun mk-proj-proj-names ()
  "All the projects names."
  (interactive)
  (let ((names '()))
    (maphash (lambda (k p)
               (progn
                 (setq names (append names `(,k)))))
               mk-proj-list)
    names))

(defun project-names ()
  (interactive)
  (loop for s in (mk-proj-proj-names)
        do (message "%s" s)))

;; ---------------------------------------------------------------------
;; Save/Restore open files
;; ---------------------------------------------------------------------

(defun mk-proj-save-open-file-info ()
  "Write the list of `files' to a file"
  (when mk-proj-open-files-cache
    (with-temp-buffer
      (dolist (f (mapcar (lambda (b) (mk-proj-buffer-name b)) (mk-proj-buffers)))
        (when f
          (unless (string-equal mk-proj-tags-file f)
            (insert f "\n"))))
      (if (file-writable-p mk-proj-open-files-cache)
          (progn
            (write-region (point-min)
                          (point-max)
                          mk-proj-open-files-cache)
            (message "Wrote open files to %s" mk-proj-open-files-cache))
        (message "Cannot write to %s" mk-proj-open-files-cache)))))

(defun mk-proj-visit-saved-open-files ()
  (when mk-proj-open-files-cache
    (when (file-readable-p mk-proj-open-files-cache)
      (message "Reading open files from %s" mk-proj-open-files-cache)
      (with-temp-buffer
        (insert-file-contents mk-proj-open-files-cache)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((start (point)))
            (while (not (eolp)) (forward-char)) ; goto end of line
            (let ((line (buffer-substring start (point))))
              (message "Attempting to open %s" line)
              (find-file-noselect line t)))
          (forward-line))))))

;; ---------------------------------------------------------------------
;; Etags
;; ---------------------------------------------------------------------

(defun mk-proj-tags-load ()
  "Load TAGS file (if tags-file set)"
  (mk-proj-tags-clear)
  (setq tags-file-name  mk-proj-tags-file
        tags-table-list nil)
  (when (and mk-proj-tags-file (file-readable-p mk-proj-tags-file))
    (visit-tags-table mk-proj-tags-file)))

(defun mk-proj-tags-clear ()
  "Clear the TAGS file (if tags-file set)"
  (when (and mk-proj-tags-file (get-file-buffer mk-proj-tags-file))
    (mk-proj-maybe-kill-buffer (get-file-buffer mk-proj-tags-file)))
  (setq tags-file-name  nil
        tags-table-list nil))

(defun mk-proj-etags-cb (process event)
  "Visit tags table when the etags process finishes."
  (message "Etags process %s received event %s" process event)
  (kill-buffer (get-buffer "*etags*"))
  (cond
   ((string= event "finished\n")
    (mk-proj-tags-load)
    (message "Refreshing TAGS file %s...done" mk-proj-tags-file))
   (t (message "Refreshing TAGS file %s...failed" mk-proj-tags-file))))

(defun project-tags ()
  "Regenerate the project's TAG file. Runs in the background."
  (interactive)
  (mk-proj-assert-proj)
  (if mk-proj-tags-file
      (let* ((tags-file-name (file-name-nondirectory mk-proj-tags-file))
             ;; If the TAGS file is in the basedir, we can generate
             ;; relative filenames which will allow the TAGS file to
             ;; be relocatable if moved with the source. Otherwise,
             ;; run the command from the TAGS file's directory and
             ;; generate absolute filenames.
             (relative-tags (string= (file-name-as-directory mk-proj-basedir)
                                     (file-name-directory mk-proj-tags-file)))
             (default-directory (file-name-as-directory
                                 (file-name-directory mk-proj-tags-file)))
             (default-find-cmd (concat "find '" (if relative-tags "." mk-proj-basedir)
                                       "' -type f "
                                       (mk-proj-find-cmd-src-args mk-proj-src-patterns)))
             (etags-shell-cmd (if mk-proj-etags-cmd
                                  mk-proj-etags-cmd
                                "etags -o"))
             (etags-cmd (concat (or (mk-proj-find-cmd-val 'src) default-find-cmd)
                                " | " etags-shell-cmd " '" tags-file-name "' - "))
             (proc-name "etags-process"))
        (message "project-tags default-dir %s" default-directory)
        (message "project-tags cmd \"%s\"" etags-cmd)
        (message "Refreshing TAGS file %s..." mk-proj-tags-file)
        (start-process-shell-command proc-name "*etags*" etags-cmd)
        (set-process-sentinel (get-process proc-name) 'mk-proj-etags-cb))
    (message "mk-proj-tags-file is not set")))

(defun mk-proj-find-cmd-src-args (src-patterns &optional proj-name)
  "Generate the ( -name <pat1> -o -name <pat2> ...) pattern for find cmd"
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (if src-patterns
      (let ((name-expr " \\(")
            (regex-or-name-arg (if (mk-proj-get-config-val 'patterns-are-regex proj-name)
                                   "-regex"
                                 "-name")))
        (dolist (pat src-patterns)
          (setq name-expr (concat name-expr " " regex-or-name-arg " \"" pat "\" -o ")))
        (concat (mk-proj-replace-tail name-expr "-o " "") "\\) "))
    ""))

(defun mk-proj-find-cmd-ignore-args (ignore-patterns &optional proj-name)
  "Generate the -not ( -name <pat1> -o -name <pat2> ...) pattern for find cmd"
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (if ignore-patterns
      (concat " -not " (mk-proj-find-cmd-src-args ignore-patterns proj-name))
    ""))

;; ---------------------------------------------------------------------
;; Grep
;; ---------------------------------------------------------------------

(defun project-grep (&optional phrase from-current-dir)
  "Run find-grep on the project's basedir, excluding files in
mk-proj-ignore-patterns, tag files, etc.

If the phrase argument is not included, it will prompt for a
search phrase.  If the from-current-dir argument is true, or with
C-u prefix, start from the current directory."
  (interactive)
  (mk-proj-assert-proj)
  (let* ((wap (word-at-point))
         (regex (or phrase
                    (if wap (read-string (concat "Grep project for (default \"" wap "\"): ") nil nil wap)
                      (read-string "Grep project for: "))))
         (find-cmd "find . -type f")
         (grep-cmd (concat "grep -i -n \"" regex "\""))
         (default-directory (file-name-as-directory
                             (if (or from-current-dir (mk-proj-has-univ-arg))
                                 default-directory
                               mk-proj-basedir))))
    (when mk-proj-ignore-patterns
      (setq find-cmd (concat find-cmd (mk-proj-find-cmd-ignore-args mk-proj-ignore-patterns))))
    (when mk-proj-tags-file
      (setq find-cmd (concat find-cmd " -not -name 'TAGS'")))
    (when (mk-proj-get-vcs-path)
      (setq find-cmd (concat find-cmd " -not -path " (concat "'*/" (mk-proj-get-vcs-path) "/*'"))))
    (let* ((whole-cmd (concat (or (mk-proj-find-cmd-val 'grep)
                                  (concat find-cmd " -print0"))
                              " | xargs -0 -e " grep-cmd))
           (confirmed-cmd (read-string "Grep command: " whole-cmd nil whole-cmd)))
      (message "project-grep cmd: \"%s\"" confirmed-cmd)
      (grep-find confirmed-cmd))))

;; ---------------------------------------------------------------------
;; Ack (betterthangrep.com)
;; ---------------------------------------------------------------------

(define-compilation-mode ack-mode "Ack" "Ack compilation mode." nil)

(defvar mk-proj-ack-default-args "--nocolor --nogroup")

(defun mk-proj-ack-cmd (regex)
  "Generate the ack command string given a regex to search for."
  (concat mk-proj-ack-cmd " "
          mk-proj-ack-default-args " "
          (if (and mk-proj-ack-respect-case-fold case-fold-search) "-i " "")
          mk-proj-ack-args " "
          regex))

(defun project-ack (&optional phrase from-current-dir)
  "Run ack from project's basedir, using the `ack-args' configuration.
With C-u prefix, start ack from the current directory."
  (interactive)
  (mk-proj-assert-proj)
  (let* ((wap (word-at-point))
         (regex (or phrase
                    (if wap (read-string (concat "Ack project for (default \"" wap "\"): ") nil nil wap)
                  (read-string "Ack project for: "))))
         (whole-cmd (mk-proj-ack-cmd regex))
         (confirmed-cmd (read-string "Ack command: " whole-cmd nil whole-cmd))
         (default-directory (file-name-as-directory
                             (if (or from-current-dir (mk-proj-has-univ-arg))
                                 default-directory
                               mk-proj-basedir))))
    (compilation-start confirmed-cmd 'ack-mode)))

;; ---------------------------------------------------------------------
;; Compile
;; ---------------------------------------------------------------------

(defun project-make (&optional opts cmd proj-name)
 "Run the compile command (string or function) for this project."
 (interactive)
 (unless proj-name
   (mk-proj-assert-proj)
   (setq proj-name mk-proj-name))
 (unless cmd
   (setq cmd (mk-proj-get-config-val 'compile-cmd proj-name)))
 (let ((default-directory mk-proj-basedir))
   (cond ((stringp cmd)
          (when (and (null opts) (called-interactively-p))
            (setq opts (read-string (concat "Compile options (" cmd "):"))))
          (compile (concat cmd " " opts)))
         ((fboundp cmd)
          (cond ((commandp cmd)
                 (call-interactively cmd))
                (opts
                 (funcall cmd opts))
                (t (funcall cmd))))
         (t (message "No compile command defined.")))))

(defun project-compile (&optional opts)
  (interactive)
  (mk-proj-assert-proj)
  (project-make opts mk-proj-compile-cmd))

(defun project-install (&optional opts)
  (interactive)
  (mk-proj-assert-proj)
  (project-make opts mk-proj-install-cmd))

(defun project-run (&optional opts)
  (interactive)
  (mk-proj-assert-proj)
  (project-make opts mk-proj-run-cmd))

(defun project-install-and-run (&optional opts1 opts2)
  (interactive)
  (mk-proj-assert-proj)
  (project-make opts1 mk-proj-install-cmd)
  (project-make opts2 mk-proj-run-cmd))

;; (defun project-syntaxcheck (&optional opts)
;;   (interactive)
;;   (mk-proj-assert-proj)
;;   (project-make opts mk-proj-syntaxcheck-cmd))

;; (defun project-compile (&optional opts)
;;   "Run the compile command for this project."
;;   (interactive)
;;   (mk-proj-assert-proj)
;;   (project-home)
;;   (if (stringp mk-proj-compile-cmd)
;;       (if opts
;;           (funcall 'mk-proj-compile opts)
;;         (call-interactively 'mk-proj-compile))
;;     (if (fboundp mk-proj-compile-cmd)
;;         (if (commandp mk-proj-compile-cmd)
;;             (call-interactively mk-proj-compile-cmd)
;;           (funcall mk-proj-compile-cmd)))))

;; ---------------------------------------------------------------------
;; Dired
;; ---------------------------------------------------------------------

(defun project-dired ()
  "Open dired in the project's basedir (or jump to the existing dired buffer)"
  (interactive)
  (mk-proj-assert-proj)
  (dired mk-proj-basedir))

;; ---------------------------------------------------------------------
;; Find-file
;; ---------------------------------------------------------------------

(defun mk-proj-fib-init (&optional proj-name)
  "Either load the *file-index* buffer from the file cache, or create it afresh."
  (if (and (mk-proj-get-config-val 'file-list-cache proj-name t)
           (file-readable-p (mk-proj-get-config-val 'file-list-cache proj-name t)))
      (with-current-buffer (find-file-noselect (mk-proj-get-config-val 'file-list-cache proj-name t))
          (with-current-buffer (rename-buffer (mk-proj-fib-name proj-name))
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            (message (concat "Loading " (mk-proj-fib-name proj-name) " from %s") (mk-proj-get-config-val 'file-list-cache proj-name t))))
    (project-index proj-name)))

;;(mk-proj-fib-init nil)
;;(setq proj-name nil)

(defun mk-proj-fib-clear (&optional proj-name)
  "Clear the contents of the fib buffer"
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((buf (get-buffer (mk-proj-fib-name proj-name))))
    (when buf
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (kill-region (point-min) (point-max))
        (setq buffer-read-only t)))))

(defun mk-proj-fib-cb (process event &optional proj-name)
  "Handle failure to complete fib building"
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  ;;(print proj-name)
  (cond
   ((string= event "finished\n")
    (with-current-buffer (get-buffer (mk-proj-fib-name proj-name))
      (setq buffer-read-only t)
      (when (mk-proj-get-config-val 'file-list-cache proj-name t)
        (write-file (mk-proj-get-config-val 'file-list-cache proj-name t))
        (rename-buffer (mk-proj-fib-name proj-name))))
    (message "Refreshing %s buffer...done" (mk-proj-fib-name proj-name)))
   (t
    (mk-proj-fib-clear proj-name)
    (message "Failed to generate the %s buffer!" (mk-proj-fib-name proj-name)))))

(defun project-index (&optional proj-name)
  "Regenerate the *file-index* buffer that is used for project-find-file"
  (interactive)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (when (mk-proj-get-config-val 'file-list-cache proj-name t)
    (mk-proj-fib-clear proj-name)
    (let* ((default-directory (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t)))
           (start-dir (if mk-proj-file-index-relative-paths "." mk-proj-get-config-val 'basedir proj-name t))
           (find-cmd (concat "find '" start-dir "' -type f "
                             (mk-proj-find-cmd-src-args mk-proj-src-patterns proj-name)
                             (mk-proj-find-cmd-ignore-args mk-proj-ignore-patterns proj-name)))
           (proc-name "index-process"))
      (when (mk-proj-get-vcs-path)
        (setq find-cmd (concat find-cmd " -not -path " (concat "'*/" (mk-proj-get-vcs-path) "/*'"))))
      (setq find-cmd (or (mk-proj-find-cmd-val 'index proj-name) find-cmd))
      (with-current-buffer (get-buffer-create (mk-proj-fib-name proj-name))
        (buffer-disable-undo) ;; this is a large change we don't need to undo
        (setq buffer-read-only nil))
      (message "project-index cmd: \"%s\"" find-cmd)
      (message "Refreshing %s buffer..." (mk-proj-fib-name proj-name))
      (start-process-shell-command proc-name (mk-proj-fib-name proj-name) find-cmd)
      (set-process-sentinel (get-process proc-name) 'mk-proj-fib-cb)
      )))

(defun mk-proj-fib-matches (&optional regex proj-name)
  "Return list of files in *file-index* matching regex.

If regex is nil, return all files. Returned file paths are
relative to the project's basedir."
  (let ((files '()))
    (unless (get-buffer (mk-proj-fib-name proj-name))
      (mk-proj-fib-init proj-name))
    (with-current-buffer (mk-proj-fib-name proj-name)
      (goto-char (point-min))
      (while
          (progn
            (let ((raw-file (mk-proj-normalize-drive-letter
                             (buffer-substring
                              (line-beginning-position) (line-end-position)))))
              (when (> (length raw-file) 0)
                ;; file names in buffer can be absolute or relative to basedir
                (let ((file (if (file-name-absolute-p raw-file)
                                (file-relative-name raw-file (mk-proj-get-config-val 'basedir proj-name))
                              raw-file)))
                  (if regex
                      (when (string-match regex file) (add-to-list 'files file))
                    (add-to-list 'files file)))
                (= (forward-line) 0))))) ; loop test
      (sort files #'string-lessp))))

(defun mk-proj-normalize-drive-letter (file)
  "Convert drive letters to lowercase to be compatible with
file-relative-name, file-name-as-directory"
  (if (or (null file) (< (length file) 2))
      file
    (let ((c1 (aref file 0))
          (c2 (aref file 1)))
      (if (and (= (aref ":" 0) c2)
               (and (>= c1 (aref "A" 0)) (<= c1 (aref "Z" 0))))
          (concat (char-to-string (+ c1 32)) (substring file 1))
        file))))

(defun* project-match-file (regex)
  "Ask for REGEX and match files in project accordingly."
  (interactive "sFind file in project matching: ")
  (project-find-file regex))

(defun* project-find-file (&optional regex)
  "Find file in the current project.

The files listed in buffer *file-index* are scanned for REGEX
matches. If only one match is found, the file is opened
automatically. If more than one match is found, prompt for
completion.

If REGEX is nil all project files are matched.

See also: `project-index', `project-find-file-ido'."
  (mk-proj-assert-proj)
  (unless (get-buffer (mk-proj-fib-name))
    (message "Please use project-index to create the index before running project-find-file")
    (return-from "project-find-file" nil))
  (let* ((matches (mk-proj-fib-matches (or regex ".*")))
         (match-cnt (length matches)))
    (cond
     ((= 0 match-cnt)
      (message "No matches for \"%s\" in this project" regex))
     ((= 1 match-cnt )
      (find-file (car matches)))
     (t
      (let ((file (if (mk-proj-use-ido)
                      (ido-completing-read "Select match (ido): " matches)
                    (completing-read "Select match: " matches))))
        (when file
          (find-file (concat (file-name-as-directory mk-proj-basedir) file))))))))

(defun* project-find-file-ido ()
  "Find file in the current project using 'ido'.

Choose a file to open from among the files listed in buffer
*file-index*.  The ordinary 'ido' methods allow advanced
selection of the file. See also: `project-index',
`project-find-file'."
  (interactive)
  (mk-proj-assert-proj)
  (unless (get-buffer (mk-proj-fib-name))
    (message "Please use project-index to create the index before running project-find-file-ido")
    (return-from "project-find-file-ido" nil))
  (let ((file (ido-completing-read "Find file in project matching (ido): "
                                   (mk-proj-fib-matches))))
    (when file
      (find-file (concat (file-name-as-directory mk-proj-basedir) file)))))

(defun project-multi-occur (regex)
  "Search all open project files for 'regex' using `multi-occur'"
  (interactive "sRegex: ")
  (multi-occur (mk-proj-filter (lambda (b) (if (buffer-file-name b) b nil))
                               (mk-proj-buffers))
               regex))

;; ---------------------------------------------------------------------
;; Menus
;; ---------------------------------------------------------------------

(defun mk-proj-menu-item (key label fn &optional always-enabled-p)
  "Define a mk-project menu item that may not be enabled if a
project is not loaded."
  (let ((whole-key `[menu-bar mkproject ,key]))
    (define-key global-map whole-key
      `(menu-item ,label ,fn :enable ,(if always-enabled-p 't 'mk-proj-name)))))

(defun mk-proj-menu-item-separator (key)
  "Define a separator line in the mk-project menu."
  (define-key global-map `[menu-bar mkproject ,key] '(menu-item "--")))

(defun project-menu ()
  "Define a menu for mk-project operations."
  (interactive)
  ;; define a menu in the top-level menu
  (define-key-after
    global-map
    [menu-bar mkproject]
    (cons "mk-project" (make-sparse-keymap))
    'tools)

  ;; define the menu items in reverse order
  (mk-proj-menu-item 'tags   "Build TAGS"     'project-tags)
  (mk-proj-menu-item 'index  "Build Index"    'project-index)
  (mk-proj-menu-item-separator 's2)
  (mk-proj-menu-item 'dired  "Browse (dired)" 'project-dired)
  (mk-proj-menu-item 'comp   "Compile   "     'project-compile)
  (mk-proj-menu-item 'occur  "Multi-occur"    'project-multi-occur)
  (mk-proj-menu-item 'ack    "Ack"            'project-ack)
  (mk-proj-menu-item 'grep   "Grep"           'project-grep)
  (mk-proj-menu-item-separator 's1)
  (mk-proj-menu-item 'status "Status"         'project-status)
  (mk-proj-menu-item 'unload "Unload Project" 'project-unload)
  (mk-proj-menu-item 'load   "Load Project"   'project-load t))

(defun project-menu-remove ()
  "Remove the mk-project menu from the menu bar"
  (interactive)
  (global-unset-key [menu-bar mkproject]))

(when mk-proj-menu-on
  (project-menu))

;; ---------------------------------------------------------------------
;; Friends
;; ---------------------------------------------------------------------

(defvar mk-proj-friends nil)

(defvar mk-proj-open-friends-cache nil)

(defun mk-proj-find-friendly-projects (&optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  ;; go through all configs
  ;; collect all projects which have the requested name in their friend list
  ;; remove duplicates and return
  (let ((r '()))
    (maphash (lambda (k c)
               (unless (string-equal k proj-name)
                 (when (mk-proj-any (lambda (f)
                                      (string-equal f proj-name))
                                    (mk-proj-config-val 'friends c))
                   (setq r (append r `(,k)))))) mk-proj-list)
    (remove-duplicates (append r (mk-proj-config-val 'friends proj-name t)) :test #'string-equal)))

(defun mk-proj-fib-friend-matches (&optional regex proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((resulting-matches '()))
    (dolist (friend (mk-proj-get-config-val 'friends proj-name) resulting-matches)
      (if (file-exists-p (expand-file-name friend))
          (if regex
              (when (string-match regex friend) (add-to-list 'resulting-matches (expand-file-name friend)))
            (add-to-list 'resulting-matches (expand-file-name friend)))
        (setq resulting-matches (append resulting-matches
                                        (mapcar (lambda (f)
                                                  (expand-file-name (concat (mk-proj-get-config-val 'basedir friend) "/" f)))
                                                (mk-proj-fib-matches regex friend))))))
    ;;(remove-duplicates resulting-matches :test #'string-equal)
    ))

(defun mk-proj-friendly-buffer-p (buf)
  (let ((file-name (mk-proj-buffer-name buf)))
    (if (and file-name
             (block "friend-loop"
               (dolist (f (mk-proj-find-friendly-projects))
                 (if (not (stringp f)) (error "Error in mk-proj-friendly-buffer-p, did you quote the friends list?"))
                 (if (file-exists-p (expand-file-name f))
                     (when (string-equal f file-name)
                       (return-from "friend-loop" t))
                   (when (mk-proj-find-config f)
                     (let* ((friend-config (mk-proj-find-config f))
                            (basedir (expand-file-name (car (cdr (assoc 'basedir friend-config)))))
                            (friend-basedir (if (string-equal (substring basedir -1) "/")
                                                basedir
                                              (concat basedir "/"))))
                       (when (string-match (concat "^" (regexp-quote friend-basedir)) file-name)
                         ;;(print friend-basedir)
                         (return-from "friend-loop" t))))))))
        t
      nil)))

(defun mk-proj-friendly-buffers (&optional friends-only)
  (let ((buffers nil))
    (dolist (b (buffer-list))
      (when (or (and friends-only
                     (mk-proj-friendly-buffer-p b)
                     (not (some (lambda (buf) (eq buf b)) (mk-proj-buffers))))
                (and (not friends-only)
                     (mk-proj-friendly-buffer-p b)))
        (push b buffers)))
      buffers))

(defun mk-proj-save-open-friends-info ()
  (when mk-proj-open-friends-cache
    (with-temp-buffer
      (dolist (f (remove-duplicates (mapcar (lambda (b) (mk-proj-buffer-name b)) (mk-proj-friendly-buffers)) :test #'string-equal))
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
    (dolist (f (mk-proj-find-friendly-projects) basedirs)
      (if (file-exists-p (expand-file-name f))
          (add-to-list 'basedirs f)
        (add-to-list 'basedirs (mk-proj-config-val 'basedir f))))))

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
           (whole-cmd (concat (mk-proj-ack-cmd regex) " " mk-proj-basedir "; "
                              (let ((s ""))
                                (dolist (d (mk-proj-friend-basedirs) s)
                                  (setq s (concat s (mk-proj-ack-cmd regex) " " d "; "))))))
           (confirmed-cmd (read-string "Ack command: " whole-cmd nil whole-cmd))
           (default-directory (file-name-as-directory
                               (if (mk-proj-has-univ-arg)
                                   default-directory
                                 mk-proj-basedir))))
      (compilation-start confirmed-cmd 'ack-mode))))

(defun mk-proj-find-projects-owning-file (file))

(defun project-friend-this (&optional proj-name)
  (interactive "P")
  (setq proj-name (cond ((and (listp proj-name) (numberp (car proj-name)))
                    (mk-proj-get-config-val 'parent))
                   ((stringp proj-name)
                    proj-name)
                   (t nil)))
  (mk-proj-assert-proj)
  (unless proj-name
    (setq proj-name mk-proj-name))
  (mk-proj-set-config-val 'friends (append mk-proj-friends `(,(buffer-file-name (current-buffer)))) proj-name))

(defun project-friend-add (&optional friend)
  (interactive "P")
  (let ((parent nil))
    (when (and (listp friend) (numberp (car friend)))
      (setq parent (mk-proj-get-config-val 'parent)
            friend nil))
    (unless friend
      (setq friend (expand-file-name
                    (concat "~/"
                            (ido-completing-read "Friend: "
                                                 (append (ido-file-name-all-completions "~")
                                                         (mk-proj-names)))))))
    (mk-proj-assert-proj)
    (when (or (and (file-exists-p friend) (not (file-directory-p friend))) (gethash friend mk-proj-list))
      (mk-proj-set-config-val 'friends (append mk-proj-friends `(,friend)) parent))))

(provide 'mk-project)









;;; mk-project.el ends here
