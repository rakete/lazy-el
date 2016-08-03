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

(require 'thingatpt)
(require 'cl)
(require 'etags-table)

(defvar mk-proj-version "2.0.0")

(defvar mk-global-cache-root "~/.mk-project"
  "Root path under which to create files that contain project metadata like open
files, open friends etc. These are automatically created for a project under a
directory created under this path. Makes the open-files-cache, file-list-cache,
open-friends-cache directives optional.

See also `mk-proj-open-files-cache', `mk-proj-open-friends-cache',
`mk-proj-file-list-cache'")

;; ---------------------------------------------------------------------
;; Project Variables
;; ---------------------------------------------------------------------

(defvar mk-proj-name nil
  "Name of the current project. Required. First argument to project-def.")

(defun mk-proj-fib-name (&optional proj-name)
  "Buffer name of the file-list cache. This buffer contains a
list of all the files under the project's basedir (minus those
matching ignore-patterns) or, if index-find-cmd is set, the list
of files found by calling the custom find command.  The list is
used by `project-find-file' to quickly locate project files."
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((top-level-parent (mk-proj-get-config-val 'parent proj-name)))
    (while (and top-level-parent (mk-proj-get-config-val 'parent top-level-parent))
      (setq top-level-parent (mk-proj-get-config-val 'parent top-level-parent)))
    (concat "*" (or top-level-parent proj-name) " file-index*")))

(defconst mk-proj-vcs-path '((git . ".git")
                             (cvs . ".CVS")
                             (svn . ".svn")
                             (bzr . ".bzr")
                             (hg  . ".hg")
                             (darcs . "_darcs"))
  "When `mk-proj-vcs' is one of the VCS types listed here, ignore
the associated paths when greping or indexing the project.")

(defvar mk-proj-required-vars '((name . (stringp))
                                (basedir . (stringp)))
  "Project config vars that are required in every project.

See also `mk-proj-optional-vars' `mk-proj-var-before-get-functions'.")

(defvar mk-proj-optional-vars '((parent . (stringp)) ;; parent needs to come first!
                                (languages . (listp))
                                (src-patterns . (listp))
                                (ignore-patterns . (listp))
                                (vcs . (symbolp))
                                (compile-cmd . (functionp commandp stringp listp))
                                (startup-hook . (functionp commandp stringp listp))
                                (shutdown-hook . (functionp commandp stringp listp))
                                (file-list-cache . (stringp))
                                (open-files-cache . (stringp))
                                (patterns-are-regex . (symbolp))
                                (friends . (listp))
                                (open-friends-cache . (stringp))
                                (gtags-config . (stringp file-exists-p))
                                (gtags-arguments . (stringp)))
  "Project config vars that are optional.

parent            : a project name that acts as parent to this one
languages         : a list of languages that are used in this project
src-patterns      : file patterns that match files that should belong to this project
ignore-patterns   : file patterns that match files that should be ignored by this
                    project
vcs               : a symbol that specifies which vcs is used for this project, this
                    has no real function other then being helpful for basedir detection
compile-cmd       : compile commands, can be a history list or a single string
startup-hook      : functions to be called when project is loaded
shutdown-hook     : functions to be called when project is unloaded
file-list-cache   : filesystem location of the file index cache
open-files-cache  : filesystem location of the open files cache
patterns-are-regex: legacy, when src and ignore-patterns are not regex, but instead
                    just a simple file pattern
friends           : friends are other projects that are relevant to this project, they
                    will be included when browsing for files, updating the tag database
                    and when completing symbols
open-friends-cache: filesystem location for open files from friend projects
gtags-config      : gtags config file that should be used
gtags-arguments   : additional gtags arguments

See also `mk-proj-required-vars' `mk-proj-var-before-get-functions'")

(defvar mk-proj-internal-vars '()
  "Project config vars that are ignored when saving the project config.")

(defun mk-proj-basedir-expand (var val &optional proj-name config-alist)
  "Helper used in `mk-proj-var-before-get-functions' to expand basedir."
  (when (stringp val)
    (file-name-as-directory (expand-file-name val))))

(defun mk-proj-var-get-open-file-cache (var val &optional proj-name config-alist)
  "Helper used in `mk-proj-var-before-get-functions' to find cache location."
  (if val
      (expand-file-name val)
    (mk-proj-get-cache-file var proj-name nil)))

(defun mk-proj-var-get-file-list-cache (var val &optional proj-name config-alist)
  "Helper used in `mk-proj-var-before-get-functions' to find cache location."
  (if val
      (expand-file-name val)
    (mk-proj-get-cache-file var proj-name t)))

(defun mk-proj-var-guess-languages (var val &optional proj-name config-alist)
  "Helper used in `mk-proj-var-before-get-functions' to guess project languages."
  (or val (mk-proj-src-pattern-languages (mk-proj-get-config-val 'src-patterns proj-name))))

(defvar mk-proj-var-before-get-functions '((basedir . mk-proj-basedir-expand)
                                           (file-list-cache . mk-proj-var-get-file-list-cache)
                                           (open-files-cache . mk-proj-var-get-open-file-cache)
                                           (open-friends-cache . mk-proj-var-get-open-file-cache)
                                           (languages . mk-proj-var-guess-languages)
                                           (patterns-are-regex . (lambda (var val &optional proj-name config-alist)
                                                                   (if (and config-alist
                                                                            (not (assoc 'patterns-are-regex config-alist)))
                                                                       t
                                                                     val)))
                                           (friends . (lambda (var val &optional proj-name config-alist)
                                                        (loop for friend in val
                                                              if (gethash friend mk-proj-list)
                                                              collect friend))))
  "Config vars from `mk-proj-required-vars' and `mk-proj-optional-vars' (except 'name')
can be associated with a function in this association list, which will be
applied to the value of the var right after it is taken from the config-alist.

That means when querying the configuration with `mk-proj-get-config-val', the var
symbol is used to look up a function in this list and, if present, that function is then
applied to the var symbol and var value pair and its result used as new var value.

See also `mk-proj-get-config-val'.")

(defvar mk-proj-var-ask-functions '((name . (lambda ()
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
                                    (vcs . (lambda ()
                                             (loop for v = (read-string "vcs: " super) then (read-string "vcs: " super)
                                                   until (some (lambda (x) (eq (car x) (read v))) mk-proj-vcs-path)
                                                   finally return (read v))))
                                    (compile-cmd . (lambda ()
                                                     (read-string "Compile command: " super)))
                                    (patterns-are-regex . (lambda () t)))
  "Functions that are used to ask the user about what a vars value should be.")

(defvar mk-proj-before-load-hook '())
(defvar mk-proj-before-files-load-hook '())
(defvar mk-proj-after-load-hook '())

(defvar mk-proj-before-unload-hook '())
(defvar mk-proj-before-files-unload-hook '())
(defvar mk-proj-after-unload-hook '())

(defvar mk-proj-history '())

(defvar mk-proj-buildsystems '((gnu-make ((files ("autogen.sh" "configure" "Makefile"))
                                          (build "make")))
                               (cmake ((files ("CMakeLists.txt"))
                                       (build "mkdir -p build; cd build; cmake ..; make")))
                               (cabal ((files ("Setup.lhs"))
                                       (build "runhaskell Setup.lhs build $MK_BUILD_OPTS")))
                               (python ((files ("setup.py"))
                                        (build "python setup.py build $MK_BUILD_OPTS"))))
  "Used when guessing a project root or its compile-cmd.")

(defvar mk-proj-src-pattern-table '(("h" . (c ".*\\.c" ".*\\.cpp" ".*\\.cc" ".*\\.h" ".*\\.hpp" ".*\\.hh"))
                                    ("hpp" . (cpp ".*\\.cpp" ".*\\.c" ".*\\.h" ".*\\.hh" ".*\\.hpp"))
                                    ("hh" . (cpp ".*\\.cc" ".*\\.c" ".*\\.h" ".*\\.hh" ".*\\.hpp"))
                                    ("c" . (c ".*\\.c" ".*\\.h"))
                                    ("cpp" . (cpp ".*\\.cpp"  ".*\\.c" ".*\\.h" ".*\\.hh" ".*\\.hpp"))
                                    ("cc" . (cpp  ".*\\.cc"  ".*\\.c" ".*\\.h" ".*\\.hh" ".*\\.hpp"))
                                    ("hs" . (haskell  ".*\\.hs" ".*\\.lhs" ".*\\.cabal"))
                                    ("lhs" . (haskell  ".*\\.hs" ".*\\.lhs" ".*\\.cabal"))
                                    ("cabal" . (haskell  ".*\\.hs" ".*\\.lhs" ".*\\.cabal"))
                                    ("php" . (php  ".*\\.php" ".*\\.html"))
                                    ("js" . (javascript  ".*\\.js" ".*\\.html"))
                                    ("el" . (elisp  ".*\\.el" ".*\\.el\\.gz"))
                                    ("el.gz" . (elisp  ".*\\.el" ".*\\.el\\.gz"))
                                    ("lisp" . (lisp  ".*\\.lisp"  ".*\\.lsp"))
                                    ("lsp" . (lisp  ".*\\.lisp"  ".*\\.lsp"))
                                    ("scm" . (scheme  ".*\\.scm"))
                                    ("lua" . (lua ".*\\.lua"))
                                    ("clojure" . (clojure ".*\\.clj" ".*\\.clojure"))
                                    ("clj" . (clojure ".*\\.clojure" ".*\\.clj"))
                                    ("java" . (java ".*\\.java"))
                                    ("pl" . (perl ".*\\.pl" ".*\\.pm" ".*\\.pod" ".*\\.t"))
                                    ("pm" . (perl ".*\\.pl" ".*\\.pm" ".*\\.pod" ".*\\.t"))
                                    ("pod" . (perl ".*\\.pl" ".*\\.pm" ".*\\.pod" ".*\\.t"))
                                    ("t" . (perl ".*\\.pl" ".*\\.pm" ".*\\.pod" ".*\\.t"))
                                    ("py" . (python ".*\\.py"))
                                    ("sh" . (shell ".*\\.sh")))
  "Maps file suffixes to regexps used as source-patterns when guessing a
project config from the currently opened file in the active buffer.")

(defvar mk-proj-config-save-location nil
  "Where to save project configs in elisp. If this is a filename project
configs will be written to that file. If it is a directory an elisp
file with the projects name will be created in that directory.")

(defvar mk-proj-language-tag-systems '((awk . (gtags+exuberant-ctags))
                                       (batch . (gtags+exuberant-ctags))
                                       (cobol . (gtags+exuberant-ctags))
                                       (c . (gtags))
                                       (cpp . (gtags))
                                       (csharp . (gtags+exuberant-ctags))
                                       (elisp . (gtags+exuberant-ctags))
                                       (erlang . (gtags+exuberant-ctags))
                                       (fortran . (gtags+exuberant-ctags))
                                       (haskell . ())
                                       (java . (gtags))
                                       (javascript . (gtags+exuberant-ctags))
                                       (lisp . (gtags+exuberant-ctags))
                                       (lua . (gtags+exuberant-ctags))
                                       (ocaml . (gtags+exuberant-ctags))
                                       (perl . (gtags+exuberant-ctags))
                                       (php . (gtags))
                                       (python . (gtags+exuberant-ctags))
                                       (ruby . (gtags+exuberant-ctags))
                                       (scheme . (gtags+exuberant-ctags))
                                       (shell . (gtags+exuberant-ctags))
                                       (tcl . (gtags+exuberant-ctags))
                                       (tex . (gtags+exuberant-ctags))
                                       (verilog . (gtags+exuberant-ctags))
                                       (vhdl . (gtags+exuberant-ctags))
                                       (vim . (gtags+exuberant-ctags))
                                       (yacc . (gtags)))
  "Defines which tagging system to use for which language.

Only gtags and gtags+exuberant-ctags are implemented.")

(defvar mk-proj-thing-selector 'symbol)

(defvar mk-proj-completions-cache (make-hash-table :test 'equal))
(defvar mk-proj-definitions-cache (make-hash-table :test 'equal))

(defvar mk-proj-list (make-hash-table :test 'equal))

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

(defun mk-proj-save-state ()
  "Save the currently open project files."
  (when mk-proj-name
    (mk-proj-save-open-file-info)
    (mk-proj-save-open-friends-info)))

(defun mk-proj-zip (&rest lists)
  "A zipper takes lists like (1 2 3) (a b c) and produces a result like ((1 a) (2 b) (3 c))"
  (let* ((n (- (length lists) 1))
         (i 0)
         (rs '()))
    (while (some 'identity (mapcar (lambda (l) (> (length l) i)) lists))
      (setq rs (append rs (list (loop for m from 0 to n
                                      collect (nth i (nth m lists))))))
      (setq i (1+ i)))
    rs))

(defun mk-proj-dirname (path)
  "Take a path and return the directory only, without filename."
  (file-name-as-directory (apply #'concat (reverse (mapcar (lambda (s)
                                                             (if (> (length s) 0)
                                                                 (file-name-as-directory s)
                                                               (file-name-as-directory "/")))
                                                           (cdr (reverse (split-string (expand-file-name path) "/"))))))))

(defun mk-proj-assert-proj (&optional try-guessing)
  (unless mk-proj-name
    (let* ((continue-prevent-restore t)
           (guessed-alist (cond ((eq try-guessing 'quiet)
                                 (mk-proj-guess-alist nil nil))
                                (try-guessing
                                 (mk-proj-guess-alist t t)))))
      (cond ((and guessed-alist
                  (eq try-guessing 'quiet)
                  (gethash (cadr (assoc 'name guessed-alist)) mk-proj-list nil))
             (mk-proj-load (cadr (assoc 'name guessed-alist))))
            ((and guessed-alist
                  (eq try-guessing 'quiet)
                  (not (gethash (cadr (assoc 'name guessed-alist)) mk-proj-list nil)))
             (project-def (cadr (assoc 'name guessed-alist)) guessed-alist)
             (mk-proj-load (cadr (assoc 'name guessed-alist))))
            ((and guessed-alist
                  try-guessing
                  (gethash (cadr (assoc 'name guessed-alist)) mk-proj-list nil)
                  (y-or-n-p (concat "Load project " (cadr (assoc 'name guessed-alist)) "? ")))
             (mk-proj-load (cadr (assoc 'name guessed-alist))))
            ((and guessed-alist
                  try-guessing
                  (not (gethash (cadr (assoc 'name guessed-alist)) mk-proj-list nil))
                  (y-or-n-p (concat "Create project " (cadr (assoc 'name guessed-alist)) "? ")))
             (project-def (cadr (assoc 'name guessed-alist)) guessed-alist)
             (mk-proj-load (cadr (assoc 'name guessed-alist))))
            (t
             (error "No project is set!"))))))

(defun mk-proj-maybe-kill-buffer (bufname)
  (let ((b (get-buffer bufname)))
    (when b (kill-buffer b))))

(defun mk-proj-get-vcs-path (&optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (if (mk-proj-get-config-val 'vcs proj-name)
      (cdr (assoc (mk-proj-get-config-val 'vcs proj-name) mk-proj-vcs-path))
    nil))

(defun mk-proj-has-univ-arg ()
  (eql (prefix-numeric-value current-prefix-arg) 4))

(defun mk-proj-names ()
  (let ((names nil))
    (maphash (lambda (k v) (when k (add-to-list 'names k))) mk-proj-list)
    names))

(defun mk-proj-use-ido ()
  (and (boundp 'ido-mode) mk-proj-use-ido-selection))

(defun mk-proj-filter (condp lst)
  "Filter LST with CONDP. All elements for which CONDP returns t will be kept,
all others filtered."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun* mk-proj-any (condp lst)
  "Apply CONDP to all elements of LST."
  (some condp lst))

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

(defun mk-proj-buildsystem-patterns ()
  (mapcar 'regexp-quote
          (mk-proj-flatten (loop for bs in mk-proj-buildsystems
                                 collect (cadr (assoc 'files (cadr bs)))))))

(defun mk-proj-path-complement (path1 path2)
  "This will return the part of the two paths PATH1 and PATH2 that is _not_ equal.

If both paths have a common prefix this will return the part that is not
found in both of them:
\(mk-proj-path-complement \"/foo/bar/blah\" \"/foo/bar\") -> \"blah/\"

If they do not share a common prefix this will return nil. If they are
equal this will return t."
  (loop for x = (split-string path1 "/" t) then (cdr x)
        for y = (split-string path2 "/" t) then (cdr y)
        until (or (not (and (car x) (car y)))
                  (not (string-equal (car x) (car y))))
        finally return (cond ((equal x y)
                              t)
                             ((and x y)
                              nil)
                             ((and (not (car x)) (not (car y)))
                              nil)
                             (t (apply #'concat (mapcar (lambda (s) (concat s "/")) (cond (x x) (y y))))))))

(defun* mk-proj-path-equal (a b &optional string-equal)
  "If path B is equal to or fully contained in path A, this will be true.

If STRING-EQUAL is non-nil behaviour is as if string-equal wouldn't
care about slashes.

Examples:
\(mk-proj-path-equal \"/foo/bar\" \"/foo/bar\")      -> true
\(mk-proj-path-equal \"/foo/bar\" \"/foo\")          -> true
\(mk-proj-path-equal \"/foo/bar\" \"/foo/bar/blah\") -> false
\(mk-proj-path-equal \"/foo/bar\" \"/foo/blah/bar\") -> false

\(mk-proj-path-equal \"/foo/bar\" \"/foo/bar/\" t)     -> true
\(mk-proj-path-equal \"/foo/bar\" \"/foo\" t)          -> false
\(mk-proj-path-equal \"/foo/bar\" \"/foo/bar/blah\" t) -> false
\(mk-proj-path-equal \"/foo/bar\" \"/foo/blah/bar\" t) -> false"
  (let ((x (split-string a "/" t))
        (y (split-string b "/" t)))
    (while (and (car x)
                (car y)
                (string-equal (car x) (car y)))
      (setq x (cdr x)
            y (cdr y)))
    (cond
     ;; "/foo/bar" "/foo/bar"      true
     ((and (null x) (null y))
      t)
     ;; "/foo/bar" "/foo/bar/blah" false
     ((and (null x) (not (null y)))
      nil)
     ;; "/foo/bar" "/foo"          true
     ((and (not (null x)) (null y) (not string-equal))
      t)
     ;; "/foo/bar" "/foo/blah/bar" false
     ((and (not (null a)) (not (null b)))
      nil))))

(defun* mk-proj-search-path (re path &optional stop-paths ignore-paths)
  (let ((xs (reverse (split-string path "/" t))))
    (while (and (cdr xs)
                (loop for ig in stop-paths
                      if (mk-proj-path-equal ig (apply 'concat (mapcar (lambda (s) (concat "/" s)) (reverse xs))))
                      return nil
                      finally return t))
      (when (and (directory-files (apply 'concat (mapcar (lambda (s) (concat "/" s)) (reverse xs))) nil re)
                 (not (some (lambda (re) (string-match re (car xs))) ignore-paths)))
        (return-from "mk-proj-search-path" (apply 'concat (mapcar (lambda (s) (concat "/" s)) (reverse xs)))))
      (setq xs (cdr xs)))))

(defun mk-proj-join (delimiter strings)
  (reduce (lambda (a b)
            (concatenate 'string a delimiter b))
          strings))

(defmacro mk-proj-with-directory (path &rest body)
  `(let ((default-directory ,path))
     (with-local-quit
       ,@body)))

(defmacro with-or-without-marker (marker &rest body)
  `(let ((marker ,marker))
     (if (markerp marker)
         (with-current-buffer (marker-buffer marker)
           (save-excursion
             (goto-char (marker-position marker))
             ,@body))
       ,@body)))

(defmacro with-marker (marker &rest body)
  `(let ((marker ,marker))
     (if (markerp marker)
         (with-current-buffer (marker-buffer marker)
           (save-excursion
             (goto-char (marker-position marker))
             ,@body)))))


;; ---------------------------------------------------------------------
;; Project Configuration
;; ---------------------------------------------------------------------

(defun mk-proj-search-projects ()
  (cond ((file-exists-p mk-proj-config-save-location)
         (load mk-proj-config-save-location))
        ((file-exists-p (concat (file-name-as-directory (expand-file-name mk-global-cache-root)) "projects.el"))
         (load (concat (file-name-as-directory (expand-file-name mk-global-cache-root)) "projects.el")))))

(defun* mk-proj-find-alist (&optional proj-name (inherit t))
  "Get a projects config-alist from the global projects hashmap."
  (when (or proj-name (setq proj-name mk-proj-name))
    (let* ((child (gethash proj-name mk-proj-list))
           (alist child))
      (while (and (assoc 'parent child)
                  inherit)
        (setq child (gethash (cadr (assoc 'parent child)) mk-proj-list)
              alist (append alist (remove-if (lambda (x) (some (lambda (y) (eq (first x) (first y))) alist)) child))))
      alist)))

(defun* mk-proj-get-config-val (key &optional proj-name (inherit t) (proj-alist nil))
  "Finds the value associated with KEY. A project PROJ can optionally
be specified.

If the third argument INHERIT is non-nil, all parents will queried
for the KEY and the first value that is found is returned.

See also `mk-proj-var-before-get-functions'."
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let* ((proj-alist (or proj-alist (mk-proj-find-alist proj-name nil)))
         (fn (cdr (assoc key mk-proj-var-before-get-functions)))
         (val  (or (when fn
                     (funcall fn key (cadr (assoc key proj-alist)) proj-name proj-alist))
                   (and (assoc key proj-alist)
                        (cadr (assoc key proj-alist)))
                   (let ((parent (cadr (assoc 'parent proj-alist))))
                     (when (and inherit parent)
                       (mk-proj-get-config-val key parent t))))))
    (if fn (funcall fn key val proj-name proj-alist) val)))

(defalias 'mk-proj-config-val 'mk-proj-get-config-val
  "Alias for `mk-proj-get-config-val' to ensure backward compatibility.")

(defun mk-proj-set-config-val (key value &optional proj-name)
  "Set the value associated with KEY to VALUE in config of project NAME."
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let* ((current-alist (mk-proj-find-alist proj-name nil))
         (new-alist current-alist))
    (when current-alist
      (while (assoc key new-alist)
        (setq new-alist (delq (assoc key new-alist) new-alist)))
      (add-to-list 'new-alist `(,key ,value))
      (unless (equal new-alist current-alist)
        (puthash proj-name new-alist mk-proj-list)
        (mk-proj-backend-funcall (mk-proj-detect-backend proj-name)
                                 'save proj-name new-alist)))))

(defun* mk-proj-eval-alist (proj-name config-alist)
  "Evaluates a CONFIG-ALIST for PROJ-NAME by calling eval on every
value.

It then goes through all values and checks them with the functions
from mk-proj-required-vars and mk-proj-optional-vars."
  (interactive)
  (let* ((evaluated-config-alist `((name ,proj-name)))
         (result-alist (dolist (cv config-alist evaluated-config-alist)
                         (let* ((key (car cv))
                                ;; super behaves like a keyword that can be used within a configuration
                                ;; to refer to the parents value
                                ;; I haven't tested this, it is a experimental feature
                                (super (when (cadr (assoc 'parent config-alist))
                                         (mk-proj-get-config-val key (cadr (assoc 'parent config-alist)) t)))
                                (lisp (car (cdr cv)))
                                (value (condition-case nil (eval lisp) (error lisp))))
                           (unless (eq key 'name)
                             (add-to-list 'evaluated-config-alist `(,key ,value)))))))
    (when (gethash proj-name mk-proj-list)
      (setq result-alist (mk-proj-alist-union (gethash proj-name mk-proj-list) result-alist)))
    (loop for varchecks in (append mk-proj-required-vars mk-proj-optional-vars)
          if (or (and (assoc (car varchecks) mk-proj-required-vars)
                      (assoc (car varchecks) result-alist)
                      (or (not (cadr (assoc (car varchecks) result-alist)))
                          (not (some (lambda (check) (funcall check (cadr (assoc (car varchecks) result-alist))))
                                     (cdr varchecks)))))
                 (and (assoc (car varchecks) mk-proj-optional-vars)
                      (assoc (car varchecks) result-alist)
                      (not (some (lambda (check) (funcall check (cadr (assoc (car varchecks) result-alist))))
                                 (cdr varchecks)))
                      (not (eq (cadr (assoc (car varchecks) result-alist)) nil))))
          do (progn (error "Project \"%s\" contains error: %S %s" proj-name (car varchecks) (assoc (car varchecks) result-alist))
                    (return-from "mk-proj-eval-alist" nil)))
    result-alist))

(defun* project-def (&optional proj-name config-alist)
  "Associate the settings in CONFIG-ALIST with project PROJ-NAME.

All values within CONFIG-ALIST will be evaluated when they look
like a lisp expression or symbol. So make sure to quote lists!

See also `project-undef', `mk-proj-required-vars' and `mk-proj-optional-vars'."
  (interactive)
  (cond ((stringp proj-name)
         (let ((alist (mk-proj-eval-alist proj-name config-alist)))
           (when (and alist (file-exists-p (cadr (assoc 'basedir alist))))
             (puthash proj-name alist mk-proj-list)
             (message "Defined: %s" proj-name)
             alist)))
        ((and (functionp 'mk-org-entry-define-project)
              (eq major-mode 'org-mode)
              (looking-at org-complex-heading-regexp)
              (mk-org-entry-define-project)))))



























(defun mk-proj-find-project-elisp-configuration-in-buffer (proj-name &optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (block "while-search-loop"
        (while (and (re-search-forward (regexp-quote proj-name) nil t)
                    (not (eobp)))
          (when (re-search-backward (regexp-quote "(project-def") (save-excursion (re-search-backward ")" nil t)) t)
            (return-from "while-search-loop" (point))))))))

(defun mk-proj-find-save-location-marker (&optional proj-name config-alist)
  "This tries to find a suitable location to save the projects configuration."
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((save-location nil))
    (cond
     ;; find file in directory named after project
     ((condition-case nil (directory-files (expand-file-name mk-proj-config-save-location)) (error nil))
      (with-current-buffer (find-file-noselect (concat (expand-file-name mk-proj-config-save-location) proj-name ".el"))
        (save-excursion
          (goto-char (or (mk-proj-find-project-elisp-configuration-in-buffer proj-name) (point-max)))
          (point-marker))))
     ;; find section to save under in single el file
     ((or (and (stringp mk-proj-config-save-location)
               (or (file-exists-p (expand-file-name mk-proj-config-save-location))
                   (write-region "" nil (expand-file-name mk-proj-config-save-location))
                   t)
               (setq save-location mk-proj-config-save-location))
          (and (or (file-exists-p (concat (file-name-as-directory (expand-file-name mk-global-cache-root)) "projects.el"))
                   (write-region "" nil (concat (file-name-as-directory (expand-file-name mk-global-cache-root)) "projects.el"))
                   t)
               (setq save-location (concat (file-name-as-directory (expand-file-name mk-global-cache-root)) "projects.el"))))
      (with-current-buffer (find-file-noselect (expand-file-name save-location))
        (save-excursion
          (cond ((condition-case nil (goto-char (mk-proj-find-project-elisp-configuration-in-buffer proj-name)) (error nil))
                 (point-marker))
                ((goto-char (point-max))
                 (point-marker))
                (t (error "mk-proj: could not find a location to save %s, see mk-proj-config-save-location" proj-name))))))
     ;; no suitable location found
     (t (error "mk-proj: could not find a location to save %s, see mk-proj-config-save-location" proj-name)))))

(defun mk-proj-config-insert (proj-name config-alist &optional insert-undefined insert-internal)
  (save-excursion
    (insert (concat "(project-def \"" proj-name "\" '("))
    (loop for k in (append mk-proj-required-vars mk-proj-optional-vars)
          if (and (not (eq (car k) 'name))
                  (or (not (some (lambda (j) (eq (car k) j)) mk-proj-internal-vars))
                      insert-internal)
                  (or (not (cdr (assoc (car k) mk-proj-var-before-get-functions)))
                      (not (string-equal (prin1-to-string (funcall (cdr (assoc (car k) mk-proj-var-before-get-functions)) (car k) nil))
                                         (prin1-to-string (mk-proj-get-config-val (car k) proj-name))))
                      insert-internal))
          do (when (or insert-undefined
                       (assoc (car k) config-alist))
               (insert (concat "(" (symbol-name (car k)) " " (prin1-to-string (cadr (assoc (car k) config-alist))) ")"))
               (unless (eq (car k) (car (last mk-proj-optional-vars)))
                 (newline))
               (indent-according-to-mode)))
    (insert "))\n")))

(defun mk-proj-config-save (proj-name config-alist)
  (let ((marker (mk-proj-find-save-location-marker proj-name config-alist)))
    (with-marker marker
                 (let ((mod (buffer-modified-p)))
                   (cond ((looking-at "(project-def.*")
                          (let* ((begin (point))
                                 (end (save-excursion (end-of-sexp) (point)))
                                 (old-alist (eval (nth 2 (read (buffer-substring begin end)))))
                                 (new-alist (mk-proj-alist-union old-alist config-alist)))
                            (kill-region begin end)
                            (mk-proj-config-insert proj-name new-alist)
                            (call-interactively 'eval-defun)))
                         (t
                          (goto-char (point-max))
                          (newline)
                          (newline)
                          (mk-proj-config-insert proj-name config-alist)
                          (call-interactively 'eval-defun)))
                   (save-buffer)
                   (set-buffer-modified-p mod)))))

(defun* mk-proj-config-buffer (&optional (state :create) proj-name config-alist)
  (case state
    (:create
     (let* ((proj-b (current-buffer))
            (buf (get-buffer-create "*mk-proj: new project*"))
            (window (display-buffer buf))
            (config-alist (or config-alist (mk-proj-guess-alist))))
       (select-window window)
       (set-window-dedicated-p window t)
       (emacs-lisp-mode)
       (buffer-disable-undo)
       (mk-proj-config-insert (or proj-name (cadr (assoc 'name config-alist)) "NewProject") config-alist t)
       (goto-char 0)
       (end-of-line)
       (mk-proj-backend-create-project-mode 'elisp)
       (buffer-enable-undo)))
    (:edit
     (let* ((marker (mk-proj-find-save-location-marker))
            (buf (make-indirect-buffer (marker-buffer marker) "*mk-proj: edit project*"))
            (window (display-buffer buf)))
       (select-window window)
       (set-window-start window (marker-position marker))
       (lisp-interaction-mode)
       (goto-char (marker-position marker))
       (mk-proj-config-save mk-proj-name (mk-proj-find-alist mk-proj-name nil))
       (set-window-dedicated-p window t)
       (mk-proj-backend-edit-project-mode 'elisp)
       (buffer-enable-undo)))
    (:finalize-create
     (let ((result nil))
       (while (not (setq result (condition-case nil (eval (read (buffer-string))) (error nil)))))
       (mk-proj-config-save (cadr (assoc 'name result)) result)
       (kill-buffer (buffer-name))))
    (:finalize-edit
     (let ((marker (mk-proj-find-save-location-marker)))
       (save-excursion
         (goto-char (marker-position marker))
         (let (edited-alist)
           (unless (eq (condition-case nil (setq edited-alist (call-interactively 'eval-defun)) (error 'error))
                       'error)
             (save-buffer)
             (kill-buffer)
             (when (and (not (condition-case nil (mk-proj-assert-proj) (error t)))
                        (string-equal (cadr (assoc 'name edited-alist)) mk-proj-name))
               (project-def mk-proj-name edited-alist)))))))))







(defvar mk-proj-backend-list (make-hash-table))

(defun* mk-proj-define-backend (backend &key buffer-fun save-fun insert-fun test-fun)
  (puthash backend `((buffer . ,buffer-fun)
                     (save . ,save-fun)
                     (insert . ,insert-fun)
                     (test . ,test-fun))
           mk-proj-backend-list))

(mk-proj-define-backend 'elisp
                        :buffer-fun 'mk-proj-config-buffer
                        :save-fun 'mk-proj-config-save
                        :insert-fun 'mk-proj-config-insert)

(defvar mk-proj-config-backend 'elisp)

(defun mk-proj-backend-funcall (backend sym &rest args)
  (apply (cdr (assoc sym (gethash backend mk-proj-backend-list))) args))

(defun* mk-proj-detect-backend (&optional proj-name config-alist)
  (if (and (condition-case nil (mk-proj-assert-proj) (error t))
           (not proj-name))
      (return-from "mk-proj-detect-backend" mk-proj-config-backend)
    (unless proj-name
      (setq proj-name mk-proj-name))
    (unless config-alist
      (setq config-alist (mk-proj-find-alist proj-name t)))
    (maphash (lambda (k v)
               (unless (eq k 'elisp)
                 (when (funcall (cdr (assoc 'test v)) config-alist)
                   (return-from "mk-proj-detect-backend" k)))) mk-proj-backend-list)
    'elisp))

(defun project-save ()
  (interactive)
  (mk-proj-assert-proj)
  (mk-proj-backend-funcall (mk-proj-detect-backend)
                           'save mk-proj-name (mk-proj-find-alist nil nil)))

(defun project-insert ()
  (interactive)
  (mk-proj-assert-proj)
  (cond ((derived-mode-p 'emacs-lisp-mode 'inferior-emacs-lisp-mode)
         (mk-proj-backend-funcall 'elisp
                                  'insert mk-proj-name (mk-proj-find-alist nil nil)))
        ((derived-mode-p 'org-mode)
         (mk-proj-backend-funcall 'orgmode
                                  'insert mk-proj-name (mk-proj-find-alist nil nil)))))

(defun* project-create ()
  (interactive)
  (if (and (gethash 'org-mode mk-proj-backend-list)
           (boundp 'org-complex-heading-regexp)
           (if (eq major-mode 'org-mode)
               (save-excursion
                 (org-back-to-heading)
                 (looking-at org-complex-heading-regexp))
             t))
      (mk-proj-backend-funcall 'org-mode
                               'buffer :create)
    (mk-proj-backend-funcall (mk-proj-detect-backend)
                             'buffer :create)))

(defun project-edit (&optional proj-name)
  (interactive)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (mk-proj-backend-funcall (mk-proj-detect-backend proj-name)
                           'buffer :edit proj-name))




(defvar mk-proj-create-project-mode-map (make-sparse-keymap))

(defvar mk-proj-create-project-mode-hook nil)

(define-minor-mode mk-proj-create-project-mode nil nil " NewProject" mk-proj-create-project-mode-map
  (run-hooks 'mk-proj-create-project-mode-hook))

(defun mk-proj-backend-create-project-mode (backend)
  (mk-proj-create-project-mode)
  (setq mk-proj-create-project-mode backend))

(define-key mk-proj-create-project-mode-map "\C-c\C-c"
  (lambda ()
    (interactive)
    (mk-proj-backend-funcall mk-proj-create-project-mode
                             'buffer :finalize-create)))

(define-key mk-proj-create-project-mode-map "\C-c\C-k"
  (lambda ()
    (interactive)
    (kill-buffer (buffer-name))))


(defvar mk-proj-edit-project-mode-map (make-sparse-keymap))

(defvar mk-proj-edit-project-mode-hook nil)

(define-minor-mode mk-proj-edit-project-mode nil nil " EditProject" mk-proj-edit-project-mode-map
  (run-hooks 'mk-proj-edit-project-mode-hook))

(defun mk-proj-backend-edit-project-mode (backend)
  (mk-proj-edit-project-mode)
  (setq mk-proj-edit-project-mode backend))

(define-key mk-proj-edit-project-mode-map "\C-c\C-c"
  (lambda ()
    (interactive)
    (mk-proj-backend-funcall mk-proj-edit-project-mode
                             'buffer :finalize-edit)))

(define-key mk-proj-edit-project-mode-map "\C-c\C-k"
  (lambda ()
    (interactive)
    (kill-buffer (buffer-name))))












(defun project-undef (&optional proj-name)
  "Opposite of `project-define'."
  (interactive "sProject: ")
  (remhash proj-name mk-proj-list))

(defmacro mk-proj-with-current-project (proj-name &rest body)
  `(let ((mk-proj-name ,proj-name))
     (condition-case nil ,@body (error nil))))

(defun mk-proj-check-required-vars (proj-name)
  (catch 'mk-proj-check-required-vars
    (dolist (v mk-proj-required-vars)
      (unless (mk-proj-get-config-val (car v) proj-name t)
        (throw 'mk-proj-check-required-vars (car v))))))

(defun* mk-proj-get-cache-file (symbol &optional proj-name (inherit t))
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((directory (concat mk-global-cache-root
                           (cond ((mk-proj-get-config-val 'parent proj-name nil)
                                  (let ((a (concat "/" (mk-proj-join "/" (mk-proj-ancestry proj-name)))))
                                    (concat a "/")))
                                 (t
                                  (concat "/" proj-name "/")))))
        (file (concat (symbol-name symbol))))
    (make-directory directory t)
    (let ((r (concat directory file)))
      (cond ((file-exists-p r)
             r)
            ((and (mk-proj-get-config-val 'parent proj-name nil)
                  (file-exists-p (or (mk-proj-get-config-val symbol (mk-proj-get-config-val 'parent proj-name nil) nil)
                                     (mk-proj-get-cache-file symbol (mk-proj-get-config-val 'parent proj-name nil) t)))
                  (eq inherit 'copy))
             (progn
               (copy-file (or (mk-proj-get-config-val symbol (mk-proj-get-config-val 'parent proj-name nil) nil)
                              (mk-proj-get-cache-file symbol (mk-proj-get-config-val 'parent proj-name nil) t)) r)
               r))
            ((and (mk-proj-get-config-val 'parent proj-name nil)
                  (eq (mk-proj-get-config-val 'basedir proj-name nil) nil)
                  (eq inherit t))
             (or (mk-proj-get-config-val symbol (mk-proj-get-config-val 'parent proj-name nil) nil)
                 (mk-proj-get-cache-file symbol (mk-proj-get-config-val 'parent proj-name nil) t)))
            (t r)))))

(defun mk-proj-get-root-cache-dir (&optional dirname proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((cachedir (concat mk-global-cache-root "/" (or (car-safe (mk-proj-ancestry proj-name)) proj-name) "/" dirname)))
    (unless (file-directory-p cachedir)
      (make-directory cachedir t))
    (expand-file-name cachedir)))

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

(defvar mk-proj-prevent-after-save-update nil)

(defun mk-proj-load (proj-name)
  (interactive)
  (let* ((oldname mk-proj-name)
         (proj-alist (mk-proj-find-alist proj-name nil))
         (quiet (and (cadr (assoc 'parent proj-alist))
                     (or (string-equal (cadr (assoc 'parent proj-alist))
                                       mk-proj-name)
                         (and (not (condition-case nil (mk-proj-assert-proj) (error t)))
                              (string-equal (cadr (assoc 'parent proj-alist))
                                            (mk-proj-get-config-val 'parent nil nil))))))
         (mk-proj-prevent-after-save-update t))
    (unless proj-name
      (error "mk-proj-load: proj-name is nil"))
    (run-hooks 'mk-proj-before-load-hook)
    (unless (or (string= oldname proj-name)
                (eq proj-alist nil))
      (project-unload))
    (if proj-alist
        (let ((v (mk-proj-check-required-vars proj-name)))
          (when v (error "Required config value '%s' missing in %s!" (symbol-name v) proj-name)))
      (error "Project %s does not exist!" proj-name))
    (setq mk-proj-name proj-name)
    (while (not (file-directory-p (mk-proj-get-config-val 'basedir)))
      (mk-proj-set-config-val 'basedir (read-string "Missing base directory? : " (mk-proj-get-config-val 'basedir))))
    (when (and (mk-proj-get-config-val 'vcs) (not (mk-proj-get-vcs-path)))
      (error "Invalid VCS setting!"))
    (message "Loading project %s ..." proj-name)
    ;;(cd (file-name-as-directory (mk-proj-get-config-val 'basedir)))
    (mk-proj-fib-init)
    (add-hook 'kill-emacs-hook 'mk-proj-kill-emacs-hook)
    (run-hooks 'mk-proj-before-files-load-hook)
    (mk-proj-visit-saved-open-files)
    (mk-proj-visit-saved-open-friends)
    (modify-frame-parameters (selected-frame) (list (cons 'name proj-name)))
    (run-hooks 'mk-proj-after-load-hook)
    (when (mk-proj-get-config-val 'startup-hook)
      (let ((startup-hook (mk-proj-get-config-val 'startup-hook)))
        (cond ((functionp startup-hook)
               (progn (message "funcall startup-hook...") (funcall startup-hook)))
              ((commandp startup-hook)
               (progn (message "call-interactively startup-hook...") (call-interactively startup-hook)))
              ((and (listp startup-hook)
                    (symbolp (car startup-hook)))
               (progn (message "eval startup-hook...") (eval startup-hook))))))
    (project-update-tags proj-name)
    (message "Loading project %s done" proj-name)))

(defun project-load (&optional proj-name)
  "Load a project's settings."
  (interactive)
  (let* ((guessed-alist (mk-proj-guess-alist))
         (names (let ((ns (mk-proj-names)))
                  (when (cadr (assoc 'name guessed-alist))
                    (add-to-list 'ns (cadr (assoc 'name guessed-alist))))
                  ns))
         (name (or proj-name
                   (if (mk-proj-use-ido)
                       (ido-completing-read "Project Name (ido): " names nil nil nil nil (cadr (assoc 'name guessed-alist)))
                     (completing-read "Project Name: " names)))))
    (when (and (cadr (assoc 'name guessed-alist))
               (string-equal name (cadr (assoc 'name guessed-alist)))
               (not (mk-proj-find-alist name nil)))
      (project-def name guessed-alist))
    (when (not (mk-proj-find-alist name nil))
      (add-to-list 'guessed-alist `(name ,name))
      (project-def name guessed-alist))
    (mk-proj-load name)))

(defun mk-proj-kill-emacs-hook ()
  "Ensure we save the open-files-cache info on emacs exit"
  (when (and mk-proj-name
             (mk-proj-get-config-val 'open-files-cache))
    (mk-proj-save-open-file-info))
  (when (and mk-proj-name
             (mk-proj-get-config-val 'friends)
             (mk-proj-get-config-val 'open-friends-cache))
    (mk-proj-save-open-friends-info))
  (project-unload t))

(defun project-unload (&optional quiet)
  "Unload the current project's settings after running the shutdown hook."
  (interactive "P")
  (let ((mk-proj-prevent-after-save-update t)
        (close-files (when mk-proj-name (y-or-n-p (concat "Close all '" mk-proj-name "' project files? ")))))
    (when mk-proj-name
      (condition-case nil
          (progn
            (unless quiet (message "Unloading project %s" mk-proj-name))
            (run-hooks 'mk-proj-before-unload-hook)
            (mk-proj-maybe-kill-buffer (mk-proj-fib-name))
            (mk-proj-save-open-friends-info)
            (mk-proj-save-open-file-info)
            (run-hooks 'mk-proj-before-files-unload-hook)
            (and (or (mk-proj-buffers) (mk-proj-friendly-buffers))
                 (not quiet)
                 close-files
                 (project-close-friends)
                 (project-close-files))
            (when (mk-proj-get-config-val 'shutdown-hook)
              (if (functionp (mk-proj-get-config-val 'shutdown-hook))
                  (funcall (mk-proj-get-config-val 'shutdown-hook))
                (mapc 'funcall (mk-proj-get-config-val 'shutdown-hook))))
            (run-hooks 'mk-proj-after-unload-hook))
        (error nil)))
    (add-to-list 'mk-proj-history mk-proj-name)
    (setq mk-proj-name nil)
    ;; (when (and (buffer-file-name (current-buffer))
    ;;            (file-exists-p (buffer-file-name (current-buffer))))
    ;;   (cd (mk-proj-dirname (buffer-file-name (current-buffer)))))
    (modify-frame-parameters (selected-frame) (list (cons 'name "Emacs")))
    (setq compile-command nil)
    (unless quiet (message "Project settings have been cleared"))))

(defun project-close-files ()
  "Close all unmodified files that reside in the project's basedir"
  (interactive)
  (mk-proj-assert-proj)
  (let ((closed nil)
        (dirty nil)
        (zeitgeist-prevent-send t)
        (continue-prevent-save t))
    (dolist (b (append (mk-proj-file-buffers) (mk-proj-dired-buffers)))
      (cond
       ((string-equal (buffer-name b) "*scratch*")
        nil)
       ((buffer-modified-p b)
        (push (buffer-name b) dirty))
       (t
        (push (buffer-name b) closed)
        (kill-buffer b))))
    (message "Closed %d buffers, %d modified buffers where left open"
             (length closed) (length dirty))))

(defun mk-proj-buffer-name (buf)
  "Return buffer's name based on filename or dired's location"
  (let ((file-name (or (buffer-file-name (or (buffer-base-buffer buf) buf))
                       (with-current-buffer (or (buffer-base-buffer buf) buf) list-buffers-directory))))
    (if file-name
        (expand-file-name file-name)
      nil)))

(defun mk-proj-buffer-p (buf &optional proj-name proj-alist)
  "Is the given buffer in our project, is a file opened? Also detects dired buffers open to basedir/*"
  (setq proj-alist (or proj-alist
                       (mk-proj-find-alist proj-name)
                       (mk-proj-find-alist mk-proj-name)
                       (mk-proj-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (mk-proj-assert-proj))
  (let ((file-name (mk-proj-buffer-name buf))
        (basedir (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t proj-alist)))
        (case-fold-search nil))
    (if (and (stringp file-name)
             (file-exists-p file-name)
             (mk-proj-get-config-val 'basedir proj-name t proj-alist)
             (loop for pattern in (mk-proj-get-config-val 'src-patterns proj-name t proj-alist)
                   if (string-match (if (mk-proj-get-config-val 'patterns-are-regex proj-name t proj-alist)
                                        pattern
                                      (regexp-quote pattern)) file-name)
                   return t
                   finally return nil)
             (or (string-match (concat "^" (regexp-quote basedir)) file-name)
                 (string-match (concat "^" (regexp-quote (file-truename basedir))) file-name)))
        proj-name
      nil)))

(defun mk-proj-file-buffer-p (buf &optional proj-name)
  (and (buffer-file-name buf)
       (mk-proj-buffer-p buf proj-name)))

(defun mk-proj-special-buffer-p (buf &optional proj-name)
  (let ((case-fold-search nil))
    (and (string-match "\*[^\*]\*" (buffer-name buf))
         (mk-proj-buffer-p buf proj-name))))

(defun mk-proj-dired-buffer-p (buf &optional proj-name)
  (and (with-current-buffer buf
         (eq major-mode 'dired-mode))
       (mk-proj-buffer-p buf proj-name)))

(defun mk-proj-buffers (&optional proj-name)
  "Get a list of buffers that reside in this project's basedir"
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((buffers nil))
    (dolist (b (buffer-list))
      (when (mk-proj-buffer-p b proj-name) (push b buffers)))
    buffers))

(defun mk-proj-file-buffers (&optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (remove-if (lambda (buf) (not (buffer-file-name buf))) (mk-proj-buffers proj-name)))

(defun mk-proj-special-buffers (&optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((case-fold-search nil))
    (append (remove-if (lambda (buf) (not (string-match "\*[^\*]\*" (buffer-name buf)))) (mk-proj-buffers proj-name))
            (remove-if (lambda (buf) (or (and (symbolp 'mk-org-project-buffer-name)
                                              (not (string-equal (mk-org-project-buffer-name proj-name) (buffer-name buf))))
                                         (compilation-buffer-p buf)))
                       (buffer-list)))))

(defun mk-proj-dired-buffers (&optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (remove-if (lambda (buf) (not (mk-proj-dired-buffer-p buf))) (mk-proj-buffers proj-name)))

(defun project-status (&optional proj-name)
  "View project's variables."
  (interactive)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (if (mk-proj-get-config-val 'basedir proj-name t)
      (let ((b (get-buffer-create "*mk-proj: project-status*")))
        (with-current-buffer b
          (kill-region (point-min) (point-max))
          (dolist (v (append mk-proj-required-vars mk-proj-optional-vars))
            (insert (format "%-32s = %s\n" (symbol-name (car v)) (mk-proj-get-config-val (car v) proj-name t)))))
        (when (not (eq b (current-buffer)))
          (display-buffer b)))
    (message "No project loaded.")))

;; ---------------------------------------------------------------------
;; Save/Restore open files
;; ---------------------------------------------------------------------

(defun mk-proj-save-open-file-info ()
  "Write the list of `files' to a file"
  (when (mk-proj-get-config-val 'open-files-cache)
    (with-temp-buffer
      (dolist (f (remove-duplicates (mapcar (lambda (b) (mk-proj-buffer-name b)) (mk-proj-buffers)) :test 'string-equal))
        (when f
          (unless (string-equal (mk-proj-get-config-val 'etags-file) f)
            (insert f "\n"))))
      (if (file-writable-p (mk-proj-get-config-val 'open-files-cache))
          (progn
            (write-region (point-min)
                          (point-max)
                          (mk-proj-get-config-val 'open-files-cache))
            (message "Wrote open files to %s" (mk-proj-get-config-val 'open-files-cache)))
        (message "Cannot write to %s" (mk-proj-get-config-val 'open-files-cache))))))

(defun mk-proj-visit-saved-open-files ()
  (let ((zeitgeist-prevent-send t))
    (when (mk-proj-get-config-val 'open-files-cache)
      (when (file-readable-p (mk-proj-get-config-val 'open-files-cache))
        (message "Reading open files from %s" (mk-proj-get-config-val 'open-files-cache))
        (with-temp-buffer
          (insert-file-contents (mk-proj-get-config-val 'open-files-cache))
          (goto-char (point-min))
          (while (not (eobp))
            (let ((start (point)))
              (while (not (eolp)) (forward-char)) ; goto end of line
              (let ((line (buffer-substring start (point)))
                    (enable-local-variables :safe))
                (message "Attempting to open %s" line)
                (if (file-exists-p line)
                    (unless (get-file-buffer line)
                      (find-file-noselect line t t))
                  (kill-line))))
            (forward-line)))))))

(defadvice switch-to-buffer (after mk-proj-switch-to-buffer-set-auto-mode)
  (when (eq major-mode 'fundamental-mode)
    (when (and (eq system-type 'windows-nt)
               (not (eq (default-value 'buffer-file-coding-system) buffer-file-coding-system)))
      (recode-region (point-min) (point-max) (default-value 'buffer-file-coding-system) buffer-file-coding-system)
      (mk-sourcemarker-restore)
      (set-buffer-modified-p nil))
    (set-auto-mode)))
(ad-activate 'switch-to-buffer)
;;(ad-unadvise 'switch-to-buffer)

(defadvice pop-to-buffer (after mk-proj-pop-to-buffer-set-auto-mode)
  (when (eq major-mode 'fundamental-mode)
    (when (and (eq system-type 'windows-nt)
               (not (eq (default-value 'buffer-file-coding-system) buffer-file-coding-system)))
      (recode-region (point-min) (point-max) (default-value 'buffer-file-coding-system) buffer-file-coding-system)
      (mk-sourcemarker-restore)
      (set-buffer-modified-p nil))
    (set-auto-mode)))
(ad-activate 'pop-to-buffer)
;;(ad-unadvise 'pop-to-buffer)

(defadvice display-buffer (after mk-proj-display-buffer-set-auto-mode)
  (with-current-buffer (ad-get-arg 0)
    (when (eq major-mode 'fundamental-mode)
      (when (and (eq system-type 'windows-nt)
                 (not (eq (default-value 'buffer-file-coding-system) buffer-file-coding-system)))
        (recode-region (point-min) (point-max) (default-value 'buffer-file-coding-system) buffer-file-coding-system)
        (mk-sourcemarker-restore)
        (set-buffer-modified-p nil))
      (set-auto-mode))))
(ad-activate 'display-buffer)

(defvar mk-proj-suppress-y-or-n-p-patterns (list "Do you want to revisit the file normally now"
                                                 "Do you want to revisit the file literally now"))

(defadvice y-or-n-p (around mk-proj-y-or-n-p)
  (let ((prompt (ad-get-arg 0))
        (suppress nil))
    (loop for pattern in mk-proj-suppress-y-or-n-p-patterns
          until (setq suppress (string-match pattern prompt)))
    (setq ad-return-value (or (and suppress t) ad-do-it))))
(ad-activate 'y-or-n-p)
;;(ad-unadvise 'y-or-n-p)

;; ---------------------------------------------------------------------
;; Tagging
;; ---------------------------------------------------------------------

(defvar mk-proj-default-gtags-config nil)
(defvar mk-proj-c++-gtags-config nil)
(defvar mk-proj-after-save-update-in-progress nil)
(defvar mk-proj-after-save-line-numbers (make-hash-table))
(defvar mk-proj-after-save-current-buffer nil)
(defvar mk-proj-after-save-current-project nil)

(defun project-update-tags (&optional proj-name proj-alist files debug)
  "Create or update the projects TAG database."
  (interactive)
  (setq proj-alist (or proj-alist
                       (mk-proj-find-alist proj-name)
                       (mk-proj-find-alist mk-proj-name)
                       (mk-proj-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (mk-proj-assert-proj))
  (unless files
    (setq files (mk-proj-unique-files proj-name)))
  (let ((default-directory (mk-proj-get-config-val 'basedir proj-name nil proj-alist))
        (gtags-executable (executable-find "gtags"))
        (global-executable (executable-find "global"))
        (rtags-executable (executable-find "rtags"))
        (ctags-exuberant-executable (or (executable-find "ctags-exuberant") (executable-find "ctags.exe")))
        (sys-files (make-hash-table))
        (languages '()))
    ;; - go through all project files, decide which tagging system to use for each individual file
    ;; and put the file in tagging system bins (the sys-files hashmap)
    ;; - the nested loops look like the could be switched, but the language of a file needs to detected
    ;; first, so that we can then decide which tagging systems to use for that language
    (dolist (f files)
      (let ((lang (car-safe (mk-proj-src-pattern-languages (list f)))))
        (push lang languages)
        (dolist (sys (cdr (assoc lang mk-proj-language-tag-systems)))
          (cond ((and (or (eq sys 'gtags+rtags))
                      gtags-executable
                      global-executable
                      rtags-executable)
                 (return (puthash 'gtags+rtags
                                  (append (list f) (gethash 'gtags+rtags sys-files))
                                  sys-files)))
                ((and (eq sys 'rtags)
                      rtags-executable)
                 (return (puthash 'rtags
                                  (append (list f) (gethash 'rtags sys-files))
                                  sys-files)))
                ((and (eq sys 'gtags)
                      gtags-executable
                      global-executable)
                 (return (puthash 'gtags
                                  (append (list f) (gethash 'gtags sys-files))
                                  sys-files)))
                ((and (eq sys 'gtags+exuberant-ctags)
                      gtags-executable
                      global-executable
                      ctags-exuberant-executable)
                 ;; - I had a fallback to gtags here if exuberant is not found, but removed it,
                 ;; it does not make sense, while exuberant can parse many languages, gtags alone
                 ;; can only parse a few
                 (return (puthash 'gtags+exuberant-ctags
                                  (append (list f) (gethash 'gtags+exuberant-ctags sys-files))
                                  sys-files)))))))
    ;; - why did I do this, I don't know anymore
    ;; - it looks like it was supposed to make sure everything that scanned with either gtags
    ;; or gtags+exuberant-ctags is guaranteed to be scanned by both, but I can't remember
    ;; why I thought this makes sense, right now it just seems like a waste of cpu cycles
    ;; (puthash 'gtags (append (gethash 'gtags+exuberant-ctags sys-files)
    ;;                         (gethash 'gtags sys-files))
    ;;          sys-files)

    ;; - for each tagging system we'll have a section where we check if there are any files for
    ;; that system, then build the appropriate commands to scan those file and create the tag database
    ;; - here, we do all gtags (gtags, gtags+rtags and gtags+exuberant-ctags) systems first
    (let* ((gtags-root "/")
           ;; - I am a setting gtags-root to / and then just make it scan all project files and friendly files,
           ;; that way I can get tags and completions for everything related to this project, I just have to
           ;; set the root again when building the query command
           ;; - the database is stored in the dbpath, that should be ~/.mk-project/project
           (gtags-dbpath (file-name-as-directory (file-truename (mk-proj-get-root-cache-dir nil proj-name))))
           (gtags-config (or (let ((c (mk-proj-get-config-val 'gtags-config proj-name nil proj-alist)))
                               (when (and c
                                          (file-exists-p c))
                                 c))
                             (let ((c (concat (mk-proj-get-config-val 'basedir proj-name nil proj-alist) "/.globalrc")))
                               (when (file-exists-p c)
                                 c))
                             (when (and mk-proj-c++-gtags-config
                                        (find 'cpp languages)
                                        (file-exists-p (expand-file-name mk-proj-c++-gtags-config)))
                               (expand-file-name mk-proj-c++-gtags-config))
                             (expand-file-name mk-proj-default-gtags-config)
                             (let ((c (expand-file-name "~/.globalrc")))
                               (when (file-exists-p c)
                                 c))
                             ""))
           (gtags-arguments (or (mk-proj-get-config-val 'gtags-arguments proj-name nil proj-alist)
                                ""))
           (gtags-commands (make-hash-table))
           (cmd-seperator (if (eq system-type 'windows-nt) " & " " ; ")))
      ;; - hack, gnu global under windows has problems with directories that have a trailing slash
      ;; so this just removes the last slash from the path
      (when (eq system-type 'windows-nt)
        (string-match "\\(.*\\)/" gtags-dbpath)
        (setq gtags-dbpath (match-string 1 gtags-dbpath)))
      (when (gethash 'gtags sys-files)
        (puthash 'gtags
                 (concat "cd " gtags-root cmd-seperator
                         "env GTAGSROOT=" gtags-root " "
                         (when (file-exists-p gtags-config) (concat "GTAGSCONF=" gtags-config " "))
                         "gtags " gtags-dbpath " -i -v -f - " gtags-arguments cmd-seperator)
                 gtags-commands))
      (when (gethash 'gtags+exuberant-ctags sys-files)
        (puthash 'gtags+exuberant-ctags
                 (concat "cd " gtags-root cmd-seperator
                         "env GTAGSLABEL=exuberant-ctags "
                         (when (file-exists-p gtags-config) (concat "GTAGSCONF=" gtags-config " "))
                         "GTAGSROOT=" gtags-root " "
                         "gtags " gtags-dbpath " -i -v -f - " gtags-arguments cmd-seperator)
                 gtags-commands))
      (let* ((ordering (list 'gtags+exuberant-ctags 'gtags))
             (commands (loop for sys in ordering
                             if (gethash sys gtags-commands)
                             collect (gethash sys gtags-commands)))
             (inputs (loop for sys in ordering
                           if (gethash sys gtags-commands)
                           collect (concat (mapconcat #'identity (gethash sys sys-files) "\n") "\n"))))
        (mk-proj-process-group "gtags" commands inputs 'mk-proj-update-completions-cache (list proj-name) nil)
        ))
    ;; - if I'd ever implement rtags, or any other system really, then this would become another section
    (when (gethash 'rtags sys-files)
      (message "rtags not implemented yet")))
  ;; - after updating the tag databases we may as well setup the env variables again, just to be sure
  (project-setup-tags proj-name))

(defun mk-proj-process-group (name commands inputs &optional terminator terminator-args debug n process event)
  "Create a process group with NAME, that runs all COMMANDS with INPUTS after each other
and the calls TERMINATOR with TERMINATOR-ARGS.

If DEBUG is true it will leave behind buffers named name-0, name-1 and so on for all commands
in COMMANDS.

This function is called recursivly as process sentinel when a command finishes, increasing N to
indicate which command to run next, and PROCESS and EVENT are the arguments this function
recieves when it acts as process sentinel."
  (unless n (setq n 0))
  (if (and (nth n commands)
           (or (not event)
               (string-equal event "finished\n")))
      (let* ((proc-name (concat name "-" (prin1-to-string n)))
             (shell-file-name (if (eq system-type 'windows-nt) (default-value 'shell-file-name) "/bin/sh"))
             (process (start-process-shell-command proc-name (when debug proc-name) (nth n commands)))
             (input (nth n inputs)))
        (set-process-sentinel process (apply-partially 'mk-proj-process-group name commands inputs terminator terminator-args debug (1+ n)))
        (when input
          (process-send-string process input))
        (process-send-eof process))
    (when terminator
      (apply terminator terminator-args))))

(defun mk-proj-src-pattern-tag-systems (src-patterns)
  "Takes a list of src patterns (like \"*.cpp\") and returns tag systems that can parse those."
  (let ((systems '()))
    (dolist (lang (mk-proj-src-pattern-languages src-patterns))
      (dolist (sys (cdr (assoc lang mk-proj-language-tag-systems)))
        (add-to-list 'systems sys)))
    systems))

(defun project-setup-tags (&optional proj-name)
  "Setup environment for existing TAG database."
  (interactive)
  (setq proj-name (or proj-name
                      mk-proj-name
                      (cadr (assoc 'name (mk-proj-guess-alist)))))
  (unless proj-name
    (mk-proj-assert-proj))
  (let ((proj-systems (mk-proj-src-pattern-tag-systems (mk-proj-get-config-val 'src-patterns proj-name)))
        (available-systems '()))
    (when (or (find 'gtags proj-systems)
              (find 'gtags+rtags proj-systems)
              (find 'gtags+exuberant-ctags proj-systems))
      (let* ((gtags-file (concat (file-truename (concat (mk-proj-get-root-cache-dir nil proj-name) "GTAGS"))))
             (gtags-file-alternative (concat (mk-proj-get-config-val 'basedir proj-name) "GTAGS")))
        (cond ((file-exists-p gtags-file)
               (let ((gtags-dbpath (mk-proj-dirname gtags-file))
                     (gtags-root "/"))
                 ;; - hack, gnu global under windows has problems with directories that have a trailing slash
                 ;; so this just removes the last slash from the path
                 (when (eq system-type 'windows-nt)
                   (string-match "\\(.*\\)/" gtags-dbpath)
                   (setq gtags-dbpath (match-string 1 gtags-dbpath)))
                 (setenv "GTAGSDBPATH" gtags-dbpath)
                 (setenv "GTAGSROOT" gtags-root)
                 (add-to-list 'available-systems 'gtags))))))
    available-systems))

(defun mk-proj-find-symbol-elisp-location-helper (symbol)
  (let ((sym symbol))
    `(lambda ()
       (let* ((previous-buf-list (buffer-list))
              (location (cond ((fboundp (quote ,sym)) (find-definition-noselect (quote ,sym) nil))
                              ((boundp (quote ,sym)) (find-definition-noselect (quote ,sym) 'defvar))
                              ((featurep (quote ,sym)) (cons (find-file-noselect (find-library-name
                                                                                  (symbol-name (quote ,sym))))
                                                             0))
                              ((facep (quote ,sym)) (find-definition-noselect (quote ,sym) 'defface)))))
         location))))

(defun mk-proj-find-symbol (proj-name proj-alist system regexp &rest args)
  (setq proj-alist (or proj-alist
                       (mk-proj-find-alist proj-name)
                       (mk-proj-find-alist mk-proj-name)
                       (mk-proj-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (mk-proj-assert-proj))
  (let* ((basedir (or (when proj-alist (cadr (assoc 'basedir proj-alist)))
                      (when proj-name (mk-proj-get-config-val 'basedir proj-name))
                      default-directory))
         (default-directory basedir)
         (case-fold-search nil)
         (cmd-sep (if (eq system-type 'windows-nt) " & " " ; ")))
    (cond ((eq 'gtags system)
           (let ((cmd (nth 0 args)))
             (mapcar (lambda (line)
                       (let ((tokens (split-string line " " t)))
                         (list :word (nth 0 tokens)
                               :line-number (read (or (nth 1 tokens) "-1"))
                               :file-path (or (when (nth 2 tokens) (expand-file-name (nth 2 tokens))) "")
                               :definition (mapconcat 'identity (nthcdr 3 tokens) " ")
                               :system system
                               :regexp regexp)))
                     (split-string (condition-case nil (shell-command-to-string (concat "cd " default-directory cmd-sep cmd)) (error "")) "\n" t))))
          ((eq 'rtags system)
           (message "rtags not implemented yet"))
          ((eq 'cscope system)
           (message "cscope not implemented yet"))
          ((eq 'obarray system)
           (let ((jumps nil)
                 (prev-buf-list (buffer-list)))
             (when (and proj-name (position 'elisp (mk-proj-src-pattern-languages (cadr (assoc 'src-patterns (mk-proj-find-alist proj-name))))))
               (do-all-symbols (sym)
                 (let ((sym-name (symbol-name sym)))
                   (when (and (string-match regexp sym-name)
                              (or (fboundp sym)
                                  (boundp sym)
                                  (facep sym)
                                  (featurep sym)))
                     (let* ((word sym-name)
                            (doc (condition-case nil
                                     (if (fboundp sym)
                                         (documentation sym t)
                                       (documentation-property sym 'variable-documentation t))
                                   (error nil)))
                            (docstring (and (stringp doc)
                                            (string-match ".*$" doc)
                                            (match-string 0 doc)))
                            (locator (mk-proj-find-symbol-elisp-location-helper sym)))
                       (push (list :word word
                                   :locator locator
                                   :docstring docstring
                                   :system system
                                   :regexp regexp)
                             jumps))))))
             jumps))
          ((eq 'imenu system)
           (let* ((imenu-alist (condition-case nil
                                   (if (functionp 'imenu--make-many-index-alist)
                                       (imenu--make-many-index-alist)
                                     (imenu--make-index-alist))
                                 (error nil)))
                  (marker-list (append (cdr (assoc "Types" imenu-alist))
                                       (cdr (assoc "Variables" imenu-alist))
                                       (nthcdr 3 imenu-alist))))
             (loop for item in marker-list
                   if (or (string-match (concat "[^ (]*\\(" regexp "[^ ]*\\)[ ]*(" ) (car item))
                          (string-match (concat "\\(" regexp "[^ ]*\\)") (car item)))
                   collect (list :word (match-string 1 (car item))
                                 :line-number (with-current-buffer (marker-buffer (cdr item))
                                                (save-excursion
                                                  (line-number-at-pos (marker-position (cdr item)))))
                                 :file-path (buffer-file-name (marker-buffer (cdr item)))
                                 :definition (with-current-buffer (marker-buffer (cdr item))
                                               (save-excursion
                                                 (goto-char (marker-position (cdr item)))
                                                 (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
                                 :system system
                                 :regexp regexp)))))))

(defun mk-proj-score-jumps (jumps regexp buffer)
  (let ((file-map (make-hash-table :test 'equal))
        (file-score-map (make-hash-table))
        (buffer-languages (mk-proj-src-pattern-languages (when (buffer-file-name buffer) (list (buffer-file-name buffer)))))
        (buffer-path (file-truename (buffer-file-name buffer)))
        (basedir-path (file-truename (or (condition-case nil (mk-proj-get-config-val 'basedir) (error nil))
                                         default-directory)))
        (inc (if (boundp 'ido-buffer-history) (* (ceiling (/ (float (length ido-buffer-history)) 100)) 100) 100))
        (truenames-cache (make-hash-table :test 'equal))
        (buffer-position-cache (make-hash-table :test 'equal :size 1000))
        (obarray-counter 0)
        (case-fold-search nil))
    (dolist (jump jumps)
      (let* ((jump-word (plist-get jump :word))
             (locator (plist-get jump :locator))
             (jump-path (when (stringp (plist-get jump :file-path))
                          (or (gethash (plist-get jump :file-path) truenames-cache)
                              (puthash (plist-get jump :file-path) (file-truename (plist-get jump :file-path)) truenames-cache))))
             (jump-line-number (plist-get jump :line-number))
             (jump-docstring (plist-get jump :docstring))
             (jump-definition (plist-get jump :definition))
             (system (plist-get jump :system))
             (file-tuple (gethash (or jump-path "obarray") file-map (list 0 (make-hash-table :size 1000))))
             (file-score (nth 0 file-tuple))
             (line-map (nth 1 file-tuple))
             (score 0))
        (unless (and (stringp jump-path)
                     (not (file-exists-p jump-path)))
          (when (and jump-word
                     (string-match-p regexp jump-word))
            (setq score (+ score inc)))
          (when (and jump-word
                     jump-definition
                     (> score 0)
                     (string-match-p (concat "\\<" regexp "\\>") jump-definition))
            (setq score (+ score inc)))
          (when (and jump-word
                     (> score 0)
                     (string-match-p (concat "^" regexp) jump-word))
            (setq score (+ score inc)))
          (when (eq system 'imenu)
            (setq score (+ score inc)))
          (when (and (functionp locator)
                     (eq system 'obarray))
            (setq score (+ score inc)))
          (when (and (functionp locator)
                     (eq system 'obarray)
                     (> (length jump-docstring) 0))
            (setq score (+ score inc)))
          (when (and jump-path
                     (buffer-file-name buffer)
                     (mk-proj-path-equal jump-path buffer-path))
            (setq score (+ score inc)))
          (when (and jump-path
                     mk-proj-name
                     (mk-proj-path-equal jump-path basedir-path))
            (setq score (+ score inc)))
          (when (and jump-path
                     (boundp 'ido-buffer-history)
                     (find-buffer-visiting jump-path))
            (let ((buffer-position (or (gethash jump-path buffer-position-cache)
                                       (puthash jump-path (condition-case nil (position (buffer-name (find-buffer-visiting jump-path))
                                                                                        ido-buffer-history
                                                                                        :test 'equal)
                                                            (error nil)) buffer-position-cache)
                                       (length ido-buffer-history))))
              (setq score (+ score (- (length ido-buffer-history)
                                      buffer-position)))))
          (when (and jump-path
                     buffer-languages)
            (dolist (lang buffer-languages)
              (when (find lang (mk-proj-src-pattern-languages (list jump-path)))
                (setq score (+ score inc)))))
          (let ((existing-jump (gethash (or jump-line-number (setq obarray-counter (1+ obarray-counter))) line-map)))
            (unless (and existing-jump (> (plist-get existing-jump :line-score) score))
              (puthash (or jump-line-number obarray-counter) (plist-put jump :line-score score) line-map)))
          (when (> score file-score)
            (setq file-score score))
          (puthash (or jump-path "obarray") (list file-score line-map) file-map))))
    (let ((jump-list '()))
      (maphash (lambda (full-path file-tuple)
                 (let ((file-score (nth 0 file-tuple))
                       (line-map (nth 1 file-tuple)))
                   (maphash (lambda (line-number jump)
                              (push (plist-put (plist-put jump :file-score file-score) :full-path full-path) jump-list))
                            line-map)))
               file-map)
      jump-list)))

(defun mk-proj-compare-jumps (a b)
  (let ((file-score-a (plist-get a :file-score))
        (file-score-b (plist-get b :file-score))
        (line-score-a (plist-get a :line-score))
        (line-score-b (plist-get b :line-score))
        (file-name-a (file-name-base (or (plist-get a :file-path) "obarray")))
        (file-name-b (file-name-base (or (plist-get b :file-path) "obarray")))
        (line-number-a (or (plist-get a :line-number) 0))
        (line-number-b (or (plist-get b :line-number) 0))
        (word-a (plist-get a :word))
        (word-b (plist-get b :word)))
    (cond ((< file-score-a file-score-b)
           t)
          ((and (= file-score-a file-score-b)
                (< line-score-a line-score-b))
           t)
          ((and (= file-score-a file-score-b)
                (= line-score-a line-score-b)
                (string-lessp file-name-b file-name-a))
           t)
          ((and (= file-score-a file-score-b)
                (= line-score-a line-score-b)
                (string= file-name-a file-name-b)
                (> line-number-a line-number-b))
           t)
          ((and (= file-score-a file-score-b)
                (= line-score-a line-score-b)
                (string= file-name-a file-name-b)
                (= line-number-a line-number-b)
                (string-lessp word-b word-a))
           t)
          (t nil))))

(defun mk-proj-sort-jumps (jump-list)
  (message "sort-jumps")
  (sort jump-list 'mk-proj-compare-jumps))

(defconst mk-proj-jump-buffer "*mk-proj: jumps*")

(defvar mk-proj-auto-jump-to-first-jump nil)

(defun mk-proj-select-jumps (jump-list &optional invoke-window)
  (unless invoke-window
    (setq invoke-window (get-buffer-window (current-buffer))))
  (let ((n (length jump-list)))
    (when (> n 0)
      (ring-insert find-tag-marker-ring (point-marker))
      (if (= n 1)
          (let* ((jump (car jump-list))
                 (locator (plist-get jump :locator))
                 (full-path (plist-get jump :full-path))
                 (line-number (plist-get jump :line-number))
                 (word (plist-get jump :word)))
            (if (and (string-equal full-path "obarray")
                     (functionp locator))
                (let* ((previous-buffer-list (buffer-list))
                       (location (funcall locator))
                       (location-buffer (car location))
                       (location-point (cdr location)))
                  (with-current-buffer location-buffer
                    (setq full-path (file-truename (buffer-file-name location-buffer))
                          line-number (line-number-at-pos location-point)))
                  (mk-proj-jump-highlight (find location-buffer previous-buffer-list)
                                          (mk-proj-jump invoke-window full-path line-number word)))
              (mk-proj-jump-highlight (find-buffer-visiting full-path)
                                      (mk-proj-jump invoke-window full-path line-number word))))
        (when (get-buffer-window mk-proj-jump-buffer)
          (delete-window (get-buffer-window mk-proj-jump-buffer)))
        (when (get-buffer mk-proj-jump-buffer)
          (kill-buffer mk-proj-jump-buffer))
        (with-current-buffer (get-buffer-create mk-proj-jump-buffer)
          (buffer-disable-undo (current-buffer))
          (mk-proj-jump-list-mode)
          (setq tabulated-list-entries nil)
          (let ((id 0))
            (dolist (jump (mk-proj-sort-jumps jump-list))
              (let ((locator (plist-get jump :locator))
                    (file-path (plist-get jump :file-path))
                    (line-number (plist-get jump :line-number))
                    (line-score (plist-get jump :line-score))
                    (system (plist-get jump :system))
                    (word (plist-get jump :word))
                    (docstring (plist-get jump :docstring))
                    (definition (plist-get jump :definition))
                    (full-path (plist-get jump :full-path))
                    (file-score (plist-get jump :file-score)))
                (push (list id (vector (list word
                                             'mouse-face 'highlight
                                             'face 'compilation-warning-face
                                             'action 'mk-proj-jump-action)
                                       (list (if (and file-path line-number) (concat (file-relative-name full-path (file-truename default-directory))
                                                                                     ":"
                                                                                     (format "%d" line-number))  "obarray")
                                             'mouse-face 'highlight
                                             'face 'compilation-info
                                             'action 'mk-proj-jump-action)
                                       (list (format "% 5d" line-score)
                                             'mouse-face 'highlight
                                             'face 'compilation-error
                                             'action 'mk-proj-jump-action)
                                       (list (prin1-to-string system)
                                             'mouse-face 'highlight
                                             'face 'compilation-info-face
                                             'action 'mk-proj-jump-action)
                                       (list (or docstring definition "")
                                             'mouse-face 'highlight
                                             'face 'compilation-face
                                             'action 'mk-proj-jump-action)
                                       (list invoke-window
                                             locator
                                             full-path
                                             line-number
                                             word
                                             file-score)))
                      tabulated-list-entries)
                (setq id (1+ id)))))
          (tabulated-list-print t)
          (goto-char (point-min))
          (forward-button 1)
          (when mk-proj-auto-jump-to-first-jump
            (mk-proj-jump-action)))
        (display-buffer mk-proj-jump-buffer)
        ))))

(defun mk-proj-jump (invoke-window full-path line-number &optional word)
  (select-window invoke-window)
  (let* ((marker nil)
         (continue-prevent-restore t))
    (with-current-buffer (find-file full-path)
      (goto-char (point-min))
      (forward-line (1- line-number))
      (when word
        (re-search-forward word (point-at-eol) t))
      (setq marker (point-marker))
      (recenter-top-bottom))
    (select-window (or (get-buffer-window mk-proj-jump-buffer) invoke-window))
    marker))

(defun mk-proj-jump-action (&optional button)
  (interactive)
  (unless button
    (setq button (point-marker)))
  (when (get-buffer mk-proj-jump-buffer)
    (with-current-buffer (get-buffer mk-proj-jump-buffer)
      (let* ((list-entry (with-current-buffer (marker-buffer button)
                           (get-text-property (marker-position button) 'tabulated-list-entry)))
             (n (1- (length list-entry)))
             (jump-info (elt list-entry n))
             (invoke-window (nth 0 jump-info))
             (locator (nth 1 jump-info))
             (full-path (nth 2 jump-info))
             (line-number (nth 3 jump-info))
             (word (nth 4 jump-info))
             (zeitgeist-prevent-send t))
        (if (and (string-equal full-path "obarray")
                 (functionp locator))
            (let* ((previous-buffer-list (buffer-list))
                   (location (funcall locator))
                   (location-buffer (car location))
                   (location-point (cdr location)))
              (with-current-buffer location-buffer
                (setq full-path (file-truename (buffer-file-name location-buffer))
                      line-number (line-number-at-pos location-point)))
              (mk-proj-jump-highlight (find location-buffer previous-buffer-list)
                                      (mk-proj-jump invoke-window full-path line-number word)))
          (mk-proj-jump-highlight (find-buffer-visiting full-path)
                                  (mk-proj-jump invoke-window full-path line-number word)))))))

(defvar mk-proj-jump-overlays nil)

(defun mk-proj-jump-cleanup-highlight (&optional existing-buffer)
  (let ((zeitgeist-prevent-send t)
        (continue-prevent-save t)
        (delete-buffer nil))
    (mapc (lambda (ov)
            (when (overlay-buffer ov)
              (with-current-buffer (overlay-buffer ov)
                (when (overlay-get ov 'delete-buffer)
                  (setq delete-buffer (overlay-get ov 'delete-buffer))
                  (unless (and (bufferp existing-buffer)
                               (eq delete-buffer existing-buffer))
                    (kill-buffer delete-buffer)
                    (setq delete-buffer nil)))
                (delete-overlay ov))))
          mk-proj-jump-overlays)
    (setq mk-proj-jump-overlays nil)
    delete-buffer))

;; existing-buffer=nil && delete-buffer=nil
;; -> set delete-buffer in overlay to (current-buffer) to kill current buffer
;; existing-buffer=<some buffer> && delete-buffer=nil
;; -> don't set delete-buffer in overlay and don't kill any buffers
;; existing-buffer=<some buffer> && delete-buffer=<some buffer>
;; -> there have been jumps through delete-buffer so it exists, but needs to be killed
;; existing-buffer=nil && delete-buffer=<some buffer>
;; -> this should not happen, without an existing-buffer but an marked delete-buffer,
;; cleanup-highlight should always kill delete-buffer and then return nil
(defun mk-proj-jump-highlight (&optional existing-buffer marker final)
  (let ((delete-buffer (mk-proj-jump-cleanup-highlight existing-buffer))
        (highlight-color (color-darken-name (face-attribute 'default :background) 10)))
    (when (get-buffer mk-proj-jump-buffer)
      (with-current-buffer (get-buffer mk-proj-jump-buffer)
        (let ((ov (make-overlay (point-at-bol) (point-at-bol 2))))
          (overlay-put ov 'face `((:background ,highlight-color)))
          (overlay-put ov 'jump-highlight 'select)
          (push ov mk-proj-jump-overlays))))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char (marker-position marker))
          (let ((ov (make-overlay (point-at-bol) (point-at-bol 2))))
            (when (or (not existing-buffer) delete-buffer)
              (overlay-put ov 'delete-buffer (or delete-buffer (current-buffer))))
            (overlay-put ov 'pop-tag-marker (ring-ref find-tag-marker-ring 0))
            (overlay-put ov 'face `((:background ,highlight-color)))
            (overlay-put ov 'jump-highlight 'view)
            (push ov mk-proj-jump-overlays)))))))

(defun mk-proj-jump-next ()
  (interactive)
  (let ((last-window (get-buffer-window (current-buffer))))
    (when (get-buffer mk-proj-jump-buffer)
      (select-window (get-buffer-window (get-buffer mk-proj-jump-buffer)))
      (with-current-buffer (get-buffer mk-proj-jump-buffer)
        (when (< (point-at-eol 2) (point-max))
          (when (overlays-at (point))
            (forward-button (length tabulated-list-format)))
          (mk-proj-jump-action))))
    (select-window last-window)))

(defun mk-proj-jump-prev ()
  (interactive)
  (let ((last-window (get-buffer-window (current-buffer))))
    (when (get-buffer mk-proj-jump-buffer)
      (select-window (get-buffer-window (get-buffer mk-proj-jump-buffer)))
      (with-current-buffer (get-buffer mk-proj-jump-buffer)
        (when (> (point-at-bol) (point-min))
          (when (overlays-at (point))
            (backward-button (length tabulated-list-format)))
          (mk-proj-jump-action))))
    (select-window last-window)))

(defun mk-proj-jump-abort ()
  (interactive)
  (when (get-buffer-window mk-proj-jump-buffer)
    (delete-window (get-buffer-window mk-proj-jump-buffer)))
  (when (get-buffer mk-proj-jump-buffer)
    (kill-buffer mk-proj-jump-buffer))
  (pop-tag-mark)
  (mk-proj-jump-cleanup-highlight))

(defun mk-proj-jump-quit ()
  (interactive)
  (when (get-buffer-window mk-proj-jump-buffer)
    (delete-window (get-buffer-window mk-proj-jump-buffer)))
  (when (get-buffer mk-proj-jump-buffer)
    (kill-buffer mk-proj-jump-buffer))
  (mk-proj-jump-cleanup-highlight (current-buffer)))

(defun mk-proj-jump-go ()
  (interactive)
  (mk-proj-jump-action)
  (mk-proj-jump-quit))

(define-derived-mode mk-proj-jump-list-mode tabulated-list-mode "Mk-Project jumps"
  (setq tabulated-list-format [("Word" 30 t)
                               ("File" 70 t)
                               ("Score" 5 t)
                               ("Sys" 7 t)
                               ("Text" 0 nil)]
        tabulated-list-padding 1
        tabulated-list-sort-key nil)
  (define-key mk-proj-jump-list-mode-map (kbd "q") 'mk-proj-jump-quit)
  (define-key mk-proj-jump-list-mode-map (kbd "C-g") 'mk-proj-jump-abort)
  (define-key mk-proj-jump-list-mode-map (kbd "C-n") 'mk-proj-jump-next)
  (define-key mk-proj-jump-list-mode-map (kbd "n") 'mk-proj-jump-next)
  (define-key mk-proj-jump-list-mode-map (kbd "<down>") 'mk-proj-jump-next)
  (define-key mk-proj-jump-list-mode-map (kbd "C-p") 'mk-proj-jump-prev)
  (define-key mk-proj-jump-list-mode-map (kbd "p") 'mk-proj-jump-prev)
  (define-key mk-proj-jump-list-mode-map (kbd "<up>") 'mk-proj-jump-prev)
  (define-key mk-proj-jump-list-mode-map (kbd "<return>") 'mk-proj-jump-go)
  (tabulated-list-init-header))

(defun mk-proj-update-gtags-definitions-cache (proj-name)
  (let* ((cmd (concat "global --match-part=first -xGq -d \".*\""))
         (lines (split-string (condition-case nil (shell-command-to-string cmd) (error nil)) "\n" t))
         (case-fold-search nil))
    (when lines
      (loop for line in lines
            do (when (string-match (concat "^"
                                           "\\([^ ]*\\)" ;; completion
                                           "[ \t]+\\([[:digit:]]+\\)" ;; linum
                                           "[ \t]+\\([^ \t]+\\)" ;; file
                                           "[ \t]+\\(.*\\)" ;; definition
                                           "$")
                                   line)
                 (let* ((completion (match-string 1 line))
                        (cached-definition (gethash completion mk-proj-definitions-cache)))
                   (plist-put cached-definition :line-number (match-string 2 line))
                   (plist-put cached-definition :file-path (match-string 3 line))
                   (plist-put cached-definition :definition (match-string 4 line))
                   (unless cached-definition
                     (puthash completion
                              cached-definition
                              mk-proj-definitions-cache))))))))

(defun mk-proj-update-gtags-completions-cache (proj-name)
  (let* ((cmd (concat "global --match-part=first -Gq -c \"\""))
         (completions (split-string (condition-case nil (shell-command-to-string cmd) (error nil)) "\n" t))
         (completions-cache (gethash proj-name mk-proj-completions-cache)))
    (when completions
      (loop for completion in completions
            do (puthash completion
                        nil
                        completions-cache)))))

(defun mk-proj-update-obarray-definitions-cache (proj-name)
  (do-all-symbols (sym)
    (when (or (fboundp sym)
              (boundp sym))
      (let* ((completion (symbol-name sym))
             (doc (condition-case nil
                      (if (fboundp sym)
                          (documentation sym t)
                        (documentation-property sym 'variable-documentation t))
                    (error nil)))
             (case-fold-search nil)
             (docstring (and (stringp doc)
                             (string-match ".*$" doc)
                             (match-string 0 doc)))
             (cached-definition (when docstring (gethash completion mk-proj-definitions-cache))))
        (if (and docstring cached-definition)
            (plist-put cached-definition :docstring docstring)
          (puthash completion
                   (plist-put cached-definition :docstring docstring)
                   mk-proj-definitions-cache))))))

(defun mk-proj-update-obarray-completions-cache (proj-name)
  (let ((completions-cache (gethash proj-name mk-proj-completions-cache)))
    (do-all-symbols (sym)
      (when (or (fboundp sym)
                (boundp sym))
        (let* ((completion (symbol-name sym)))
          (puthash completion
                   nil
                   completions-cache))))))

(defun mk-proj-update-completions-cache (&optional proj-name)
  (when (and mk-proj-name (not proj-name))
    (let ((guessed-name (cadr (assoc 'name (mk-proj-guess-alist)))))
      (when guessed-name (setq proj-name guessed-name))))
  (setq proj-name (or proj-name
                      mk-proj-after-save-current-project
                      mk-proj-name
                      (cadr (assoc 'name (mk-proj-guess-alist)))))
  (unless proj-name
    (mk-proj-assert-proj))
  (if (not (hash-table-p (gethash proj-name mk-proj-completions-cache)))
      (puthash proj-name (make-hash-table :test 'equal :size 100000) mk-proj-completions-cache)
    (maphash (lambda (k v)
               (remhash k (gethash proj-name mk-proj-completions-cache)))
             (gethash proj-name mk-proj-completions-cache)))
  (unless (string-equal mk-proj-name proj-name)
    (project-setup-tags proj-name))
  (mk-proj-update-gtags-completions-cache proj-name)
  (unless (string-equal mk-proj-name proj-name)
    (project-setup-tags mk-proj-name))
  ;; (mk-proj-update-imenu-completions-cache proj-name)
  (when (find 'elisp (mk-proj-src-pattern-languages (mk-proj-get-config-val 'src-patterns proj-name)))
    (mk-proj-update-obarray-completions-cache proj-name))
  (garbage-collect))

(defun mk-proj-completions (&optional prefix proj-name buffer)
  (when (and mk-proj-name (not proj-name))
    (let ((guessed-name (cadr (assoc 'name (mk-proj-guess-alist)))))
      (when guessed-name (setq proj-name guessed-name))))
  (setq proj-name (or proj-name
                      mk-proj-name
                      (cadr (assoc 'name (mk-proj-guess-alist)))))
  (unless proj-name
    (mk-proj-assert-proj))
  (unless prefix
    (setq prefix ""))
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((unique-completions '())
        (case-fold-search nil))
    (when (not (gethash proj-name mk-proj-completions-cache))
      (mk-proj-update-completions-cache proj-name))
    (maphash (lambda (k v)
               (when (or (string-equal prefix "")
                         (string-match (concat "^" prefix) k))
                 (push k unique-completions)))
             (gethash proj-name mk-proj-completions-cache))
    (reverse unique-completions)))

(defun mk-proj-merge-obarray-jumps (obarray-jumps &rest rest)
  (if obarray-jumps
      (let ((obarray-map (make-hash-table :test 'equal :size 100000))
            (merged-jumps nil))
        (dolist (jump obarray-jumps)
          (puthash (plist-get jump :word) jump obarray-map))
        (dolist (jump (apply #'append rest))
          (let* ((keyword (plist-get jump :word))
                 (obarray-jump (gethash keyword obarray-map)))
            (if (not obarray-jump)
                (push jump merged-jumps)
              (let ((jump-path (plist-get jump :file-path))
                    (jump-line-number (plist-get jump :line-number))
                    (jump-docstring (plist-get jump :docstring))
                    (jump-definition (plist-get jump :definition))
                    (new-jump obarray-jump))
                (when (position 'elisp (mk-proj-src-pattern-languages (list jump-path)))
                  (plist-put new-jump :file-path jump-path)
                  (plist-put new-jump :line-number jump-line-number)
                  (when (and (= (length (plist-get obarray-jump :docstring)) 0)
                             (> (length jump-docstring) 0))
                    (plist-put new-jump :docstring jump-docstring))
                  (when (and (= (length (plist-get obarray-jump :definition)) 0)
                             (> (length jump-definition) 0))
                    (plist-put new-jump :definition jump-definition))
                  (puthash keyword new-jump obarray-map))))))
        (maphash (lambda (k v)
                   (push v merged-jumps))
                 obarray-map)
        merged-jumps)
    (apply #'append rest)))

(defun project-jump-definition (word &optional proj-name proj-alist buffer)
  (interactive (list (let* ((ido-enable-flex-matching t)
                            (case-fold-search nil)
                            (ido-case-fold nil))
                       (substring-no-properties (ido-completing-read "Symbol: "
                                                                     (mk-proj-completions) nil nil
                                                                     (substring-no-properties (or (thing-at-point mk-proj-thing-selector) "")))))))
  (when (and mk-proj-name (not proj-alist))
    (let ((guessed-name (cadr (assoc 'name (mk-proj-guess-alist)))))
      (when guessed-name (setq proj-name guessed-name))))
  (setq proj-alist (or proj-alist
                       (mk-proj-find-alist proj-name)
                       (mk-proj-find-alist mk-proj-name)
                       (mk-proj-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (mk-proj-assert-proj))
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((jumps (mk-proj-merge-obarray-jumps (mk-proj-find-symbol proj-name proj-alist 'obarray (concat "^" word "$"))
                                            (or (mk-proj-find-symbol proj-name proj-alist 'gtags word (concat "global -x -d " (prin1-to-string word)))
                                                (mk-proj-find-symbol proj-name proj-alist 'gtags word (concat "global -x -s " (prin1-to-string word))))
                                            (mk-proj-find-symbol proj-name proj-alist 'imenu (concat "^" word "$")))))
    (mk-proj-select-jumps (mk-proj-score-jumps jumps (regexp-quote word) buffer))))

(defun project-jump-regexp (regexp &optional proj-name proj-alist buffer)
  (interactive (list (let* ((ido-enable-flex-matching t))
                       (substring-no-properties (ido-completing-read "Match: "
                                                                     (mk-proj-completions))))))
  (when (and mk-proj-name (not proj-alist))
    (let ((guessed-name (cadr (assoc 'name (mk-proj-guess-alist)))))
      (when guessed-name (setq proj-name guessed-name))))
  (setq proj-alist (or proj-alist
                       (mk-proj-find-alist proj-name)
                       (mk-proj-find-alist mk-proj-name)
                       (mk-proj-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (mk-proj-assert-proj))
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((jumps (mk-proj-merge-obarray-jumps (mk-proj-find-symbol proj-name proj-alist 'obarray (concat "^" regexp))
                                            (or (mk-proj-find-symbol proj-name proj-alist 'gtags regexp (concat "global -x -e " (prin1-to-string (concat regexp ".*"))))
                                                (mk-proj-find-symbol proj-name proj-alist 'gtags regexp (concat "global -x -s " (prin1-to-string (concat regexp ".*")))))
                                            (mk-proj-find-symbol proj-name proj-alist 'imenu regexp))))
    (mk-proj-select-jumps (mk-proj-score-jumps jumps regexp buffer))))

(defun project-jump-references (word &optional proj-name buffer)
  )

(defun project-jump-callees (word &optional proj-name buffer)
  ;; only supported by cscope afaik
  )

(defun project-jump-callers (word &optional proj-name buffer)
  ;; only supported by cscope afaik
  )

;; ---------------------------------------------------------------------
;; Compile
;; ---------------------------------------------------------------------

(defun mk-proj-buffer-lang (&optional buffer)
  (when (buffer-file-name (or buffer (current-buffer)))
    (cadr (assoc (car (last (split-string (buffer-file-name (or buffer (current-buffer))) "\\.")))
                 mk-proj-src-pattern-table))))

(defun project-compile ()
  (interactive)
  (if (and (called-interactively-p 'interactive)
           (not mk-proj-name))
      (call-interactively 'compile)
    (mk-proj-assert-proj nil)
    (cl-flet ((internal-compile (&optional cmd)
                                (let ((saved-compile-command compile-command)
                                      (compile-command (or cmd compile-command))
                                      (result-compile-command nil))
                                  (if (not (called-interactively-p 'interactive))
                                      (let ((compilation-ask-about-save t)
                                            (compilation-read-command t))
                                        (call-interactively 'compile))
                                    (call-interactively 'compile))
                                  (setq result-compile-command compile-command
                                        compile-command saved-compile-command)
                                  result-compile-command)))
      ;;(mk-proj-save-state)
      (with-current-buffer (get-buffer (mk-proj-fib-name))
        (save-buffer))
      (let ((cmd (mk-proj-get-config-val 'compile-cmd)))
        (when (string-equal (buffer-name (current-buffer)) "*compilation*")
          (call-interactively 'other-window))
        (mk-proj-with-directory (mk-proj-get-config-val 'basedir)
                                (cond ((listp cmd)
                                       (let* ((old-history cmd)
                                              (compile-history (remove-duplicates (append old-history compile-history) :test 'equal :from-end t))
                                              (old-cmd (car cmd))
                                              (new-cmd (internal-compile old-cmd)))
                                         (unless (string-equal old-cmd new-cmd)
                                           (mk-proj-set-config-val 'compile-cmd (remove-duplicates (append (list new-cmd) old-history) :test 'equal :from-end t)))))
                                      ((stringp cmd)
                                       (let* ((old-cmd cmd)
                                              (new-cmd (internal-compile old-cmd))
                                              (new-list (list new-cmd old-cmd)))
                                         (unless (string-equal old-cmd new-cmd)
                                           (mk-proj-set-config-val 'compile-cmd new-list))))
                                      ((commandp cmd)
                                       (call-interactively cmd))
                                      ((functionp cmd)
                                       (funcall cmd))
                                      (t
                                       (mk-proj-set-config-val 'compile-cmd (list (internal-compile))))))
        ;;(project-after-save-update)
        ))))

;; ---------------------------------------------------------------------
;; Files
;; ---------------------------------------------------------------------

(defun mk-proj-fib-init (&optional proj-name quiet)
  "Either load the *file-index* buffer from the file cache, or create it afresh."
  (if (and (mk-proj-get-config-val 'file-list-cache proj-name t)
           (file-readable-p (mk-proj-get-config-val 'file-list-cache proj-name t)))
      (let ((zeitgeist-prevent-send t))
        (with-current-buffer (find-file-noselect (mk-proj-get-config-val 'file-list-cache proj-name t))
          (with-current-buffer (rename-buffer (mk-proj-fib-name proj-name))
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            (unless quiet
              (message (concat "Loading " (mk-proj-fib-name proj-name) " from %s") (mk-proj-get-config-val 'file-list-cache proj-name t))))))
    (project-index proj-name)))

(defun mk-proj-fib-clear (&optional proj-name)
  "Clear the contents of the fib buffer"
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((buf (get-buffer (mk-proj-fib-name proj-name))))
    (when buf
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (delete-region (point-min) (point-max))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)))))

(defun mk-proj-fib-cb (process event &optional proj-name proj-alist quiet)
  "Handle failure to complete fib building"
  (setq proj-alist (or proj-alist
                       (mk-proj-find-alist proj-name)
                       (mk-proj-find-alist mk-proj-name)
                       (mk-proj-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (cond
   ((string= event "finished\n")
    (let ((zeitgeist-prevent-send t))
      (with-current-buffer (get-buffer (mk-proj-fib-name proj-name))
        (when (mk-proj-get-config-val 'file-list-cache proj-name t proj-alist)
          (write-file (mk-proj-get-config-val 'file-list-cache proj-name t proj-alist))
          (rename-buffer (mk-proj-fib-name proj-name))
          (set-buffer-modified-p nil))
        (setq buffer-read-only t)))
    (unless quiet
      (message "Refreshing %s buffer...done" (mk-proj-fib-name proj-name))))
   (t
    (mk-proj-fib-clear proj-name)
    (unless quiet
      (message "Failed to generate the %s buffer!" (mk-proj-fib-name proj-name))))))

(defun mk-proj-find-cmd-src-args (src-patterns &optional proj-name proj-alist)
  "Generate the ( -name <pat1> -o -name <pat2> ...) pattern for find cmd"
  (setq proj-alist (or proj-alist
                       (mk-proj-find-alist proj-name)
                       (mk-proj-find-alist mk-proj-name)
                       (mk-proj-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (if src-patterns
      (let ((name-expr (if (eq system-type 'windows-nt) " \(" " \\("))
            (regex-or-name-arg (if (mk-proj-get-config-val 'patterns-are-regex proj-name t proj-alist)
                                   "-regex"
                                 "-name")))
        (dolist (pat src-patterns)
          (setq name-expr (concat name-expr " " regex-or-name-arg " \"" pat "\" -o ")))
        (concat (if (string-match "-o $"  name-expr)
                    (replace-match "" t t name-expr)
                  name-expr)
                (if (eq system-type 'windows-nt)
                    "\) "
                  "\\) ")))
    ""))

(defun mk-proj-find-cmd-ignore-args (ignore-patterns &optional proj-name proj-alist)
  "Generate the -not ( -name <pat1> -o -name <pat2> ...) pattern for find cmd"
  (setq proj-alist (or proj-alist
                       (mk-proj-find-alist proj-name)
                       (mk-proj-find-alist mk-proj-name)
                       (mk-proj-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (concat " -not " (mk-proj-find-cmd-src-args (append ignore-patterns (list ".*/flycheck_.*")) proj-name proj-alist)))

(defvar mk-proj-index-processes (make-hash-table))

(defun* project-index (&optional proj-name proj-alist (async t) (do-friends nil) (quiet nil) (terminator nil) (parent nil))
  "Regenerate the *file-index* buffer that is used for project-find-file"
  (interactive)
  (setq proj-alist (or proj-alist
                       (mk-proj-find-alist proj-name)
                       (mk-proj-find-alist mk-proj-name)
                       (mk-proj-guess-alist)
                       ))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (mk-proj-assert-proj))
  (unless do-friends
    (setq do-friends (and (string-equal proj-name mk-proj-name)
                          (mk-proj-has-univ-arg))))
  (let* ((process)
         (friends (mk-proj-get-config-val 'friends proj-name nil proj-alist))
         (default-directory (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t proj-alist)))
         (start-dir (if mk-proj-file-index-relative-paths
                        "."
                      (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t proj-alist))))
         (find-exe (or (executable-find "gfind.exe")
                       (executable-find "find")))
         (find-cmd (concat "\"" find-exe "\" \"" start-dir "\" -type f "
                           (mk-proj-find-cmd-src-args (mk-proj-get-config-val 'src-patterns proj-name t proj-alist) proj-name proj-alist)
                           (mk-proj-find-cmd-ignore-args (mk-proj-get-config-val 'ignore-patterns proj-name t proj-alist) proj-name proj-alist)
                           ;; - had a problem under windows where gfind could not read some file and then always exit with error, this hack
                           ;; works around that and makes the command always exit with 0, may be neccessary on linux at some point too
                           (when (eq system-type 'windows-nt)
                               " 2> nul & exit /b 0")))
         (proc-name (concat "index-process-" proj-name)))
    (when (mk-proj-get-config-val 'file-list-cache proj-name t proj-alist)
      (mk-proj-fib-clear proj-name)
      (when (mk-proj-get-vcs-path proj-name)
        (setq find-cmd (concat find-cmd " -not -path " (concat "'*/" (mk-proj-get-vcs-path proj-name) "/*'"))))
      (with-current-buffer (get-buffer-create (mk-proj-fib-name proj-name))
        (buffer-disable-undo) ;; this is a large change we don't need to undo
        (setq buffer-read-only nil))
      (unless quiet
        (message "project-index cmd: \"%s\"" find-cmd)
        (message "Refreshing %s buffer..." (mk-proj-fib-name proj-name)))
      (setq process (start-process-shell-command proc-name (mk-proj-fib-name proj-name) find-cmd))
      (if parent
          (push process (cadr (gethash parent mk-proj-index-processes)))
        (puthash process (list (length friends) nil) mk-proj-index-processes)
        (setq parent process))
      (set-process-sentinel (get-process proc-name) `(lambda (p e)
                                                       (mk-proj-fib-cb p e ,proj-name (quote ,proj-alist) ,quiet)
                                                       (let ((tuple (gethash ,parent mk-proj-index-processes)))
                                                         (when (and tuple (quote ,terminator))
                                                           (let ((friends-num (nth 0 tuple))
                                                                 (friends-list (nth 1 tuple)))
                                                             (when (and (eq (process-status ,parent) 'exit)
                                                                        (or (not ,do-friends)
                                                                            (and (= friends-num (length friends-list))
                                                                                 (every (lambda (o) (eq (process-status o) 'exit)) friends-list))))
                                                               (remhash ,parent mk-proj-index-processes)
                                                               (funcall (quote ,terminator) ,proj-name (quote ,proj-alist))))))))
      (unless async
        (while (or (string-equal (process-status process) "run")
                   (equal (process-status process) 'run))
          (sleep-for 0 4)))
      (when do-friends
        (dolist (friend friends)
          (let ((friend-alist (mk-proj-find-alist friend)))
            (when friend-alist
              (project-index friend friend-alist async nil quiet terminator parent))))))))

(defun mk-proj-fib-matches (&optional regex proj-name proj-alist)
  "Return list of files in *file-index* matching regex.

REGEX can be a list or a single regex.
If it is nil, return all files.

Returned file paths are relative to the project's basedir."
  (setq proj-alist (or proj-alist
                       (mk-proj-find-alist proj-name)
                       (mk-proj-find-alist mk-proj-name)
                       (mk-proj-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (get-buffer (mk-proj-fib-name proj-name))
    (mk-proj-fib-init proj-name))
  (when (or proj-alist (gethash proj-name mk-proj-list nil))
    (with-current-buffer (mk-proj-fib-name proj-name)
      (let ((basedir (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t proj-alist)))
            (current-filename nil)
            (case-fold-search nil))
        (sort (loop for line in (split-string (buffer-string) "\n" t)
                    if (> (length line) 0)
                    do (setq current-filename (if (file-name-absolute-p line)
                                                  (file-relative-name line basedir)
                                                line))
                    if (or (not regex)
                           (and (stringp regex)
                                (string-match regex current-filename))
                           (and (listp regex)
                                (some (lambda (re) (string-match re current-filename)) regex)))
                    collect current-filename)
              #'string-lessp)))))

(defun mk-proj-files (&optional proj-name proj-alist truenames)
  (setq proj-alist (or proj-alist
                       (mk-proj-find-alist proj-name)
                       (mk-proj-find-alist mk-proj-name)
                       (mk-proj-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((basedir (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t proj-alist))))
    (when truenames (setq basedir (file-truename basedir)))
    (mapcar (lambda (f) (expand-file-name (concat basedir f)))
            (mk-proj-fib-matches nil proj-name proj-alist))))

(defun mk-proj-unique-files (&optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((proj-files (mk-proj-files proj-name nil t))
        (friendly-files (mk-proj-friendly-files proj-name nil t))
        (unique-friends '()))
    (dolist (f friendly-files)
      (unless (find f proj-files :test 'equal)
        (setq unique-friends (append (list f) unique-friends))))
    (append proj-files unique-friends)))

(defun mk-proj-friendly-files (&optional proj-name proj-alist truenames)
  (setq proj-alist (or proj-alist
                       (mk-proj-find-alist proj-name)
                       (mk-proj-find-alist mk-proj-name)
                       (mk-proj-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let* ((basedir (file-truename (mk-proj-get-config-val 'basedir proj-name t proj-alist)))
         (friendly-files (mapcan (lambda (friend)
                                   (let ((friend-file (mk-proj-with-directory basedir (expand-file-name friend))))
                                     (if (file-exists-p friend-file)
                                         (list friend-file)
                                       (mk-proj-files friend nil truenames))))
                                 (mk-proj-get-config-val 'friends proj-name t proj-alist))))
    friendly-files))

(defun project-dired ()
  "Open dired in the project's basedir (or jump to the existing dired buffer)"
  (interactive)
  (mk-proj-assert-proj t)
  (dired (mk-proj-get-config-val 'basedir)))

(defun project-multi-occur (regex)
  "Search all open project files for 'regex' using `multi-occur'.

Act like `project-multi-occur-with-friends' if called with prefix arg."
  (interactive "sRegex: ")
  (mk-proj-assert-proj t)
  (if (mk-proj-has-univ-arg)
      (project-multi-occur-with-friends regex)
    (multi-occur (mk-proj-filter (lambda (b) (if (buffer-file-name b) b nil))
                                 (mk-proj-buffers))
                 regex)))

;; ---------------------------------------------------------------------
;; Friends
;; ---------------------------------------------------------------------

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
                 (when (some (lambda (f)
                               (string-equal f proj-name))
                             (mk-proj-config-val 'friends c))
                   (setq r (append r `(,k)))))) mk-proj-list)
    (remove-duplicates (append r (mk-proj-config-val 'friends proj-name t)) :test #'string-equal)))

(defun mk-proj-fib-friend-matches (&optional regex proj-name proj-alist)
  (setq proj-alist (or proj-alist
                       (mk-proj-find-alist proj-name)
                       (mk-proj-find-alist mk-proj-name)
                       (mk-proj-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((resulting-matches '())
        (case-fold-search nil))
    (dolist (friend (mk-proj-get-config-val 'friends proj-name t proj-alist) resulting-matches)
      (if (file-exists-p (mk-proj-with-directory (mk-proj-get-config-val 'basedir proj-name t proj-alist)
                                                 (expand-file-name friend)))
          (if regex
              (when (string-match regex friend) (add-to-list 'resulting-matches (expand-file-name friend)))
            (add-to-list 'resulting-matches (expand-file-name friend)))
        (setq resulting-matches (append resulting-matches
                                        (let ((friend-alist (mk-proj-find-alist friend)))
                                          (when friend-alist
                                            (mapcar (lambda (f)
                                                      (expand-file-name (concat (file-name-as-directory (mk-proj-get-config-val 'basedir friend t friend-alist)) f)))
                                                    (mk-proj-fib-matches regex friend friend-alist))))))))
    ))

(defun mk-proj-friendly-buffer-p (buf &optional proj-name)
  "Check if BUF is a friend of PROJ-NAME."
  (unless (mk-proj-buffer-p buf)
    (let ((file-name (mk-proj-buffer-name buf)))
      (if (and file-name
               (block "friend-loop"
                 (dolist (f (mk-proj-find-friendly-projects proj-name))
                   (if (file-exists-p (expand-file-name f))
                       (when (string-equal f file-name)
                         (return-from "friend-loop" t))
                     (when (mk-proj-find-alist f t)
                       (let* ((friend-config (mk-proj-find-alist f t))
                              (non-slash-basedir (expand-file-name (car (cdr (assoc 'basedir friend-config)))))
                              (slash-basedir (if (string-equal (substring non-slash-basedir -1) "/")
                                                  non-slash-basedir
                                                (concat non-slash-basedir "/")))
                              (case-fold-search nil))
                         (when (or (string-match (concat "^" (regexp-quote slash-basedir)) file-name)
                                   (string-match (concat "^" (regexp-quote (file-truename slash-basedir))) file-name))
                           (return-from "friend-loop" t))))))))
          t
        nil))))

(defun mk-proj-friendly-file-buffer-p (buf &optional proj-name)
  (and (buffer-file-name buf)
       (mk-proj-friendly-buffer-p buf proj-name)))

(defun mk-proj-friendly-special-buffer-p (buf &optional proj-name)
  (let ((case-fold-search nil))
    (and (string-match "\*[^\*]\*" (buffer-name buf))
         (mk-proj-friendly-buffer-p buf proj-name))))

(defun mk-proj-friendly-dired-buffer-p (buf &optional proj-name)
  (and (with-current-buffer buf
         (eq major-mode 'dired-mode))
       (mk-proj-friendly-buffer-p buf proj-name)))


(defun mk-proj-friendly-buffers (&optional proj-name)
  "Return all buffers that are friendly to the project"
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((buffers nil))
    (dolist (b (buffer-list))
      (when (mk-proj-friendly-buffer-p b proj-name)
        (push b buffers)))
      buffers))

(defun mk-proj-friendly-file-buffers (&optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (remove-if (lambda (buf) (not (buffer-file-name buf))) (mk-proj-friendly-buffers proj-name)))

(defun mk-proj-friendly-special-buffers (&optional proj-name friends-only)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((case-fold-search nil))
    (append (remove-if (lambda (buf) (not (string-match "\*[^\*]\*" (buffer-name buf)))) (mk-proj-friendly-buffers proj-name))
            (remove-if (lambda (buf) (or (and (symbolp 'mk-org-project-buffer-name)
                                              (not (string-equal (mk-org-project-buffer-name proj-name) (buffer-name buf))))
                                         (compilation-buffer-p buf)))
                       (buffer-list)))))

(defun mk-proj-friendly-dired-buffers (&optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (remove-if (lambda (buf) (not (mk-proj-friendly-dired-buffer-p buf))) (mk-proj-friendly-buffers proj-name)))

(defun mk-proj-save-open-friends-info ()
  (when (mk-proj-get-config-val 'open-friends-cache)
    (let ((zeitgeist-prevent-send t))
      (with-temp-buffer
        (dolist (f (remove-duplicates (mapcar (lambda (b) (mk-proj-buffer-name b)) (mk-proj-friendly-buffers)) :test #'string-equal))
          (when f
            (unless (string-equal (mk-proj-get-config-val 'etags-file) f)
              (insert f "\n"))))
        (if (file-writable-p (mk-proj-get-config-val 'open-friends-cache))
            (write-region (point-min)
                          (point-max)
                          (mk-proj-get-config-val 'open-friends-cache))
          (message "Wrote open friends to %s" (mk-proj-get-config-val 'open-friends-cache))
          (message "Cannot write to %s" (mk-proj-get-config-val 'open-friends-cache)))))))

(defun mk-proj-visit-saved-open-friends ()
  (let ((zeitgeist-prevent-send t))
    (when (mk-proj-get-config-val 'open-friends-cache)
      (when (file-readable-p (mk-proj-get-config-val 'open-friends-cache))
        (message "Reading open friends from %s" (mk-proj-get-config-val 'open-friends-cache))
        (with-temp-buffer
          (insert-file-contents (mk-proj-get-config-val 'open-friends-cache))
          (goto-char (point-min))
          (while (not (eobp))
            (let ((start (point)))
              (while (not (eolp)) (forward-char)) ; goto end of line
              (let ((line (buffer-substring start (point)))
                    (enable-local-variables :safe)
                    (continue-prevent-restore t))
                (message "Attempting to open %s" line)
                (if (file-exists-p line)
                    (unless (get-file-buffer line)
                      (find-file-noselect line t t))
                  (kill-line))))
            (forward-line)))))))

(defun project-close-friends ()
  (interactive)
  (mk-proj-assert-proj)
  (let ((closed nil)
        (dirty nil)
        (zeitgeist-prevent-send t)
        (continue-prevent-save t))
    (dolist (b (append (mk-proj-friendly-buffers) (mk-proj-friendly-dired-buffers)))
      (cond
       ((buffer-modified-p b)
        (push (buffer-name) dirty))
       (t
        (kill-buffer b)
        (push (buffer-name) closed))))
    (message "Closed %d friendly buffers, %d modified friendly buffers where left open"
             (length closed) (length dirty))))

(defun project-multi-occur-with-friends (regex)
  "Search all open project files (including friends) for 'regex' using `multi-occur'."
  (interactive "sRegex: ")
  (mk-proj-assert-proj t)
  (multi-occur (mk-proj-filter (lambda (b) (if (buffer-file-name b) b nil))
                               (append (mk-proj-buffers) (mk-proj-friendly-buffers)))
               regex))

(defun mk-proj-friend-basedirs ()
  "Return all friends basedirs. This may also return single filenames instead of a directory."
  (let* ((basedirs '()))
    (dolist (f (mk-proj-find-friendly-projects) basedirs)
      (if (file-exists-p (expand-file-name f))
          (add-to-list 'basedirs f)
        (when (and f (mk-proj-config-val 'basedir f))
          (add-to-list 'basedirs (mk-proj-config-val 'basedir f)))))))

;; ---------------------------------------------------------------------
;; After saving
;; ---------------------------------------------------------------------

(defun mk-proj-after-save-add-pattern (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (when mk-proj-name
    (let* ((file-name (expand-file-name (buffer-file-name buffer)))
           (extension (car (last (split-string file-name "\\."))))
           (new-pattern (concat ".*\\." extension))
           (src-patterns (mk-proj-get-config-val 'src-patterns mk-proj-name t))
           (case-fold-search nil)
           (buildsystem-files (loop for bs in mk-proj-buildsystems
                                    append (cadr (assoc 'files (cadr bs)))))
           (buildsystem-file-found (some (lambda (buildsystem-file)
                                           (when (string-match (concat (regexp-quote buildsystem-file) "$") file-name)
                                             buildsystem-file)) buildsystem-files)))
      (when (and (assoc extension mk-proj-src-pattern-table)
                 (or (string-match (concat "^" (regexp-quote (file-name-as-directory (mk-proj-get-config-val 'basedir mk-proj-name t)))) file-name)
                     (string-match (concat "^" (regexp-quote (file-name-as-directory (mk-proj-get-config-val 'basedir mk-proj-name t)))) (file-truename file-name)))
                 (not (some (lambda (pattern) (string-match pattern file-name)) src-patterns)))
        (mk-proj-set-config-val 'src-patterns (add-to-list 'src-patterns new-pattern)))
      (when (and buildsystem-file-found
                 (not (some (lambda (pattern) (string-match pattern buildsystem-file-found)) src-patterns)))
        (mk-proj-set-config-val 'src-patterns (add-to-list 'src-patterns (concat ".*" (regexp-quote buildsystem-file-found) "$")))))))


(defun mk-proj-after-save-update (&optional proj-name)
  (unless mk-proj-after-save-update-in-progress
    (if (and (not (or mk-proj-prevent-after-save-update
                      (string-match ".*recentf.*" (buffer-name (current-buffer)))
                      (string-match ".*file-list-cache.*" (buffer-name (current-buffer)))
                      (string-match ".*sourcemarker-db.*" (buffer-name (current-buffer)))
                      (string-match ".*continue-db.*" (buffer-name (current-buffer)))
                      (string-match ".*archive-contents.*" (buffer-name (current-buffer)))
                      (string-match ".*\*http .*\*" (buffer-name (current-buffer)))))
             (buffer-file-name (current-buffer))
             (get-buffer-window (current-buffer) 'visible))
        (progn
          (setq proj-name (or proj-name
                              mk-proj-name
                              (cadr (assoc 'name (mk-proj-guess-alist)))))
          (when proj-name
            (setq mk-proj-after-save-update-in-progress t
                  mk-proj-after-save-current-buffer (current-buffer)
                  mk-proj-after-save-current-project proj-name)
            (run-with-idle-timer 10 nil 'project-after-save-update))))))

(defun project-after-save-update (&optional p proj-name buffer)
  (interactive "p")
  (setq proj-name (or proj-name
                      mk-proj-after-save-current-project
                      mk-proj-name
                      (cadr (assoc 'name (mk-proj-guess-alist)))))
  (when mk-proj-name
    (let ((guessed-name (cadr (assoc 'name (mk-proj-guess-alist)))))
      (when guessed-name (setq proj-name guessed-name))))
  (unless proj-name
    (mk-proj-assert-proj))
  (unless buffer
    (setq buffer (or mk-proj-after-save-current-buffer
                     (current-buffer))))
  (condition-case e
      (progn
        (if (or p (mk-proj-buffer-p buffer proj-name) (mk-proj-friendly-buffer-p buffer proj-name))
            (progn
              (when (buffer-file-name buffer)
                (mk-proj-after-save-add-pattern buffer))
              (project-index proj-name nil t nil t
                             (lambda (&optional proj-name proj-alist files debug)
                               (project-update-tags proj-name proj-alist files debug)
                               (setq mk-proj-after-save-update-in-progress nil
                                     mk-proj-after-save-current-buffer nil
                                     mk-proj-after-save-current-project nil))))
          (setq mk-proj-after-save-update-in-progress nil
                mk-proj-after-save-current-buffer nil
                mk-proj-after-save-current-project nil)))
    (error (progn
             (setq mk-proj-after-save-update-in-progress nil
                   mk-proj-after-save-current-buffer nil
                   mk-proj-after-save-current-project nil)
             (message "error in project-after-save-update: %s" (prin1-to-string e)))))
  t)


(defun mk-proj-pre-command-remove-jump-delete-buffer ()
  (unless (or (eq this-command 'mk-proj-jump-next)
              (eq this-command 'mk-proj-jump-prev)
              (eq this-command 'mk-proj-jump-abort)
              (eq this-command 'mk-proj-jump))
    (mapc (lambda (ov)
            (when (eq (overlay-buffer ov) (current-buffer))
              (overlay-put ov 'delete-buffer nil)))
          mk-proj-jump-overlays)))

(eval-after-load "mk-project"
  '(progn
     (run-with-idle-timer 60 t 'mk-proj-save-state)
     (add-hook 'after-save-hook 'mk-proj-after-save-update)
     (add-hook 'after-load-hook 'mk-proj-after-save-update)
     (add-hook 'after-save-hook 'mk-proj-jump-cleanup-highlight)
     (add-hook 'pre-command-hook 'mk-proj-pre-command-remove-jump-delete-buffer)))

;; ---------------------------------------------------------------------
;; Guessing
;; ---------------------------------------------------------------------

(defun mk-proj-find-projects-matching-patterns (test-patterns &optional name-list)
  (let ((results nil)
        (case-fold-search nil))
    (maphash (lambda (k v)
               (let ((proj-patterns))
                 (when (and (setq proj-patterns (cadr (assoc 'src-patterns v)))
                            (loop for tp in test-patterns
                                  for pp in proj-patterns
                                  if (string-match tp pp)
                                  return t
                                  finally return nil))
                   (add-to-list 'results k))))
             (or (and name-list
                      (let ((temp-hash (make-hash-table :test 'equal)))
                        (mapc (lambda (name) (puthash name (gethash name mk-proj-list) temp-hash)) name-list)
                        temp-hash))
                 mk-proj-list))
    results))


(defun* mk-proj-find-projects-in-directory (path &optional name-list)
  (let ((results nil))
    (maphash (lambda (k v)
               (when (or (string-equal (expand-file-name (concat path "/"))
                                       (expand-file-name (concat (mk-proj-get-config-val 'basedir k t) "")))
                         (string-equal (file-truename (expand-file-name (concat path "/")))
                                       (file-truename (expand-file-name (concat (mk-proj-get-config-val 'basedir k t) "")))))
                 (add-to-list 'results k)))
             (or (and name-list
                      (let ((temp-hash (make-hash-table :test 'equal)))
                        (mapc (lambda (name) (puthash name (gethash name mk-proj-list) temp-hash)) name-list)
                        temp-hash))
                 mk-proj-list))
    results))

(defun mk-proj-find-projects-owning-buffer (buf &optional name-list)
  (let ((projects nil)
        (case-fold-search nil))
    (maphash (lambda (k v)
               (when (and (buffer-file-name buf)
                          (mk-proj-get-config-val 'basedir k t)
                          (mk-proj-path-equal (buffer-file-name buf) (mk-proj-get-config-val 'basedir k t))
                          (some (lambda (re) (string-match re (buffer-file-name buf))) (mk-proj-get-config-val 'src-patterns k t)))
                 (add-to-list 'projects k)))
             (or (and name-list
                      (let ((temp-hash (make-hash-table :test 'equal)))
                        (mapc (lambda (name) (puthash name (gethash name mk-proj-list) temp-hash)) name-list)
                        temp-hash))
                 mk-proj-list))
    projects))

(defun mk-proj-find-unique-paths (paths)
  (let ((result '()))
    (dolist (path paths result)
      (unless (some (lambda (a) (mk-proj-path-equal path a)) (mk-proj-filter (lambda (p) (not (string-equal p path))) paths))
        (add-to-list 'result path)))))

(defun mk-proj-find-common-path-of-buffers (&optional buffers ignore-paths)
  (let* ((common-path 'undefined)
         (result (dolist (buf
                          (or buffers (buffer-list))
                          ;; at the end of the dolist loop over the buffers transform the of strings in common-path
                          ;; into a real path by interspersing "/" between then, then returning it as result
                          (unless (or (eq common-path 'undefined)
                                      (null common-path))
                            (expand-file-name (apply 'concat (unless (eq system-type 'windows-nt) "/") (mapcar 'file-name-as-directory common-path)))))
                   (when (buffer-file-name buf)
                     (if (eq common-path 'undefined)
                         ;; set common-path on first iteration if it is undefined, we'll be unecessarily
                         ;; checking it against itself once
                         (setq common-path (split-string (mk-proj-dirname (expand-file-name (buffer-file-name buf))) "/" (not (eq system-type 'windows-nt))))
                       ;; we split both paths by "/" and create a zipper from the resulting lists
                       ;; /foo/bar     -> '\("foo" "bar"\)
                       ;; /foo/bar/bla -> '\("foo" "bar" "bla"\)
                       ;; -> '\(\("foo" "foo"\) \("bar" "bar"\) \(nil "bla"\)\)
                       ;; then walking over the zipper while both tuple's strings match, stopping at a mismatch
                       ;; and collecting matching strings on the way along
                       (let ((tuples (mk-proj-zip common-path (split-string (expand-file-name (buffer-file-name buf)) "/" (not (eq system-type 'windows-nt)))))
                             (temp-path '()))
                         (while (string-equal (first (car tuples)) (second (car tuples)))
                           (add-to-list 'temp-path (first (car tuples)))
                           (setq tuples (cdr tuples)))
                         ;; we'll set the new common-path before the next iteration, but only if it wouldn't be
                         ;; 'equal' (see mk-proj-path-equal) to any of the ignore-paths
                         (unless (loop for ig in ignore-paths
                                       if (mk-proj-path-equal (file-name-as-directory (expand-file-name ig))
                                                              (expand-file-name (apply 'concat (mapcar 'file-name-as-directory (reverse temp-path)))))
                                       return t
                                       finally return nil)
                           (setq common-path (reverse temp-path)))))))))
    result))

(defun* mk-proj-guess-buffers (&optional test-buffer ignore-paths mode)
  (unless test-buffer
    (setq test-buffer (current-buffer)))
  (let ((buffers (buffer-list)))
    (while (and (not (buffer-file-name test-buffer))
                buffers)
      (setq test-buffer (car buffers)
            buffers (cdr buffers))))
  (unless (buffer-file-name test-buffer)
    (return-from "mk-proj-guess-buffers" nil))
  (let* ((ignore-paths (sort ignore-paths (lambda (a b) (> (length a) (length b)))))
         (test-mode (with-current-buffer test-buffer major-mode))
         (test-path (or (mk-proj-dirname (buffer-file-name test-buffer))
                        default-directory))
         (buffer-projects (mk-proj-find-projects-owning-buffer test-buffer))
         result)
    (setq result (remove-if-not 'identity
                                (mapcar (lambda (buf)
                                          (let (buf-projects)
                                            (when (and (buffer-file-name buf)
                                                       (mk-proj-path-complement (mk-proj-dirname (buffer-file-name buf)) test-path)
                                                       ;; same projects for all buffers considered, all guessed buffers must belong to the
                                                       ;; same projects as test-buffer, even if test-buffer belongs to multiple projects
                                                       (or (mk-proj-path-equal test-path (mk-proj-dirname (buffer-file-name buf)) t)
                                                           (loop for ig in ignore-paths
                                                                 if (mk-proj-path-equal ig (mk-proj-dirname (buffer-file-name buf)) t)
                                                                 return nil
                                                                 finally return t))
                                                       (equal (setq buf-projects (mk-proj-find-projects-owning-buffer buf)) buffer-projects))
                                              ;; special treatment for buffers when a project is loaded, exclude project buffers of loaded
                                              ;; project if test-buffer does not belong to loaded project, exclude buffers that do not
                                              ;; belong to loaded project when test-buffer belongs to the loaded project
                                              (unless (or (and (boundp 'mk-proj-name)
                                                               mk-proj-name
                                                               (not (some (lambda (p) (string-equal p mk-proj-name)) buffer-projects))
                                                               (some (lambda (p) (string-equal p mk-proj-name)) buf-projects))
                                                          (and (boundp 'mk-proj-name)
                                                               mk-proj-name
                                                               (not (eq buf test-buffer))
                                                               (not (some (lambda (p) (string-equal p mk-proj-name)) buffer-projects))
                                                               (mk-proj-friendly-buffer-p buf mk-proj-name)))
                                                buf))))
                                        (if mode
                                            (remove-if-not (lambda (b) (eq (with-current-buffer b major-mode) mode)) (buffer-list))
                                          (buffer-list)))))
    result))

(defun mk-proj-src-pattern-languages (src-patterns)
  (let ((lang nil)
        (languages nil))
    (loop for pattern in src-patterns
          do (let* ((parts (split-string pattern "\\." t))
                    (ending (if (and (> (length parts) 2)
                                     (string-equal (car (last parts)) "gz"))
                                (concat (nth 0 (last parts 2)) "." (nth 1 (last parts 2)))
                              (car (last parts)))))
               (setq lang (cadr (assoc ending mk-proj-src-pattern-table))))
          if (not (eq lang nil))
          do (add-to-list 'languages lang))
    languages))

(defvar mk-proj-incubator-paths `(,(expand-file-name "~"))
  "An incubator is a location where multiple projects are kept. These will be
ignored when guessing a projects basedir thus giving preference to subdirectories
within it.

It is not impossible for an incubator path to be guessed as project basedir though.
If you'll guess while in a buffer with a file from an incubator root open, that
incubator root could be guessed as basedir.")

(defvar mk-proj-common-project-subdir-names '("src" "include" "demo[?s]" "example[?s]" "doc[?s]" "build" "tool[?s]" "test[?s]" "misc")
  "Common subdirectory names found in projects as regular expressions. These
help guessing a projects basedir. Matching directory names will be ignored
and their parent directory used as basedir.")

(defvar mk-proj-guess-functions '((buffer . ((()
                                              `(1 . ,(current-buffer)))))
                                  (mode . (((buffer)
                                            `(1 . ,(with-current-buffer buffer major-mode)))))
                                  (basedir . (;; default-directory
                                              (()
                                               `(1 . ,(expand-file-name default-directory)))
                                              ;; buffer-file-name
                                              ((buffer)
                                               (when (buffer-file-name buffer)
                                                 `(10 . ,(mk-proj-dirname (expand-file-name (buffer-file-name buffer))))))
                                              ;; longest-common-path of buffers with same mode
                                              ((mode buffer)
                                               (let ((found-path (mk-proj-find-common-path-of-buffers (mk-proj-guess-buffers buffer mk-proj-incubator-paths mode))))
                                                 (when (and found-path
                                                            (not (string-equal (expand-file-name "~") found-path)))
                                                   `(50 . ,found-path))))
                                              ;; find directory that is not a common project subdir
                                              ((buffer)
                                               (let* ((path (mk-proj-find-common-path-of-buffers (mk-proj-guess-buffers buffer mk-proj-incubator-paths)))
                                                      (splitted-path (when path (split-string path "/"))))
                                                 (while (and path
                                                             splitted-path
                                                             (not (some (lambda (incubator-path) (mk-proj-path-equal incubator-path path))
                                                                        mk-proj-incubator-paths))
                                                             (some (lambda (dir) (string-equal dir (car (last splitted-path))))
                                                                   mk-proj-common-project-subdir-names))
                                                   (setq splitted-path (butlast splitted-path)
                                                         path (reduce (lambda (a b) (concat a "/" b)) splitted-path)))
                                                 (when path
                                                   `(100 . ,path))))
                                              ;; find basedir by searching for buildsystem patterns
                                              ((buffer)
                                               (let ((path (mk-proj-find-common-path-of-buffers (mk-proj-guess-buffers buffer mk-proj-incubator-paths))))
                                                 (when path
                                                   (let ((found-paths (sort (loop for re in (mk-proj-buildsystem-patterns)
                                                                                  collect (mk-proj-search-path re path mk-proj-incubator-paths mk-proj-common-project-subdir-names))
                                                                            (lambda (a b) (> (length a) (length b))))))
                                                     (when (car found-paths)
                                                       `(200 . ,(car found-paths)))))))
                                              ;; find basedir by searching for vcs pattern
                                              ((buffer)
                                               (let* ((filename (buffer-file-name buffer))
                                                      (found-paths (when filename
                                                                     (sort (loop for re in (mapcar 'regexp-quote (mapcar 'cdr mk-proj-vcs-path))
                                                                                 collect (mk-proj-search-path re (file-name-directory (expand-file-name filename)) mk-proj-incubator-paths mk-proj-common-project-subdir-names))
                                                                           (lambda (a b) (> (length a) (length b)))))))
                                                 (if (car found-paths)
                                                     `(500 . ,(car found-paths))
                                                   (let ((path (mk-proj-find-common-path-of-buffers (mk-proj-guess-buffers buffer mk-proj-incubator-paths))))
                                                     (when path
                                                       (let ((found-paths (sort (loop for re in (mapcar 'regexp-quote (mapcar 'cdr mk-proj-vcs-path))
                                                                                      collect (mk-proj-search-path re path mk-proj-incubator-paths mk-proj-common-project-subdir-names))
                                                                                (lambda (a b) (> (length a) (length b))))))
                                                         (when (car found-paths)
                                                           `(300 . ,(car found-paths)))))))))
                                              ;; find basedir by trying to match buffers directory to project basedirs
                                              ((buffer)
                                               (let ((basedirs '()))
                                                 (dolist (proj-name (mk-proj-find-projects-owning-buffer buffer))
                                                   (let ((basedir (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t))))
                                                     (unless (some (apply-partially 'string-equal basedir) basedirs)
                                                       (add-to-list 'basedirs basedir))))
                                                 (let ((basedirs-without-incubators (remove-if (lambda (dir)
                                                                                                 (some (lambda (incubator)
                                                                                                         (string-equal (file-name-as-directory dir)
                                                                                                                       (file-name-as-directory incubator)))
                                                                                                       mk-proj-incubator-paths))
                                                                                               basedirs)))
                                                   (if (and (eq (length basedirs) 1)
                                                            (eq (length basedirs-without-incubators) 1))
                                                       `(400 . ,(car basedirs))
                                                     (if (eq (length basedirs-without-incubators) 1)
                                                         `(250 . ,(car basedirs-without-incubators))
                                                       (if (> (length basedirs-without-incubators) 1)
                                                           `(25 . ,(car basedirs-without-incubators))
                                                         (if (> (length basedirs) 1)
                                                             `(10 . ,(car basedirs)))))))))))
                                  (name . (((buffer)
                                            (progn
                                              (unless buffer
                                                (setq buffer (current-buffer)))
                                              (when (buffer-file-name buffer)
                                                `(10 . ,(car (split-string (file-name-nondirectory (expand-file-name (buffer-file-name buffer))) "\\."))))))
                                           ((basedir)
                                            (let ((pname (car (reverse (split-string basedir "/" t)))))
                                              (when (loop for ig in mk-proj-incubator-paths
                                                          if (mk-proj-path-equal ig basedir)
                                                          return nil
                                                          finally return t)
                                                `(100 . ,pname))))))
                                  (src-patterns . (((basedir mode)
                                                    (let* ((all-incubator 'undefined)
                                                           (case-fold-search nil)
                                                           ;; guess buffers, collect files and set all-incubator, files from incubator
                                                           ;; roots are ignored except the current buffers file, if all relevant files
                                                           ;; we found are from incubator roots all-incubator will be true
                                                           (files (loop for buf in (mk-proj-guess-buffers (current-buffer) nil mode)
                                                                        do (cond ((and (some (lambda (ig)
                                                                                               (let ((file-name (buffer-file-name buf)))
                                                                                                 (when file-name
                                                                                                   (mk-proj-path-equal (mk-proj-dirname (expand-file-name file-name)) ig t))))
                                                                                             mk-proj-incubator-paths)
                                                                                       all-incubator)
                                                                                  (setq all-incubator t))
                                                                                 (t
                                                                                  (setq all-incubator nil)))
                                                                        if (and (buffer-file-name buf)
                                                                                (stringp basedir)
                                                                                (string-match (regexp-quote (expand-file-name basedir)) (expand-file-name (buffer-file-name buf)))
                                                                                (or (eq (current-buffer) buf)
                                                                                    (not (some (lambda (ig)
                                                                                                 (mk-proj-path-equal (mk-proj-dirname (expand-file-name (buffer-file-name buf))) ig t))
                                                                                               mk-proj-incubator-paths))))
                                                                        append (list (file-name-nondirectory (expand-file-name (buffer-file-name buf))))))
                                                           patterns)
                                                      ;; get unique file endings from filenames and make regexp patterns
                                                      (unless (and (= (length files) 1)
                                                                   all-incubator)
                                                        (loop for f in files
                                                              if (let ((splits (split-string f "\\." t)))
                                                                   (and (last splits)
                                                                        (> (length splits) 1)))
                                                              do (let ((file-ending (car (last (split-string f "\\." t)))))
                                                                   (mapc (lambda (s)
                                                                           (add-to-list 'patterns s))
                                                                         (cddr (assoc file-ending mk-proj-src-pattern-table))))
                                                              else
                                                              do (add-to-list 'patterns (regexp-quote f))))
                                                      (when files
                                                        ;; add files full names as well to the pattern list
                                                        (mapc (lambda (name) (add-to-list 'patterns name))
                                                              (mapcar (lambda (fname) (concat ".*" (regexp-quote fname))) files))
                                                        ;; patterns might look something like this: "foo\\.el" "bar\\.el" "*\\.el
                                                        `(100 . ,patterns))))))
                                  (patterns-are-regex . ((nil
                                                          '(10 . t))))
                                  (compile-cmd . ((nil
                                                   (when (and (boundp 'compile-command)
                                                              compile-command)
                                                     `(50 . ,compile-command)))
                                                  ((basedir)
                                                   (let ((bsystem))
                                                     (loop for filename in (directory-files basedir)
                                                           until (let ((r nil))
                                                                   (loop for bs in mk-proj-buildsystems
                                                                         until (setq r (when (assoc 'files (cadr bs))
                                                                                         (some (apply-partially 'string-equal filename) (cadr (assoc 'files (cadr bs))))))
                                                                         finally return (when r
                                                                                          (setq bsystem bs)))))
                                                     (cond ((and bsystem
                                                                 (assoc 'build (cadr bsystem)))
                                                            `(100 . ,(cadr (assoc 'build (cadr bsystem)))))
                                                           ((and (boundp 'compile-command)
                                                                 compile-command)
                                                            `(100 . ,compile-command)))
                                                     ))))
                                  (vcs . (((basedir)
                                           (let ((r nil))
                                             (loop for f in (directory-files basedir)
                                                   if (some (lambda (y)
                                                              (string-equal (cdr y) f)) mk-proj-vcs-path)
                                                   return `(10 . ,(car (rassoc f mk-proj-vcs-path))))))))
                                  (languages . (((src-patterns)
                                                 (let ((languages (mk-proj-src-pattern-languages src-patterns)))
                                                   (when languages
                                                     `(10 . ,languages))))))))

(defun mk-proj-guess-alist (&optional ask-basedir ask-name)
  ;; go through mk-proj-guess-functions and collect all symbols that are used
  ;; as arguments, we'll bind those in a closure around the execution
  ;; of the function bodies
  (let ((languages 'undefined)
        (compile-cmd 'undefined)
        (vcs 'undefined)
        (patterns-are-regex 'undefined)
        (src-patterns 'undefined)
        (name 'undefined)
        (basedir 'undefined)
        (mode 'undefined)
        (buffer 'undefined)
        (result '())
        (start-time (current-time)))
    (cl-labels ((best-result (rs)
                             (let (bestscore bestresult)
                               (dolist (tuple rs bestresult)
                                 (when (or (not bestscore)
                                           (> (car tuple) bestscore))
                                   (setq bestscore (car tuple)
                                         bestresult (cdr tuple))))))
                (guess-symbol (sym)
                              (let ((scores '()))
                                (dolist (flist (cdr (assoc sym mk-proj-guess-functions)) (best-result scores))
                                  (let ((args (first flist))
                                        (expr (second flist)))
                                    (dolist (arg args)
                                      ;; check if neccessary symbols are set, this sets a symbol after guessing it so
                                      ;; we do not have to guess something twice
                                      (when (eq (symbol-value arg) 'undefined)
                                        (setf (symbol-value arg) (guess-symbol arg))
                                        ))
                                    (let ((r (condition-case e (eval expr)
                                               (error (message "error while guessing %S: %S in %s" sym e (prin1-to-string expr))))))
                                      (when r (add-to-list 'scores r))))))))
      (dolist (varchecks (append mk-proj-required-vars mk-proj-optional-vars))
        ;; for each var check if it is already set, if not use guess-symbol to guess it
        ;; since only args from mk-proj-guess-functions are defined by the alet, not all
        ;; possible project symbols, we have to check if a var is bound before setting it
        ;; with setf or returning its value if it is not 'undefined, default is to just
        ;; guess and not set anything
        (let* ((var (car varchecks))
               (checks (cdr varchecks))
               (gv (cond ((and ask-basedir
                               (boundp var)
                               (eq var 'basedir))
                          (progn
                            (setf (symbol-value var)
                                  (let ((guessed-dir (guess-symbol var)))
                                    (ido-read-directory-name (format "Continue with this basedir? (%s): " guessed-dir)
                                                             guessed-dir
                                                             nil
                                                             t)))))
                         ((and ask-name
                               (boundp var)
                               (eq var 'name))
                          (progn
                            (setf (symbol-value var)
                                  (let ((guessed-name (guess-symbol var)))
                                    (read-string (format "Use this name? (%s): " guessed-name)
                                                 nil
                                                 nil
                                                 guessed-name)))))
                         ((and (boundp var)
                               (not (eq (symbol-value var) 'undefined)))
                          (symbol-value var))
                         ((and (boundp var)
                               (eq (symbol-value var) 'undefined))
                          (setf (symbol-value var) (guess-symbol var)))
                         (t (guess-symbol var)))))
          (if gv
              (add-to-list 'result `(,var ,gv))
            ;; when a required var couldn't be found, abort
            (when (some (apply-partially 'eq var) mk-proj-required-vars)
              (return-from "mk-proj-guess-alist" nil)))))
      ;; find already defined project that fits the guessed project so well that we'll use that instead
      ;; creates list of all projects in same basedir, then selects those matching the same src-patterns
      ;; as the guessed, uses the first of those if multiple match
      (let ((already-defined (or (and (buffer-file-name (current-buffer))
                                      (mk-proj-find-projects-in-directory (mk-proj-dirname (buffer-file-name (current-buffer)))))
                                 (mk-proj-find-projects-in-directory (cadr (assoc 'basedir result)))))
            (pattern-projects nil))
        (if already-defined
            (loop for proj-name in already-defined
                  if (setq pattern-projects
                           (mk-proj-find-projects-matching-patterns (mk-proj-get-config-val 'src-patterns proj-name t)
                                                                    already-defined))
                  return (let ((already-defined-result (gethash (car pattern-projects) mk-proj-list)))
                           ;; add name if it does not already exist to alist, doubles functionality in project-def
                           (unless (assoc 'name already-defined-result)
                             (add-to-list 'already-defined-result `(name ,(car pattern-projects))))
                           already-defined-result))
          result)))))

(provide 'mk-project)

;; mk-project.el ends here
