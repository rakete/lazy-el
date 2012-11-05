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
(require 'xcscope)

(defvar mk-proj-version "1.6.0")

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

See also `mk-proj-optional-vars' `mk-proj-var-before-get-functions'")

(defvar mk-proj-optional-vars '(parent ;; parent needs to come first!
                                languages
                                src-patterns
                                ignore-patterns
                                ack-args
                                vcs
                                compile-cmd
                                install-cmd
                                run-cmd
                                startup-hook
                                shutdown-hook
                                file-list-cache
                                open-files-cache
                                src-find-cmd
                                grep-find-cmd
                                index-find-cmd
                                tags-file
                                etags-cmd
                                cscope-namefile
                                cscope-cmd
                                patterns-are-regex
                                friends
                                open-friends-cache)
  "Project config vars that are optional.

See also `mk-proj-required-vars' `mk-proj-var-before-get-functions'")

(defvar mk-proj-internal-vars '()
  "Project config vars that are ignored when saving the project config.")

(defun mk-proj-var-expand (var val &optional proj-name config-alist)
  (when (stringp val)
    (expand-file-name val)))

(defun mk-proj-basedir-expand (var val &optional proj-name config-alist)
  (when (stringp val)
    (file-name-as-directory (expand-file-name val))))

(defun mk-proj-var-get-tags-file (var val &optional proj-name config-alist)
  (if val
      (expand-file-name val)
    (mk-proj-get-cache-path var proj-name t)))

(defun mk-proj-var-get-open-file-cache (var val &optional proj-name config-alist)
  (if val
      (expand-file-name val)
    (mk-proj-get-cache-path var proj-name nil)))

(defun mk-proj-var-get-file-list-cache (var val &optional proj-name config-alist)
  (if val
      (expand-file-name val)
    (mk-proj-get-cache-path var proj-name t)))

(defun mk-proj-var-guess-languages (var val &optional proj-name config-alist)
  (or val (mk-proj-src-pattern-languages (mk-proj-get-config-val 'src-patterns proj-name))))

(defvar mk-proj-var-before-get-functions '((basedir . mk-proj-basedir-expand)
                                           (tags-file . mk-proj-var-get-tags-file)
                                           (cscope-namefile . mk-proj-var-get-tags-file)
                                           (file-list-cache . mk-proj-var-get-file-list-cache)
                                           (open-files-cache . mk-proj-var-get-open-file-cache)
                                           (open-friends-cache . mk-proj-var-get-open-file-cache)
                                           (languages . mk-proj-var-guess-languages)
                                           (patterns-are-regex . (lambda (var val &optional proj-name config-alist)
                                                                   (if (and config-alist
                                                                            (not (assoc 'patterns-are-regex config-alist)))
                                                                       t
                                                                     val))))
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
                                    (ack-args . (lambda ()
                                                  (read-string "Ack arguments: " super)))
                                    (vcs . (lambda ()
                                             (loop for v = (read-string "vcs: " super) then (read-string "vcs: " super)
                                                   until (some (lambda (x) (eq (car x) (read v))) mk-proj-vcs-path)
                                                   finally return (read v))))
                                    (compile-cmd . (lambda ()
                                                     (read-string "Compile command: " super)))
                                    (patterns-are-regex . (lambda () t))))

(defvar mk-proj-before-load-hook '())
(defvar mk-proj-before-files-load-hook '())
(defvar mk-proj-after-load-hook '())

(defvar mk-proj-before-unload-hook '())
(defvar mk-proj-before-files-unload-hook '())
(defvar mk-proj-after-unload-hook '())

(defvar mk-proj-history '())

(defun mk-proj-cmake-build (&optional opts)
  (interactive)
  (mk-proj-assert-proj)
  (when (file-exists-p (expand-file-name (concat (mk-proj-get-config-val 'basedir) "/CMakeLists.txt")))
    (let ((dir (cond ((file-exists-p (expand-file-name (concat (mk-proj-get-config-val 'basedir) "/build")))
                      (expand-file-name (concat (mk-proj-get-config-val 'basedir) "/build")))
                     ((file-exists-p (expand-file-name (concat (mk-proj-get-config-val 'basedir) "/cmake")))
                      (expand-file-name (concat (mk-proj-get-config-val 'basedir) "/cmake")))
                     ((y-or-n-p (concat "Create " (expand-file-name (concat (mk-proj-get-config-val 'basedir) "/build")) "?"))
                      (make-directory (expand-file-name (concat (mk-proj-get-config-val 'basedir) "/build")))
                      (expand-file-name (concat (mk-proj-get-config-val 'basedir) "/build")))
                     (t nil))))
      (when dir
        (mk-proj-with-directory
         dir
         (if (file-exists-p "Makefile")
             (compile (concat "pushd " dir "; make " opts))
           (if (> (shell-command "cmake ..") 0)
               (error "cmake failed")
             (compile (concat "pushd " dir "; make " opts)))))))))

(defvar mk-proj-buildsystems '((gnu-make ((files ("autogen.sh" "configure" "Makefile"))
                                          (build "make $MK_BUILD_OPTS")))
                               (cmake ((files ("CMakeLists.txt"))
                                       (build (lambda () (mk-proj-cmake-build)))))
                               (cabal ((files ("Setup.lhs"))
                                       (build "runhaskell Setup.lhs build $MK_BUILD_OPTS")))
                               (python ((files ("setup.py"))
                                        (build "python setup.py build $MK_BUILD_OPTS"))))
  "Used when guessing a project root or its compile-cmd.")

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

(defvar mk-proj-src-pattern-table '(("h" . (c ".*\\.c" ".*\\.cpp" ".*\\.cc" ".*\\.h"))
                                    ("hpp" . (cpp ".*\\.cpp" ".*\\.c" ".*\\.h" ".*\\.hpp"))
                                    ("hh" . (cpp ".*\\.cc" ".*\\.c" ".*\\.h" ".*\\.hh"))
                                    ("c" . (c ".*\\.c" ".*\\.h"))
                                    ("cpp" . (cpp ".*\\.cpp"  ".*\\.c" ".*\\.h" ".*\\.hpp"))
                                    ("cc" . (cpp  ".*\\.cc"  ".*\\.c" ".*\\.h" ".*\\.hh"))
                                    ("hs" . (haskell  ".*\\.hs" ".*\\.lhs" ".*\\.cabal"))
                                    ("lhs" . (haskell  ".*\\.hs" ".*\\.lhs" ".*\\.cabal"))
                                    ("cabal" . (haskell  ".*\\.hs" ".*\\.lhs" ".*\\.cabal"))
                                    ("php" . (php  ".*\\.php" ".*\\.html"))
                                    ("js" . (javascript  ".*\\.js" ".*\\.html"))
                                    ("el" . (elisp  ".*\\.el"))
                                    ("lisp" . (lisp  ".*\\.lisp"  ".*\\.lsp"))
                                    ("lsp" . (lisp  ".*\\.lisp"  ".*\\.lsp"))
                                    ("lua" . (lua ".*\\.lua"))
                                    ("clojure" . (clojure ".*\\.clj" ".*\\.clojure"))
                                    ("clj" . (clojure ".*\\.clojure" ".*\\.clj"))
                                    ("java" . (java ".*\\.java"))
                                    ("pl" . (perl ".*\\.pl" ".*\\.pm" ".*\\.pod" ".*\\.t"))
                                    ("pm" . (perl ".*\\.pl" ".*\\.pm" ".*\\.pod" ".*\\.t"))
                                    ("pod" . (perl ".*\\.pl" ".*\\.pm" ".*\\.pod" ".*\\.t"))
                                    ("t" . (perl ".*\\.pl" ".*\\.pm" ".*\\.pod" ".*\\.t"))
                                    ("py" . (python ".*\\.py"))
                                    ("sh" . (shell . ".*\\.sh")))
  "Maps file suffixes to regexps used as source-patterns when guessing a
project config from the currently opened file in the active buffer.")

(defvar mk-proj-config-save-location nil
  "Where to save project configs in elisp. If this is a filename project
configs will be written to that file. If it is a directory an elisp
file with the projects name will be created in that directory.

See also `mk-proj-config-save-section'")

(defvar mk-proj-config-save-section ";; autogenerated projects"
  "Mk-Project saves every new project right below this line (if it
can find it, if not it creates it at the end of the file).

See also `mk-proj-config-save-location'")

(defvar mk-proj-language-source-tagging '((c . (etags cscope gtags semantic))
                                          (cpp . (etags cscope gtags semantic))
                                          (csharp . (etags semantic))
                                          (elisp . (etags semantic))
                                          (erlang . (etags semantic))
                                          (lisp . (etags semantic))
                                          (lua . (etags))
                                          (scheme . (etags semantic))
                                          (haskell . (htags))
                                          (ocaml . (etags))
                                          (perl . (etags))
                                          (python . (etags semantic))
                                          (php . (etags gtags semantic))
                                          (shell . (etags))
                                          (ruby . (etags))
                                          (java . (etags gtags semantic))
                                          (javascript . (etags semantic))))


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

(defun mk-proj-save-state ()
  (mk-proj-save-open-file-info)
  (mk-proj-save-open-friends-info))

(eval-after-load "mk-project"
  '(progn
     (add-hook 'emacs-kill-hook (lambda ()
                                  (mk-proj-save-state)))
     (run-with-idle-timer 90 t 'mk-proj-save-state)))

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

(defun mk-proj-assert-proj (&optional try-guessing)
  (unless mk-proj-name
    (let ((guessed-alist (when try-guessing (mk-proj-guess-alist t t))))
      (cond ((and guessed-alist
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

(defun mk-proj-find-cmd-val (context &optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((cmd (ecase context
               ('src   (mk-proj-get-config-val 'src-find-cmd proj-name t))
               ('grep  (mk-proj-get-config-val 'grep-find-cmd proj-name t))
               ('index (mk-proj-get-config-val 'index-find-cmd proj-name t)))))
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

;; (mk-proj-path-complement "/home/me/blah/blubber" "/home/me/blah/blubber/")

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

;; (mk-proj-path-equal "/foo/bar" "/foo/bar/")
;; (mk-proj-path-equal "/foo/bar" "/foo/" t)
;; (mk-proj-path-equal "/foo/bar" "/foo/bar/blah")
;; (mk-proj-path-equal "/foo/bar" "/foo/blah/bar")

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

;; (mk-proj-search-path (regexp-quote ".git")
;;                      (mk-proj-find-common-path-of-buffers (mk-proj-guess-buffers (current-buffer) mk-proj-incubator-paths)
;;                                           mk-proj-incubator-paths)
;;                     mk-proj-incubator-paths)

;; (mk-proj-search-path (regexp-quote ".bzr") "/home/rakete/ubuntu/compiz-grid-plugin/src" mk-proj-incubator-paths)

(defun mk-proj-join (delimiter strings)
  (reduce (lambda (a b)
            (concatenate 'string a delimiter b))
          strings))

(defmacro mk-proj-with-directory (path &rest body)
  `(let ((currentdir default-directory))
     (cd ,path)
     (let ((result ,@body))
       (cd currentdir)
       result)))

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

(defvar mk-proj-list (make-hash-table :test 'equal))

(defun* mk-proj-find-config (&optional proj-name (inherit t))
  "Get a projects config-alist from the global projects hashmap."
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let* ((child (gethash proj-name mk-proj-list))
         (alist child))
    (while (and (assoc 'parent child)
                inherit)
      (setq child (gethash (cadr (assoc 'parent child)) mk-proj-list)
            alist (append alist (remove-if (lambda (x) (some (lambda (y) (eq (first x) (first y))) alist)) child))))
    alist))

(defun* mk-proj-get-config-val (key &optional proj-name (inherit t))
  "Finds the value associated with KEY. A project PROJ
can optionally be specified.
If the third argument INHERIT is non-nil, all parents will queried
for the KEY and the first value that is found is returned."
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let* ((proj-alist (mk-proj-find-config proj-name nil))
         (fn (cdr (assoc key mk-proj-var-before-get-functions)))
         (val  (or (when fn
                     (funcall fn key (cadr (assoc key proj-alist)) proj-name proj-alist))
                   (and (assoc key proj-alist)
                        (cadr (assoc key proj-alist)))
                   (let ((parent (cadr (assoc 'parent proj-alist))))
                     (when (and inherit parent)
                       (mk-proj-get-config-val key parent t))))))
    (if fn (funcall fn key val proj-name proj-alist) val)))

;;(mk-proj-get-config-val 'file-list-cache "pcl" nil)

(defalias 'mk-proj-config-val 'mk-proj-get-config-val
  "Alias for `mk-proj-get-config-val' to ensure backward compatibility.")

(defun mk-proj-set-config-val (key value &optional proj-name)
  "Set the value associated with KEY to VALUE in config of project NAME."
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let* ((current-alist (mk-proj-find-config proj-name nil))
         (new-alist current-alist))
    (when current-alist
      (add-to-list 'new-alist `(,key ,value))
      (unless (equal new-alist current-alist)
        (puthash proj-name new-alist mk-proj-list)
        (mk-proj-backend-funcall (mk-proj-detect-backend proj-name)
                                 'save proj-name new-alist)))))

;; (mk-proj-get-config-val 'org-file "emacs-config")
;; (mk-proj-detect-backend "emacs-config")
;; (mk-proj-set-config-val 'vcs 'git "emacs-config")
;; (project-status "emacs-config")

(defun* mk-proj-eval-alist (proj-name &optional config-alist)
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
    (when (cadr (assoc 'parent result-alist))
      (message "%s inherits from %s" proj-name (cadr (assoc 'parent result-alist)))
      ;; (setq result-alist (mk-proj-alist-union (gethash (cadr (assoc 'parent result-alist)) mk-proj-list)
      ;;                                         result-alist))
      )
    (when (gethash proj-name mk-proj-list)
      (message "union with %s" proj-name)
      (setq result-alist (mk-proj-alist-union (gethash proj-name mk-proj-list) result-alist)))
    (loop for req-var in mk-proj-required-vars
          if (or (and (assoc req-var result-alist)
                      (not (cadr (assoc req-var result-alist)))))
          do (progn (error "Project \"%s\" contains errors" proj-name)
                    (return-from "mk-proj-eval-alist" nil)))
    result-alist))

(defun* project-def (&optional proj-name config-alist)
  "Associate the settings in CONFIG-ALIST with project PROJ-NAME.

All values within CONFIG-ALIST will be evaluated when they look
like a lisp expression or symbol. So make sure to quote lists!

See also `project-undef'.

basedir:
Base directory of the current project. Required. Value is expanded with
expand-file-name. Example: ~me/my-proj/.

src-patterns:
List of shell patterns to include in the TAGS file. Optional. Example:
'(\"*.java\" \"*.jsp\").

This value is not used when `mk-proj-src-find-cmd' is set.

ignore-patterns:
List of shell patterns to avoid searching for with project-find-file and
project-grep. Optional. Example: '(\"*.class\").

This value is not used in indexing when `mk-proj-index-find-cmd'
is set -- or in grepping when `mk-proj-grep-find-cmd' is set.

ack-args:
String of arguments to pass to the `ack' command. Optional.
Example: \"--java\".

vcs:
When set to one of the VCS types in `mk-proj-vcs-path', grep
and index commands will ignore the VCS's private files (e.g.,
.CVS/). Example: 'git.

This value is not used in indexing when `mk-proj-index-find-cmd'
is set -- or in grepping when `mk-proj-grep-find-cmd' is set.

tags-file:
Path to the TAGS file for this project. Optional. Use an absolute path,
not one relative to basedir. Value is expanded with expand-file-name.

compile-cmd:
Command to build the entire project. Can be either a string specifying
a shell command or the name of a function. Optional. Example: make -k.

startup-hook:
Hook function to run after the project is loaded. Optional. Project
variables (e.g. mk-proj-basedir) will be set and can be referenced from this
function.

shutdown-hook:
Hook function to run after the project is unloaded. Optional.  Project
variables (e.g. mk-proj-basedir) will still be set and can be referenced
from this function.

file-list-cache:
Cache *file-index* buffer to this file. Optional. If set, the *file-index*
buffer will take its initial value from this file and updates to the buffer
via 'project-index' will save to this file. Value is expanded with
expand-file-name.

open-files-cache:
Cache the names of open project files in this file. Optional. If set,
project-load will open all files listed in this file and project-unload will
write all open project files to this file. Value is expanded with
expand-file-name.

src-find-cmd:
Specifies a custom \"find\" command to locate all files to be
included in the TAGS file (see `project-tags'). Optional. The
value is either a string or a function of one argument that
returns a string. The argument to the function will be the symbol
\"'src\".

If non-null (or if the function returns non-null), the custom
find command will be used and the `mk-proj-src-patterns' and
`mk-proj-vcs' settings are ignored when finding files to include
in TAGS.

grep-find-cmd:
Specifies a custom \"find\" command to use as the default when
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
directory.

index-find-cmd:
Specifies a custom \"find\" command to use when building an
listing of all files in the project (to be used by
`project-find-file'). Optional. The value is either a string or a
function of one argument that returns a string. The argument to
the function will be the symbol \"'index\".

If non-null (or if the function returns non-null), the custom
find command will be used and the `mk-proj-ignore-patterns' and
`mk-proj-vcs' settings are not used when in the grep command."
  (interactive)
  (cond ((stringp proj-name)
         (let ((alist (mk-proj-eval-alist proj-name config-alist)))
           (when alist
             (puthash proj-name alist mk-proj-list)
             (message "Defined: %s" proj-name)
             alist)))
        ((and (functionp 'mk-org-entry-define-project)
              (eq major-mode 'org-mode)
              (looking-at org-complex-heading-regexp)
              (mk-org-entry-define-project)))))











(defun mk-proj-find-projects-matching-patterns (test-patterns &optional name-list)
  (let ((results nil))
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
                        (mapcar (lambda (name) (puthash name (gethash name mk-proj-list) temp-hash)) name-list)
                        temp-hash))
                 mk-proj-list))
    results))

;;(mk-proj-find-projects-matching-patterns '(".*\\.el") (mk-proj-find-projects-matching-patterns '(".*\\.el") ))

(defun* mk-proj-find-projects-in-directory (path &optional name-list)
  (let ((results nil))
    (maphash (lambda (k v)
               (when (string-equal (expand-file-name (concat path "/"))
                                   (expand-file-name (concat (mk-proj-get-config-val 'basedir k t) "")))
                 (add-to-list 'results k)))
             (or (and name-list
                      (let ((temp-hash (make-hash-table :test 'equal)))
                        (mapcar (lambda (name) (puthash name (gethash name mk-proj-list) temp-hash)) name-list)
                        temp-hash))
                 mk-proj-list))
    results))

;; (mk-proj-find-projects-in-directory "~/.emacs.d" '("emacs-config"))

;; (mk-proj-guess-alist)

;; (define-minor-mode mk-project)

(defun mk-proj-find-projects-owning-buffer (buf &optional name-list)
  (let ((projects nil))
    (maphash (lambda (k v)
               (when (and (buffer-file-name buf)
                          (mk-proj-get-config-val 'basedir k t)
                          (mk-proj-path-equal (buffer-file-name buf) (mk-proj-get-config-val 'basedir k t))
                          (some (lambda (re) (string-match re (buffer-file-name buf))) (mk-proj-get-config-val 'src-patterns k t)))
                 (add-to-list 'projects k)))
             (or (and name-list
                      (let ((temp-hash (make-hash-table :test 'equal)))
                        (mapcar (lambda (name) (puthash name (gethash name mk-proj-list) temp-hash)) name-list)
                        temp-hash))
                 mk-proj-list))
    projects))

;;(mk-proj-find-projects-owning-buffer (current-buffer))

(defun mk-proj-find-unique-paths (paths)
  (let ((result '()))
    (dolist (path paths result)
      (unless (some (lambda (a) (mk-proj-path-equal path a)) (mk-proj-filter (lambda (p) (not (string-equal p path))) paths))
        (add-to-list 'result path)))))

;;(mk-proj-find-unique-paths '( "/home/rakete/.emacs.d/" "/home/rakete/testlab" "/home/rakete/Documents" "/home/rakete/.emacs.d/mk-project"))

(defun mk-proj-find-common-path-of-buffers (&optional buffers ignore-paths)
  (let* ((common-path 'undefined)
         (result (dolist (buf
                          (or buffers (buffer-list))
                          ;; at the end of the dolist loop over the buffers transform the of strings in common-path
                          ;; into a real path by interspersing "/" between then, then returning it as result
                          (unless (or (eq common-path 'undefined)
                                      (null common-path))
                            (apply 'concat (mapcar (lambda (s) (concat "/" s)) common-path))))
                   (when (buffer-file-name buf)
                     (if (eq common-path 'undefined)
                         ;; set common-path on first iteration if it is undefined, we'll be unecessarily
                         ;; checking it against itself once
                         (setq common-path (split-string (mk-proj-dirname (buffer-file-name buf)) "/" t))
                       ;; we split both paths by "/" and create a zipper from the resulting lists
                       ;; /foo/bar     -> '("foo" "bar")
                       ;; /foo/bar/bla -> '("foo" "bar" "bla")
                       ;; -> '(("foo" "foo") ("bar" "bar") (nil "bla"))
                       ;; then walking over the zipper while both tuple's strings match, stopping at a mismatch
                       ;; and collecting matching strings on the way along
                       (let ((tuples (mk-proj-zip common-path (split-string (buffer-file-name buf) "/" t)))
                             (temp-path '()))
                         (while (string-equal (first (car tuples)) (second (car tuples)))
                           (add-to-list 'temp-path (first (car tuples)))
                           (setq tuples (cdr tuples)))
                         ;; we'll set the new common-path before the next iteration, but only if it wouldn't be
                         ;; 'equal' (see mk-proj-path-equal) to any of the ignore-paths
                         (unless (loop for ig in ignore-paths
                                       if (mk-proj-path-equal ig (apply 'concat (mapcar (lambda (s) (concat "/" s)) (reverse temp-path))))
                                       return t
                                       finally return nil)
                           (setq common-path (reverse temp-path)))))))))
    result))

;; (mk-proj-find-common-path-of-buffers (mk-proj-guess-buffers (current-buffer)
;;                                             mk-proj-incubator-paths)
;;                      mk-proj-incubator-paths)

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

;; (mk-proj-guess-buffers (current-buffer) mk-proj-incubator-paths)

(defun mk-proj-src-pattern-languages (src-patterns)
  (let ((lang nil)
        (languages nil))
    (loop for pattern in src-patterns
          do (setq lang (cadr (assoc (car (last (split-string pattern "\\." t))) mk-proj-src-pattern-table)))
          if (not (eq lang nil))
          do (add-to-list 'languages lang))
    languages))

;;(mk-proj-src-pattern-languages (mk-proj-get-config-val 'src-patterns))

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
                                                 `(10 . ,(mk-proj-dirname (buffer-file-name buffer)))))
                                              ;; longest-common-path of buffers with same mode
                                              ((mode buffer)
                                               (let ((found-path (mk-proj-find-common-path-of-buffers (mk-proj-guess-buffers buffer mk-proj-incubator-paths mode))))
                                                 (when (and found-path
                                                            (not (string-equal (expand-file-name "~") found-path)))
                                                   `(50 . ,found-path))))
                                              ;; find directory that is not a common project subdir
                                              ((buffer)
                                               (let* ((path (mk-proj-find-common-path-of-buffers (mk-proj-guess-buffers buffer mk-proj-incubator-paths)))
                                                      (splitted-path (split-string path "/")))
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
                                               (let ((path (mk-proj-find-common-path-of-buffers (mk-proj-guess-buffers buffer mk-proj-incubator-paths))))
                                                 (when path
                                                   (let ((found-paths (sort (loop for re in (mapcar 'regexp-quote (mapcar 'cdr mk-proj-vcs-path))
                                                                                  collect (mk-proj-search-path re path mk-proj-incubator-paths mk-proj-common-project-subdir-names))
                                                                            (lambda (a b) (> (length a) (length b))))))
                                                     (when (car found-paths)
                                                       `(300 . ,(car  found-paths)))))))
                                              ))
                                  (name . (((buffer)
                                            (progn
                                              (unless buffer
                                                (setq buffer (current-buffer)))
                                              (when (buffer-file-name buffer)
                                                `(10 . ,(car (split-string (mk-proj-filename (buffer-file-name buffer)) "\\."))))))
                                           ((basedir)
                                            (let ((pname (car (reverse (split-string basedir "/" t)))))
                                              (when (loop for ig in mk-proj-incubator-paths
                                                          if (mk-proj-path-equal ig basedir)
                                                          return nil
                                                          finally return t)
                                                `(100 . ,pname))))))
                                  (src-patterns . (((basedir mode)
                                                    (let* ((all-incubator 'undefined)
                                                           ;; guess buffers, collect files and set all-incubator, files from incubator
                                                           ;; roots are ignored except the current buffers file, if all relevant files
                                                           ;; we found are from incubator roots all-incubator will be true
                                                           (files (loop for buf in (mk-proj-guess-buffers (current-buffer) nil mode)
                                                                        do (cond ((and (some (lambda (ig)
                                                                                               (mk-proj-path-equal (mk-proj-dirname (buffer-file-name buf)) ig t))
                                                                                             mk-proj-incubator-paths)
                                                                                       all-incubator)
                                                                                  (setq all-incubator t))
                                                                                 (t
                                                                                  (setq all-incubator nil)))
                                                                        if (and (buffer-file-name buf)
                                                                                (string-match (regexp-quote basedir) (buffer-file-name buf))
                                                                                (or (eq (current-buffer) buf)
                                                                                    (not (some (lambda (ig)
                                                                                                 (mk-proj-path-equal (mk-proj-dirname (buffer-file-name buf)) ig t))
                                                                                               mk-proj-incubator-paths))))
                                                                        append (list (mk-proj-filename (buffer-file-name buf)))))
                                                           patterns)
                                                      ;; get unique file endings from filenames and make regexp patterns
                                                      (unless (and (= (length files) 1)
                                                                   all-incubator)
                                                        (loop for f in files
                                                              if (let ((splits (split-string f "\\." t)))
                                                                   (and (last splits)
                                                                        (> (length splits) 1)))
                                                              do (let ((file-ending (car (last (split-string f "\\." t)))))
                                                                   ;;(add-to-list 'patterns (concat ".*\\." (regexp-quote file-ending)))
                                                                   (mapc (lambda (s)
                                                                           (add-to-list 'patterns s))
                                                                         (cddr (assoc file-ending mk-proj-src-pattern-table))))
                                                              else
                                                              do (add-to-list 'patterns (regexp-quote f))))
                                                      (when files
                                                        ;; add files full names as well to the pattern list
                                                        (mapc (apply-partially 'add-to-list 'patterns)
                                                              (mapcar (lambda (fname) (concat ".*" (regexp-quote fname))) files))
                                                        ;; patterns might look something like this: "foo\\.el" "bar\\.el" "*\\.el
                                                        `(100 . ,patterns))))))
                                  (patterns-are-regex . ((nil
                                                          '(10 . t))))
                                  (compile-cmd . ((()
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
                                  (etags-cmd . ((nil
                                                 '(10 . "etags --extra=fq --fields=+afiklmnsSzt --C++-kinds=+p --C-kinds=+p -o"))))
                                  (languages . (((src-patterns)
                                                 (let ((languages (mk-proj-src-pattern-languages src-patterns)))
                                                   (when languages
                                                     `(10 . ,languages))))))
                                  (ack-args . (((languages)
                                                (let ((args nil))
                                                  (dolist (lang languages)
                                                    (cond ((eq lang 'c)
                                                           (add-to-list 'args "--cc"))
                                                          ((eq lang 'cpp)
                                                           (add-to-list 'args "--cpp"))
                                                          ((eq lang 'elisp)
                                                           (add-to-list 'args "--el"))
                                                          ((eq lang 'perl)
                                                           (add-to-list 'args "--perl"))
                                                          ((eq lang 'python)
                                                           (add-to-list 'args "--python"))
                                                          ((eq lang 'lisp)
                                                           (add-to-list 'args "--lisp"))
                                                          ((eq lang 'scheme)
                                                           (add-to-list 'args "--scheme"))
                                                          ((eq lang 'shell)
                                                           (add-to-list 'args "--shell"))
                                                          ((eq lang 'haskell)
                                                           (add-to-list 'args "--haskell"))))
                                                  (when args
                                                    `(10 . ,(reduce (lambda (a b) (concat a " " b))
                                                                    args)))))))))

(defun* mk-proj-guess-alist (&optional ask-basedir ask-name)
  ;; go through mk-proj-guess-functions and collect all symbols that are used
  ;; as arguments, we'll bind those in a closure around the execution
  ;; of the function bodies
  (let ((let-args (let (symbols)
                    (dolist (element mk-proj-guess-functions)
                      (add-to-list 'symbols `(,(car element) 'undefined))
                      (dolist (token (cdr element))
                        (dolist (arg (car token))
                          (add-to-list 'symbols `(,arg 'undefined)))))
                    symbols))
        (result '())
        (start-time (current-time)))
    ;; we define a macro that takes the list of let-args we constructed from mk-proj-guess-functions
    ;; and makes a let clause out of them, thus binding them in the scope of body
    (macrolet ((alet (&rest body) `(let ,let-args ,@body)))
      (alet
       (flet ((best-result (rs)
                           (let (bestscore bestresult)
                             (dolist (tuple rs bestresult)
                               (when (or (not bestscore)
                                         (> (car tuple) bestscore))
                                 (setq bestscore (car tuple)
                                       bestresult (cdr tuple))))))
              (guess-symbol (sym)
                            ;;(message (concat (format-time-string "%H:%M:%S" (current-time)) " " (prin1-to-string sym)))
                            (let ((scores '()))
                              (dolist (flist (cdr (assoc sym mk-proj-guess-functions)) (best-result scores))
                                (let ((args (first flist))
                                      (expr (second flist)))
                                  (dolist (arg args)
                                    ;; check if neccessary symbols are set, this sets a symbol after guessing it so
                                    ;; we do not have to guess something twice
                                    (when (eq (symbol-value arg) 'undefined)
                                      ;;(message "setting symbol %S" arg)
                                      (setf (symbol-value arg) (guess-symbol arg))
                                      ))
                                  (let ((r (condition-case e (eval expr)
                                             (error (message "error while guessing %S: %S" sym e)))))
                                    (when r (add-to-list 'scores r))))))))
         ;;(message (concat (format-time-string "%H:%M:%S" (current-time)) " start"))
         (dolist (var (append mk-proj-required-vars mk-proj-optional-vars))
           ;; for each var check if it is already set, if not use guess-symbol to guess it
           ;; since only args from mk-proj-guess-functions are defined by the alet, not all
           ;; possible project symbols, we have to check if a var is bound before setting it
           ;; with setf or returning its value if it is not 'undefined, default is to just
           ;; guess and not set anything
           (let ((gv (cond ((and ask-basedir
                                 (boundp var)
                                 (eq var 'basedir))
                            (progn
                              (interactive)
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
                              (interactive)
                              (setf (symbol-value var)
                                    (let ((guessed-name (guess-symbol var)))
                                      (read-input (format "Use this name? (%s): " guessed-name)
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
         ;;(message (concat (format-time-string "%H:%M:%S" (current-time)) " check defined"))
         (let ((already-defined (or (and (buffer-file-name (current-buffer))
                                         (mk-proj-find-projects-in-directory (mk-proj-dirname (buffer-file-name (current-buffer)))))
                                    (mk-proj-find-projects-in-directory (cadr (assoc 'basedir result)))))
               (pattern-projects nil))
           ;;(message (concat (format-time-string "%H:%M:%S" (current-time)) " do lookup"))
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
             result)))))))

















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

;; (mk-proj-find-project-elisp-configuration-in-buffer "mk-project" (find-file-noselect "~/.emacs.d/init-mkproject.el"))

(defun mk-proj-find-save-location-marker (&optional proj-name config-alist)
  "This tries to find a suitable location to save the projects configuration.

See also `mk-proj-config-save-section', `mk-proj-config-save-section'"
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (cond
   ;; find file in directory named after project
   ((condition-case nil (directory-files (expand-file-name mk-proj-config-save-location)) (error nil))
    (with-current-buffer (find-file-noselect (concat (expand-file-name mk-proj-config-save-location) proj-name ".el"))
      (save-excursion
        (goto-char (or (mk-proj-find-project-elisp-configuration-in-buffer proj-name) (point-max)))
        (point-marker))))
   ;; find section to save under in single el file
   ((and (stringp mk-proj-config-save-location)
         (stringp mk-proj-config-save-section)
         (file-exists-p (expand-file-name mk-proj-config-save-location)))
    (with-current-buffer (find-file-noselect (expand-file-name mk-proj-config-save-location))
      (save-excursion
        (cond ((condition-case nil (goto-char (mk-proj-find-project-elisp-configuration-in-buffer proj-name)) (error nil))
               (point-marker))
              ((progn (goto-char (point-max)) (re-search-backward mk-proj-config-save-section nil t))
               (point-marker))
              (t (error "mk-proj: could not find a location to save %s, see mk-proj-config-save-location" proj-name))))))
   ;; find file in project basedir
   ((and mk-proj-config-save-location
         (or (mk-proj-get-config-val 'basedir proj-name t)
             (cadr (assoc 'basedir config-alist))))
    (with-current-buffer (find-file-noselect (concat (or (mk-proj-get-config-val 'basedir proj-name t)
                                                         (cadr (assoc 'basedir config-alist))) "/" proj-name ".el"))
      (goto-char (point-max))
      (point-marker)))
   ;; no suitable location found
   (t (error "mk-proj: could not find a location to save %s, see mk-proj-config-save-location" proj-name))))

;; (setq mk-proj-config-save-location "~/.emacs.d/")
;; (mk-proj-find-save-location-marker "new-project")







(defun mk-proj-config-insert (proj-name config-alist &optional insert-undefined insert-internal)
  (save-excursion
    (insert (concat "(project-def \"" proj-name "\" '("))
    (loop for k in (append mk-proj-required-vars mk-proj-optional-vars)
          if (and (not (eq k 'name))
                  (or (not (some (lambda (j) (eq k j)) mk-proj-internal-vars))
                      insert-internal)
                  (or (not (cdr (assoc k mk-proj-var-before-get-functions)))
                      (not (string-equal (funcall (cdr (assoc k mk-proj-var-before-get-functions)) k nil) (mk-proj-get-config-val k proj-name)))
                      insert-internal))
          do (when (or insert-undefined
                       (assoc k config-alist))
               (insert (concat "(" (symbol-name k) " " (prin1-to-string (cadr (assoc k config-alist))) ")"))
               (unless (eq k (car (last mk-proj-optional-vars)))
                 (newline))
               (indent-according-to-mode)))
    (insert "))\n")))

(defun mk-proj-config-save (proj-name config-alist)
  (let ((marker (mk-proj-find-save-location-marker proj-name config-alist)))
    (with-marker marker
                 (cond ((looking-at (regexp-quote mk-proj-config-save-section))
                        (end-of-line)
                        (newline)
                        (mk-proj-config-insert proj-name config-alist)
                        (call-interactively 'eval-defun))
                       ((looking-at "(project-def.*")
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
                        (mk-proj-config-insert proj-name config-alist)
                        (call-interactively 'eval-defun)))
                 (let ((mod (buffer-modified-p)))
                   (save-buffer)
                   (set-buffer-modified-p mod)))))

;; (mk-proj-config-save "mk-project" (mk-proj-find-config "mk-project"))

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
       (mk-proj-config-save mk-proj-name (mk-proj-find-config mk-proj-name nil))
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


;; (mk-proj-with-current-project "emacs-config" (project-status))






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

;; (cdr (assoc 'save (gethash (mk-proj-detect-backend "emacs-config") mk-proj-backend-list)))

(defun* mk-proj-detect-backend (&optional proj-name config-alist)
  (if (and (condition-case nil (mk-proj-assert-proj) (error t))
           (not proj-name))
      (return-from "mk-proj-detect-backend" mk-proj-config-backend)
    (unless proj-name
      (setq proj-name mk-proj-name))
    (unless config-alist
      (setq config-alist (mk-proj-find-config proj-name t)))
    (maphash (lambda (k v)
               (unless (eq k 'elisp)
                 (when (funcall (cdr (assoc 'test v)) config-alist)
                   (return-from "mk-proj-detect-backend" k)))) mk-proj-backend-list)
    'elisp))

;; (mk-proj-with-current-project nil (mk-proj-detect-backend))

(defun project-save ()
  (interactive)
  (mk-proj-assert-proj)
  (mk-proj-backend-funcall (mk-proj-detect-backend)
                           'save mk-proj-name (mk-proj-find-config)))

(defun* project-create ()
  (interactive)
  (if (and (gethash 'org-mode mk-proj-backend-list)
           (boundp 'org-complex-heading-regexp)
           (save-excursion
             (org-back-to-heading)
             (looking-at org-complex-heading-regexp)))
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
      (unless (mk-proj-get-config-val v proj-name t)
        (throw 'mk-proj-check-required-vars v)))))

(defun* mk-proj-get-cache-path (symbol &optional proj-name (inherit t))
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
                                     (mk-proj-get-cache-path symbol (mk-proj-get-config-val 'parent proj-name nil) t)))
                  (eq inherit 'copy))
             (progn
               (copy-file (or (mk-proj-get-config-val symbol (mk-proj-get-config-val 'parent proj-name nil) nil)
                              (mk-proj-get-cache-path symbol (mk-proj-get-config-val 'parent proj-name nil) t)) r)
               r))
            ((and (mk-proj-get-config-val 'parent proj-name nil)
                  (eq (mk-proj-get-config-val 'basedir proj-name nil) nil)
                  (eq inherit t))
             (or (mk-proj-get-config-val symbol (mk-proj-get-config-val 'parent proj-name nil) nil)
                 (mk-proj-get-cache-path symbol (mk-proj-get-config-val 'parent proj-name nil) t)))
            (t r)))))

;;(mk-proj-get-cache-path 'file-list-cache mk-proj-name t)

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
  (let* ((oldname mk-proj-name)
         (proj-alist (mk-proj-find-config proj-name nil))
         (quiet (and (cadr (assoc 'parent proj-alist))
                     (or (string-equal (cadr (assoc 'parent proj-alist))
                                       mk-proj-name)
                         (and (not (condition-case nil (mk-proj-assert-proj) (error t)))
                              (string-equal (cadr (assoc 'parent proj-alist))
                                            (mk-proj-get-config-val 'parent nil nil)))))))
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
    (cd (file-name-as-directory (mk-proj-get-config-val 'basedir)))
    (mk-proj-etags-load mk-proj-name)
    (dolist (friend (mk-proj-get-config-val 'friends))
      (mk-proj-etags-load mk-proj-name friend))
    (mk-proj-cscope-load mk-proj-name)
    (dolist (friend (mk-proj-get-config-val 'friends))
      (mk-proj-cscope-load mk-proj-name friend))
    (mk-proj-fib-init)
    (add-hook 'kill-emacs-hook 'mk-proj-kill-emacs-hook)
    (when (mk-proj-get-config-val 'startup-hook)
      (if (functionp (mk-proj-get-config-val 'startup-hook))
          (funcall (mk-proj-get-config-val 'startup-hook))
        (mapc 'funcall (mk-proj-get-config-val 'startup-hook))))
    (run-hooks 'mk-proj-before-files-load-hook)
    (mk-proj-visit-saved-open-files)
    (mk-proj-visit-saved-open-friends)
    (modify-frame-parameters (selected-frame) (list (cons 'name proj-name)))
    (run-hooks 'mk-proj-after-load-hook)
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
               (not (mk-proj-find-config name nil)))
      (project-def name guessed-alist))
    (when (not (mk-proj-find-config name nil))
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
  (when mk-proj-name
    (condition-case nil
        (progn
          (message "Unloading project %s" mk-proj-name)
          (run-hooks 'mk-proj-before-unload-hook)
          (mk-proj-etags-clear)
          (mk-proj-cscope-clear)
          (mk-proj-maybe-kill-buffer (mk-proj-fib-name))
          (mk-proj-save-open-file-info)
          (mk-proj-save-open-friends-info)
          (run-hooks 'mk-proj-before-files-unload-hook)
          (and (or (mk-proj-buffers) (mk-proj-friendly-buffers))
               (not quiet)
               (y-or-n-p (concat "Close all '" mk-proj-name "' project files? "))
               (project-close-files)
               (project-close-friends)
               )
          (when (mk-proj-get-config-val 'shutdown-hook)
            (if (functionp (mk-proj-get-config-val 'shutdown-hook))
                (funcall (mk-proj-get-config-val 'shutdown-hook))
              (mapc 'funcall (mk-proj-get-config-val 'shutdown-hook))))
          (run-hooks 'mk-proj-after-unload-hook))
      (error nil)))
  (add-to-list 'mk-proj-history mk-proj-name)
  (setq mk-proj-name nil)
  (when (and (buffer-file-name (current-buffer))
             (file-exists-p (buffer-file-name (current-buffer))))
    (cd (mk-proj-dirname (buffer-file-name (current-buffer)))))
  (modify-frame-parameters (selected-frame) (list (cons 'name "Emacs")))
  (message "Project settings have been cleared"))

(defun project-close-files ()
  "Close all unmodified files that reside in the project's basedir"
  (interactive)
  (mk-proj-assert-proj)
  (let ((closed nil)
        (dirty nil)
        (zeitgeist-prevent-send t))
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
  (let ((file-name (or (buffer-file-name buf)
                       (with-current-buffer buf list-buffers-directory))))
    (if file-name
        (expand-file-name file-name)
      nil)))

(defun mk-proj-buffer-p (buf &optional proj-name)
  "Is the given buffer in our project, is a file opened? Also detects dired buffers open to basedir/*"
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((file-name (mk-proj-buffer-name buf)))
    (if (and file-name
             (file-exists-p file-name)
             (mk-proj-get-config-val 'basedir proj-name t)
             (string-match (concat "^" (regexp-quote (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t)))) file-name)
             (loop for pattern in (mk-proj-get-config-val 'src-patterns proj-name t)
                   if (string-match (if (mk-proj-get-config-val 'patterns-are-regex proj-name t)
                                        pattern
                                      (regexp-quote pattern)) file-name)
                   return t
                   finally return nil))
        t
      nil)))

(defun mk-proj-file-buffer-p (buf &optional proj-name)
  (and (buffer-file-name buf)
       (mk-proj-buffer-p buf proj-name)))

(defun mk-proj-special-buffer-p (buf &optional proj-name)
  (and (string-match "\*[^\*]\*" (buffer-name buf))
       (mk-proj-buffer-p buf proj-name)))

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
  (append (remove-if (lambda (buf) (not (string-match "\*[^\*]\*" (buffer-name buf)))) (mk-proj-buffers proj-name))
          (remove-if (lambda (buf) (or (and (symbolp 'mk-org-project-buffer-name)
                                            (not (string-equal (mk-org-project-buffer-name proj-name) (buffer-name buf))))
                                       (compilation-buffer-p buf)))
                     (buffer-list))))

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
            (insert (format "%-32s = %s\n" (symbol-name v) (mk-proj-get-config-val v proj-name t)))))
        (when (not (eq b (current-buffer)))
          (set-window-dedicated-p (display-buffer b) t)))
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
          (unless (string-equal (mk-proj-get-config-val 'tags-file) f)
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
              (let ((line (buffer-substring start (point))))
                (message "Attempting to open %s" line)
                (if (file-exists-p line)
                    (find-file-noselect line t)
                  (kill-line))))
            (forward-line)))))))

;; ---------------------------------------------------------------------
;; Etags
;; ---------------------------------------------------------------------

(defun mk-proj-etags-load (&optional proj-name tags-proj)
  "Load TAGS file (if tags-file set)"
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((zeitgeist-prevent-send t)
        (tags-add-tables t))
    (when (and (mk-proj-get-config-val 'tags-file proj-name t)
               (file-readable-p (mk-proj-get-config-val 'tags-file proj-name t)))
      (visit-tags-table (mk-proj-get-config-val 'tags-file proj-name t)))
    (when (and (boundp 'etags-table-alist)
               tags-proj
               (mk-proj-get-config-val 'tags-file tags-proj t)
               (file-readable-p (mk-proj-get-config-val 'tags-file tags-proj t)))
      (let* ((proj-name-entry-key (concat (regexp-quote (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t))) ".*"))
             (proj-name-entry-value (cdr (assoc proj-name-entry-key etags-table-alist)))
             (tags-proj-entry-key (concat (regexp-quote (file-name-as-directory (mk-proj-get-config-val 'basedir tags-proj t))) ".*")))
        ;; main entry proj-name -> tags-file from proj-name, and from tags-proj, and all other that were already present in the list
        (add-to-list 'proj-name-entry-value (mk-proj-get-config-val 'tags-file tags-proj t) nil)
        (unless (string-equal proj-name tags-proj)
          (add-to-list 'proj-name-entry-value (mk-proj-get-config-val 'tags-file proj-name t) t))
        (setq etags-table-alist (remove-if (lambda (xs) (string-equal (car xs) proj-name-entry-key)) etags-table-alist))
        (add-to-list 'etags-table-alist (append (list proj-name-entry-key) proj-name-entry-value))
        ;; another entry for tags-proj
        (setq etags-table-alist (remove-if (lambda (xs) (string-equal (car xs) tags-proj-entry-key)) etags-table-alist))
        (add-to-list 'etags-table-alist (append (list tags-proj-entry-key) proj-name-entry-value))
        ))))

(defun mk-proj-cscope-load (&optional proj-name tags-proj)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (unless cscope-initial-directory
    (setq cscope-initial-directory (file-name-directory (mk-proj-get-config-val 'basedir proj-name t))))
  (let* ((db-key (regexp-quote (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name))))
         (current-db-list (or (assoc db-key cscope-database-regexps)
                              (list (file-name-directory (mk-proj-get-config-val 'basedir proj-name t)))))
         (proj-entry (list (file-name-directory (mk-proj-get-config-val 'cscope-namefile proj-name t))
                           (list "-s" (mk-proj-get-config-val 'basedir proj-name))))
         (tags-proj-entry (when tags-proj
                            (list (file-name-directory (mk-proj-get-config-val 'cscope-namefile tags-proj t))
                                  (list "-s" (mk-proj-get-config-val 'basedir tags-proj))))))
    (add-to-list 'current-db-list proj-entry 'eq)
    (when tags-proj
      (add-to-list 'current-db-list tags-proj-entry 'eq))
    (setq cscope-database-regexps (remove-if (lambda (xs)
                                               (string-equal (car xs) (car current-db-list)))
                                             cscope-database-regexps))
    (add-to-list 'cscope-database-regexps current-db-list 'eq)))

(defun mk-proj-etags-clear ()
  "Clear the TAGS file (if tags-file set)"
  (dolist (proj-name (append (list mk-proj-name) (mk-proj-get-config-val 'friends mk-proj-name t)))
    (let ((zeitgeist-prevent-send t))
      (when (and (mk-proj-get-config-val 'tags-file proj-name t)
                 (get-file-buffer (mk-proj-get-config-val 'tags-file proj-name t)))
        (mk-proj-maybe-kill-buffer (get-file-buffer (mk-proj-get-config-val 'tags-file proj-name t))))))
  (setq tags-file-name  nil
        tags-table-list nil)
  (when (boundp 'etags-table-alist)
    (setq etags-table-alist nil)))

(defun mk-proj-cscope-clear ()
  (setq cscope-initial-directory nil
        cscope-database-regexps nil))

(defun mk-proj-etags-cb (process event &optional proj-name)
  "Visit tags table when the etags process finishes."
  (message "Etags process %s received event %s" process event)
  (kill-buffer (get-buffer (concat "*etags " proj-name "*")))
  (cond
   ((string= event "finished\n")
    (mk-proj-etags-load mk-proj-name proj-name)
    (message "Refreshing TAGS file %s...done" (mk-proj-get-config-val 'tags-file proj-name t)))
   (t (message "Refreshing TAGS file %s...failed" (mk-proj-get-config-val 'tags-file proj-name t)))))

(defun mk-proj-cscope-cb (process event &optional proj-name)
  (message "Cscope process %s received event %s" process event)
  (kill-buffer (get-buffer (concat "*cscope " proj-name "*")))
  (cond
   ((string= event "finished\n")
    (mk-proj-cscope-load mk-proj-name proj-name)
    (message "Refreshing CSCOPE files in %s...done" (file-name-directory (mk-proj-get-config-val 'cscope-namefile proj-name t))))
   (t (message "Refreshing CSCOPE files in %s...failed" (file-name-directory (mk-proj-get-config-val 'cscope-namefile proj-name t))))))

(defun mk-proj-cscope (&optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (if (mk-proj-get-config-val 'cscope-namefile proj-name t)
      (let* ((cscope-files (mk-proj-get-config-val 'cscope-namefile proj-name t))
             (default-directory (mk-proj-get-config-val 'basedir proj-name))
             (default-find-cmd (concat "find '" (mk-proj-get-config-val 'basedir proj-name t)
                                       "' -type f "
                                       (mk-proj-find-cmd-src-args (mk-proj-get-config-val 'src-patterns proj-name t) proj-name)
                                       (mk-proj-find-cmd-ignore-args (mk-proj-get-config-val 'ignore-patterns proj-name t) proj-name)
                                       ))
             (cscope-find-cmd (concat default-find-cmd " > " cscope-files))
             (cscope-shell-cmd (if (mk-proj-get-config-val 'cscope-cmd proj-name t)
                                   (mk-proj-get-config-val 'cscope-cmd proj-name t)
                                 (concat "cscope -b -q -k -i " cscope-files)))
             (cscope-cmd (concat cscope-find-cmd "; " cscope-shell-cmd))
             (cscope-proc-name (concat proj-name "-cscope-process")))
        (message "project-cscope default-dir %s" default-directory)
        (message "project-cscope cscope-cmd \"%s\"" cscope-cmd)
        (message "Refreshing CSCOPE files in %s..." (file-name-directory cscope-files))
        (start-process-shell-command cscope-proc-name (concat "*cscope " proj-name "*") cscope-cmd)
        (set-process-sentinel (get-process cscope-proc-name) `(lambda (p e) (mk-proj-cscope-cb p e ,proj-name)))
        )))

(defun mk-proj-etags (&optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (if (mk-proj-get-config-val 'tags-file proj-name t)
      (let* ((tags-file-name (file-name-nondirectory (mk-proj-get-config-val 'tags-file proj-name t)))
             ;; If the TAGS file is in the basedir, we can generate
             ;; relative filenames which will allow the TAGS file to
             ;; be relocatable if moved with the source. Otherwise,
             ;; run the command from the TAGS file's directory and
             ;; generate absolute filenames.
             (relative-tags (string= (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t))
                                     (file-name-directory (mk-proj-get-config-val 'tags-file proj-name t))))
             (default-directory (file-name-as-directory (file-name-directory (mk-proj-get-config-val 'tags-file proj-name t))))
             (default-find-cmd (concat "find '" (if relative-tags "." (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t)))
                                       "' -type f "
                                       (mk-proj-find-cmd-src-args (mk-proj-get-config-val 'src-patterns proj-name t) proj-name)
                                       (mk-proj-find-cmd-ignore-args (mk-proj-get-config-val 'ignore-patterns proj-name t) proj-name)
                                       ))
             (etags-shell-cmd (if (mk-proj-get-config-val 'etags-cmd proj-name t)
                                  (mk-proj-get-config-val 'etags-cmd proj-name t)
                                "etags --extra=fq --fields=+afiklmnsSzt --C++-kinds=+p --C-kinds=+p -o"))
             (etags-cmd (concat (or (mk-proj-find-cmd-val 'src proj-name) default-find-cmd)
                                " | " etags-shell-cmd " '" tags-file-name "' -L - "))
             (etags-proc-name (concat proj-name "-etags-process")))
        (message "project-tags default-dir %s" default-directory)
        (message "project-tags etags-cmd \"%s\"" etags-cmd)
        (message "Refreshing TAGS file %s..." (mk-proj-get-config-val 'tags-file proj-name t))
        (start-process-shell-command etags-proc-name (concat "*etags " proj-name "*") etags-cmd)
        (set-process-sentinel (get-process etags-proc-name) `(lambda (p e) (mk-proj-etags-cb p e ,proj-name)))
        )))

(defun project-tags ()
  "Regenerate the project's TAG file. Runs in the background."
  (interactive)
  (mk-proj-assert-proj)
  (if (mk-proj-has-univ-arg)
      (project-tags-with-friends)
    (dolist (tagging (remove-duplicates (apply 'append (mapcar (lambda (l) (assoc l mk-proj-language-source-tagging))
                                                               (mk-proj-src-pattern-languages (mk-proj-get-config-val 'src-patterns))))))
      (cond ((eq tagging 'etags)
             (mk-proj-etags-clear)
             (mk-proj-etags))
            ((eq tagging 'cscope)
             (mk-proj-cscope-clear)
             (mk-proj-cscope))))))

(defun project-tags-with-friends ()
  (interactive)
  (mk-proj-assert-proj)
  (dolist (tagging (remove-duplicates (apply 'append (mapcar (lambda (l) (assoc l mk-proj-language-source-tagging))
                                                             (mk-proj-src-pattern-languages (mk-proj-get-config-val 'src-patterns))))))
    (cond ((eq tagging 'etags)
           (mk-proj-etags-clear)
           (mk-proj-etags mk-proj-name))
          ((eq tagging 'cscope)
           (mk-proj-cscope-clear)
           (mk-proj-cscope mk-proj-name))))
  (dolist (friend (mk-proj-get-config-val 'friends mk-proj-name t))
    (dolist (tagging (remove-duplicates (apply 'append (mapcar (lambda (l) (assoc l mk-proj-language-source-tagging))
                                                               (mk-proj-src-pattern-languages (mk-proj-get-config-val 'src-patterns friend))))))
      (cond ((eq tagging 'etags)
             (mk-proj-etags friend))
            ((eq tagging 'cscope)
             (mk-proj-cscope friend))))))

(defun mk-proj-find-cmd-src-args (src-patterns &optional proj-name)
  "Generate the ( -name <pat1> -o -name <pat2> ...) pattern for find cmd"
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (if src-patterns
      (let ((name-expr " \\(")
            (regex-or-name-arg (if (mk-proj-get-config-val 'patterns-are-regex proj-name t)
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
  (mk-proj-assert-proj t)
  (let* ((wap (word-at-point))
         (regex (or phrase
                    (if wap (read-string (concat "Grep project for (default \"" wap "\"): ") nil nil wap)
                      (read-string "Grep project for: "))))
         (find-cmd "find . -type f")
         (grep-cmd (concat "grep -i -n \"" regex "\""))
         (default-directory (file-name-as-directory
                             (if (or from-current-dir (mk-proj-has-univ-arg))
                                 default-directory
                               (mk-proj-get-config-val 'basedir)))))
    (when (mk-proj-get-config-val 'ignore-patterns)
      (setq find-cmd (concat find-cmd (mk-proj-find-cmd-ignore-args (mk-proj-get-config-val 'ignore-patterns)))))
    (when (mk-proj-get-config-val 'tags-file)
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

(defvar mk-proj-ack-default-args "--nocolor --nogroup --column")

(defun mk-proj-ack-cmd (regex)
  "Generate the ack command string given a regex to search for."
  (concat mk-proj-ack-cmd " "
          mk-proj-ack-default-args " "
          (if (and mk-proj-ack-respect-case-fold case-fold-search) "-i " "")
          (mk-proj-get-config-val 'ack-args) " "
          regex))

(defun project-ack (&optional phrase from-current-dir)
  "Run ack from project's basedir, using the `ack-args' configuration.
With C-u prefix act as `project-ack-with-friends'."
  (interactive)
  (mk-proj-assert-proj t)
  (if (mk-proj-has-univ-arg)
      (project-ack-with-friends)
    (let* ((wap (word-at-point))
           (regex (or phrase
                      (if wap (read-string (concat "Ack project for (default \"" wap "\"): ") nil nil wap)
                        (read-string "Ack project for: "))))
           (path (file-name-as-directory (mk-proj-get-config-val 'basedir)))
           (whole-cmd (concat (mk-proj-ack-cmd regex) " " path))
           (confirmed-cmd (read-string "Ack command: " whole-cmd nil whole-cmd))
           (default-directory (file-name-as-directory (mk-proj-get-config-val 'basedir))))
      (compilation-start confirmed-cmd 'ack-and-a-half-mode))))

;; ---------------------------------------------------------------------
;; Compile
;; ---------------------------------------------------------------------

(defun project-compile ()
  (interactive)
  (mk-proj-assert-proj t)
  (flet ((internal-compile (&optional cmd)
                           (let ((saved-compile-command compile-command)
                                 (compile-command (or cmd compile-command))
                                 (result-compile-command nil))
                             (call-interactively 'compile)
                             (setq result-compile-command compile-command
                                   compile-command saved-compile-command)
                             result-compile-command)))
    (let ((cmd (mk-proj-get-config-val 'compile-cmd)))
      (mk-proj-with-directory (mk-proj-get-config-val 'basedir)
                              (cond ((stringp cmd)
                                     (let ((new-cmd (internal-compile cmd)))
                                       (unless (string-equal cmd new-cmd)
                                         (mk-proj-set-config-val 'compile-cmd new-cmd))))
                                    ((commandp cmd)
                                     (call-interactively cmd))
                                    ((functionp cmd)
                                     (cd default-directory)
                                     (funcall cmd))
                                    (t
                                     (mk-proj-set-config-val 'compile-cmd (internal-compile))))))))

;; ---------------------------------------------------------------------
;; Dired
;; ---------------------------------------------------------------------

(defun project-dired ()
  "Open dired in the project's basedir (or jump to the existing dired buffer)"
  (interactive)
  (mk-proj-assert-proj t)
  (dired (mk-proj-get-config-val 'basedir)))

;; ---------------------------------------------------------------------
;; Find-file
;; ---------------------------------------------------------------------

(defun mk-proj-fib-init (&optional proj-name)
  "Either load the *file-index* buffer from the file cache, or create it afresh."
  (if (and (mk-proj-get-config-val 'file-list-cache proj-name t)
           (file-readable-p (mk-proj-get-config-val 'file-list-cache proj-name t)))
      (let ((zeitgeist-prevent-send t))
        (with-current-buffer (find-file-noselect (mk-proj-get-config-val 'file-list-cache proj-name t))
          (with-current-buffer (rename-buffer (mk-proj-fib-name proj-name))
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            (message (concat "Loading " (mk-proj-fib-name proj-name) " from %s") (mk-proj-get-config-val 'file-list-cache proj-name t)))))
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
  (cond
   ((string= event "finished\n")
    (let ((zeitgeist-prevent-send t))
      (with-current-buffer (get-buffer (mk-proj-fib-name proj-name))
        (setq buffer-read-only t)
        (when (mk-proj-get-config-val 'file-list-cache proj-name t)
          (write-file (mk-proj-get-config-val 'file-list-cache proj-name t))
          (rename-buffer (mk-proj-fib-name proj-name)))))
    (message "Refreshing %s buffer...done" (mk-proj-fib-name proj-name)))
   (t
    (mk-proj-fib-clear proj-name)
    (message "Failed to generate the %s buffer!" (mk-proj-fib-name proj-name)))))

(defun project-index (&optional proj-name)
  "Regenerate the *file-index* buffer that is used for project-find-file"
  (interactive)
  (unless proj-name
    (mk-proj-assert-proj t)
    (setq proj-name mk-proj-name))
  (when (mk-proj-get-config-val 'file-list-cache proj-name t)
    (mk-proj-fib-clear proj-name)
    (cd (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t)))
    (let* ((default-directory (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t)))
           (start-dir (if mk-proj-file-index-relative-paths
                          "."
                        (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t))))
           (find-cmd (concat "find '" start-dir "' -type f "
                             (mk-proj-find-cmd-src-args (mk-proj-get-config-val 'src-patterns proj-name t) proj-name)
                             (mk-proj-find-cmd-ignore-args (mk-proj-get-config-val 'ignore-patterns proj-name t) proj-name)))
           (proc-name "index-process"))
      (when (mk-proj-get-vcs-path proj-name)
        (setq find-cmd (concat find-cmd " -not -path " (concat "'*/" (mk-proj-get-vcs-path proj-name) "/*'"))))
      (setq find-cmd (or (mk-proj-find-cmd-val 'index proj-name) find-cmd))
      (with-current-buffer (get-buffer-create (mk-proj-fib-name proj-name))
        (buffer-disable-undo) ;; this is a large change we don't need to undo
        (setq buffer-read-only nil))
      (message "project-index cmd: \"%s\"" find-cmd)
      (message "Refreshing %s buffer..." (mk-proj-fib-name proj-name))
      (start-process-shell-command proc-name (mk-proj-fib-name proj-name) find-cmd)
      (set-process-sentinel (get-process proc-name) `(lambda (p e) (mk-proj-fib-cb p e ,proj-name)))
      )))

(defun mk-proj-fib-matches (&optional regex proj-name)
  "Return list of files in *file-index* matching regex.

REGEX can be a list or a single regex.
If it is nil, return all files.

Returned file paths are relative to the project's basedir."
  (unless (get-buffer (mk-proj-fib-name proj-name))
    (mk-proj-fib-init proj-name))
  (when (gethash proj-name mk-proj-list nil)
    (with-current-buffer (mk-proj-fib-name proj-name)
      (let ((basedir (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t)))
            (current-filename nil))
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

(defun mk-proj-files (&optional proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (mapcar (lambda (f) (expand-file-name (concat (file-name-as-directory (mk-proj-get-config-val 'basedir proj-name t)) f)))
          (mk-proj-fib-matches nil proj-name)))

(defun mk-proj-friendly-files (&optional proj-name friends-only)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((friendly-files (mapcan (lambda (friend)
                                  (if (file-exists-p (mk-proj-with-directory (mk-proj-get-config-val 'basedir proj-name t)
                                                                             (expand-file-name friend)))
                                      (list friend)
                                    (mk-proj-files friend)))
                                (mk-proj-get-config-val 'friends proj-name t))))
    friendly-files))

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
  (mk-proj-assert-proj t)
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
          (find-file (concat (file-name-as-directory (mk-proj-get-config-val 'basedir mk-proj-name t)) file))))))))

(defun* project-find-file-ido ()
  "Find file in the current project using 'ido'.

Choose a file to open from among the files listed in buffer
*file-index*.  The ordinary 'ido' methods allow advanced
selection of the file. See also: `project-index',
`project-find-file'."
  (interactive)
  (mk-proj-assert-proj t)
  (unless (get-buffer (mk-proj-fib-name))
    (message "Please use project-index to create the index before running project-find-file-ido")
    (return-from "project-find-file-ido" nil))
  (let ((file (ido-completing-read "Find file in project matching (ido): "
                                   (mk-proj-fib-matches))))
    (when file
      (find-file (concat (file-name-as-directory (mk-proj-get-config-val 'basedir mk-proj-name t)) file)))))

(defun project-multi-occur (regex)
  "Search all open project files for 'regex' using `multi-occur'.

Act like `project-multi-occur-with-friends' if called with prefix arg."
  (interactive "sRegex: ")
  (mk-proj-assert-proj t)
  (if (mk-proj-has-univ-arg)
      (project-multi-occur-with-friends)
    (multi-occur (mk-proj-filter (lambda (b) (if (buffer-file-name b) b nil))
                                 (mk-proj-buffers))
                 regex)))

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

(defun mk-proj-fib-friend-matches (&optional regex proj-name)
  (unless proj-name
    (mk-proj-assert-proj)
    (setq proj-name mk-proj-name))
  (let ((resulting-matches '()))
    (dolist (friend (mk-proj-get-config-val 'friends proj-name t) resulting-matches)
      (if (file-exists-p (mk-proj-with-directory (mk-proj-get-config-val 'basedir proj-name t)
                                                 (expand-file-name friend)))
          (if regex
              (when (string-match regex friend) (add-to-list 'resulting-matches (expand-file-name friend)))
            (add-to-list 'resulting-matches (expand-file-name friend)))
        (setq resulting-matches (append resulting-matches
                                        (mapcar (lambda (f)
                                                  (expand-file-name (concat (file-name-as-directory (mk-proj-get-config-val 'basedir friend t)) f)))
                                                (mk-proj-fib-matches regex friend))))))
    ;;(remove-duplicates resulting-matches :test #'string-equal)
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
                     (when (mk-proj-find-config f t)
                       (let* ((friend-config (mk-proj-find-config f t))
                              (basedir (expand-file-name (car (cdr (assoc 'basedir friend-config)))))
                              (friend-basedir (if (string-equal (substring basedir -1) "/")
                                                  basedir
                                                (concat basedir "/"))))
                         (when (string-match (concat "^" (regexp-quote friend-basedir)) file-name)
                           (return-from "friend-loop" t))))))))
          t
        nil))))

(defun mk-proj-friendly-file-buffer-p (buf &optional proj-name)
  (and (buffer-file-name buf)
       (mk-proj-friendly-buffer-p buf proj-name)))

(defun mk-proj-friendly-special-buffer-p (buf &optional proj-name)
  (and (string-match "\*[^\*]\*" (buffer-name buf))
       (mk-proj-friendly-buffer-p buf proj-name)))

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
  (append (remove-if (lambda (buf) (not (string-match "\*[^\*]\*" (buffer-name buf)))) (mk-proj-friendly-buffers proj-name))
          (remove-if (lambda (buf) (or (and (symbolp 'mk-org-project-buffer-name)
                                            (not (string-equal (mk-org-project-buffer-name proj-name) (buffer-name buf))))
                                       (compilation-buffer-p buf)))
                     (buffer-list))))

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
            (unless (string-equal (mk-proj-get-config-val 'tags-file) f)
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
              (let ((line (buffer-substring start (point))))
                (message "Attempting to open %s" line)
                (find-file-noselect line t)))
            (forward-line)))))))

(defun project-close-friends ()
  (interactive)
  (mk-proj-assert-proj)
  (let ((closed nil)
        (dirty nil)
        (zeitgeist-prevent-send t))
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

(defun project-ack-with-friends ()
  "Run ack with project's basedir and all friend basedirs as arguments, using the `ack-args' configuration."
  (interactive)
  (mk-proj-assert-proj t)
  (let* ((wap (word-at-point))
         (regex (if wap (read-string (concat "Ack project for (default \"" wap "\"): ") nil nil wap)
                  (read-string "Ack project for: ")))
         (paths (mk-proj-find-unique-paths (append (list (mk-proj-get-config-val 'basedir)) (mk-proj-friend-basedirs))))
         (whole-cmd (concat (let ((s ""))
                              (dolist (d paths s)
                                (setq s (concat s (mk-proj-ack-cmd regex) " " d "; "))))))
         (confirmed-cmd (read-string "Ack command: " whole-cmd nil whole-cmd))
         (default-directory (file-name-as-directory (mk-proj-get-config-val 'basedir))))
    (compilation-start confirmed-cmd 'ack-and-a-half-mode)))

;;(defun mk-proj-find-projects-owning-file (file))

(provide 'mk-project)









;;; mk-project.el ends here
