;;; lazy.el ---  Lightweight project handling

;; Copyright (C) 2010  Matt Keller <mattkeller at gmail dot com>
;; Copyright (C) 2017 Andreas Raster <lazor at affenbande dot org>
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

;;; Code:

(require 'thingatpt)
(require 'cl-lib)
(require 'etags-table)
(require 'compile)
(require 'color)
(require 'imenu)

(declare-function lazy-org-project-buffer-name "lazy-orgmode")
(declare-function lazy-sourcemarker-restore "lazy-sourcemarker")

(defvar lazy-version "2.0.0")

(defvar lazy-global-cache-root (expand-file-name "~/.lazy/")
  "Root path under which to create files that contain project metadata like open
files, open friends etc. These are automatically created for a project under a
directory created under this path. Makes the open-files-cache, file-list-cache,
open-friends-cache directives optional.

See also `lazy-open-files-cache', `lazy-open-friends-cache',
`lazy-file-list-cache'")

;; ---------------------------------------------------------------------
;; Project Variables
;; ---------------------------------------------------------------------

(defvar lazy-name nil
  "Name of the current project. Required. First argument to lazy-def.")

(defun lazy-fib-name (&optional proj-name)
  "Buffer name of the file-list cache. This buffer contains a
list of all the files under the project's basedir (minus those
matching ignore-patterns) or, if index-find-cmd is set, the list
of files found by calling the custom find command.  The list is
used by `lazy-find-file' to quickly locate project files."
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let ((top-level-parent (lazy-get-config-val 'parent proj-name)))
    (while (and top-level-parent (lazy-get-config-val 'parent top-level-parent))
      (setq top-level-parent (lazy-get-config-val 'parent top-level-parent)))
    (concat "*" (or top-level-parent proj-name) " file-index*")))

(defconst lazy-vcs-path '((git . ".git")
                          (cvs . ".CVS")
                          (svn . ".svn")
                          (bzr . ".bzr")
                          (hg  . ".hg")
                          (darcs . "_darcs"))
  "When `lazy-vcs' is one of the VCS types listed here, ignore
the associated paths when greping or indexing the project.")

(defvar lazy-required-vars '((name . (identity stringp))
                                (basedir . (identity stringp (lambda (v) (> (length v) 0)) file-exists-p)))
  "Project config vars that are required in every project. Each entry is a (symbol . (functions)) tuple where the
functions are used by `lazy-check-required-vars' to test if a value given to symbol is valid.

name   : project name
basedir: root directory of the project

See also `lazy-optional-vars' `lazy-var-before-get-functions'.")

(defvar lazy-optional-vars '((parent . (stringp)) ;; parent needs to come first!
                             (languages . (listp))
                             (src-patterns . (listp))
                             (ignore-patterns . (listp))
                             (vcs . (symbolp))
                             (compile-cmd . ((lambda (v) (or (functionp v) (commandp v) (stringp v) (listp v)))))
                             (startup-hook . ((lambda (v) (or (functionp v) (commandp v) (stringp v) (listp v)))))
                             (shutdown-hook . ((lambda (v) (or (functionp v) (commandp v) (stringp v) (listp v)))))
                             (file-list-cache . (stringp))
                             (open-files-cache . (stringp))
                             (patterns-are-regex . (symbolp))
                             (friends . (listp))
                             (open-friends-cache . (stringp))
                             (gtags-config . (stringp file-exists-p))
                             (gtags-arguments . (stringp)))
  "Project config vars that are optional. Each entry is a (symbol . (functions)) tuple where the
functions are used by `lazy-check-optional-vars' to test if a value given to symbol is valid.


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

See also `lazy-required-vars' `lazy-var-before-get-functions'")

(defvar lazy-internal-vars '()
  "Project config vars that are ignored when saving the project config.")

(defun lazy-basedir-expand (var val &optional proj-name config-alist)
  "Helper used in `lazy-var-before-get-functions' to expand basedir."
  ;; need to check all these to make sure the value is not nil, but a string that does
  ;; not have zero length and also exists as file/directory
  (when (and (identity val) (stringp val) (> (length val) 0) (file-exists-p val))
    (file-name-as-directory (expand-file-name val))))

(defun lazy-var-get-open-file-cache (var val &optional proj-name config-alist)
  "Helper used in `lazy-var-before-get-functions' to find cache location."
  (if val
      (expand-file-name val)
    (lazy-get-cache-file var proj-name nil)))

(defun lazy-var-get-file-list-cache (var val &optional proj-name config-alist)
  "Helper used in `lazy-var-before-get-functions' to find cache location."
  (if val
      (expand-file-name val)
    (lazy-get-cache-file var proj-name t)))

(defun lazy-var-guess-languages (var val &optional proj-name config-alist)
  "Helper used in `lazy-var-before-get-functions' to guess project languages."
  (or val (lazy-src-pattern-languages (lazy-get-config-val 'src-patterns proj-name))))

(defvar lazy-var-before-get-functions '((basedir . lazy-basedir-expand)
                                        (file-list-cache . lazy-var-get-file-list-cache)
                                        (open-files-cache . lazy-var-get-open-file-cache)
                                        (open-friends-cache . lazy-var-get-open-file-cache)
                                        (languages . lazy-var-guess-languages)
                                        (patterns-are-regex . (lambda (var val &optional proj-name config-alist)
                                                                (if (and config-alist
                                                                         (not (assoc 'patterns-are-regex config-alist)))
                                                                    t
                                                                  val)))
                                        (friends . (lambda (var val &optional proj-name config-alist)
                                                     (loop for friend in val
                                                           if (gethash friend lazy-list)
                                                           collect friend))))
  "Config vars from `lazy-required-vars' and `lazy-optional-vars' (except 'name')
can be associated with a function in this association list, which will be
applied to the value of the var right after it is taken from the config-alist.

That means when querying the configuration with `lazy-get-config-val', the var
symbol is used to look up a function in this list and, if present, that function is then
applied to the var symbol and var value pair and its result used as new var value.

See also `lazy-get-config-val'.")

(defvar lazy-var-ask-functions '((name . (lambda ()
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
                                                until (cl-some (lambda (x) (eq (car x) (read v))) lazy-vcs-path)
                                                finally return (read v))))
                                 (compile-cmd . (lambda ()
                                                  (read-string "Compile command: " super)))
                                 (patterns-are-regex . (lambda () t)))
  "Functions that are used to ask the user about what a vars value should be.")

(defvar lazy-before-load-hook '()
  "Hook that runs before loading a project.")
(defvar lazy-before-files-load-hook '()
  "Hook that runs before the files are visited when loading.")
(defvar lazy-after-load-hook '()
  "Hook that runs after loading a project.")

(defvar lazy-before-unload-hook '()
  "Hook that runs before unloading a project.")
(defvar lazy-before-files-unload-hook '()
  "Hook that runs before files are closed when unloading.")
(defvar lazy-after-unload-hook '()
  "Hook that runs after unloading a project.")

(defvar lazy-history '())

(defvar lazy-buildsystems '((gnu-make ((files ("autogen.sh" "configure" "Makefile"))
                                       (build ("make"))))
                            (cmake ((files ("CMakeLists.txt"))
                                    (build ("mkdir -p build; cd build; cmake .. && make"))))
                            (cabal ((files ("Setup.lhs"))
                                    (build ("runhaskell Setup.lhs build"))))
                            (python ((files ("setup.py"))
                                     (build ("python setup.py build"))))
                            (ninja ((files ("build.ninja"))
                                    (build ("ninja"))))
                            (premake5 ((files ("premake5.lua"))
                                       (build ("premake5 gmake; make"))))
                            (scons ((files ("SConstruct"))
                                    (build ("scons"))))
                            (waf ((files ("wscript"))
                                  (build ("waf"))))
                            (tup ((files ("Tupfile"))
                                  (build ("tup upd"))))
                            (meson ((files ("meson.build"))
                                    (build ("meson build"))))
                            (grunt ((files ("Gruntfile" "package.json"))
                                      (build ("grunt"))))
                            (leiningen ((files ("project.clj"))
                                        (build ("lein run"))))
                            (ant ((files ("build.xml"))
                                  (build ("ant compile"))))
                            (gradle ((files ("build.gradle" "settings.gradle" "gradle" "gradlew" "gradlew.bat"))
                                     (build ("gradle"))))
                            (maven ((files (".mvn"))
                                    (build ("mvn package"))))
                            (bazel ((files ("BUILD"))
                                    (build ("bazel build"))))
                            (stack ((files ("stack.yaml"))
                                    (build ("stack build"))))
                            (mbld ((files ("bld.proj"))
                                   (build ("mbld"))))
                            (xbuild ((files (".*\.csproj" ".*\.sln"))
                                     (build ("xbuild"))))
                            (msbuild ((files (".*\.csproj" ".*\.sln"))
                                      (build ("msbuild"))))
                            (rake ((files ("Rakefile"))
                                   (build ("rake"))))
                            (ansible ((files ("site.yml" "hosts"))
                                      (build ("ansible-playbook -i hosts site.yml"))))
                            (fabric ((files ("fabfile.py"))
                                     (build ("fab")))))
  "Used when guessing a project root or its compile-cmd.")

(defvar lazy-src-pattern-table '(("h" . (c ".*\\.c" ".*\\.cpp" ".*\\.cc" ".*\\.h" ".*\\.hpp" ".*\\.hh"))
                                 ("hpp" . (cpp ".*\\.cpp" ".*\\.c" ".*\\.h" ".*\\.hh" ".*\\.hpp"))
                                 ("hh" . (cpp ".*\\.cc" ".*\\.c" ".*\\.h" ".*\\.hh" ".*\\.hpp"))
                                 ("c" . (c ".*\\.c" ".*\\.h"))
                                 ("cpp" . (cpp ".*\\.cpp"  ".*\\.c" ".*\\.h" ".*\\.hh" ".*\\.hpp"))
                                 ("cc" . (cpp  ".*\\.cc"  ".*\\.c" ".*\\.h" ".*\\.hh" ".*\\.hpp"))
                                 ("protobuf". (protobuf ".*\.protobuf"))
                                 ("objc" . (objc ".*\.objc"))
                                 ("d" . (d ".*\.d"))
                                 ("hs" . (haskell  ".*\\.hs" ".*\\.lhs" ".*\\.cabal"))
                                 ("lhs" . (haskell  ".*\\.hs" ".*\\.lhs" ".*\\.cabal"))
                                 ("cabal" . (haskell  ".*\\.hs" ".*\\.lhs" ".*\\.cabal"))
                                 ("php" . (php  ".*\\.php" ".*\\.js" ".*\\.html" ".*\.css" ".*\.json"))
                                 ("html". (html ".*\.html" ".*\.css" ".*\.js" ".*\.json"))
                                 ("js" . (javascript  ".*\.html" ".*\.css" ".*\.js" ".*\.json"))
                                 ("json" . (json ".*\.html" ".*\.css" ".*\.js" ".*\.json"))
                                 ("css" . (css ".*\.html" ".*\.css" ".*\.js" ".*\.json"))
                                 ("elm" . (elm ".*\.elm" ".*\\.js" ".*\\.html" ".*\.css" ".*\.json"))
                                 ("coffee" . (coffeescript ".*\.coffee" ".*\.litcoffee" ".*\\.js" ".*\\.html" ".*\.css" ".*\.json"))
                                 ("litcoffee" . (coffeescript ".*\.coffee" ".*\.litcoffee" ".*\\.js" ".*\\.html" ".*\.css" ".*\.json"))
                                 ("ts" . (typescript ".*\.ts" ".*\.d\.ts" ".*\.map" ".*\\.js" ".*\\.html" ".*\.css" ".*\.json"))
                                 ("d.ts" . (typescript ".*\.ts" ".*\.d\.ts" ".*\.map" ".*\\.js" ".*\\.html" ".*\.css" ".*\.json"))
                                 ("map" . (typescript ".*\.ts" ".*\.d\.ts" ".*\.map" ".*\\.js" ".*\\.html" ".*\.css" ".*\.json"))
                                 ("el" . (elisp  ".*\\.el" ".*\\.el\\.gz"))
                                 ("el.gz" . (elisp  ".*\\.el" ".*\\.el\\.gz"))
                                 ("lisp" . (lisp  ".*\\.lisp"  ".*\\.lsp"))
                                 ("lsp" . (lisp  ".*\\.lisp"  ".*\\.lsp"))
                                 ("scm" . (scheme  ".*\\.scm"))
                                 ("lua" . (lua ".*\\.lua"))
                                 ("r" . (r ".*\.r"))
                                 ("clojure" . (clojure ".*\\.clj" ".*\\.clojure"))
                                 ("clj" . (clojure ".*\\.clojure" ".*\\.clj"))
                                 ("java" . (java ".*\\.java"))
                                 ("pl" . (perl ".*\\.pl" ".*\\.pm" ".*\\.pod" ".*\\.t"))
                                 ("pm" . (perl ".*\\.pl" ".*\\.pm" ".*\\.pod" ".*\\.t"))
                                 ("pod" . (perl ".*\\.pl" ".*\\.pm" ".*\\.pod" ".*\\.t"))
                                 ("t" . (perl ".*\\.pl" ".*\\.pm" ".*\\.pod" ".*\\.t"))
                                 ("py" . (python ".*\\.py"))
                                 ("sh" . (shell ".*\\.sh"))
                                 ("go" . (go ".*\.go"))
                                 ("rs" . (rust ".*\.rs"))
                                 ("scala" . (scala ".*\.scala"))
                                 ("myr" . (myrddin ".*\.myr"))
                                 ("zep" . (zephir ".*\.zep"))
                                 ("awk" . (awk ".*\.awk"))
                                 ("bat" . (batch ".*\.bat" ".*\.BAT" ".*\.cmd" ".*\.CMD"))
                                 ("cmd" . (batch ".*\.bat" ".*\.BAT" ".*\.cmd" ".*\.CMD"))
                                 ("cs" . (csharp ".*\.cs" ".*\.xaml" ".*\.xml"))
                                 ("erl" . (erlang ".*\.erl"))
                                 ("f90" . (fortran ".*\.f90" ".*\.f95" ".*\.f03" ".*\.f"))
                                 ("f95" . (fortran ".*\.f95" ".*\.f90" ".*\.f03" ".*\.f"))
                                 ("f03" . (fortran ".*\.f03" ".*\.f95" ".*\.f90" ".*\.f"))
                                 ("f" . (fortran ".*\.f" ".*\.f95" ".*\.f03" ".*\.f90"))
                                 ("ml" . (ocaml ".*\.ml" ".*\.mli"))
                                 ("mli" . (ocaml ".*\.mli" ".*\.ml"))
                                 ("rb" . (ruby ".*\.rb"))
                                 ("tcl" . (tcl ".*\.tcl"))
                                 ("tex" . (tex ".*\.tex"))
                                 ("v" . (verilog ".*\.v" ".*\.vh"))
                                 ("vh" . (verilog ".*\.v" ".*\.vh"))
                                 ("vhdl" . (vhdl ".*\.vhdl"))
                                 ("vim" . (vim ".*\.vim"))
                                 ("yacc" . (yacc ".*\.y"))
                                 ("adb" . (ada ".*\.adb"))
                                 ("m" . (matlab ".*\.m"))
                                 ("frag" . (glsl ".*\.frag" ".*\.vert" ".*\.geo"))
                                 ("vert" . (glsl ".*\.frag" ".*\.vert" ".*\.geo"))
                                 ("geo" . (glsl ".*\.frag" ".*\.vert" ".*\.geo"))
                                 ("cu" . (cuda ".*\.cu" ".*\.cuh"))
                                 ("cuh" . (cuda ".*\.cu" ".*\.cuh"))
                                 ("yaml" . (yaml ".*\.yaml" ".*\.yml"))
                                 ("yml" . (yaml ".*\.yaml" ".*\.yml")))
  "Maps file suffixes to regexps used as source-patterns when guessing a
project config from the currently opened file in the active buffer.")

(defvar lazy-config-save-location (concat (file-name-as-directory lazy-global-cache-root) "projects.el")
  "Where to save project configs in elisp. If this is a filename project
configs will be written to that file. If it is a directory an elisp
file with the projects name will be created in that directory.")

(defvar lazy-language-tag-systems '((ada . (gtags+universal-ctags))
                                    (awk . (gtags+exuberant-ctags))
                                    (batch . (gtags+exuberant-ctags))
                                    (cobol . (gtags+exuberant-ctags))
                                    (c . (gtags))
                                    (clojure . (gtags+universal-ctags))
                                    (coffeescript . (gtags+universal-ctags))
                                    (cpp . (gtags))
                                    (csharp . (gtags+exuberant-ctags))
                                    (css . (gtags+universal-ctags))
                                    (cuda . (gtags+pygments))
                                    (d . (gtags+universal-ctags))
                                    (elisp . (gtags+exuberant-ctags))
                                    (elm . (gtags+universal-ctags))-
                                    (erlang . (gtags+exuberant-ctags))
                                    (fortran . (gtags+exuberant-ctags))
                                    (glsl . (gtags+pygments))
                                    (go . (gtags+universal-ctags))
                                    (haskell . (gtags+pygments))
                                    (html . (gtags+universal-ctags))
                                    (java . (gtags))
                                    (javascript . (gtags+exuberant-ctags))
                                    (json . (gtags+universal-ctags))
                                    (lisp . (gtags+exuberant-ctags))
                                    (lua . (gtags+exuberant-ctags))
                                    (matlab . (gtags+pygments))
                                    (myrddin . (gtags+universal-ctags))
                                    (ocaml . (gtags+exuberant-ctags))
                                    (perl . (gtags+exuberant-ctags))
                                    (php . (gtags))
                                    (protobuf . (gtags+universal-ctags))
                                    (python . (gtags+exuberant-ctags))
                                    (r . (gtags+universal-ctags))
                                    (ruby . (gtags+exuberant-ctags))
                                    (rust . (gtags+universal-ctags))
                                    (scheme . (gtags+exuberant-ctags))
                                    (shell . (gtags+exuberant-ctags))
                                    (tcl . (gtags+exuberant-ctags))
                                    (tex . (gtags+exuberant-ctags))
                                    (verilog . (gtags+exuberant-ctags))
                                    (vhdl . (gtags+exuberant-ctags))
                                    (vim . (gtags+exuberant-ctags))
                                    (yacc . (gtags))
                                    (yaml . (gtags+universal-ctags))
                                    (zephir . (gtags+universal-ctags)))
  "Defines which tagging system to use for which language.

Only gtags and gtags+exuberant-ctags are implemented.")

(defvar lazy-thing-selector 'symbol)

(defvar lazy-completions-cache (make-hash-table :test 'equal))

(defvar lazy-list (make-hash-table :test 'equal))

;; ---------------------------------------------------------------------
;; Customization
;; ---------------------------------------------------------------------

(defgroup lazy nil
  "A programming project management library."
  :group 'tools)

(defcustom lazy-use-ido-selection nil
  "If ido-mode is available, use ido selection where appropriate."
  :type 'boolean
  :group 'lazy)

(defcustom lazy-file-index-relative-paths t
  "If non-nil, generate relative path names in the file-index buffer"
  :type 'boolean
  :group 'lazy)

;; ---------------------------------------------------------------------
;; Utils
;; ---------------------------------------------------------------------

(defun lazy-save-state ()
  "Save the currently open project files."
  (when lazy-name
    (lazy-save-open-file-info)
    (lazy-save-open-friends-info)))

(defun lazy-zip (&rest lists)
  "A zipper takes lists like (1 2 3) (a b c) and produces a result like ((1 a) (2 b) (3 c))"
  (let* ((n (- (length lists) 1))
         (i 0)
         (rs '()))
    (while (cl-some 'identity (mapcar (lambda (l) (> (length l) i)) lists))
      (setq rs (append rs (list (cl-loop for m from 0 to n
                                         collect (nth i (nth m lists))))))
      (setq i (1+ i)))
    rs))

(defun lazy-dirname (path)
  "Take a path and return the directory only, without filename."
  (file-name-as-directory (apply #'concat (reverse (mapcar (lambda (s)
                                                             (if (> (length s) 0)
                                                                 (file-name-as-directory s)
                                                               (file-name-as-directory "/")))
                                                           (cdr (reverse (split-string (expand-file-name path) "/"))))))))

(defun lazy-assert-proj (&optional try-guessing)
  (unless lazy-name
    (let* ((continue-prevent-restore t)
           (guessed-alist (cond ((eq try-guessing 'quiet)
                                 (lazy-guess-alist nil nil))
                                (try-guessing
                                 (lazy-guess-alist t t)))))
      (cond ((and guessed-alist
                  (eq try-guessing 'quiet)
                  (gethash (cadr (assoc 'name guessed-alist)) lazy-list nil))
             (lazy-load-project (cadr (assoc 'name guessed-alist))))
            ((and guessed-alist
                  (eq try-guessing 'quiet)
                  (not (gethash (cadr (assoc 'name guessed-alist)) lazy-list nil)))
             (lazy-def (cadr (assoc 'name guessed-alist)) guessed-alist)
             (lazy-load-project (cadr (assoc 'name guessed-alist))))
            ((and guessed-alist
                  try-guessing
                  (gethash (cadr (assoc 'name guessed-alist)) lazy-list nil)
                  (y-or-n-p (concat "Load project " (cadr (assoc 'name guessed-alist)) "? ")))
             (lazy-load-project (cadr (assoc 'name guessed-alist))))
            ((and guessed-alist
                  try-guessing
                  (not (gethash (cadr (assoc 'name guessed-alist)) lazy-list nil))
                  (y-or-n-p (concat "Create project " (cadr (assoc 'name guessed-alist)) "? ")))
             (lazy-def (cadr (assoc 'name guessed-alist)) guessed-alist)
             (lazy-load-project (cadr (assoc 'name guessed-alist))))
            (t
             (error "No project is set!"))))))

(defun lazy-maybe-kill-buffer (bufname)
  (let ((b (get-buffer bufname)))
    (when b (kill-buffer b))))

(defun lazy-get-vcs-path (&optional proj-name)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (if (lazy-get-config-val 'vcs proj-name)
      (cdr (assoc (lazy-get-config-val 'vcs proj-name) lazy-vcs-path))
    nil))

(defun lazy-has-univ-arg ()
  (eql (prefix-numeric-value current-prefix-arg) 4))

(defun lazy-names ()
  (let ((names nil))
    (maphash (lambda (k v) (when k (add-to-list 'names k))) lazy-list)
    names))

(defun lazy-use-ido ()
  (and (boundp 'ido-mode) lazy-use-ido-selection))

(defun lazy-filter (condp lst)
  "Filter LST with CONDP. All elements for which CONDP returns t will be kept,
all others filtered."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(cl-defun lazy-any (condp lst)
  "Apply CONDP to all elements of LST."
  (cl-some condp lst))

(cl-defun lazy-all (condp lst)
  "Apply CONDP to all elements of LST, return nil as soon as
CONDP yields nil."
  (let ((b t))
    (dolist (x lst b)
      (unless b
        (cl-return-from "lazy-all" nil))
       (setq b (funcall condp x)))))

(defun lazy-flatten (xs)
  "Takes a list XS of lists and returns a list with only the elements of
all lists within the list XS.

\(lazy-flatten '(1 (2 (3 4)))) = '(1 2 3 4)"
  (if (listp xs)
      (let ((ret nil))
        (while xs
          (setq ret (append ret (lazy-flatten (car xs))))
          (setq xs (cdr xs)))
        ret)
    (list xs)))

(defmacro lazy-assoc-pop (key alist)
  "Like `assoc', but remove the (KEY . value) pair from the ALIST."
  `(let ((result (assoc ,key ,alist)))
     (setq ,alist (delete result ,alist))
     result))

(defun lazy-alist-union (alist1 alist2)
  "Make a union alist out of ALIST1 and ALIST2. The second alist
is the one that overwrites values in the first alist if they both
contain a similar key."
  (append (mapcar (lambda (c)
                    (or (lazy-assoc-pop (car c) alist2) c)) alist1) alist2))

(defun lazy-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(cl-defun lazy-buffer-has-markers-p (&optional buf)
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (forward-char)
        (when (buffer-has-markers-at (point))
          (cl-return-from "lazy-buffer-has-markers-p" t)))
      (cl-return-from "lazy-buffer-has-markers-p" nil))))

(defun lazy-buildsystem-patterns ()
  (mapcar 'regexp-quote
          (lazy-flatten (cl-loop for bs in lazy-buildsystems
                                 collect (cadr (assoc 'files (cadr bs)))))))

(defun lazy-path-complement (path1 path2)
  "This will return the part of the two paths PATH1 and PATH2 that is _not_ equal.

If both paths have a common prefix this will return the part that is not
found in both of them:
\(lazy-path-complement \"/foo/bar/blah\" \"/foo/bar\") -> \"blah/\"

If they do not share a common prefix this will return nil. If they are
equal this will return t."
  (cl-loop for x = (split-string path1 "/" t) then (cdr x)
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

(cl-defun lazy-path-equal (a b &optional string-equal)
  "If path B is equal to or fully contained in path A, this will be true.

If STRING-EQUAL is non-nil behaviour is as if string-equal wouldn't
care about slashes.

Examples:
\(lazy-path-equal \"/foo/bar\" \"/foo/bar\")      -> true
\(lazy-path-equal \"/foo/bar\" \"/foo\")          -> true
\(lazy-path-equal \"/foo/bar\" \"/foo/bar/blah\") -> false
\(lazy-path-equal \"/foo/bar\" \"/foo/blah/bar\") -> false

\(lazy-path-equal \"/foo/bar\" \"/foo/bar/\" t)     -> true
\(lazy-path-equal \"/foo/bar\" \"/foo\" t)          -> false
\(lazy-path-equal \"/foo/bar\" \"/foo/bar/blah\" t) -> false
\(lazy-path-equal \"/foo/bar\" \"/foo/blah/bar\" t) -> false"
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

(cl-defun lazy-search-path (re path &optional stop-paths ignore-paths)
  (let ((xs (reverse (split-string path "/" t))))
    (while (and (cdr xs)
                (cl-loop for ig in stop-paths
                         if (lazy-path-equal ig (apply 'concat (mapcar (lambda (s) (concat "/" s)) (reverse xs))))
                         return nil
                         finally return t))
      (when (and (directory-files (apply 'concat (mapcar (lambda (s) (concat "/" s)) (reverse xs))) nil re)
                 (not (cl-some (lambda (re) (string-match re (car xs))) ignore-paths)))
        (cl-return-from "lazy-search-path" (apply 'concat (mapcar (lambda (s) (concat "/" s)) (reverse xs)))))
      (setq xs (cdr xs)))))

(defun lazy-join (delimiter strings)
  (cl-reduce (lambda (a b)
            (cl-concatenate 'string a delimiter b))
          strings))

(defmacro lazy-with-directory (path &rest body)
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

(defun lazy-search-projects ()
  "Search for projects by evaluating `lazy-config-save-location' as if it were an elisp file. Falls back to
trying to evaluate `lazy-global-cache-root'/projects.el"
  (cond ((file-exists-p lazy-config-save-location)
         (load lazy-config-save-location))
        ((file-exists-p (concat (file-name-as-directory (expand-file-name lazy-global-cache-root)) "projects.el"))
         (load (concat (file-name-as-directory (expand-file-name lazy-global-cache-root)) "projects.el")))))

(cl-defun lazy-find-alist (&optional proj-name (inherit t))
  "Get a projects config-alist from the global projects hashmap."
  (when (or proj-name (setq proj-name lazy-name))
    (let* ((child (gethash proj-name lazy-list))
           (alist child))
      (while (and (assoc 'parent child)
                  inherit)
        (setq child (gethash (cadr (assoc 'parent child)) lazy-list)
              alist (append alist (cl-remove-if (lambda (x) (cl-some (lambda (y) (eq (cl-first x) (cl-first y))) alist)) child))))
      alist)))

(cl-defun lazy-get-config-val (key &optional proj-name (inherit t) (proj-alist nil))
  "Finds the value associated with KEY. A project PROJ can optionally
be specified.

If the third argument INHERIT is non-nil, all parents will queried
for the KEY and the first value that is found is returned.

See also `lazy-var-before-get-functions'."
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let* ((proj-alist (or proj-alist (lazy-find-alist proj-name nil)))
         (fn (cdr (assoc key lazy-var-before-get-functions)))
         (val (or (when fn
                    (funcall fn key (cadr (assoc key proj-alist)) proj-name proj-alist))
                  (and (assoc key proj-alist)
                       (cadr (assoc key proj-alist)))
                  (let ((parent (cadr (assoc 'parent proj-alist))))
                    (when (and inherit parent)
                      (lazy-get-config-val key parent t))))))
    (if fn (funcall fn key val proj-name proj-alist) val)))

(defalias 'lazy-config-val 'lazy-get-config-val
  "Alias for `lazy-get-config-val' to ensure backward compatibility.")

(defun lazy-set-config-val (key value &optional proj-name)
  "Set the value associated with KEY to VALUE in config of project NAME."
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let* ((current-alist (lazy-find-alist proj-name nil))
         (new-alist current-alist))
    (when current-alist
      (while (assoc key new-alist)
        (setq new-alist (delq (assoc key new-alist) new-alist)))
      (add-to-list 'new-alist `(,key ,value))
      (unless (equal new-alist current-alist)
        (puthash proj-name new-alist lazy-list)
        (lazy-backend-funcall (lazy-detect-backend proj-name)
                                 'save proj-name new-alist)))))

(cl-defun lazy-eval-alist (proj-name config-alist)
  "Evaluates a CONFIG-ALIST for PROJ-NAME by calling eval on every
value.

It then uses `lazy-check-required-vars' and `lazy-check-optional-vars'
to verify the evaluated configuration."
  (interactive)
  (let* ((evaluated-config-alist `((name ,proj-name)))
         (result-alist (dolist (cv config-alist evaluated-config-alist)
                         (let* ((key (car cv))
                                ;; super behaves like a keyword that can be used within a configuration
                                ;; to refer to the parents value
                                ;; I haven't tested this, it is a experimental feature
                                (super (when (cadr (assoc 'parent config-alist))
                                         (lazy-get-config-val key (cadr (assoc 'parent config-alist)) t)))
                                (lisp (car (cdr cv)))
                                (value (condition-case nil (eval lisp) (error lisp))))
                           (unless (eq key 'name)
                             (add-to-list 'evaluated-config-alist `(,key ,value)))))))
    (when (gethash proj-name lazy-list)
      (setq result-alist (lazy-alist-union (gethash proj-name lazy-list) result-alist)))
    ;; both check vars functions error, but I don't want to interrupt when loading emacs,
    ;; so this catches the errors but still outputs the error message
    (if (or (condition-case err (lazy-check-required-vars proj-name result-alist) (error (message (error-message-string err))))
            (condition-case err (lazy-check-optional-vars proj-name result-alist) (error (message (error-message-string err)))))
        (progn (message "Project %s could not be evaluated because of errors!" proj-name) nil)
      result-alist)))

(cl-defun lazy-def (&optional proj-name config-alist)
  "Associate the settings in CONFIG-ALIST with project PROJ-NAME.

All values within CONFIG-ALIST will be evaluated when they look
like a lisp expression or symbol. So make sure to quote lists!

See also `lazy-undef', `lazy-required-vars' and `lazy-optional-vars'."
  (interactive)
  (cond ((stringp proj-name)
         (let ((alist (lazy-eval-alist proj-name config-alist)))
           (when (and alist (file-exists-p (cadr (assoc 'basedir alist))))
             (puthash proj-name alist lazy-list)
             (message "Defined: %s" proj-name)
             alist)))
        ((and (functionp 'lazy-org-entry-define-project)
              (eq major-mode 'org-mode)
              (lazy-org-entry-define-project)))))



























(defun lazy-find-project-elisp-configuration-in-buffer (proj-name &optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (cl-block "while-search-loop"
        (while (and (re-search-forward (regexp-quote proj-name) nil t)
                    (not (eobp)))
          (when (re-search-backward (regexp-quote "(lazy-def") (save-excursion (re-search-backward ")" nil t)) t)
            (cl-return-from "while-search-loop" (point))))))))

(defun lazy-find-save-location-marker (&optional proj-name config-alist)
  "This tries to find a suitable location to save the projects configuration to disk."
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let ((save-location nil))
    (cond
     ;; find file in directory named after project
     ((condition-case nil (directory-files (expand-file-name lazy-config-save-location)) (error nil))
      (with-current-buffer (find-file-noselect (concat (expand-file-name lazy-config-save-location) proj-name ".el"))
        (save-excursion
          (goto-char (or (lazy-find-project-elisp-configuration-in-buffer proj-name) (point-max)))
          (point-marker))))
     ;; find section to save under in single el file
     ((or (and (stringp lazy-config-save-location)
               (or (file-exists-p (expand-file-name lazy-config-save-location))
                   (write-region "" nil (expand-file-name lazy-config-save-location))
                   t)
               (setq save-location lazy-config-save-location))
          (and (or (file-exists-p (concat (file-name-as-directory (expand-file-name lazy-global-cache-root)) "projects.el"))
                   (write-region "" nil (concat (file-name-as-directory (expand-file-name lazy-global-cache-root)) "projects.el"))
                   t)
               (setq save-location (concat (file-name-as-directory (expand-file-name lazy-global-cache-root)) "projects.el"))))
      (with-current-buffer (find-file-noselect (expand-file-name save-location))
        (save-excursion
          (cond ((condition-case nil (goto-char (lazy-find-project-elisp-configuration-in-buffer proj-name)) (error nil))
                 (point-marker))
                ((goto-char (point-max))
                 (point-marker))
                (t (error "lazy: could not find a location to save %s, see lazy-config-save-location" proj-name))))))
     ;; no suitable location found
     (t (error "lazy: could not find a location to save %s, see lazy-config-save-location" proj-name)))))

(defun lazy-config-insert (proj-name config-alist &optional insert-undefined insert-internal)
  (save-excursion
    (insert (concat "(lazy-def \"" proj-name "\" '("))
    (cl-loop for k in (append lazy-required-vars lazy-optional-vars)
             if (and (not (eq (car k) 'name))
                     (or (not (cl-some (lambda (j) (eq (car k) j)) lazy-internal-vars))
                         insert-internal)
                     (or (not (cdr (assoc (car k) lazy-var-before-get-functions)))
                         (not (string-equal (prin1-to-string (funcall (cdr (assoc (car k) lazy-var-before-get-functions)) (car k) nil))
                                            (prin1-to-string (lazy-get-config-val (car k) proj-name))))
                         insert-internal))
             do (when (or insert-undefined
                          (assoc (car k) config-alist))
                  (insert (concat "(" (symbol-name (car k)) " " (prin1-to-string (cadr (assoc (car k) config-alist))) ")"))
                  (unless (eq (car k) (car (last lazy-optional-vars)))
                    (newline))
                  (indent-according-to-mode)))
    (insert "))\n")))

(defun lazy-config-save (proj-name config-alist)
  (let ((marker (lazy-find-save-location-marker proj-name config-alist)))
    (with-marker marker
                 (let ((mod (buffer-modified-p)))
                   (cond ((looking-at "(lazy-def.*")
                          (let* ((begin (point))
                                 (end (save-excursion (thing-at-point--end-of-sexp) (point)))
                                 (old-alist (eval (nth 2 (read (buffer-substring begin end)))))
                                 (new-alist (lazy-alist-union old-alist config-alist)))
                            (kill-region begin end)
                            (lazy-config-insert proj-name new-alist)
                            (call-interactively 'eval-defun)))
                         (t
                          (goto-char (point-max))
                          (newline)
                          (newline)
                          (lazy-config-insert proj-name config-alist)
                          (call-interactively 'eval-defun)))
                   (save-buffer)
                   (set-buffer-modified-p mod)))))

(cl-defun lazy-config-buffer (&optional (state :create) proj-name config-alist)
  (cl-case state
    (:create
     (let* ((proj-b (current-buffer))
            (buf (get-buffer-create "*lazy: new project*"))
            (window (display-buffer buf))
            (config-alist (or config-alist (lazy-guess-alist))))
       (select-window window)
       (set-window-dedicated-p window t)
       (emacs-lisp-mode)
       (buffer-disable-undo)
       (lazy-config-insert (or proj-name (cadr (assoc 'name config-alist)) "NewProject") config-alist t)
       (goto-char 0)
       (end-of-line)
       (lazy-backend-create-project-mode 'elisp)
       (buffer-enable-undo)))
    (:edit
     (let* ((marker (lazy-find-save-location-marker))
            (buf (make-indirect-buffer (marker-buffer marker) "*lazy: edit project*"))
            (window (display-buffer buf)))
       (select-window window)
       (set-window-start window (marker-position marker))
       (lisp-interaction-mode)
       (goto-char (marker-position marker))
       (lazy-config-save lazy-name (lazy-find-alist lazy-name nil))
       (set-window-dedicated-p window t)
       (lazy-backend-edit-project-mode 'elisp)
       (buffer-enable-undo)))
    (:finalize-create
     (let ((result nil))
       (while (not (setq result (condition-case nil (eval (read (buffer-string))) (error nil)))))
       (lazy-config-save (cadr (assoc 'name result)) result)
       (kill-buffer (buffer-name))))
    (:finalize-edit
     (let ((marker (lazy-find-save-location-marker)))
       (save-excursion
         (goto-char (marker-position marker))
         (let (edited-alist)
           (unless (eq (condition-case nil (setq edited-alist (call-interactively 'eval-defun)) (error 'error))
                       'error)
             (save-buffer)
             (kill-buffer)
             (when (and (not (condition-case nil (lazy-assert-proj) (error t)))
                        (string-equal (cadr (assoc 'name edited-alist)) lazy-name))
               (lazy-def lazy-name edited-alist)))))))))







(defvar lazy-backend-list (make-hash-table))

(cl-defun lazy-define-backend (backend &key buffer-fun save-fun insert-fun test-fun)
  (puthash backend `((buffer . ,buffer-fun)
                     (save . ,save-fun)
                     (insert . ,insert-fun)
                     (test . ,test-fun))
           lazy-backend-list))

(lazy-define-backend 'elisp
                        :buffer-fun 'lazy-config-buffer
                        :save-fun 'lazy-config-save
                        :insert-fun 'lazy-config-insert)

(defvar lazy-config-backend 'elisp)

(defun lazy-backend-funcall (backend sym &rest args)
  (apply (cdr (assoc sym (gethash backend lazy-backend-list))) args))

(cl-defun lazy-detect-backend (&optional proj-name config-alist)
  (if (and (condition-case nil (lazy-assert-proj) (error t))
           (not proj-name))
      (cl-return-from "lazy-detect-backend" lazy-config-backend)
    (unless proj-name
      (setq proj-name lazy-name))
    (unless config-alist
      (setq config-alist (lazy-find-alist proj-name t)))
    (maphash (lambda (k v)
               (unless (eq k 'elisp)
                 (when (funcall (cdr (assoc 'test v)) config-alist)
                   (cl-return-from "lazy-detect-backend" k)))) lazy-backend-list)
    'elisp))

(defun lazy-save ()
  (interactive)
  (lazy-assert-proj)
  (lazy-backend-funcall (lazy-detect-backend)
                           'save lazy-name (lazy-find-alist nil nil)))

(defun lazy-insert ()
  (interactive)
  (lazy-assert-proj)
  (cond ((derived-mode-p 'emacs-lisp-mode 'inferior-emacs-lisp-mode)
         (lazy-backend-funcall 'elisp
                                  'insert lazy-name (lazy-find-alist nil nil)))
        ((derived-mode-p 'org-mode)
         (lazy-backend-funcall 'orgmode
                                  'insert lazy-name (lazy-find-alist nil nil)))))

(cl-defun lazy-create ()
  (interactive)
  (if (and (gethash 'org-mode lazy-backend-list))
      (lazy-backend-funcall 'org-mode
                               'buffer :create)
    (lazy-backend-funcall (lazy-detect-backend)
                             'buffer :create)))

(defun lazy-edit (&optional proj-name)
  (interactive)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (lazy-backend-funcall (lazy-detect-backend proj-name)
                           'buffer :edit proj-name))




(defvar lazy-create-project-mode-map (make-sparse-keymap))

(defvar lazy-create-project-mode-hook nil)

(define-minor-mode lazy-create-project-mode nil nil " NewProject" lazy-create-project-mode-map
  (run-hooks 'lazy-create-project-mode-hook))

(defun lazy-backend-create-project-mode (backend)
  (lazy-create-project-mode)
  (setq lazy-create-project-mode backend))

(define-key lazy-create-project-mode-map "\C-c\C-c"
  (lambda ()
    (interactive)
    (lazy-backend-funcall lazy-create-project-mode
                             'buffer :finalize-create)))

(define-key lazy-create-project-mode-map "\C-c\C-k"
  (lambda ()
    (interactive)
    (kill-buffer (buffer-name))))


(defvar lazy-edit-project-mode-map (make-sparse-keymap))

(defvar lazy-edit-project-mode-hook nil)

(define-minor-mode lazy-edit-project-mode nil nil " EditProject" lazy-edit-project-mode-map
  (run-hooks 'lazy-edit-project-mode-hook))

(defun lazy-backend-edit-project-mode (backend)
  (lazy-edit-project-mode)
  (setq lazy-edit-project-mode backend))

(define-key lazy-edit-project-mode-map "\C-c\C-c"
  (lambda ()
    (interactive)
    (lazy-backend-funcall lazy-edit-project-mode
                             'buffer :finalize-edit)))

(define-key lazy-edit-project-mode-map "\C-c\C-k"
  (lambda ()
    (interactive)
    (kill-buffer (buffer-name))))












(defun lazy-undef (&optional proj-name)
  "Opposite of `lazy-def'."
  (interactive "sProject: ")
  (remhash proj-name lazy-list)
  (remhash proj-name lazy-completions-cache))

(defmacro lazy-with-current-project (proj-name &rest body)
  "Execute BODY with PROJ-NAME as current project. It just sets `lazy-name' to PROJ-NAME temporarily."
  `(let ((lazy-name ,proj-name))
     (condition-case nil ,@body (error nil))))

(defun lazy-check-required-vars (proj-name &optional proj-alist)
  "Go through all `lazy-required-vars' and check them in PROJ-NAME or PROJ-ALIST using the functions
defined in `lazy-required-vars'.

See also `lazy-optional-vars'"
  (unless proj-alist
    (setq proj-alist (lazy-find-alist proj-name)))
  (dolist (v lazy-required-vars)
    (let* ((config-symbol (car v))
           (config-value (cadr (assoc config-symbol proj-alist)))
           (config-checks (cdr v)))
      (if (not config-value)
          (error "Required config value '%s' missing in %s!" (symbol-name config-symbol) proj-name)
        (when (not (cl-every (lambda (check) (funcall check config-value)) config-checks))
          (error "Required config value '%s' has invalid value '%s' in %s!" (symbol-name config-symbol) (prin1-to-string config-value) proj-name))))))

(defun lazy-check-optional-vars (proj-name &optional proj-alist)
  "Go through all `lazy-optional-vars' and check them in PROJ-NAME or PROJ-ALIST using the functions
defined in `lazy-optional-vars'.

See also `lazy-required-vars'"
  (dolist (v lazy-optional-vars)
    (let* ((config-symbol (car v))
           (config-value (cadr (assoc config-symbol proj-alist)))
           (config-checks (cdr v)))
      (when (and config-value (not (cl-every (lambda (check) (funcall check config-value)) config-checks)))
        (error "Optional config value '%s' has invalid value '%s' in %s!" (symbol-name config-symbol) (prin1-to-string config-value) proj-name)))))

(cl-defun lazy-get-cache-file (symbol &optional proj-name (inherit t))
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let ((directory (concat lazy-global-cache-root
                           (cond ((lazy-get-config-val 'parent proj-name nil)
                                  (let ((a (concat "/" (lazy-join "/" (lazy-ancestry proj-name)))))
                                    (concat a "/")))
                                 (t
                                  (concat "/" proj-name "/")))))
        (file (concat (symbol-name symbol))))
    (make-directory directory t)
    (let ((r (concat directory file)))
      (cond ((file-exists-p r)
             r)
            ((and (lazy-get-config-val 'parent proj-name nil)
                  (file-exists-p (or (lazy-get-config-val symbol (lazy-get-config-val 'parent proj-name nil) nil)
                                     (lazy-get-cache-file symbol (lazy-get-config-val 'parent proj-name nil) t)))
                  (eq inherit 'copy))
             (progn
               (copy-file (or (lazy-get-config-val symbol (lazy-get-config-val 'parent proj-name nil) nil)
                              (lazy-get-cache-file symbol (lazy-get-config-val 'parent proj-name nil) t)) r)
               r))
            ((and (lazy-get-config-val 'parent proj-name nil)
                  (eq (lazy-get-config-val 'basedir proj-name nil) nil)
                  (eq inherit t))
             (or (lazy-get-config-val symbol (lazy-get-config-val 'parent proj-name nil) nil)
                 (lazy-get-cache-file symbol (lazy-get-config-val 'parent proj-name nil) t)))
            (t r)))))

(defun lazy-get-root-cache-dir (&optional dirname proj-name)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let ((cachedir (concat lazy-global-cache-root "/" (or (car-safe (lazy-ancestry proj-name)) proj-name) "/" dirname)))
    (unless (file-directory-p cachedir)
      (make-directory cachedir t))
    (expand-file-name cachedir)))

(defun lazy-ancestry (&optional proj-name)
  (let* ((current (or proj-name
                      (progn
                        (lazy-assert-proj)
                        lazy-name)))
         (ancestry `(,current)))
    (while (lazy-config-val 'parent current)
      (setq ancestry (cons (lazy-config-val 'parent current) ancestry)
            current (lazy-config-val 'parent current)))
    ancestry))

(defvar lazy-prevent-after-save-update nil)

(defun lazy-load-project (proj-name)
  "Load PROJ-NAME configuration. This is the main loading function that does all the work, it
is supposed to be called from `lazy-load'.

See also `lazy-load', `lazy-unload', `lazy-fib-init', `lazy-visit-saved-open-files',
`lazy-visit-saved-open-friends', `lazy-before-load-hook', `lazy-after-load-hook'"
  (let* ((oldname lazy-name)
         (proj-alist (lazy-find-alist proj-name nil))
         (quiet (and (cadr (assoc 'parent proj-alist))
                     (or (string-equal (cadr (assoc 'parent proj-alist))
                                       lazy-name)
                         (and (not (condition-case nil (lazy-assert-proj) (error t)))
                              (string-equal (cadr (assoc 'parent proj-alist))
                                            (lazy-get-config-val 'parent nil nil))))))
         (lazy-prevent-after-save-update t))
    (unless proj-name
      (error "lazy-load: proj-name is nil"))
    (unless (or (string= oldname proj-name)
                (eq proj-alist nil))
      (lazy-unload))
    (let ((lazy-name proj-name))
      (run-hooks 'lazy-before-load-hook))
    (if (not  proj-alist)
        (error "Project %s does not exist!" proj-name)
      (lazy-check-required-vars proj-name proj-alist)
      (lazy-check-optional-vars proj-name proj-alist))
    (setq lazy-name proj-name)
    (while (not (file-directory-p (lazy-get-config-val 'basedir)))
      (lazy-set-config-val 'basedir (read-string "Missing base directory? : " (lazy-get-config-val 'basedir))))
    (when (and (lazy-get-config-val 'vcs) (not (lazy-get-vcs-path)))
      (error "Invalid VCS setting!"))
    (message "Loading project %s ..." proj-name)
    (lazy-fib-init)
    (add-hook 'kill-emacs-hook 'lazy-kill-emacs-hook)
    (run-hooks 'lazy-before-files-load-hook)
    (lazy-visit-saved-open-files)
    (lazy-visit-saved-open-friends)
    (modify-frame-parameters (selected-frame) (list (cons 'name proj-name)))
    (run-hooks 'lazy-after-load-hook)
    (when (lazy-get-config-val 'startup-hook)
      (let ((startup-hook (lazy-get-config-val 'startup-hook)))
        (cond ((functionp startup-hook)
               (progn (message "funcall startup-hook...") (funcall startup-hook)))
              ((commandp startup-hook)
               (progn (message "call-interactively startup-hook...") (call-interactively startup-hook)))
              ((and (listp startup-hook)
                    (symbolp (car startup-hook)))
               (progn (message "eval startup-hook...") (eval startup-hook))))))
    (lazy-update-tags proj-name)
    (message "Loading project %s done" proj-name)))

(defun lazy-load (&optional proj-name)
  "Load PROJ-NAME or ask the user about which project to load.

See also `lazy-load-project'"
  (interactive)
  (let* ((guessed-alist (lazy-guess-alist))
         (names (let ((ns (lazy-names)))
                  (when (cadr (assoc 'name guessed-alist))
                    (add-to-list 'ns (cadr (assoc 'name guessed-alist))))
                  ns))
         (name (or proj-name
                   (if (lazy-use-ido)
                       (ido-completing-read "Project Name (ido): " names nil nil nil nil (cadr (assoc 'name guessed-alist)))
                     (completing-read "Project Name: " names)))))
    (when (and (cadr (assoc 'name guessed-alist))
               (string-equal name (cadr (assoc 'name guessed-alist)))
               (not (lazy-find-alist name nil)))
      (lazy-def name guessed-alist))
    (when (not (lazy-find-alist name nil))
      (add-to-list 'guessed-alist `(name ,name))
      (lazy-def name guessed-alist))
    (lazy-load-project name)))

(defun lazy-kill-emacs-hook ()
  "Ensure we save the open-files-cache info on emacs exit"
  (when (and lazy-name
             (lazy-get-config-val 'open-files-cache))
    (lazy-save-open-file-info))
  (when (and lazy-name
             (lazy-get-config-val 'friends)
             (lazy-get-config-val 'open-friends-cache))
    (lazy-save-open-friends-info))
  (lazy-unload t))

(defun lazy-unload (&optional quiet)
  "Unload the current project configuration after running the shutdown hook.

See also `lazy-close-files', `lazy-close-friends', `lazy-history'
`lazy-before-files-unload-hook', `lazy-before-unload-hook', `lazy-after-unload-hook'"
  (interactive "P")
  (let ((lazy-prevent-after-save-update t)
        (close-files (when lazy-name (y-or-n-p (concat "Close all '" lazy-name "' project files? ")))))
    (when lazy-name
      (condition-case nil
          (progn
            (unless quiet (message "Unloading project %s" lazy-name))
            (run-hooks 'lazy-before-unload-hook)
            (lazy-maybe-kill-buffer (lazy-fib-name))
            (lazy-save-open-friends-info)
            (lazy-save-open-file-info)
            (run-hooks 'lazy-before-files-unload-hook)
            (and (or (lazy-buffers) (lazy-friendly-buffers))
                 (not quiet)
                 close-files
                 (lazy-close-friends)
                 (lazy-close-files))
            (when (lazy-get-config-val 'shutdown-hook)
              (if (functionp (lazy-get-config-val 'shutdown-hook))
                  (funcall (lazy-get-config-val 'shutdown-hook))
                (mapc 'funcall (lazy-get-config-val 'shutdown-hook))))
            (run-hooks 'lazy-after-unload-hook))
        (error nil)))
    (add-to-list 'lazy-history lazy-name)
    (setq lazy-name nil)
    (modify-frame-parameters (selected-frame) (list (cons 'name "Emacs")))
    (setq compile-command nil)
    (unless quiet (message "Project settings have been cleared"))))

(defun lazy-close-files ()
  "Close all unmodified files that reside in the project's basedir"
  (interactive)
  (lazy-assert-proj)
  (let ((closed nil)
        (dirty nil)
        (zeitgeist-prevent-send t)
        (continue-prevent-save t))
    (dolist (b (append (lazy-file-buffers) (lazy-dired-buffers)))
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

(defun lazy-buffer-name (buf)
  "Return buffer's name based on filename or dired's location"
  (let ((file-name (or (buffer-file-name (or (buffer-base-buffer buf) buf))
                       (with-current-buffer (or (buffer-base-buffer buf) buf) list-buffers-directory))))
    (if file-name
        (expand-file-name file-name)
      nil)))

(defun lazy-buffer-p (buf &optional proj-name proj-alist)
  "Is the given buffer in our project, is a file opened? Also detects dired buffers open to basedir/*"
  (setq proj-alist (or proj-alist
                       (lazy-find-alist proj-name)
                       (lazy-find-alist lazy-name)
                       (lazy-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (lazy-assert-proj))
  (let ((file-name (lazy-buffer-name buf))
        (basedir (file-name-as-directory (lazy-get-config-val 'basedir proj-name t proj-alist)))
        (case-fold-search nil))
    (if (and (stringp file-name)
             (file-exists-p file-name)
             (lazy-get-config-val 'basedir proj-name t proj-alist)
             (cl-loop for pattern in (lazy-get-config-val 'src-patterns proj-name t proj-alist)
                      if (string-match (if (lazy-get-config-val 'patterns-are-regex proj-name t proj-alist)
                                           pattern
                                         (regexp-quote pattern)) file-name)
                      return t
                      finally return nil)
             (or (string-match (concat "^" (regexp-quote basedir)) file-name)
                 (string-match (concat "^" (regexp-quote (file-truename basedir))) file-name)))
        proj-name
      nil)))

(defun lazy-file-buffer-p (buf &optional proj-name)
  (and (buffer-file-name buf)
       (lazy-buffer-p buf proj-name)))

(defun lazy-special-buffer-p (buf &optional proj-name)
  (let ((case-fold-search nil))
    (and (string-match "\*[^\*]\*" (buffer-name buf))
         (lazy-buffer-p buf proj-name))))

(defun lazy-dired-buffer-p (buf &optional proj-name)
  (and (with-current-buffer buf
         (eq major-mode 'dired-mode))
       (lazy-buffer-p buf proj-name)))

(defun lazy-buffers (&optional proj-name)
  "Get a list of buffers that reside in this project's basedir"
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let ((buffers nil))
    (dolist (b (buffer-list))
      (when (lazy-buffer-p b proj-name) (push b buffers)))
    buffers))

(defun lazy-file-buffers (&optional proj-name)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (cl-remove-if (lambda (buf) (not (buffer-file-name buf))) (lazy-buffers proj-name)))

(defun lazy-special-buffers (&optional proj-name)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let ((case-fold-search nil))
    (append (cl-remove-if (lambda (buf) (not (string-match "\*[^\*]\*" (buffer-name buf)))) (lazy-buffers proj-name))
            (cl-remove-if (lambda (buf) (or (and (symbolp 'lazy-org-project-buffer-name)
                                                 (not (string-equal (lazy-org-project-buffer-name proj-name) (buffer-name buf))))
                                            (compilation-buffer-p buf)))
                       (buffer-list)))))

(defun lazy-dired-buffers (&optional proj-name)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (cl-remove-if (lambda (buf) (not (lazy-dired-buffer-p buf))) (lazy-buffers proj-name)))

(defun lazy-status (&optional proj-name)
  "View project's variables."
  (interactive)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (if (lazy-get-config-val 'basedir proj-name t)
      (let ((b (get-buffer-create "*lazy: status*")))
        (with-current-buffer b
          (kill-region (point-min) (point-max))
          (dolist (v (append lazy-required-vars lazy-optional-vars))
            (insert (format "%-32s = %s\n" (symbol-name (car v)) (lazy-get-config-val (car v) proj-name t)))))
        (when (not (eq b (current-buffer)))
          (display-buffer b)))
    (message "No project loaded.")))

;; ---------------------------------------------------------------------
;; Save/Restore open files
;; ---------------------------------------------------------------------

(defun lazy-save-open-file-info ()
  "Write the list of `files' to a file"
  (when (lazy-get-config-val 'open-files-cache)
    (with-temp-buffer
      (dolist (f (cl-remove-duplicates (mapcar (lambda (b) (lazy-buffer-name b)) (lazy-buffers)) :test 'string-equal))
        (when f
          (unless (string-equal (lazy-get-config-val 'etags-file) f)
            (insert f "\n"))))
      (if (file-writable-p (lazy-get-config-val 'open-files-cache))
          (progn
            (write-region (point-min)
                          (point-max)
                          (lazy-get-config-val 'open-files-cache))
            (message "Wrote open files to %s" (lazy-get-config-val 'open-files-cache)))
        (message "Cannot write to %s" (lazy-get-config-val 'open-files-cache))))))

(defun lazy-visit-saved-open-files ()
  (let ((zeitgeist-prevent-send t))
    (when (lazy-get-config-val 'open-files-cache)
      (when (file-readable-p (lazy-get-config-val 'open-files-cache))
        (message "Reading open files from %s" (lazy-get-config-val 'open-files-cache))
        (with-temp-buffer
          (insert-file-contents (lazy-get-config-val 'open-files-cache))
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

(defadvice switch-to-buffer (after lazy-switch-to-buffer-set-auto-mode)
  (when (eq major-mode 'fundamental-mode)
    (when (and (eq system-type 'windows-nt)
               (not (eq (default-value 'buffer-file-coding-system) buffer-file-coding-system)))
      (recode-region (point-min) (point-max) (default-value 'buffer-file-coding-system) buffer-file-coding-system)
      (when (fboundp 'lazy-sourcemarker-restore)
        (lazy-sourcemarker-restore))
      (set-buffer-modified-p nil))
    (set-auto-mode)))
(ad-activate 'switch-to-buffer)
;;(ad-unadvise 'switch-to-buffer)

(defadvice pop-to-buffer (after lazy-pop-to-buffer-set-auto-mode)
  (when (eq major-mode 'fundamental-mode)
    (when (and (eq system-type 'windows-nt)
               (not (eq (default-value 'buffer-file-coding-system) buffer-file-coding-system)))
      (recode-region (point-min) (point-max) (default-value 'buffer-file-coding-system) buffer-file-coding-system)
      (when (fboundp 'lazy-sourcemarker-restore)
        (lazy-sourcemarker-restore))
      (set-buffer-modified-p nil))
    (set-auto-mode)))
(ad-activate 'pop-to-buffer)
;;(ad-unadvise 'pop-to-buffer)

(defadvice display-buffer (after lazy-display-buffer-set-auto-mode)
  (with-current-buffer (ad-get-arg 0)
    (when (eq major-mode 'fundamental-mode)
      (when (and (eq system-type 'windows-nt)
                 (not (eq (default-value 'buffer-file-coding-system) buffer-file-coding-system)))
        (recode-region (point-min) (point-max) (default-value 'buffer-file-coding-system) buffer-file-coding-system)
        (when (fboundp 'lazy-sourcemarker-restore)
          (lazy-sourcemarker-restore))
        (set-buffer-modified-p nil))
      (set-auto-mode))))
(ad-activate 'display-buffer)

(defvar lazy-suppress-y-or-n-p-patterns (list "Do you want to revisit the file normally now"
                                                 "Do you want to revisit the file literally now"))

(defadvice y-or-n-p (around lazy-y-or-n-p)
  (let ((prompt (ad-get-arg 0))
        (suppress nil))
    (loop for pattern in lazy-suppress-y-or-n-p-patterns
          until (setq suppress (string-match pattern prompt)))
    (setq ad-return-value (or (and suppress t) ad-do-it))))
(ad-activate 'y-or-n-p)
;;(ad-unadvise 'y-or-n-p)

;; ---------------------------------------------------------------------
;; Tagging
;; ---------------------------------------------------------------------

(defvar lazy-default-gtags-config (expand-file-name "gtags.conf" (file-name-directory (or buffer-file-name load-file-name))))
(defvar lazy-after-save-update-in-progress nil)
(defvar lazy-after-save-line-numbers (make-hash-table))
(defvar lazy-after-save-current-buffer nil)
(defvar lazy-after-save-current-project nil)

(defun lazy-update-tags (&optional proj-name proj-alist files debug)
  "Create or update the projects TAG database."
  (interactive)
  (setq proj-alist (or proj-alist
                       (lazy-find-alist proj-name)
                       (lazy-find-alist lazy-name)
                       (lazy-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (lazy-assert-proj))
  (unless files
    (setq files (lazy-unique-files proj-name)))
  (let ((default-directory (lazy-get-config-val 'basedir proj-name nil proj-alist))
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
      (let ((lang (car-safe (lazy-src-pattern-languages (list f)))))
        (push lang languages)
        (dolist (sys (cdr (assoc lang lazy-language-tag-systems)))
          (cond ((and (or (eq sys 'gtags+rtags))
                      gtags-executable
                      global-executable
                      rtags-executable)
                 (cl-return (puthash 'gtags+rtags
                                  (append (list f) (gethash 'gtags+rtags sys-files))
                                  sys-files)))
                ((and (eq sys 'rtags)
                      rtags-executable)
                 (cl-return (puthash 'rtags
                                  (append (list f) (gethash 'rtags sys-files))
                                  sys-files)))
                ((and (eq sys 'gtags)
                      gtags-executable
                      global-executable)
                 (cl-return (puthash 'gtags
                                  (append (list f) (gethash 'gtags sys-files))
                                  sys-files)))
                ((and (eq sys 'gtags+exuberant-ctags)
                      gtags-executable
                      global-executable
                      ctags-exuberant-executable)
                 ;; - I had a fallback to gtags here if exuberant is not found, but removed it,
                 ;; it does not make sense, while exuberant can parse many languages, gtags alone
                 ;; can only parse a few
                 (cl-return (puthash 'gtags+exuberant-ctags
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
           ;; - the database is stored in the dbpath, that should be ~/.lazy/project
           (gtags-dbpath (file-name-as-directory (file-truename (lazy-get-root-cache-dir nil proj-name))))
           (gtags-config (or (let ((c (lazy-get-config-val 'gtags-config proj-name nil proj-alist)))
                               (when (and c (> (length c) 0) (file-exists-p c))
                                 c))
                             (let ((c (concat (lazy-get-config-val 'basedir proj-name nil proj-alist) "/.globalrc")))
                               (when (file-exists-p c)
                                 c))
                             (when (and lazy-c++-gtags-config
                                        (cl-find 'cpp languages)
                                        (file-exists-p (expand-file-name lazy-c++-gtags-config)))
                               (expand-file-name lazy-c++-gtags-config))
                             (when (and lazy-default-gtags-config
                                        (file-exists-p (expand-file-name lazy-default-gtags-config)))
                               (expand-file-name lazy-default-gtags-config))
                             (let ((c (expand-file-name "~/.globalrc")))
                               (when (file-exists-p c)
                                 c))
                             nil))
           (gtags-arguments (or (lazy-get-config-val 'gtags-arguments proj-name nil proj-alist)
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
                         (when gtags-config (concat "GTAGSCONF=" gtags-config " "))
                         "gtags " gtags-dbpath " -i -v -f - " gtags-arguments cmd-seperator)
                 gtags-commands))
      (when (gethash 'gtags+exuberant-ctags sys-files)
        (puthash 'gtags+exuberant-ctags
                 (concat "cd " gtags-root cmd-seperator
                         "env GTAGSLABEL=exuberant-ctags "
                         (when gtags-config (concat "GTAGSCONF=" gtags-config " "))
                         "GTAGSROOT=" gtags-root " "
                         "gtags " gtags-dbpath " -i -v -f - " gtags-arguments cmd-seperator)
                 gtags-commands))
      (let* ((ordering (list 'gtags+exuberant-ctags 'gtags))
             (commands (cl-loop for sys in ordering
                                if (gethash sys gtags-commands)
                                collect (gethash sys gtags-commands)))
             (inputs (cl-loop for sys in ordering
                              if (gethash sys gtags-commands)
                              collect (concat (mapconcat #'identity (gethash sys sys-files) "\n") "\n"))))
        (lazy-process-group "gtags" commands inputs 'lazy-update-completions-cache (list proj-name) nil)
        ))
    ;; - if I'd ever implement rtags, or any other system really, then this would become another section
    (when (gethash 'rtags sys-files)
      (message "rtags not implemented yet")))
  ;; - after updating the tag databases we may as well setup the env variables again, just to be sure
  (lazy-setup-tags proj-name))

(defun lazy-process-group (name commands inputs &optional terminator terminator-args debug n process event)
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
        (set-process-sentinel process (apply-partially 'lazy-process-group name commands inputs terminator terminator-args debug (1+ n)))
        (when input
          (process-send-string process input))
        (process-send-eof process))
    (when terminator
      (apply terminator terminator-args))))

(defun lazy-src-pattern-tag-systems (src-patterns)
  "Takes a list of src patterns (like \"*.cpp\") and returns tag systems that can parse those."
  (let ((systems '()))
    (dolist (lang (lazy-src-pattern-languages src-patterns))
      (dolist (sys (cdr (assoc lang lazy-language-tag-systems)))
        (add-to-list 'systems sys)))
    systems))

(defun lazy-setup-tags (&optional proj-name)
  "Setup environment for existing TAG database."
  (interactive)
  (setq proj-name (or proj-name
                      lazy-name
                      (cadr (assoc 'name (lazy-guess-alist)))))
  (unless proj-name
    (lazy-assert-proj))
  (let ((proj-systems (lazy-src-pattern-tag-systems (lazy-get-config-val 'src-patterns proj-name)))
        (available-systems '()))
    (when (or (cl-find 'gtags proj-systems)
              (cl-find 'gtags+rtags proj-systems)
              (cl-find 'gtags+exuberant-ctags proj-systems))
      (let* ((gtags-file (concat (file-truename (concat (lazy-get-root-cache-dir nil proj-name) "GTAGS"))))
             (gtags-file-alternative (concat (lazy-get-config-val 'basedir proj-name) "GTAGS")))
        (cond ((file-exists-p gtags-file)
               (let ((gtags-dbpath (lazy-dirname gtags-file))
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

(defadvice ido-switch-buffer (around lazy-ido-switch-buffer-setup-tags first nil activate)
  (let ((previous-buffer (current-buffer)))
    ad-do-it
    (when (and (not lazy-name)
               (buffer-file-name (current-buffer))
               (not (eq (current-buffer) previous-buffer)))
      (lazy-setup-tags))))

(defadvice switch-buffer (around lazy-switch-buffer-setup-tags first nil activate)
  (let ((previous-buffer (current-buffer)))
    ad-do-it
    (when (and (not lazy-name)
               (buffer-file-name (current-buffer))
               (not (eq (current-buffer) previous-buffer)))
      (lazy-setup-tags))))

(defun lazy-find-symbol-elisp-location-helper (symbol)
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

;; - given a system and a regexp, this tries to match regexp with jumps aquired from system and returns
;; all matching jumps
;; - so this can be used to find all jumps for only part of a symbol, eg finding all jumps to defintions
;; starting with lazy-find-.* would be possible with this function
;; - this function is also used to find symbols names if we already know the whole name, check below
;; how it is used in lazy-jump-definition and lazy-jump-regexp
(defun lazy-find-symbol (proj-name proj-alist system regexp &rest args)
  (setq proj-alist (or proj-alist
                       (lazy-find-alist proj-name)
                       (lazy-find-alist lazy-name)
                       (lazy-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (lazy-assert-proj))
  (let* ((basedir (or (when proj-alist (cadr (assoc 'basedir proj-alist)))
                      (when proj-name (lazy-get-config-val 'basedir proj-name))
                      default-directory))
         (default-directory basedir)
         (case-fold-search nil)
         (cmd-sep (if (eq system-type 'windows-nt) " & " " ; ")))
    (cond ((eq 'gtags system)
           (let ((cmd (nth 0 args)))
             (lazy-setup-tags proj-name)
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
             ;; - only try to find the current symbol in the obarray when the current major-mode is a emacs-lisp-mode,
             ;; or the current project contains any elisp files
             (when (or (eq major-mode 'emacs-lisp-mode) (eq major-mode 'lisp-interaction-mode)
                       (and proj-name (cl-position 'elisp (lazy-src-pattern-languages (cadr (assoc 'src-patterns (lazy-find-alist proj-name)))))))
               (cl-do-all-symbols (sym)
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
                            (locator (lazy-find-symbol-elisp-location-helper sym)))
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
             (cl-loop for item in marker-list
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

(defun lazy-score-jumps (jumps regexp buffer)
  (let ((file-map (make-hash-table :test 'equal))
        (file-score-map (make-hash-table))
        (buffer-languages (lazy-src-pattern-languages (when (buffer-file-name buffer) (list (buffer-file-name buffer)))))
        (buffer-path (file-truename (buffer-file-name buffer)))
        (basedir-path (file-truename (or (condition-case nil (lazy-get-config-val 'basedir) (error nil))
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
                     (lazy-path-equal jump-path buffer-path))
            (setq score (+ score inc)))
          (when (and jump-path
                     lazy-name
                     (lazy-path-equal jump-path basedir-path))
            (setq score (+ score inc)))
          (when (and jump-path
                     (boundp 'ido-buffer-history)
                     (find-buffer-visiting jump-path))
            (let ((buffer-position (or (gethash jump-path buffer-position-cache)
                                       (puthash jump-path (condition-case nil (cl-position (buffer-name (find-buffer-visiting jump-path))
                                                                                        ido-buffer-history
                                                                                        :test 'equal)
                                                            (error nil)) buffer-position-cache)
                                       (length ido-buffer-history))))
              (setq score (+ score (- (length ido-buffer-history)
                                      buffer-position)))))
          (when (and jump-path
                     buffer-languages)
            (dolist (lang buffer-languages)
              (when (cl-find lang (lazy-src-pattern-languages (list jump-path)))
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

(defun lazy-compare-jumps (a b)
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

(defun lazy-sort-jumps (jump-list)
  (message "sort-jumps")
  (sort jump-list 'lazy-compare-jumps))

(defconst lazy-jump-buffer "*lazy: jumps*")

(defvar lazy-auto-jump-to-first-jump nil)

(defun lazy-select-jumps (jump-list &optional invoke-window)
  (unless invoke-window
    (setq invoke-window (get-buffer-window (current-buffer))))
  (let ((n (length jump-list)))
    (when (> n 0)
      (ring-insert xref--marker-ring (point-marker))
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
                  (lazy-jump-highlight (cl-find location-buffer previous-buffer-list)
                                          (lazy-jump invoke-window full-path line-number word)))
              (lazy-jump-highlight (find-buffer-visiting full-path)
                                      (lazy-jump invoke-window full-path line-number word))))
        (when (get-buffer-window lazy-jump-buffer)
          (delete-window (get-buffer-window lazy-jump-buffer)))
        (when (get-buffer lazy-jump-buffer)
          (kill-buffer lazy-jump-buffer))
        (with-current-buffer (get-buffer-create lazy-jump-buffer)
          (buffer-disable-undo (current-buffer))
          (lazy-jump-list-mode)
          (setq tabulated-list-entries nil)
          (let ((id 0))
            (dolist (jump (lazy-sort-jumps jump-list))
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
                                             'action 'lazy-jump-action)
                                       (list (if (and file-path line-number) (concat (file-relative-name full-path (file-truename default-directory))
                                                                                     ":"
                                                                                     (format "%d" line-number))  "obarray")
                                             'mouse-face 'highlight
                                             'face 'compilation-info
                                             'action 'lazy-jump-action)
                                       (list (format "% 5d" line-score)
                                             'mouse-face 'highlight
                                             'face 'compilation-error
                                             'action 'lazy-jump-action)
                                       (list (prin1-to-string system)
                                             'mouse-face 'highlight
                                             'face 'compilation-info-face
                                             'action 'lazy-jump-action)
                                       (list (or docstring definition "")
                                             'mouse-face 'highlight
                                             'face 'compilation-face
                                             'action 'lazy-jump-action)
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
          (when lazy-auto-jump-to-first-jump
            (lazy-jump-action)))
        (display-buffer lazy-jump-buffer)
        ))))

(defun lazy-jump (invoke-window full-path line-number &optional word)
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
    (select-window (or (get-buffer-window lazy-jump-buffer) invoke-window))
    marker))

(defun lazy-jump-action (&optional button)
  (interactive)
  (unless button
    (setq button (point-marker)))
  (when (get-buffer lazy-jump-buffer)
    (with-current-buffer (get-buffer lazy-jump-buffer)
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
              (lazy-jump-highlight (cl-find location-buffer previous-buffer-list)
                                      (lazy-jump invoke-window full-path line-number word)))
          (lazy-jump-highlight (find-buffer-visiting full-path)
                                  (lazy-jump invoke-window full-path line-number word)))))))

(defvar lazy-jump-overlays nil)

(defun lazy-jump-cleanup-highlight (&optional existing-buffer)
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
          lazy-jump-overlays)
    (setq lazy-jump-overlays nil)
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
(defun lazy-jump-highlight (&optional existing-buffer marker final)
  (let ((delete-buffer (lazy-jump-cleanup-highlight existing-buffer))
        (highlight-color (color-darken-name (face-attribute 'default :background) 10)))
    (when (get-buffer lazy-jump-buffer)
      (with-current-buffer (get-buffer lazy-jump-buffer)
        (let ((ov (make-overlay (point-at-bol) (point-at-bol 2))))
          (overlay-put ov 'face `((:background ,highlight-color)))
          (overlay-put ov 'jump-highlight 'select)
          (push ov lazy-jump-overlays))))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char (marker-position marker))
          (let ((ov (make-overlay (point-at-bol) (point-at-bol 2))))
            (when (or (not existing-buffer) delete-buffer)
              (overlay-put ov 'delete-buffer (or delete-buffer (current-buffer))))
            (overlay-put ov 'pop-tag-marker (ring-ref xref--marker-ring 0))
            (overlay-put ov 'face `((:background ,highlight-color)))
            (overlay-put ov 'jump-highlight 'view)
            (push ov lazy-jump-overlays)))))))

(defun lazy-jump-next ()
  (interactive)
  (let ((last-window (get-buffer-window (current-buffer))))
    (when (get-buffer lazy-jump-buffer)
      (select-window (get-buffer-window (get-buffer lazy-jump-buffer)))
      (with-current-buffer (get-buffer lazy-jump-buffer)
        (when (< (point-at-eol 2) (point-max))
          (when (overlays-at (point))
            (forward-button (length tabulated-list-format)))
          (lazy-jump-action))))
    (select-window last-window)))

(defun lazy-jump-prev ()
  (interactive)
  (let ((last-window (get-buffer-window (current-buffer))))
    (when (get-buffer lazy-jump-buffer)
      (select-window (get-buffer-window (get-buffer lazy-jump-buffer)))
      (with-current-buffer (get-buffer lazy-jump-buffer)
        (when (> (point-at-bol) (point-min))
          (when (overlays-at (point))
            (backward-button (length tabulated-list-format)))
          (lazy-jump-action))))
    (select-window last-window)))

(defun lazy-jump-abort ()
  (interactive)
  (when (get-buffer-window lazy-jump-buffer)
    (delete-window (get-buffer-window lazy-jump-buffer)))
  (when (get-buffer lazy-jump-buffer)
    (kill-buffer lazy-jump-buffer))
  (pop-tag-mark)
  (lazy-jump-cleanup-highlight))

(defun lazy-jump-quit ()
  (interactive)
  (when (get-buffer-window lazy-jump-buffer)
    (delete-window (get-buffer-window lazy-jump-buffer)))
  (when (get-buffer lazy-jump-buffer)
    (kill-buffer lazy-jump-buffer))
  (lazy-jump-cleanup-highlight (current-buffer)))

(defun lazy-jump-go ()
  (interactive)
  (lazy-jump-action)
  (lazy-jump-quit))

(define-derived-mode lazy-jump-list-mode tabulated-list-mode "Mk-Project jumps"
  (setq tabulated-list-format [("Word" 30 t)
                               ("File" 70 t)
                               ("Score" 5 t)
                               ("Sys" 7 t)
                               ("Text" 0 nil)]
        tabulated-list-padding 1
        tabulated-list-sort-key nil)
  (define-key lazy-jump-list-mode-map (kbd "q") 'lazy-jump-quit)
  (define-key lazy-jump-list-mode-map (kbd "C-g") 'lazy-jump-abort)
  (define-key lazy-jump-list-mode-map (kbd "C-n") 'lazy-jump-next)
  (define-key lazy-jump-list-mode-map (kbd "n") 'lazy-jump-next)
  (define-key lazy-jump-list-mode-map (kbd "<down>") 'lazy-jump-next)
  (define-key lazy-jump-list-mode-map (kbd "C-p") 'lazy-jump-prev)
  (define-key lazy-jump-list-mode-map (kbd "p") 'lazy-jump-prev)
  (define-key lazy-jump-list-mode-map (kbd "<up>") 'lazy-jump-prev)
  (define-key lazy-jump-list-mode-map (kbd "<return>") 'lazy-jump-go)
  (tabulated-list-init-header))

(defun lazy-update-gtags-completions-cache (proj-name)
  (let* ((cmd (concat "global --match-part=first -Gq -c \"\""))
         (completions (split-string (condition-case nil (shell-command-to-string cmd) (error nil)) "\n" t))
         (completions-cache (gethash proj-name lazy-completions-cache)))
    (when completions
      (cl-loop for completion in completions
               do (puthash completion
                           nil
                           completions-cache)))))

(defun lazy-update-obarray-completions-cache (proj-name)
  (let ((completions-cache (gethash proj-name lazy-completions-cache)))
    (cl-do-all-symbols (sym)
      (when (or (fboundp sym)
                (boundp sym))
        (let* ((completion (symbol-name sym)))
          (puthash completion
                   nil
                   completions-cache))))))

(defun lazy-update-completions-cache (&optional proj-name)
  (let* ((guessed-alist (lazy-guess-alist))
         (guessed-name (cadr (assoc 'name guessed-alist)))
         (proj-alist (lazy-find-alist proj-name)))
    (cond ((and (not proj-name)
                lazy-name
                (lazy-buffer-p (current-buffer) lazy-name))
           (setq proj-name lazy-name
                 proj-alist (lazy-find-alist lazy-name)))
          ((or (and proj-name
                    (not proj-alist)
                    guessed-alist)
               (and (not proj-name)
                    guessed-name))
           (setq proj-name guessed-name
                 proj-alist guessed-alist)))
    (unless proj-name
      (lazy-assert-proj))
    (if (not (hash-table-p (gethash proj-name lazy-completions-cache)))
        (puthash proj-name (make-hash-table :test 'equal :size 100000) lazy-completions-cache)
      (maphash (lambda (k v)
                 (remhash k (gethash proj-name lazy-completions-cache)))
               (gethash proj-name lazy-completions-cache)))
    (unless (string-equal lazy-name proj-name)
      (lazy-setup-tags proj-name))
    (lazy-update-gtags-completions-cache proj-name)
    (unless (string-equal lazy-name proj-name)
      (lazy-setup-tags lazy-name))
    ;; (lazy-update-imenu-completions-cache proj-name)
    (when (cl-find 'elisp (lazy-src-pattern-languages (lazy-get-config-val 'src-patterns proj-name nil proj-alist)))
      (lazy-update-obarray-completions-cache proj-name))
    (garbage-collect)))

(defun lazy-completions (&optional prefix proj-name buffer)
  (let* ((guessed-alist (lazy-guess-alist))
         (guessed-name (cadr (assoc 'name guessed-alist)))
         (proj-alist nil))
    (cond ((and (not proj-name)
                lazy-name
                (lazy-buffer-p (current-buffer) lazy-name))
           (setq proj-name lazy-name
                 proj-alist (lazy-find-alist lazy-name)))
          ((and (not proj-name)
                guessed-name)
           (setq proj-name guessed-name
                 proj-alist guessed-alist)))
    (unless proj-name
      (lazy-assert-proj))
    (unless prefix
      (setq prefix ""))
    (unless buffer
      (setq buffer (current-buffer)))
    (let ((unique-completions '())
          (case-fold-search nil))
      (when (not (gethash proj-name lazy-completions-cache))
        (lazy-update-completions-cache proj-name))
      (when (hash-table-p (gethash proj-name lazy-completions-cache))
        (maphash (lambda (k v)
                   (when (or (string-equal prefix "")
                             (string-match (concat "^" prefix) k))
                     (push k unique-completions)))
                 (gethash proj-name lazy-completions-cache)))
      (reverse unique-completions))))

(defun lazy-merge-obarray-jumps (obarray-jumps &rest rest)
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
                (when (cl-position 'elisp (lazy-src-pattern-languages (list jump-path)))
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

(defun lazy-jump-definition (word &optional proj-name proj-alist buffer)
  (interactive (list (let* ((ido-enable-flex-matching t)
                            (case-fold-search nil)
                            (ido-case-fold nil))
                       (substring-no-properties (ido-completing-read "Symbol: "
                                                                     (lazy-completions) nil nil
                                                                     (substring-no-properties (or (thing-at-point lazy-thing-selector) "")))))))
  (when (and (not lazy-name) (not proj-alist))
    (let ((guessed-name (cadr (assoc 'name (lazy-guess-alist)))))
      (when guessed-name (setq proj-name guessed-name))))
  (setq proj-alist (or proj-alist
                       (lazy-find-alist proj-name)
                       (lazy-find-alist lazy-name)
                       (lazy-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (lazy-assert-proj))
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((jumps (lazy-merge-obarray-jumps (lazy-find-symbol proj-name proj-alist 'obarray (concat "^" word "$"))
                                            (or (lazy-find-symbol proj-name proj-alist 'gtags word (concat "global -x -d " (prin1-to-string word)))
                                                (lazy-find-symbol proj-name proj-alist 'gtags word (concat "global -x -s " (prin1-to-string word))))
                                            (lazy-find-symbol proj-name proj-alist 'imenu (concat "^" word "$")))))
    (lazy-select-jumps (lazy-score-jumps jumps (regexp-quote word) buffer))))

(defun lazy-jump-regexp (regexp &optional proj-name proj-alist buffer)
  (interactive (list (let* ((ido-enable-flex-matching t))
                       (substring-no-properties (ido-completing-read "Match: "
                                                                     (lazy-completions))))))
  (when (and lazy-name (not proj-alist))
    (let ((guessed-name (cadr (assoc 'name (lazy-guess-alist)))))
      (when guessed-name (setq proj-name guessed-name))))
  (setq proj-alist (or proj-alist
                       (lazy-find-alist proj-name)
                       (lazy-find-alist lazy-name)
                       (lazy-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (lazy-assert-proj))
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((jumps (lazy-merge-obarray-jumps (lazy-find-symbol proj-name proj-alist 'obarray (concat "^" regexp))
                                            (or (lazy-find-symbol proj-name proj-alist 'gtags regexp (concat "global -x -e " (prin1-to-string (concat regexp ".*"))))
                                                (lazy-find-symbol proj-name proj-alist 'gtags regexp (concat "global -x -s " (prin1-to-string (concat regexp ".*")))))
                                            (lazy-find-symbol proj-name proj-alist 'imenu regexp))))
    (lazy-select-jumps (lazy-score-jumps jumps regexp buffer))))

(defun lazy-jump-references (word &optional proj-name buffer)
  )

(defun lazy-jump-callees (word &optional proj-name buffer)
  ;; only supported by cscope afaik
  )

(defun lazy-jump-callers (word &optional proj-name buffer)
  ;; only supported by cscope afaik
  )

;; ---------------------------------------------------------------------
;; Compile
;; ---------------------------------------------------------------------

(defun lazy-buffer-lang (&optional buffer)
  (when (buffer-file-name (or buffer (current-buffer)))
    (cadr (assoc (car (last (split-string (buffer-file-name (or buffer (current-buffer))) "\\.")))
                 lazy-src-pattern-table))))

(defun lazy-compile ()
  (interactive)
  (if (and (called-interactively-p 'interactive)
           (not lazy-name))
      (call-interactively 'compile)
    (lazy-assert-proj nil)
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
      ;;(lazy-save-state)
      (with-current-buffer (get-buffer (lazy-fib-name))
        (save-buffer))
      (let ((cmd (lazy-get-config-val 'compile-cmd)))
        (when (string-equal (buffer-name (current-buffer)) "*compilation*")
          (call-interactively 'other-window))
        (lazy-with-directory (lazy-get-config-val 'basedir)
                                (cond ((listp cmd)
                                       (let* ((old-history cmd)
                                              (compile-history (cl-remove-duplicates (append old-history compile-history) :test 'equal :from-end t))
                                              (old-cmd (car cmd))
                                              (new-cmd (internal-compile old-cmd)))
                                         (unless (string-equal old-cmd new-cmd)
                                           (lazy-set-config-val 'compile-cmd (cl-remove-duplicates (append (list new-cmd) old-history) :test 'equal :from-end t)))))
                                      ((stringp cmd)
                                       (let* ((old-cmd cmd)
                                              (new-cmd (internal-compile old-cmd))
                                              (new-list (list new-cmd old-cmd)))
                                         (unless (string-equal old-cmd new-cmd)
                                           (lazy-set-config-val 'compile-cmd new-list))))
                                      ((commandp cmd)
                                       (call-interactively cmd))
                                      ((functionp cmd)
                                       (funcall cmd))
                                      (t
                                       (lazy-set-config-val 'compile-cmd (list (internal-compile))))))
        ;;(lazy-update)
        ))))

;; ---------------------------------------------------------------------
;; Files
;; ---------------------------------------------------------------------

(defun lazy-fib-init (&optional proj-name quiet)
  "Either load the *file-index* buffer from the file cache, or create it afresh."
  (if (and (lazy-get-config-val 'file-list-cache proj-name t)
           (file-readable-p (lazy-get-config-val 'file-list-cache proj-name t)))
      (let ((zeitgeist-prevent-send t))
        (with-current-buffer (find-file-noselect (lazy-get-config-val 'file-list-cache proj-name t))
          (with-current-buffer (rename-buffer (lazy-fib-name proj-name))
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            (unless quiet
              (message (concat "Loading " (lazy-fib-name proj-name) " from %s") (lazy-get-config-val 'file-list-cache proj-name t))))))
    (lazy-index proj-name)))

(defun lazy-fib-clear (&optional proj-name)
  "Clear the contents of the fib buffer"
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let ((buf (get-buffer (lazy-fib-name proj-name))))
    (when buf
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (delete-region (point-min) (point-max))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)))))

(defun lazy-fib-cb (process event &optional proj-name proj-alist quiet)
  "Handle failure to complete fib building"
  (setq proj-alist (or proj-alist
                       (lazy-find-alist proj-name)
                       (lazy-find-alist lazy-name)
                       (lazy-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (cond
   ((string= event "finished\n")
    (let ((zeitgeist-prevent-send t))
      (with-current-buffer (get-buffer (lazy-fib-name proj-name))
        (when (lazy-get-config-val 'file-list-cache proj-name t proj-alist)
          (write-file (lazy-get-config-val 'file-list-cache proj-name t proj-alist))
          (rename-buffer (lazy-fib-name proj-name))
          (set-buffer-modified-p nil))
        (setq buffer-read-only t)))
    (unless quiet
      (message "Refreshing %s buffer...done" (lazy-fib-name proj-name))))
   (t
    (lazy-fib-clear proj-name)
    (unless quiet
      (message "Failed to generate the %s buffer!" (lazy-fib-name proj-name))))))

(defun lazy-find-cmd-src-args (src-patterns &optional proj-name proj-alist)
  "Generate the ( -name <pat1> -o -name <pat2> ...) pattern for find cmd"
  (setq proj-alist (or proj-alist
                       (lazy-find-alist proj-name)
                       (lazy-find-alist lazy-name)
                       (lazy-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (if src-patterns
      (let ((name-expr (if (eq system-type 'windows-nt) " \(" " \\("))
            (regex-or-name-arg (if (lazy-get-config-val 'patterns-are-regex proj-name t proj-alist)
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

(defun lazy-find-cmd-ignore-args (ignore-patterns &optional proj-name proj-alist)
  "Generate the -not ( -name <pat1> -o -name <pat2> ...) pattern for find cmd"
  (setq proj-alist (or proj-alist
                       (lazy-find-alist proj-name)
                       (lazy-find-alist lazy-name)
                       (lazy-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (concat " -not " (lazy-find-cmd-src-args (append ignore-patterns (list ".*/flycheck_.*")) proj-name proj-alist)))

(defvar lazy-index-processes (make-hash-table))

(cl-defun lazy-index (&optional proj-name proj-alist (async t) (do-friends nil) (quiet nil) (terminator nil) (parent nil))
  "Regenerate the *file-index* buffer that is used for lazy-find-file"
  (interactive)
  (setq proj-alist (or proj-alist
                       (lazy-find-alist proj-name)
                       (lazy-find-alist lazy-name)
                       (lazy-guess-alist)
                       ))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (lazy-assert-proj))
  (unless do-friends
    (setq do-friends (and (string-equal proj-name lazy-name)
                          (lazy-has-univ-arg))))
  (let* ((process)
         (friends (lazy-get-config-val 'friends proj-name nil proj-alist))
         (default-directory (file-name-as-directory (lazy-get-config-val 'basedir proj-name t proj-alist)))
         (start-dir (if lazy-file-index-relative-paths
                        "."
                      (file-name-as-directory (lazy-get-config-val 'basedir proj-name t proj-alist))))
         (find-exe (or (executable-find "gfind.exe")
                       (executable-find "find")))
         (find-cmd (concat "\"" find-exe "\" \"" start-dir "\" -type f "
                           (lazy-find-cmd-src-args (lazy-get-config-val 'src-patterns proj-name t proj-alist) proj-name proj-alist)
                           (lazy-find-cmd-ignore-args (lazy-get-config-val 'ignore-patterns proj-name t proj-alist) proj-name proj-alist)
                           ;; - had a problem under windows where gfind could not read some file and then always exit with error, this hack
                           ;; works around that and makes the command always exit with 0, may be neccessary on linux at some point too
                           (when (eq system-type 'windows-nt)
                             " 2> nul & exit /b 0")))
         (proc-name (concat "index-process-" proj-name)))
    (when (lazy-get-config-val 'file-list-cache proj-name t proj-alist)
      (lazy-fib-clear proj-name)
      (when (lazy-get-vcs-path proj-name)
        (setq find-cmd (concat find-cmd " -not -path " (concat "'*/" (lazy-get-vcs-path proj-name) "/*'"))))
      (with-current-buffer (get-buffer-create (lazy-fib-name proj-name))
        (buffer-disable-undo) ;; this is a large change we don't need to undo
        (setq buffer-read-only nil))
      (unless quiet
        (message "lazy-index cmd: \"%s\"" find-cmd)
        (message "Refreshing %s buffer..." (lazy-fib-name proj-name)))
      (setq process (start-process-shell-command proc-name (lazy-fib-name proj-name) find-cmd))
      (if parent
          (push process (cadr (gethash parent lazy-index-processes)))
        (puthash process (list (length friends) nil) lazy-index-processes)
        (setq parent process))
      (set-process-sentinel (get-process proc-name) `(lambda (p e)
                                                       (lazy-fib-cb p e ,proj-name (quote ,proj-alist) ,quiet)
                                                       (let ((tuple (gethash ,parent lazy-index-processes)))
                                                         (when (and tuple (quote ,terminator))
                                                           (let ((friends-num (nth 0 tuple))
                                                                 (friends-list (nth 1 tuple)))
                                                             (when (and (eq (process-status ,parent) 'exit)
                                                                        (or (not ,do-friends)
                                                                            (and (= friends-num (length friends-list))
                                                                                 (cl-every (lambda (o) (eq (process-status o) 'exit)) friends-list))))
                                                               (remhash ,parent lazy-index-processes)
                                                               (funcall (quote ,terminator) ,proj-name (quote ,proj-alist))))))))
      (unless async
        (while (or (string-equal (process-status process) "run")
                   (equal (process-status process) 'run))
          (sleep-for 0 4)))
      (when do-friends
        (dolist (friend friends)
          (let ((friend-alist (lazy-find-alist friend)))
            (when friend-alist
              (lazy-index friend friend-alist async nil quiet terminator parent))))))))

(defun lazy-fib-matches (&optional regex proj-name proj-alist)
  "Return list of files in *file-index* matching regex.

REGEX can be a list or a single regex.
If it is nil, return all files.

Returned file paths are relative to the project's basedir."
  (setq proj-alist (or proj-alist
                       (lazy-find-alist proj-name)
                       (lazy-find-alist lazy-name)
                       (lazy-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (get-buffer (lazy-fib-name proj-name))
    (lazy-fib-init proj-name))
  (when (or proj-alist (gethash proj-name lazy-list nil))
    (with-current-buffer (lazy-fib-name proj-name)
      (let ((basedir (file-name-as-directory (lazy-get-config-val 'basedir proj-name t proj-alist)))
            (current-filename nil)
            (case-fold-search nil))
        (sort (cl-loop for line in (split-string (buffer-string) "\n" t)
                       if (> (length line) 0)
                       do (setq current-filename (if (file-name-absolute-p line)
                                                     (file-relative-name line basedir)
                                                   line))
                       if (or (not regex)
                              (and (stringp regex)
                                   (string-match regex current-filename))
                              (and (listp regex)
                                   (cl-some (lambda (re) (string-match re current-filename)) regex)))
                       collect current-filename)
              #'string-lessp)))))

(defun lazy-files (&optional proj-name proj-alist truenames)
  (setq proj-alist (or proj-alist
                       (lazy-find-alist proj-name)
                       (lazy-find-alist lazy-name)
                       (lazy-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let ((basedir (file-name-as-directory (lazy-get-config-val 'basedir proj-name t proj-alist))))
    (when truenames (setq basedir (file-truename basedir)))
    (mapcar (lambda (f) (expand-file-name (concat basedir f)))
            (lazy-fib-matches nil proj-name proj-alist))))

(defun lazy-unique-files (&optional proj-name)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let ((proj-files (lazy-files proj-name nil t))
        (friendly-files (lazy-friendly-files proj-name nil t))
        (unique-friends '()))
    (dolist (f friendly-files)
      (unless (cl-find f proj-files :test 'equal)
        (setq unique-friends (append (list f) unique-friends))))
    (append proj-files unique-friends)))

(defun lazy-friendly-files (&optional proj-name proj-alist truenames)
  (setq proj-alist (or proj-alist
                       (lazy-find-alist proj-name)
                       (lazy-find-alist lazy-name)
                       (lazy-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let* ((basedir (file-truename (lazy-get-config-val 'basedir proj-name t proj-alist)))
         (friendly-files (cl-mapcan (lambda (friend)
                                      (let ((friend-file (lazy-with-directory basedir (expand-file-name friend))))
                                        (if (file-exists-p friend-file)
                                            (list friend-file)
                                          (lazy-files friend nil truenames))))
                                    (lazy-get-config-val 'friends proj-name t proj-alist))))
    friendly-files))

(defun lazy-dired ()
  "Open dired in the project's basedir (or jump to the existing dired buffer)"
  (interactive)
  (lazy-assert-proj t)
  (dired (lazy-get-config-val 'basedir)))

(defun lazy-multi-occur (regex)
  "Search all open project files for 'regex' using `multi-occur'.

Act like `lazy-multi-occur-with-friends' if called with prefix arg."
  (interactive "sRegex: ")
  (lazy-assert-proj t)
  (if (lazy-has-univ-arg)
      (lazy-multi-occur-with-friends regex)
    (multi-occur (lazy-filter (lambda (b) (if (buffer-file-name b) b nil))
                              (lazy-buffers))
                 regex)))

;; ---------------------------------------------------------------------
;; Friends
;; ---------------------------------------------------------------------

(defun lazy-find-friendly-projects (&optional proj-name)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  ;; go through all configs
  ;; collect all projects which have the requested name in their friend list
  ;; remove duplicates and return
  (let ((r '()))
    (maphash (lambda (k c)
               (unless (string-equal k proj-name)
                 (when (cl-some (lambda (f)
                                  (string-equal f proj-name))
                                (lazy-config-val 'friends c))
                   (setq r (append r `(,k)))))) lazy-list)
    (cl-remove-duplicates (append r (lazy-config-val 'friends proj-name t)) :test #'string-equal)))

(defun lazy-fib-friend-matches (&optional regex proj-name proj-alist)
  (setq proj-alist (or proj-alist
                       (lazy-find-alist proj-name)
                       (lazy-find-alist lazy-name)
                       (lazy-guess-alist)))
  (setq proj-name (cadr (assoc 'name proj-alist)))
  (unless (and proj-name proj-alist)
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let ((resulting-matches '())
        (case-fold-search nil))
    (dolist (friend (lazy-get-config-val 'friends proj-name t proj-alist) resulting-matches)
      (if (file-exists-p (lazy-with-directory (lazy-get-config-val 'basedir proj-name t proj-alist)
                                              (expand-file-name friend)))
          (if regex
              (when (string-match regex friend) (add-to-list 'resulting-matches (expand-file-name friend)))
            (add-to-list 'resulting-matches (expand-file-name friend)))
        (setq resulting-matches (append resulting-matches
                                        (let ((friend-alist (lazy-find-alist friend)))
                                          (when friend-alist
                                            (mapcar (lambda (f)
                                                      (expand-file-name (concat (file-name-as-directory (lazy-get-config-val 'basedir friend t friend-alist)) f)))
                                                    (lazy-fib-matches regex friend friend-alist))))))))
    ))

(defun lazy-friendly-buffer-p (buf &optional proj-name)
  "Check if BUF is a friend of PROJ-NAME."
  (unless (lazy-buffer-p buf)
    (let ((file-name (lazy-buffer-name buf)))
      (if (and file-name
               (cl-block "friend-loop"
                 (dolist (f (lazy-find-friendly-projects proj-name))
                   (if (file-exists-p (expand-file-name f))
                       (when (string-equal f file-name)
                         (cl-return-from "friend-loop" t))
                     (when (lazy-find-alist f t)
                       (let* ((friend-config (lazy-find-alist f t))
                              (non-slash-basedir (expand-file-name (car (cdr (assoc 'basedir friend-config)))))
                              (slash-basedir (if (string-equal (substring non-slash-basedir -1) "/")
                                                 non-slash-basedir
                                               (concat non-slash-basedir "/")))
                              (case-fold-search nil))
                         (when (or (string-match (concat "^" (regexp-quote slash-basedir)) file-name)
                                   (string-match (concat "^" (regexp-quote (file-truename slash-basedir))) file-name))
                           (cl-return-from "friend-loop" t))))))))
          t
        nil))))

(defun lazy-friendly-file-buffer-p (buf &optional proj-name)
  (and (buffer-file-name buf)
       (lazy-friendly-buffer-p buf proj-name)))

(defun lazy-friendly-special-buffer-p (buf &optional proj-name)
  (let ((case-fold-search nil))
    (and (string-match "\*[^\*]\*" (buffer-name buf))
         (lazy-friendly-buffer-p buf proj-name))))

(defun lazy-friendly-dired-buffer-p (buf &optional proj-name)
  (and (with-current-buffer buf
         (eq major-mode 'dired-mode))
       (lazy-friendly-buffer-p buf proj-name)))


(defun lazy-friendly-buffers (&optional proj-name)
  "Return all buffers that are friendly to the project"
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let ((buffers nil))
    (dolist (b (buffer-list))
      (when (lazy-friendly-buffer-p b proj-name)
        (push b buffers)))
    buffers))

(defun lazy-friendly-file-buffers (&optional proj-name)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (cl-remove-if (lambda (buf) (not (buffer-file-name buf))) (lazy-friendly-buffers proj-name)))

(defun lazy-friendly-special-buffers (&optional proj-name friends-only)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (let ((case-fold-search nil))
    (append (cl-remove-if (lambda (buf) (not (string-match "\*[^\*]\*" (buffer-name buf)))) (lazy-friendly-buffers proj-name))
            (cl-remove-if (lambda (buf) (or (and (symbolp 'lazy-org-project-buffer-name)
                                                 (not (string-equal (lazy-org-project-buffer-name proj-name) (buffer-name buf))))
                                            (compilation-buffer-p buf)))
                          (buffer-list)))))

(defun lazy-friendly-dired-buffers (&optional proj-name)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (cl-remove-if (lambda (buf) (not (lazy-friendly-dired-buffer-p buf))) (lazy-friendly-buffers proj-name)))

(defun lazy-save-open-friends-info ()
  (when (lazy-get-config-val 'open-friends-cache)
    (let ((zeitgeist-prevent-send t))
      (with-temp-buffer
        (dolist (f (cl-remove-duplicates (mapcar (lambda (b) (lazy-buffer-name b)) (lazy-friendly-buffers)) :test #'string-equal))
          (when f
            (unless (string-equal (lazy-get-config-val 'etags-file) f)
              (insert f "\n"))))
        (if (file-writable-p (lazy-get-config-val 'open-friends-cache))
            (write-region (point-min)
                          (point-max)
                          (lazy-get-config-val 'open-friends-cache))
          (message "Wrote open friends to %s" (lazy-get-config-val 'open-friends-cache))
          (message "Cannot write to %s" (lazy-get-config-val 'open-friends-cache)))))))

(defun lazy-visit-saved-open-friends ()
  (let ((zeitgeist-prevent-send t))
    (when (lazy-get-config-val 'open-friends-cache)
      (when (file-readable-p (lazy-get-config-val 'open-friends-cache))
        (message "Reading open friends from %s" (lazy-get-config-val 'open-friends-cache))
        (with-temp-buffer
          (insert-file-contents (lazy-get-config-val 'open-friends-cache))
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

(defun lazy-close-friends ()
  (interactive)
  (lazy-assert-proj)
  (let ((closed nil)
        (dirty nil)
        (zeitgeist-prevent-send t)
        (continue-prevent-save t))
    (dolist (b (append (lazy-friendly-buffers) (lazy-friendly-dired-buffers)))
      (cond
       ((buffer-modified-p b)
        (push (buffer-name) dirty))
       (t
        (kill-buffer b)
        (push (buffer-name) closed))))
    (message "Closed %d friendly buffers, %d modified friendly buffers where left open"
             (length closed) (length dirty))))

(defun lazy-multi-occur-with-friends (regex)
  "Search all open project files (including friends) for 'regex' using `multi-occur'."
  (interactive "sRegex: ")
  (lazy-assert-proj t)
  (multi-occur (lazy-filter (lambda (b) (if (buffer-file-name b) b nil))
                            (append (lazy-buffers) (lazy-friendly-buffers)))
               regex))

(defun lazy-friend-basedirs ()
  "Return all friends basedirs. This may also return single filenames instead of a directory."
  (let* ((basedirs '()))
    (dolist (f (lazy-find-friendly-projects) basedirs)
      (if (file-exists-p (expand-file-name f))
          (add-to-list 'basedirs f)
        (when (and f (lazy-config-val 'basedir f))
          (add-to-list 'basedirs (lazy-config-val 'basedir f)))))))

;; ---------------------------------------------------------------------
;; After saving
;; ---------------------------------------------------------------------

(defun lazy-after-save-add-pattern (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (when lazy-name
    (let* ((file-name (expand-file-name (buffer-file-name buffer)))
           (extension (car (last (split-string file-name "\\."))))
           (new-pattern (concat ".*\\." extension))
           (src-patterns (lazy-get-config-val 'src-patterns lazy-name t))
           (case-fold-search nil)
           (buildsystem-files (cl-loop for bs in lazy-buildsystems
                                       append (cadr (assoc 'files (cadr bs)))))
           (buildsystem-file-found (cl-some (lambda (buildsystem-file)
                                              (when (string-match (concat (regexp-quote buildsystem-file) "$") file-name)
                                                buildsystem-file)) buildsystem-files)))
      (when (and (assoc extension lazy-src-pattern-table)
                 (or (string-match (concat "^" (regexp-quote (file-name-as-directory (lazy-get-config-val 'basedir lazy-name t)))) file-name)
                     (string-match (concat "^" (regexp-quote (file-name-as-directory (lazy-get-config-val 'basedir lazy-name t)))) (file-truename file-name)))
                 (not (cl-some (lambda (pattern) (string-match pattern file-name)) src-patterns)))
        (lazy-set-config-val 'src-patterns (add-to-list 'src-patterns new-pattern)))
      (when (and buildsystem-file-found
                 (not (cl-some (lambda (pattern) (string-match pattern buildsystem-file-found)) src-patterns)))
        (lazy-set-config-val 'src-patterns (add-to-list 'src-patterns (concat ".*" (regexp-quote buildsystem-file-found) "$")))))))


(defun lazy-after-save-update (&optional proj-name)
  (unless lazy-after-save-update-in-progress
    (if (and (not (or lazy-prevent-after-save-update
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
                              lazy-name
                              (cadr (assoc 'name (lazy-guess-alist)))))
          (when proj-name
            (setq lazy-after-save-update-in-progress t
                  lazy-after-save-current-buffer (current-buffer)
                  lazy-after-save-current-project proj-name)
            (run-with-idle-timer 10 nil 'lazy-update))))))

(defun lazy-update (&optional p proj-name buffer)
  (interactive "p")
  (setq proj-name (or proj-name
                      lazy-after-save-current-project
                      lazy-name
                      (cadr (assoc 'name (lazy-guess-alist)))))
  (when lazy-name
    (let ((guessed-name (cadr (assoc 'name (lazy-guess-alist)))))
      (when guessed-name (setq proj-name guessed-name))))
  (unless proj-name
    (lazy-assert-proj))
  (unless buffer
    (setq buffer (or lazy-after-save-current-buffer
                     (current-buffer))))
  (condition-case e
      (progn
        (if (or p (lazy-buffer-p buffer proj-name) (lazy-friendly-buffer-p buffer proj-name))
            (progn
              (when (buffer-file-name buffer)
                (lazy-after-save-add-pattern buffer))
              (lazy-index proj-name nil t nil t
                          (lambda (&optional proj-name proj-alist files debug)
                            (lazy-update-tags proj-name proj-alist files debug)
                            (setq lazy-after-save-update-in-progress nil
                                  lazy-after-save-current-buffer nil
                                  lazy-after-save-current-project nil))))
          (setq lazy-after-save-update-in-progress nil
                lazy-after-save-current-buffer nil
                lazy-after-save-current-project nil)))
    (error (progn
             (setq lazy-after-save-update-in-progress nil
                   lazy-after-save-current-buffer nil
                   lazy-after-save-current-project nil)
             (message "error in lazy-after-save-update: %s" (prin1-to-string e)))))
  t)


(defun lazy-pre-command-remove-jump-delete-buffer ()
  (unless (or (eq this-command 'lazy-jump-next)
              (eq this-command 'lazy-jump-prev)
              (eq this-command 'lazy-jump-abort)
              (eq this-command 'lazy-jump))
    (mapc (lambda (ov)
            (when (eq (overlay-buffer ov) (current-buffer))
              (overlay-put ov 'delete-buffer nil)))
          lazy-jump-overlays)))

;; ---------------------------------------------------------------------
;; Guessing
;; ---------------------------------------------------------------------

(defun lazy-find-projects-matching-patterns (test-patterns &optional name-list)
  (let ((results nil)
        (case-fold-search nil))
    (maphash (lambda (k v)
               (let ((proj-patterns))
                 (when (and (setq proj-patterns (cadr (assoc 'src-patterns v)))
                            (cl-loop for tp in test-patterns
                                     for pp in proj-patterns
                                     if (string-match tp pp)
                                     return t
                                     finally return nil))
                   (add-to-list 'results k))))
             (or (and name-list
                      (let ((temp-hash (make-hash-table :test 'equal)))
                        (mapc (lambda (name) (puthash name (gethash name lazy-list) temp-hash)) name-list)
                        temp-hash))
                 lazy-list))
    results))


(cl-defun lazy-find-projects-in-directory (path &optional name-list)
  (let ((results nil))
    (maphash (lambda (k v)
               (when (or (string-equal (expand-file-name (concat path "/"))
                                       (expand-file-name (concat (lazy-get-config-val 'basedir k t) "")))
                         (string-equal (file-truename (expand-file-name (concat path "/")))
                                       (file-truename (expand-file-name (concat (lazy-get-config-val 'basedir k t) "")))))
                 (add-to-list 'results k)))
             (or (and name-list
                      (let ((temp-hash (make-hash-table :test 'equal)))
                        (mapc (lambda (name) (puthash name (gethash name lazy-list) temp-hash)) name-list)
                        temp-hash))
                 lazy-list))
    results))

(defun lazy-find-projects-owning-buffer (buf &optional name-list)
  (let ((projects nil)
        (case-fold-search nil))
    (maphash (lambda (k v)
               (when (and (buffer-file-name buf)
                          (lazy-get-config-val 'basedir k t)
                          (lazy-path-equal (buffer-file-name buf) (lazy-get-config-val 'basedir k t))
                          (cl-some (lambda (re) (string-match re (buffer-file-name buf))) (lazy-get-config-val 'src-patterns k t)))
                 (add-to-list 'projects k)))
             (or (and name-list
                      (let ((temp-hash (make-hash-table :test 'equal)))
                        (mapc (lambda (name) (puthash name (gethash name lazy-list) temp-hash)) name-list)
                        temp-hash))
                 lazy-list))
    projects))

(defun lazy-find-unique-paths (paths)
  (let ((result '()))
    (dolist (path paths result)
      (unless (cl-some (lambda (a) (lazy-path-equal path a)) (lazy-filter (lambda (p) (not (string-equal p path))) paths))
        (add-to-list 'result path)))))

(defun lazy-find-common-path-of-buffers (&optional buffers ignore-paths)
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
                         (setq common-path (split-string (lazy-dirname (expand-file-name (buffer-file-name buf))) "/" (not (eq system-type 'windows-nt))))
                       ;; we split both paths by "/" and create a zipper from the resulting lists
                       ;; /foo/bar     -> '\("foo" "bar"\)
                       ;; /foo/bar/bla -> '\("foo" "bar" "bla"\)
                       ;; -> '\(\("foo" "foo"\) \("bar" "bar"\) \(nil "bla"\)\)
                       ;; then walking over the zipper while both tuple's strings match, stopping at a mismatch
                       ;; and collecting matching strings on the way along
                       (let ((tuples (lazy-zip common-path (split-string (expand-file-name (buffer-file-name buf)) "/" (not (eq system-type 'windows-nt)))))
                             (temp-path '()))
                         (while (string-equal (cl-first (car tuples)) (cl-second (car tuples)))
                           (add-to-list 'temp-path (cl-first (car tuples)))
                           (setq tuples (cdr tuples)))
                         ;; we'll set the new common-path before the next iteration, but only if it wouldn't be
                         ;; 'equal' (see lazy-path-equal) to any of the ignore-paths
                         (unless (cl-loop for ig in ignore-paths
                                          if (lazy-path-equal (file-name-as-directory (expand-file-name ig))
                                                              (expand-file-name (apply 'concat (mapcar 'file-name-as-directory (reverse temp-path)))))
                                          return t
                                          finally return nil)
                           (setq common-path (reverse temp-path)))))))))
    result))

(cl-defun lazy-guess-buffers (&optional test-buffer ignore-paths mode)
  (unless test-buffer
    (setq test-buffer (current-buffer)))
  (let ((buffers (buffer-list)))
    (while (and (not (buffer-file-name test-buffer))
                buffers)
      (setq test-buffer (car buffers)
            buffers (cdr buffers))))
  (unless (buffer-file-name test-buffer)
    (cl-return-from "lazy-guess-buffers" nil))
  (let* ((ignore-paths (sort ignore-paths (lambda (a b) (> (length a) (length b)))))
         (test-mode (with-current-buffer test-buffer major-mode))
         (test-path (or (lazy-dirname (buffer-file-name test-buffer))
                        default-directory))
         (buffer-projects (lazy-find-projects-owning-buffer test-buffer))
         result)
    (setq result (cl-remove-if-not 'identity
                                   (mapcar (lambda (buf)
                                             (let (buf-projects)
                                               (when (and (buffer-file-name buf)
                                                          (lazy-path-complement (lazy-dirname (buffer-file-name buf)) test-path)
                                                          ;; same projects for all buffers considered, all guessed buffers must belong to the
                                                          ;; same projects as test-buffer, even if test-buffer belongs to multiple projects
                                                          (or (lazy-path-equal test-path (lazy-dirname (buffer-file-name buf)) t)
                                                              (cl-loop for ig in ignore-paths
                                                                       if (lazy-path-equal ig (lazy-dirname (buffer-file-name buf)) t)
                                                                       return nil
                                                                       finally return t))
                                                          (equal (setq buf-projects (lazy-find-projects-owning-buffer buf)) buffer-projects))
                                                 ;; special treatment for buffers when a project is loaded, exclude project buffers of loaded
                                                 ;; project if test-buffer does not belong to loaded project, exclude buffers that do not
                                                 ;; belong to loaded project when test-buffer belongs to the loaded project
                                                 (unless (or (and (boundp 'lazy-name)
                                                                  lazy-name
                                                                  (not (cl-some (lambda (p) (string-equal p lazy-name)) buffer-projects))
                                                                  (cl-some (lambda (p) (string-equal p lazy-name)) buf-projects))
                                                             (and (boundp 'lazy-name)
                                                                  lazy-name
                                                                  (not (eq buf test-buffer))
                                                                  (not (cl-some (lambda (p) (string-equal p lazy-name)) buffer-projects))
                                                                  (lazy-friendly-buffer-p buf lazy-name)))
                                                   buf))))
                                           (if mode
                                               (cl-remove-if-not (lambda (b) (eq (with-current-buffer b major-mode) mode)) (buffer-list))
                                             (buffer-list)))))
    result))

(defun lazy-src-pattern-languages (src-patterns)
  (let ((lang nil)
        (languages nil))
    (cl-loop for pattern in src-patterns
             do (let* ((parts (split-string pattern "\\." t))
                       (ending (if (and (> (length parts) 2)
                                        (string-equal (car (last parts)) "gz"))
                                   (concat (nth 0 (last parts 2)) "." (nth 1 (last parts 2)))
                                 (car (last parts)))))
                  (setq lang (cadr (assoc ending lazy-src-pattern-table))))
             if (not (eq lang nil))
             do (add-to-list 'languages lang))
    languages))

(defvar lazy-incubator-paths `(,(expand-file-name "~"))
  "An incubator is a location where multiple projects are kept. These will be
ignored when guessing a projects basedir thus giving preference to subdirectories
within it.

It is not impossible for an incubator path to be guessed as project basedir though.
If you'll guess while in a buffer with a file from an incubator root open, that
incubator root could be guessed as basedir.")

(defvar lazy-common-project-subdir-names '("src" "include" "demo[?s]" "example[?s]" "doc[?s]" "build" "tool[?s]" "test[?s]" "misc")
  "Common subdirectory names found in projects as regular expressions. These
help guessing a projects basedir. Matching directory names will be ignored
and their parent directory used as basedir.")

(defvar lazy-guess-functions '((buffer . ((()
                                           `(1 . ,(current-buffer)))))
                               (mode . (((buffer)
                                         `(1 . ,(with-current-buffer buffer major-mode)))))
                               (basedir . (;; default-directory
                                           (()
                                            `(1 . ,(expand-file-name default-directory)))
                                           ;; buffer-file-name
                                           ((buffer)
                                            (when (buffer-file-name buffer)
                                              `(10 . ,(lazy-dirname (expand-file-name (buffer-file-name buffer))))))
                                           ;; longest-common-path of buffers with same mode
                                           ((mode buffer)
                                            (let ((found-path (lazy-find-common-path-of-buffers (lazy-guess-buffers buffer lazy-incubator-paths mode))))
                                              (when (and found-path
                                                         (not (string-equal (expand-file-name "~") found-path)))
                                                `(50 . ,found-path))))
                                           ;; find directory that is not a common project subdir
                                           ((buffer)
                                            (let* ((path (lazy-find-common-path-of-buffers (lazy-guess-buffers buffer lazy-incubator-paths)))
                                                   (splitted-path (when path (split-string path "/"))))
                                              (while (and path
                                                          splitted-path
                                                          (not (cl-some (lambda (incubator-path) (lazy-path-equal incubator-path path))
                                                                        lazy-incubator-paths))
                                                          (some (lambda (dir) (string-equal dir (car (last splitted-path))))
                                                                lazy-common-project-subdir-names))
                                                (setq splitted-path (butlast splitted-path)
                                                      path (cl-reduce (lambda (a b) (concat a "/" b)) splitted-path)))
                                              (when path
                                                `(100 . ,path))))
                                           ;; find basedir by searching for buildsystem patterns
                                           ((buffer)
                                            (let ((path (lazy-find-common-path-of-buffers (lazy-guess-buffers buffer lazy-incubator-paths))))
                                              (when path
                                                (let ((found-paths (sort (loop for re in (lazy-buildsystem-patterns)
                                                                               collect (lazy-search-path re path lazy-incubator-paths lazy-common-project-subdir-names))
                                                                         (lambda (a b) (> (length a) (length b))))))
                                                  (when (car found-paths)
                                                    `(200 . ,(car found-paths)))))))
                                           ;; find basedir by searching for vcs pattern
                                           ((buffer)
                                            (let* ((filename (buffer-file-name buffer))
                                                   (found-paths (when filename
                                                                  (sort (loop for re in (mapcar 'regexp-quote (mapcar 'cdr lazy-vcs-path))
                                                                              collect (lazy-search-path re (file-name-directory (expand-file-name filename)) lazy-incubator-paths lazy-common-project-subdir-names))
                                                                        (lambda (a b) (> (length a) (length b)))))))
                                              (if (car found-paths)
                                                  `(500 . ,(car found-paths))
                                                (let ((path (lazy-find-common-path-of-buffers (lazy-guess-buffers buffer lazy-incubator-paths))))
                                                  (when path
                                                    (let ((found-paths (sort (loop for re in (mapcar 'regexp-quote (mapcar 'cdr lazy-vcs-path))
                                                                                   collect (lazy-search-path re path lazy-incubator-paths lazy-common-project-subdir-names))
                                                                             (lambda (a b) (> (length a) (length b))))))
                                                      (when (car found-paths)
                                                        `(300 . ,(car found-paths)))))))))
                                           ;; find basedir by trying to match buffers directory to project basedirs
                                           ((buffer)
                                            (let ((basedirs '()))
                                              (dolist (proj-name (lazy-find-projects-owning-buffer buffer))
                                                (let ((basedir (file-name-as-directory (lazy-get-config-val 'basedir proj-name t))))
                                                  (unless (cl-some (apply-partially 'string-equal basedir) basedirs)
                                                    (add-to-list 'basedirs basedir))))
                                              (let ((basedirs-without-incubators (cl-remove-if (lambda (dir)
                                                                                                 (cl-some (lambda (incubator)
                                                                                                            (string-equal (file-name-as-directory dir)
                                                                                                                          (file-name-as-directory incubator)))
                                                                                                          lazy-incubator-paths))
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
                                           (when (loop for ig in lazy-incubator-paths
                                                       if (lazy-path-equal ig basedir)
                                                       return nil
                                                       finally return t)
                                             `(100 . ,pname))))))
                               (src-patterns . (((basedir mode)
                                                 (let* ((all-incubator 'undefined)
                                                        (case-fold-search nil)
                                                        ;; guess buffers, collect files and set all-incubator, files from incubator
                                                        ;; roots are ignored except the current buffers file, if all relevant files
                                                        ;; we found are from incubator roots all-incubator will be true
                                                        (files (loop for buf in (lazy-guess-buffers (current-buffer) nil mode)
                                                                     do (cond ((and (cl-some (lambda (ig)
                                                                                               (let ((file-name (buffer-file-name buf)))
                                                                                                 (when file-name
                                                                                                   (lazy-path-equal (lazy-dirname (expand-file-name file-name)) ig t))))
                                                                                             lazy-incubator-paths)
                                                                                    all-incubator)
                                                                               (setq all-incubator t))
                                                                              (t
                                                                               (setq all-incubator nil)))
                                                                     if (and (buffer-file-name buf)
                                                                             (stringp basedir)
                                                                             (string-match (regexp-quote (expand-file-name basedir)) (expand-file-name (buffer-file-name buf)))
                                                                             (or (eq (current-buffer) buf)
                                                                                 (not (cl-some (lambda (ig)
                                                                                                 (lazy-path-equal (lazy-dirname (expand-file-name (buffer-file-name buf))) ig t))
                                                                                               lazy-incubator-paths))))
                                                                     append (list (file-name-nondirectory (expand-file-name (buffer-file-name buf))))))
                                                        patterns)
                                                   ;; get unique file endings from filenames and make regexp patterns
                                                   (unless (and (= (length files) 1)
                                                                all-incubator)
                                                     (loop for f in files
                                                           if (let ((splits (split-string f "\\." t)))
                                                                (and (last splits)
                                                                     (> (length splits) 1)))
                                                           do (let ((file-ending (mapconcat 'identity (reverse (butlast (reverse (split-string f "\\." t)))) ".")))
                                                                (mapc (lambda (s)
                                                                        (add-to-list 'patterns s))
                                                                      (cddr (assoc file-ending lazy-src-pattern-table))))
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
                                                                (loop for bs in lazy-buildsystems
                                                                      until (setq r (when (assoc 'files (cadr bs))
                                                                                      (cl-some (apply-partially 'string-equal filename) (cadr (assoc 'files (cadr bs))))))
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
                                                if (cl-some (lambda (y)
                                                              (string-equal (cdr y) f)) lazy-vcs-path)
                                                return `(10 . ,(car (rassoc f lazy-vcs-path))))))))
                               (languages . (((src-patterns)
                                              (let ((languages (lazy-src-pattern-languages src-patterns)))
                                                (when languages
                                                  `(10 . ,languages))))))))

(defun lazy-guess-alist (&optional ask-basedir ask-name)
  ;; go through lazy-guess-functions and collect all symbols that are used
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
                                (dolist (flist (cdr (assoc sym lazy-guess-functions)) (best-result scores))
                                  (let ((args (cl-first flist))
                                        (expr (cl-second flist)))
                                    (dolist (arg args)
                                      ;; check if neccessary symbols are set, this sets a symbol after guessing it so
                                      ;; we do not have to guess something twice
                                      (when (eq (symbol-value arg) 'undefined)
                                        (setf (symbol-value arg) (guess-symbol arg))
                                        ))
                                    (let ((r (condition-case e (eval expr)
                                               (error (message "error while guessing %S: %S in %s" sym e (prin1-to-string expr))))))
                                      (when r (add-to-list 'scores r))))))))
      (dolist (varchecks (append lazy-required-vars lazy-optional-vars))
        ;; for each var check if it is already set, if not use guess-symbol to guess it
        ;; since only args from lazy-guess-functions are defined by the alet, not all
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
            (when (cl-some (apply-partially 'eq var) lazy-required-vars)
              (cl-return-from "lazy-guess-alist" nil)))))
      ;; find already defined project that fits the guessed project so well that we'll use that instead
      ;; creates list of all projects in same basedir, then selects those matching the same src-patterns
      ;; as the guessed, uses the first of those if multiple match
      (let ((already-defined (or (and (buffer-file-name (current-buffer))
                                      (lazy-find-projects-in-directory (lazy-dirname (buffer-file-name (current-buffer)))))
                                 (lazy-find-projects-in-directory (cadr (assoc 'basedir result)))))
            (pattern-projects nil))
        (if already-defined
            (cl-loop for proj-name in already-defined
                     if (setq pattern-projects
                              (lazy-find-projects-matching-patterns (lazy-get-config-val 'src-patterns proj-name t)
                                                                    already-defined))
                     return (let ((already-defined-result (gethash (car pattern-projects) lazy-list)))
                              ;; add name if it does not already exist to alist, doubles functionality in lazy-def
                              (unless (assoc 'name already-defined-result)
                                (add-to-list 'already-defined-result `(name ,(car pattern-projects))))
                              already-defined-result))
          result)))))

(with-eval-after-load 'lazy
  (progn
    (run-with-idle-timer 60 t 'lazy-save-state)
    (add-hook 'after-save-hook 'lazy-after-save-update)
    (add-hook 'after-load-hook 'lazy-after-save-update)
    (add-hook 'after-save-hook 'lazy-jump-cleanup-highlight)
    (add-hook 'pre-command-hook 'lazy-pre-command-remove-jump-delete-buffer)
    (load-file (concat (file-name-as-directory lazy-global-cache-root) "projects.el"))))

(provide 'lazy)

;; lazy.el ends here
