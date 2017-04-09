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

(defvar lazy-global-cache-root (let ((filename (expand-file-name "~/.lazy/")))
                                 (unless (file-exists-p filename)
                                   (let ((dir (file-name-directory filename)))
                                     (unless (file-exists-p dir)
                                       (make-directory dir))))
                                 filename)
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
                                                           if (gethash friend lazy-project-list)
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

(defvar lazy-project-history '()
  "History of Lazy projects that were opened in the current emacs session.")

(defvar lazy-buildsystems '((gnu-make ((files ("autogen.sh" "configure" "Makefile"))
                                       (build ("make"))))
                            (cmake ((files ("CMakeLists.txt"))
                                    (build ("mkdir -p build; cd build; cmake .. && make"))))
                            (cabal ((files ("Setup.lhs" ".*\.cabal"))
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

(defvar lazy-src-pattern-table '(("c" . (c ".*\\.c" ".*\\.h"))
                                 ("h" . (c ".*\\.c" ".*\\.h"))
                                 ("y" . (yacc ".*\\.y"))
                                 ("s" . (asm ".*\\.asm" ".*\\.ASM" ".*\\.s" ".*\\.S" ".*\\.A51" ".*\\.29k" ".*\\.29K"))
                                 ("S" . (asm ".*\\.asm" ".*\\.ASM" ".*\\.s" ".*\\.S" ".*\\.A51" ".*\\.29k" ".*\\.29K"))
                                 ("java" . (java ".*\\.java"))
                                 ("c++" . (cpp ".*\\.c\\+\\+" ".*\\.cc" ".*\\.cp" ".*\\.cpp" ".*\\.cxx" ".*\\.h" ".*\\.h\\+\\+" ".*\\.hh" ".*\\.hp" ".*\\.hpp" ".*\\.hxx" ".*\\.C" ".*\\.H"))
                                 ("cc" . (cpp ".*\\.c\\+\\+" ".*\\.cc" ".*\\.cp" ".*\\.cpp" ".*\\.cxx" ".*\\.h" ".*\\.h\\+\\+" ".*\\.hh" ".*\\.hp" ".*\\.hpp" ".*\\.hxx" ".*\\.C" ".*\\.H"))
                                 ("hh" . (cpp ".*\\.c\\+\\+" ".*\\.cc" ".*\\.cp" ".*\\.cpp" ".*\\.cxx" ".*\\.h" ".*\\.h\\+\\+" ".*\\.hh" ".*\\.hp" ".*\\.hpp" ".*\\.hxx" ".*\\.C" ".*\\.H"))
                                 ("cpp" . (cpp ".*\\.c\\+\\+" ".*\\.cc" ".*\\.cp" ".*\\.cpp" ".*\\.cxx" ".*\\.h" ".*\\.h\\+\\+" ".*\\.hh" ".*\\.hp" ".*\\.hpp" ".*\\.hxx" ".*\\.C" ".*\\.H"))
                                 ("cxx" . (cpp ".*\\.c\\+\\+" ".*\\.cc" ".*\\.cp" ".*\\.cpp" ".*\\.cxx" ".*\\.h" ".*\\.h\\+\\+" ".*\\.hh" ".*\\.hp" ".*\\.hpp" ".*\\.hxx" ".*\\.C" ".*\\.H"))
                                 ("hxx" . (cpp ".*\\.c\\+\\+" ".*\\.cc" ".*\\.cp" ".*\\.cpp" ".*\\.cxx" ".*\\.h" ".*\\.h\\+\\+" ".*\\.hh" ".*\\.hp" ".*\\.hpp" ".*\\.hxx" ".*\\.C" ".*\\.H"))
                                 ("hpp" . (cpp ".*\\.c\\+\\+" ".*\\.cc" ".*\\.cp" ".*\\.cpp" ".*\\.cxx" ".*\\.h" ".*\\.h\\+\\+" ".*\\.hh" ".*\\.hp" ".*\\.hpp" ".*\\.hxx" ".*\\.C" ".*\\.H"))
                                 ("C" . (cpp ".*\\.c\\+\\+" ".*\\.cc" ".*\\.cp" ".*\\.cpp" ".*\\.cxx" ".*\\.h" ".*\\.h\\+\\+" ".*\\.hh" ".*\\.hp" ".*\\.hpp" ".*\\.hxx" ".*\\.C" ".*\\.H"))
                                 ("H" . (cpp ".*\\.c\\+\\+" ".*\\.cc" ".*\\.cp" ".*\\.cpp" ".*\\.cxx" ".*\\.h" ".*\\.h\\+\\+" ".*\\.hh" ".*\\.hp" ".*\\.hpp" ".*\\.hxx" ".*\\.C" ".*\\.H"))
                                 ("php" . (php ".*\\.php" ".*\\.php3" ".*\\.phtml"))
                                 ("php3" . (php ".*\\.php" ".*\\.php3" ".*\\.phtml"))
                                 ("phtml" . (php ".*\\.php" ".*\\.php3" ".*\\.phtml"))
                                 ("asm" . (asm ".*\\.asm" ".*\\.ASM" ".*\\.s" ".*\\.S" ".*\\.A51" ".*\\.29k" ".*\\.29K"))
                                 ("ASM" . (asm ".*\\.asm" ".*\\.ASM" ".*\\.s" ".*\\.S" ".*\\.A51" ".*\\.29k" ".*\\.29K"))
                                 ("A51" . (asm ".*\\.asm" ".*\\.ASM" ".*\\.s" ".*\\.S" ".*\\.A51" ".*\\.29k" ".*\\.29K"))
                                 ("29k" . (asm ".*\\.asm" ".*\\.ASM" ".*\\.s" ".*\\.S" ".*\\.A51" ".*\\.29k" ".*\\.29K"))
                                 ("29K" . (asm ".*\\.asm" ".*\\.ASM" ".*\\.s" ".*\\.S" ".*\\.A51" ".*\\.29k" ".*\\.29K"))
                                 ("asp" . (asp ".*\\.asp" ".*\\.asa"))
                                 ("asa" . (asp ".*\\.asp" ".*\\.asa"))
                                 ("awk" . (awk ".*\\.awk" ".*\\.gawk" ".*\\.mawk"))
                                 ("gawk" . (awk ".*\\.awk" ".*\\.gawk" ".*\\.mawk"))
                                 ("mawk" . (awk ".*\\.awk" ".*\\.gawk" ".*\\.mawk"))
                                 ("bas" . (basic ".*\\.bas" ".*\\.bi" ".*\\.bb" ".*\\.pb"))
                                 ("bi" . (basic ".*\\.bas" ".*\\.bi" ".*\\.bb" ".*\\.pb"))
                                 ("bb" . (basic ".*\\.bas" ".*\\.bi" ".*\\.bb" ".*\\.pb"))
                                 ("pb" . (basic ".*\\.bas" ".*\\.bi" ".*\\.bb" ".*\\.pb"))
                                 ("bet" . (beta ".*\\.bet"))
                                 ("cp" . (cpp ".*\\.c\\+\\+" ".*\\.cc" ".*\\.cp" ".*\\.cpp" ".*\\.cxx" ".*\\.h" ".*\\.h\\+\\+" ".*\\.hh" ".*\\.hp" ".*\\.hpp" ".*\\.hxx" ".*\\.C" ".*\\.H"))
                                 ("h" . (cpp ".*\\.c\\+\\+" ".*\\.cc" ".*\\.cp" ".*\\.cpp" ".*\\.cxx" ".*\\.h" ".*\\.h\\+\\+" ".*\\.hh" ".*\\.hp" ".*\\.hpp" ".*\\.hxx" ".*\\.C" ".*\\.H"))
                                 ("h++" . (cpp ".*\\.c\\+\\+" ".*\\.cc" ".*\\.cp" ".*\\.cpp" ".*\\.cxx" ".*\\.h" ".*\\.h\\+\\+" ".*\\.hh" ".*\\.hp" ".*\\.hpp" ".*\\.hxx" ".*\\.C" ".*\\.H"))
                                 ("hp" . (cpp ".*\\.c\\+\\+" ".*\\.cc" ".*\\.cp" ".*\\.cpp" ".*\\.cxx" ".*\\.h" ".*\\.h\\+\\+" ".*\\.hh" ".*\\.hp" ".*\\.hpp" ".*\\.hxx" ".*\\.C" ".*\\.H"))
                                 ("cs" . (csharp ".*\\.cs"))
                                 ("cbl" . (cobol ".*\\.cbl" ".*\\.cob" ".*\\.CBL" ".*\\.COB"))
                                 ("cob" . (cobol ".*\\.cbl" ".*\\.CBL" ".*\\.cob" ".*\\.COB" ".*\\.cpy" ".*\\.CPY"))
                                 ("CBL" . (cobol ".*\\.cbl" ".*\\.cob" ".*\\.CBL" ".*\\.COB"))
                                 ("COB" . (cobol ".*\\.cbl" ".*\\.CBL" ".*\\.cob" ".*\\.COB" ".*\\.cpy" ".*\\.CPY"))
                                 ("bat" . (dosbatch ".*\\.bat" ".*\\.cmd"))
                                 ("cmd" . (dosbatch ".*\\.bat" ".*\\.cmd"))
                                 ("e" . (eiffel ".*\\.e"))
                                 ("erl" . (erlang ".*\\.ERL" ".*\\.HRL" ".*\\.erl" ".*\\.hrl" ".*\\.es" ".*\\.escript"))
                                 ("ERL" . (erlang ".*\\.erl" ".*\\.ERL" ".*\\.hrl" ".*\\.HRL"))
                                 ("hrl" . (erlang ".*\\.ERL" ".*\\.HRL" ".*\\.erl" ".*\\.hrl" ".*\\.es" ".*\\.escript"))
                                 ("HRL" . (erlang ".*\\.erl" ".*\\.ERL" ".*\\.hrl" ".*\\.HRL"))
                                 ("as" . (flex ".*\\.as" ".*\\.mxml"))
                                 ("mxml" . (flex ".*\\.as" ".*\\.mxml"))
                                 ("f" . (fortran ".*\\.for" ".*\\.ftn" ".*\\.f77" ".*\\.f95" ".*\\.f" ".*\\.f90" ".*\\.F" ".*\\.F90"))
                                 ("for" . (fortran ".*\\.f" ".*\\.for" ".*\\.ftn" ".*\\.f77" ".*\\.f90" ".*\\.f95"))
                                 ("ftn" . (fortran ".*\\.f" ".*\\.for" ".*\\.ftn" ".*\\.f77" ".*\\.f90" ".*\\.f95"))
                                 ("f77" . (fortran ".*\\.f" ".*\\.for" ".*\\.ftn" ".*\\.f77" ".*\\.f90" ".*\\.f95"))
                                 ("f90" . (fortran ".*\\.for" ".*\\.ftn" ".*\\.f77" ".*\\.f95" ".*\\.f" ".*\\.f90" ".*\\.F" ".*\\.F90"))
                                 ("f95" . (fortran ".*\\.f" ".*\\.for" ".*\\.ftn" ".*\\.f77" ".*\\.f90" ".*\\.f95"))
                                 ("htm" . (html ".*\\.htm" ".*\\.html"))
                                 ("html" . (html ".*\\.htm" ".*\\.html"))
                                 ("js" . (javascript ".*\\.js"))
                                 ("el" . (elisp  ".*\\.el" ".*\\.el\\.gz"))
                                 ("el.gz" . (elisp  ".*\\.el" ".*\\.el\\.gz"))
                                 ("cl" . (lisp ".*\\.cl" ".*\\.clisp" ".*\\.el" ".*\\.l" ".*\\.lisp" ".*\\.lsp"))
                                 ("clisp" . (lisp ".*\\.cl" ".*\\.clisp" ".*\\.el" ".*\\.l" ".*\\.lisp" ".*\\.lsp"))
                                 ("el" . (lisp ".*\\.cl" ".*\\.clisp" ".*\\.el" ".*\\.l" ".*\\.lisp" ".*\\.lsp"))
                                 ("l" . (lisp ".*\\.cl" ".*\\.clisp" ".*\\.el" ".*\\.l" ".*\\.lisp" ".*\\.lsp"))
                                 ("lisp" . (lisp ".*\\.cl" ".*\\.clisp" ".*\\.el" ".*\\.l" ".*\\.lisp" ".*\\.lsp"))
                                 ("lsp" . (lisp ".*\\.cl" ".*\\.clisp" ".*\\.el" ".*\\.l" ".*\\.lisp" ".*\\.lsp"))
                                 ("lua" . (lua ".*\\.lua" ".*\\.wlua"))
                                 ("mak" . (make ".*\\.mak" ".*\\.mk"))
                                 ("mk" . (make ".*\\.mak" ".*\\.mk"))
                                 ("m" . (matlab ".*\\.m"))
                                 ("ml" . (ocaml ".*\\.ml" ".*\\.mli" ".*\\.mll" ".*\\.mly"))
                                 ("mli" . (ocaml ".*\\.ml" ".*\\.mli" ".*\\.mll" ".*\\.mly"))
                                 ("p" . (pascal ".*\\.p" ".*\\.pas"))
                                 ("pas" . (pascal ".*\\.p" ".*\\.pas"))
                                 ("pl" . (perl ".*\\.plx" ".*\\.perl" ".*\\.pl" ".*\\.pm"))
                                 ("pm" . (perl ".*\\.plx" ".*\\.perl" ".*\\.pl" ".*\\.pm"))
                                 ("plx" . (perl ".*\\.pl" ".*\\.pm" ".*\\.plx" ".*\\.perl"))
                                 ("perl" . (perl ".*\\.pl" ".*\\.pm" ".*\\.plx" ".*\\.perl"))
                                 ("py" . (python ".*\\.pyx" ".*\\.pxd" ".*\\.pxi" ".*\\.scons" ".*\\.py" ".*\\.pyw" ".*\\.sc" ".*\\.tac" ".*\\.sage"))
                                 ("pyx" . (python ".*\\.py" ".*\\.pyx" ".*\\.pxd" ".*\\.pxi" ".*\\.scons"))
                                 ("pxd" . (python ".*\\.py" ".*\\.pyx" ".*\\.pxd" ".*\\.pxi" ".*\\.scons"))
                                 ("pxi" . (python ".*\\.py" ".*\\.pyx" ".*\\.pxd" ".*\\.pxi" ".*\\.scons"))
                                 ("scons" . (python ".*\\.py" ".*\\.pyx" ".*\\.pxd" ".*\\.pxi" ".*\\.scons"))
                                 ("cmd" . (rexx ".*\\.cmd" ".*\\.rexx" ".*\\.rx"))
                                 ("rexx" . (rexx ".*\\.cmd" ".*\\.rexx" ".*\\.rx"))
                                 ("rx" . (rexx ".*\\.cmd" ".*\\.rexx" ".*\\.rx"))
                                 ("rb" . (ruby ".*\\.ruby" ".*\\.rb" ".*\\.rbw" ".*\\.rake" ".*\\.gemspec" ".*\\.rbx" ".*\\.duby"))
                                 ("ruby" . (ruby ".*\\.rb" ".*\\.ruby"))
                                 ("SCM" . (scheme ".*\\.SCM" ".*\\.SM" ".*\\.sch" ".*\\.scheme" ".*\\.scm" ".*\\.sm"))
                                 ("SM" . (scheme ".*\\.SCM" ".*\\.SM" ".*\\.sch" ".*\\.scheme" ".*\\.scm" ".*\\.sm"))
                                 ("sch" . (scheme ".*\\.SCM" ".*\\.SM" ".*\\.sch" ".*\\.scheme" ".*\\.scm" ".*\\.sm"))
                                 ("scheme" . (scheme ".*\\.SCM" ".*\\.SM" ".*\\.sch" ".*\\.scheme" ".*\\.scm" ".*\\.sm"))
                                 ("scm" . (scheme ".*\\.SCM" ".*\\.SM" ".*\\.sch" ".*\\.scheme" ".*\\.sm" ".*\\.scm" ".*\\.ss"))
                                 ("sm" . (scheme ".*\\.SCM" ".*\\.SM" ".*\\.sch" ".*\\.scheme" ".*\\.scm" ".*\\.sm"))
                                 ;; ("sh" . (sh ".*\\.sh" ".*\\.SH" ".*\\.bsh" ".*\\.bash" ".*\\.ksh" ".*\\.zsh"))
                                 ;; ("SH" . (sh ".*\\.sh" ".*\\.SH" ".*\\.bsh" ".*\\.bash" ".*\\.ksh" ".*\\.zsh"))
                                 ;; ("bsh" . (sh ".*\\.sh" ".*\\.SH" ".*\\.bsh" ".*\\.bash" ".*\\.ksh" ".*\\.zsh"))
                                 ;; ("bash" . (sh ".*\\.sh" ".*\\.SH" ".*\\.bsh" ".*\\.bash" ".*\\.ksh" ".*\\.zsh"))
                                 ;; ("ksh" . (sh ".*\\.sh" ".*\\.SH" ".*\\.bsh" ".*\\.bash" ".*\\.ksh" ".*\\.zsh"))
                                 ;; ("zsh" . (sh ".*\\.sh" ".*\\.SH" ".*\\.bsh" ".*\\.bash" ".*\\.ksh" ".*\\.zsh"))
                                 ("sl" . (slang ".*\\.sl"))
                                 ("sml" . (sml ".*\\.sml" ".*\\.sig" ".*\\.fun"))
                                 ("sig" . (sml ".*\\.sml" ".*\\.sig" ".*\\.fun"))
                                 ("sql" . (sql ".*\\.sql"))
                                 ("tcl" . (tcl ".*\\.tk" ".*\\.wish" ".*\\.itcl" ".*\\.tcl"))
                                 ("tk" . (tcl ".*\\.tcl" ".*\\.tk" ".*\\.wish" ".*\\.itcl"))
                                 ("wish" . (tcl ".*\\.tcl" ".*\\.tk" ".*\\.wish" ".*\\.itcl"))
                                 ("itcl" . (tcl ".*\\.tcl" ".*\\.tk" ".*\\.wish" ".*\\.itcl"))
                                 ("tex" . (tex ".*\\.tex" ".*\\.aux" ".*\\.toc"))
                                 ("vr" . (vera ".*\\.vr" ".*\\.vri" ".*\\.vrh"))
                                 ("vri" . (vera ".*\\.vr" ".*\\.vri" ".*\\.vrh"))
                                 ("vrh" . (vera ".*\\.vr" ".*\\.vri" ".*\\.vrh"))
                                 ("v" . (verilog ".*\\.v"))
                                 ("vhdl" . (vhdl ".*\\.vhdl" ".*\\.vhd"))
                                 ("vhd" . (vhdl ".*\\.vhdl" ".*\\.vhd"))
                                 ("vim" . (vim ".*\\.vim"))
                                 ("adb" . (ada ".*\\.Ada" ".*\\.adb" ".*\\.ads" ".*\\.ada"))
                                 ("ads" . (ada ".*\\.Ada" ".*\\.adb" ".*\\.ads" ".*\\.ada"))
                                 ("Ada" . (ada ".*\\.adb" ".*\\.ads" ".*\\.Ada"))
                                 ("ant" . (ant ".*\\.ant"))
                                 ("clj" . (clojure ".*\\.clj"))
                                 ("coffee" . (coffeescript ".*\\.coffee"))
                                 ("inl" . (cpp ".*\\.inl"))
                                 ("css" . (css ".*\\.css"))
                                 ("ctags" . (ctags ".*\\.ctags"))
                                 ("d" . (d ".*\\.d" ".*\\.di"))
                                 ("di" . (d ".*\\.d" ".*\\.di"))
                                 ("diff" . (diff ".*\\.diff" ".*\\.patch"))
                                 ("patch" . (diff ".*\\.diff" ".*\\.patch"))
                                 ("dts" . (dts ".*\\.dts" ".*\\.dtsi"))
                                 ("dtsi" . (dts ".*\\.dts" ".*\\.dtsi"))
                                 ("elm" . (elm ".*\\.elm"))
                                 ("fal" . (falcon ".*\\.fal" ".*\\.ftd"))
                                 ("ftd" . (falcon ".*\\.fal" ".*\\.ftd"))
                                 ("f03" . (fortran ".*\\.f03" ".*\\.f08" ".*\\.f15"))
                                 ("f08" . (fortran ".*\\.f03" ".*\\.f08" ".*\\.f15"))
                                 ("f15" . (fortran ".*\\.f03" ".*\\.f08" ".*\\.f15"))
                                 ("gdb" . (gdbinit ".*\\.gdb"))
                                 ("go" . (go ".*\\.go"))
                                 ("json" . (json ".*\\.json"))
                                 ("m4" . (m4 ".*\\.m4" ".*\\.spt"))
                                 ("spt" . (m4 ".*\\.m4" ".*\\.spt"))
                                 ("mm" . (objectivec ".*\\.mm" ".*\\.m" ".*\\.h"))
                                 ("m" . (objectivec ".*\\.mm" ".*\\.m" ".*\\.h"))
                                 ("h" . (objectivec ".*\\.mm" ".*\\.m" ".*\\.h"))
                                 ("aug" . (ocaml ".*\\.aug"))
                                 ("ph" . (perl ".*\\.ph"))
                                 ("p6" . (perl6 ".*\\.p6" ".*\\.pm6" ".*\\.pm" ".*\\.pl6"))
                                 ("pm6" . (perl6 ".*\\.p6" ".*\\.pm6" ".*\\.pm" ".*\\.pl6"))
                                 ("pm" . (perl6 ".*\\.p6" ".*\\.pm6" ".*\\.pm" ".*\\.pl6"))
                                 ("pl6" . (perl6 ".*\\.p6" ".*\\.pm6" ".*\\.pm" ".*\\.pl6"))
                                 ("php4" . (php ".*\\.php4" ".*\\.php5" ".*\\.php7"))
                                 ("php5" . (php ".*\\.php4" ".*\\.php5" ".*\\.php7"))
                                 ("php7" . (php ".*\\.php4" ".*\\.php5" ".*\\.php7"))
                                 ("r" . (r ".*\\.r" ".*\\.R" ".*\\.s" ".*\\.q"))
                                 ("R" . (r ".*\\.r" ".*\\.R" ".*\\.s" ".*\\.q"))
                                 ("s" . (r ".*\\.r" ".*\\.R" ".*\\.s" ".*\\.q"))
                                 ("q" . (r ".*\\.r" ".*\\.R" ".*\\.s" ".*\\.q"))
                                 ("rest" . (restructuredtext ".*\\.rest" ".*\\.reST" ".*\\.rst"))
                                 ("reST" . (restructuredtext ".*\\.rest" ".*\\.reST" ".*\\.rst"))
                                 ("rst" . (restructuredtext ".*\\.rest" ".*\\.reST" ".*\\.rst"))
                                 ("rs" . (rust ".*\\.rs" ".*\\.rc"))
                                 ("ash" . (sh ".*\\.ash"))
                                 ("sv" . (systemverilog ".*\\.svi" ".*\\.sv" ".*\\.svh"))
                                 ("svh" . (systemverilog ".*\\.svi" ".*\\.sv" ".*\\.svh"))
                                 ("svi" . (systemverilog ".*\\.sv" ".*\\.svh" ".*\\.svi"))
                                 ("vba" . (vim ".*\\.vba"))
                                 ("rc" . (windres ".*\\.rc"))
                                 ("zep" . (zephir ".*\\.zep"))
                                 ("abap" . (abap ".*\\.abap"))
                                 ("G" . (antlr ".*\\.G" ".*\\.g"))
                                 ("g" . (antlr ".*\\.G" ".*\\.g"))
                                 ("as" . (actionscript3 ".*\\.as"))
                                 ("ada" . (ada ".*\\.adb" ".*\\.ads" ".*\\.ada"))
                                 ("applescript" . (applescript ".*\\.applescript"))
                                 ("aj" . (aspectj ".*\\.aj"))
                                 ("aspx" . (aspx-cs ".*\\.aspx" ".*\\.asax" ".*\\.ascx" ".*\\.ashx" ".*\\.asmx" ".*\\.axd"))
                                 ("asax" . (aspx-cs ".*\\.aspx" ".*\\.asax" ".*\\.ascx" ".*\\.ashx" ".*\\.asmx" ".*\\.axd"))
                                 ("ascx" . (aspx-cs ".*\\.aspx" ".*\\.asax" ".*\\.ascx" ".*\\.ashx" ".*\\.asmx" ".*\\.axd"))
                                 ("ashx" . (aspx-cs ".*\\.aspx" ".*\\.asax" ".*\\.ascx" ".*\\.ashx" ".*\\.asmx" ".*\\.axd"))
                                 ("asmx" . (aspx-cs ".*\\.aspx" ".*\\.asax" ".*\\.ascx" ".*\\.ashx" ".*\\.asmx" ".*\\.axd"))
                                 ("axd" . (aspx-cs ".*\\.aspx" ".*\\.asax" ".*\\.ascx" ".*\\.ashx" ".*\\.asmx" ".*\\.axd"))
                                 ("asy" . (asymptote ".*\\.asy"))
                                 ("au3" . (autoit ".*\\.au3"))
                                 ("bug" . (bugs ".*\\.bug"))
                                 ("sh" . (bash ".*\\.sh" ".*\\.ksh" ".*\\.bash" ".*\\.ebuild" ".*\\.eclass"))
                                 ("ksh" . (bash ".*\\.sh" ".*\\.ksh" ".*\\.bash" ".*\\.ebuild" ".*\\.eclass"))
                                 ("bash" . (bash ".*\\.sh" ".*\\.ksh" ".*\\.bash" ".*\\.ebuild" ".*\\.eclass"))
                                 ("ebuild" . (bash ".*\\.sh" ".*\\.ksh" ".*\\.bash" ".*\\.ebuild" ".*\\.eclass"))
                                 ("eclass" . (bash ".*\\.sh" ".*\\.ksh" ".*\\.bash" ".*\\.ebuild" ".*\\.eclass"))
                                 ("bat" . (bat ".*\\.bat" ".*\\.cmd"))
                                 ("cmd" . (bat ".*\\.bat" ".*\\.cmd"))
                                 ("bmx" . (blitzmax ".*\\.bmx"))
                                 ("boo" . (boo ".*\\.boo"))
                                 ("bro" . (bro ".*\\.bro"))
                                 ("cbl" . (cobolfree ".*\\.cbl" ".*\\.CBL"))
                                 ("CBL" . (cobolfree ".*\\.cbl" ".*\\.CBL"))
                                 ("cpy" . (cobol ".*\\.cob" ".*\\.COB" ".*\\.cpy" ".*\\.CPY"))
                                 ("CPY" . (cobol ".*\\.cob" ".*\\.COB" ".*\\.cpy" ".*\\.CPY"))
                                 ("cu" . (cuda ".*\\.cu" ".*\\.cuh"))
                                 ("cuh" . (cuda ".*\\.cu" ".*\\.cuh"))
                                 ("ceylon" . (ceylon ".*\\.ceylon"))
                                 ("cfm" . (cfm ".*\\.cfm" ".*\\.cfml" ".*\\.cfc"))
                                 ("cfml" . (cfm ".*\\.cfm" ".*\\.cfml" ".*\\.cfc"))
                                 ("cfc" . (cfm ".*\\.cfm" ".*\\.cfml" ".*\\.cfc"))
                                 ("cl" . (common-lisp ".*\\.cl" ".*\\.lisp" ".*\\.el"))
                                 ("lisp" . (common-lisp ".*\\.cl" ".*\\.lisp" ".*\\.el"))
                                 ("el" . (common-lisp ".*\\.cl" ".*\\.lisp" ".*\\.el"))
                                 ("v" . (coq ".*\\.v"))
                                 ("croc" . (croc ".*\\.croc"))
                                 ("tcsh" . (csh ".*\\.tcsh" ".*\\.csh"))
                                 ("csh" . (csh ".*\\.tcsh" ".*\\.csh"))
                                 ("pyx" . (cython ".*\\.pyx" ".*\\.pxd" ".*\\.pxi"))
                                 ("pxd" . (cython ".*\\.pyx" ".*\\.pxd" ".*\\.pxi"))
                                 ("pxi" . (cython ".*\\.pyx" ".*\\.pxd" ".*\\.pxi"))
                                 ("dart" . (dart ".*\\.dart"))
                                 ("dg" . (dg ".*\\.dg"))
                                 ("duel" . (duel ".*\\.duel" ".*\\.jbst"))
                                 ("jbst" . (duel ".*\\.duel" ".*\\.jbst"))
                                 ("dylan" . (dylan ".*\\.dylan" ".*\\.dyl" ".*\\.intr"))
                                 ("dyl" . (dylan ".*\\.dylan" ".*\\.dyl" ".*\\.intr"))
                                 ("intr" . (dylan ".*\\.dylan" ".*\\.dyl" ".*\\.intr"))
                                 ("ecl" . (ecl ".*\\.ecl"))
                                 ("ec" . (ec ".*\\.ec" ".*\\.eh"))
                                 ("eh" . (ec ".*\\.ec" ".*\\.eh"))
                                 ("erb" . (erb ".*\\.erb"))
                                 ("ex" . (elixir ".*\\.ex" ".*\\.exs"))
                                 ("exs" . (elixir ".*\\.ex" ".*\\.exs"))
                                 ("es" . (erlang ".*\\.erl" ".*\\.hrl" ".*\\.es" ".*\\.escript"))
                                 ("escript" . (erlang ".*\\.erl" ".*\\.hrl" ".*\\.es" ".*\\.escript"))
                                 ("evoque" . (evoque ".*\\.evoque"))
                                 ("fs" . (fsharp ".*\\.fs" ".*\\.fsi"))
                                 ("fsi" . (fsharp ".*\\.fs" ".*\\.fsi"))
                                 ("factor" . (factor ".*\\.factor"))
                                 ("fy" . (fancy ".*\\.fy" ".*\\.fancypack"))
                                 ("fancypack" . (fancy ".*\\.fy" ".*\\.fancypack"))
                                 ("fan" . (fantom ".*\\.fan"))
                                 ("flx" . (felix ".*\\.flx" ".*\\.flxh"))
                                 ("flxh" . (felix ".*\\.flx" ".*\\.flxh"))
                                 ("F" . (fortran ".*\\.f" ".*\\.f90" ".*\\.F" ".*\\.F90"))
                                 ("F90" . (fortran ".*\\.f" ".*\\.f90" ".*\\.F" ".*\\.F90"))
                                 ("s" . (gas ".*\\.s" ".*\\.S"))
                                 ("S" . (gas ".*\\.s" ".*\\.S"))
                                 ("vert" . (glsl ".*\\.vert" ".*\\.frag" ".*\\.geo"))
                                 ("frag" . (glsl ".*\\.vert" ".*\\.frag" ".*\\.geo"))
                                 ("geo" . (glsl ".*\\.vert" ".*\\.frag" ".*\\.geo"))
                                 ("kid" . (genshi ".*\\.kid"))
                                 ("feature" . (gherkin ".*\\.feature"))
                                 ("plot" . (gnuplot ".*\\.plot" ".*\\.plt"))
                                 ("plt" . (gnuplot ".*\\.plot" ".*\\.plt"))
                                 ("gdc" . (gooddata-cl ".*\\.gdc"))
                                 ("gs" . (gosu ".*\\.gs" ".*\\.gsx" ".*\\.gsp" ".*\\.vark"))
                                 ("gsx" . (gosu ".*\\.gs" ".*\\.gsx" ".*\\.gsp" ".*\\.vark"))
                                 ("gsp" . (gosu ".*\\.gs" ".*\\.gsx" ".*\\.gsp" ".*\\.vark"))
                                 ("vark" . (gosu ".*\\.gs" ".*\\.gsx" ".*\\.gsp" ".*\\.vark"))
                                 ("groovy" . (groovy ".*\\.groovy"))
                                 ("gst" . (gst ".*\\.gst"))
                                 ("hx" . (haxe ".*\\.hx"))
                                 ("haml" . (haml ".*\\.haml"))
                                 ("hs" . (haskell ".*\\.hs" ".*\\.lhs"))
                                 ("lhs" . (haskell ".*\\.hs" ".*\\.lhs"))
                                 ("hxml" . (hxml ".*\\.hxml"))
                                 ("hy" . (hybris ".*\\.hy" ".*\\.hyb"))
                                 ("hyb" . (hybris ".*\\.hy" ".*\\.hyb"))
                                 ("pro" . (idl ".*\\.pro"))
                                 ("io" . (io ".*\\.io"))
                                 ("ik" . (ioke ".*\\.ik"))
                                 ("jag" . (jags ".*\\.jag" ".*\\.bug"))
                                 ("bug" . (jags ".*\\.jag" ".*\\.bug"))
                                 ("jade" . (jade ".*\\.jade"))
                                 ("jsp" . (jsp ".*\\.jsp"))
                                 ("jl" . (julia ".*\\.jl"))
                                 ("kk" . (koka ".*\\.kk" ".*\\.kki"))
                                 ("kki" . (koka ".*\\.kk" ".*\\.kki"))
                                 ("kt" . (kotlin ".*\\.kt"))
                                 ("ll" . (llvm ".*\\.ll"))
                                 ("lasso" . (lasso ".*\\.lasso"))
                                 ("lhs" . (literate-haskell ".*\\.lhs"))
                                 ("ls" . (livescript ".*\\.ls"))
                                 ("x" . (logos ".*\\.x" ".*\\.xi" ".*\\.xm" ".*\\.xmi"))
                                 ("xi" . (logos ".*\\.x" ".*\\.xi" ".*\\.xm" ".*\\.xmi"))
                                 ("xm" . (logos ".*\\.x" ".*\\.xi" ".*\\.xm" ".*\\.xmi"))
                                 ("xmi" . (logos ".*\\.x" ".*\\.xi" ".*\\.xm" ".*\\.xmi"))
                                 ("lgt" . (logtalk ".*\\.lgt"))
                                 ("wlua" . (lua ".*\\.lua" ".*\\.wlua"))
                                 ("moo" . (moocode ".*\\.moo"))
                                 ("mxml" . (mxml ".*\\.mxml"))
                                 ("mao" . (mako ".*\\.mao"))
                                 ("m" . (mason ".*\\.m" ".*\\.mhtml" ".*\\.mc" ".*\\.mi"))
                                 ("mhtml" . (mason ".*\\.m" ".*\\.mhtml" ".*\\.mc" ".*\\.mi"))
                                 ("mc" . (mason ".*\\.m" ".*\\.mhtml" ".*\\.mc" ".*\\.mi"))
                                 ("mi" . (mason ".*\\.m" ".*\\.mhtml" ".*\\.mc" ".*\\.mi"))
                                 ("mo" . (modelica ".*\\.mo"))
                                 ("mod" . (modula2 ".*\\.mod"))
                                 ("monkey" . (monkey ".*\\.monkey"))
                                 ("moon" . (moonscript ".*\\.moon"))
                                 ("mu" . (mupad ".*\\.mu"))
                                 ("myt" . (myghty ".*\\.myt"))
                                 ("asm" . (nasm ".*\\.asm" ".*\\.ASM"))
                                 ("ASM" . (nasm ".*\\.asm" ".*\\.ASM"))
                                 ("nsi" . (nsis ".*\\.nsi" ".*\\.nsh"))
                                 ("nsh" . (nsis ".*\\.nsi" ".*\\.nsh"))
                                 ("n" . (nemerle ".*\\.n"))
                                 ("lsp" . (newlisp ".*\\.lsp" ".*\\.nl"))
                                 ("nl" . (newlisp ".*\\.lsp" ".*\\.nl"))
                                 ("ns2" . (newspeak ".*\\.ns2"))
                                 ("nim" . (nimrod ".*\\.nim" ".*\\.nimrod"))
                                 ("nimrod" . (nimrod ".*\\.nim" ".*\\.nimrod"))
                                 ("mll" . (ocaml ".*\\.ml" ".*\\.mli" ".*\\.mll" ".*\\.mly"))
                                 ("mly" . (ocaml ".*\\.ml" ".*\\.mli" ".*\\.mll" ".*\\.mly"))
                                 ("mm" . (objective-cpp ".*\\.mm" ".*\\.hh"))
                                 ("hh" . (objective-cpp ".*\\.mm" ".*\\.hh"))
                                 ("m" . (objective-c ".*\\.m" ".*\\.h"))
                                 ("h" . (objective-c ".*\\.m" ".*\\.h"))
                                 ("j" . (objective-j ".*\\.j"))
                                 ("m" . (octave ".*\\.m"))
                                 ("ooc" . (ooc ".*\\.ooc"))
                                 ("opa" . (opa ".*\\.opa"))
                                 ("p" . (openedge ".*\\.p" ".*\\.cls"))
                                 ("cls" . (openedge ".*\\.p" ".*\\.cls"))
                                 ("ps" . (postscript ".*\\.ps" ".*\\.eps"))
                                 ("eps" . (postscript ".*\\.ps" ".*\\.eps"))
                                 ("ps1" . (powershell ".*\\.ps1"))
                                 ("prolog" . (prolog ".*\\.prolog" ".*\\.pro" ".*\\.pl"))
                                 ("pro" . (prolog ".*\\.prolog" ".*\\.pro" ".*\\.pl"))
                                 ("pl" . (prolog ".*\\.prolog" ".*\\.pro" ".*\\.pl"))
                                 ("pyw" . (python ".*\\.py" ".*\\.pyw" ".*\\.sc" ".*\\.tac" ".*\\.sage"))
                                 ("sc" . (python ".*\\.py" ".*\\.pyw" ".*\\.sc" ".*\\.tac" ".*\\.sage"))
                                 ("tac" . (python ".*\\.py" ".*\\.pyw" ".*\\.sc" ".*\\.tac" ".*\\.sage"))
                                 ("sage" . (python ".*\\.py" ".*\\.pyw" ".*\\.sc" ".*\\.tac" ".*\\.sage"))
                                 ("qml" . (qml ".*\\.qml"))
                                 ("r" . (rebol ".*\\.r" ".*\\.r3"))
                                 ("r3" . (rebol ".*\\.r" ".*\\.r3"))
                                 ("rhtml" . (rhtml ".*\\.rhtml"))
                                 ("rkt" . (racket ".*\\.rkt" ".*\\.rktl"))
                                 ("rktl" . (racket ".*\\.rkt" ".*\\.rktl"))
                                 ("rl" . (ragel ".*\\.rl"))
                                 ("cw" . (redcode ".*\\.cw"))
                                 ("robot" . (robotframework ".*\\.robot"))
                                 ("rbw" . (ruby ".*\\.rb" ".*\\.rbw" ".*\\.rake" ".*\\.gemspec" ".*\\.rbx" ".*\\.duby"))
                                 ("rake" . (ruby ".*\\.rb" ".*\\.rbw" ".*\\.rake" ".*\\.gemspec" ".*\\.rbx" ".*\\.duby"))
                                 ("gemspec" . (ruby ".*\\.rb" ".*\\.rbw" ".*\\.rake" ".*\\.gemspec" ".*\\.rbx" ".*\\.duby"))
                                 ("rbx" . (ruby ".*\\.rb" ".*\\.rbw" ".*\\.rake" ".*\\.gemspec" ".*\\.rbx" ".*\\.duby"))
                                 ("duby" . (ruby ".*\\.rb" ".*\\.rbw" ".*\\.rake" ".*\\.gemspec" ".*\\.rbx" ".*\\.duby"))
                                 ("rc" . (rust ".*\\.rs" ".*\\.rc"))
                                 ("S" . (s ".*\\.S" ".*\\.R"))
                                 ("R" . (s ".*\\.S" ".*\\.R"))
                                 ("scala" . (scala ".*\\.scala"))
                                 ("scaml" . (scaml ".*\\.scaml"))
                                 ("ss" . (scheme ".*\\.scm" ".*\\.ss"))
                                 ("sci" . (scilab ".*\\.sci" ".*\\.sce" ".*\\.tst"))
                                 ("sce" . (scilab ".*\\.sci" ".*\\.sce" ".*\\.tst"))
                                 ("tst" . (scilab ".*\\.sci" ".*\\.sce" ".*\\.tst"))
                                 ("st" . (smalltalk ".*\\.st"))
                                 ("tpl" . (smarty ".*\\.tpl"))
                                 ("fun" . (sml ".*\\.sml" ".*\\.sig" ".*\\.fun"))
                                 ("snobol" . (snobol ".*\\.snobol"))
                                 ("sp" . (sourcepawn ".*\\.sp"))
                                 ("spt" . (spitfire ".*\\.spt"))
                                 ("ssp" . (ssp ".*\\.ssp"))
                                 ("stan" . (stan ".*\\.stan"))
                                 ("aux" . (tex ".*\\.tex" ".*\\.aux" ".*\\.toc"))
                                 ("toc" . (tex ".*\\.tex" ".*\\.aux" ".*\\.toc"))
                                 ("tea" . (tea ".*\\.tea"))
                                 ("treetop" . (treetop ".*\\.treetop" ".*\\.tt"))
                                 ("tt" . (treetop ".*\\.treetop" ".*\\.tt"))
                                 ("ts" . (typescript ".*\\.ts"))
                                 ("u" . (urbiscript ".*\\.u"))
                                 ("vb" . (vbnet ".*\\.vb" ".*\\.bas"))
                                 ("bas" . (vbnet ".*\\.vb" ".*\\.bas"))
                                 ("rpf" . (vgl ".*\\.rpf"))
                                 ("vala" . (vala ".*\\.vala" ".*\\.vapi"))
                                 ("vapi" . (vala ".*\\.vala" ".*\\.vapi"))
                                 ("vm" . (velocity ".*\\.vm" ".*\\.fhtml"))
                                 ("fhtml" . (velocity ".*\\.vm" ".*\\.fhtml"))
                                 ("PRG" . (xbase ".*\\.PRG" ".*\\.prg"))
                                 ("prg" . (xbase ".*\\.PRG" ".*\\.prg"))
                                 ("xqy" . (xquery ".*\\.xqy" ".*\\.xquery" ".*\\.xq" ".*\\.xql" ".*\\.xqm"))
                                 ("xquery" . (xquery ".*\\.xqy" ".*\\.xquery" ".*\\.xq" ".*\\.xql" ".*\\.xqm"))
                                 ("xq" . (xquery ".*\\.xqy" ".*\\.xquery" ".*\\.xq" ".*\\.xql" ".*\\.xqm"))
                                 ("xql" . (xquery ".*\\.xqy" ".*\\.xquery" ".*\\.xq" ".*\\.xql" ".*\\.xqm"))
                                 ("xqm" . (xquery ".*\\.xqy" ".*\\.xquery" ".*\\.xq" ".*\\.xql" ".*\\.xqm"))
                                 ("xsl" . (xslt ".*\\.xsl" ".*\\.xslt" ".*\\.xpl"))
                                 ("xslt" . (xslt ".*\\.xsl" ".*\\.xslt" ".*\\.xpl"))
                                 ("xpl" . (xslt ".*\\.xsl" ".*\\.xslt" ".*\\.xpl"))
                                 ("xtend" . (xtend ".*\\.xtend"))
                                 )
  "Maps file suffixes to regexps used as source-patterns when guessing a
project config from the currently opened file in the active buffer.")

(defvar lazy-config-save-location (expand-file-name "projects.el" (file-name-as-directory lazy-global-cache-root))
  "Where to save project configs in elisp. If this is a filename project
configs will be written to that file. If it is a directory an elisp
file with the projects name will be created in that directory.")

(defvar lazy-language-tag-systems '((c . (gtags gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (yacc . (gtags gtags+universal-ctags gtags+exuberant-ctags))
                                    (asm . (gtags gtags+universal-ctags gtags+exuberant-ctags))
                                    (java . (gtags gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (cpp . (gtags gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (php . (gtags gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (asp . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (awk . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (basic . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (beta . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (csharp . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (cobol . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (dosbatch . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (eiffel . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (erlang . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (flex . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (fortran . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (html . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (javascript . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (elisp . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (lisp . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (lua . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (make . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (matlab . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (ocaml . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (pascal . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (perl . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (python . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (rexx . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (ruby . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (scheme . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (sh . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (slang . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (sml . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (sql . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (tcl . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (tex . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (vera . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (verilog . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (vhdl . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (vim . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (ada . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (ant . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (clojure . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (coffeescript . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (css . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (ctags . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (d . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (diff . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (dts . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (elm . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (falcon . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (gdbinit . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (go . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (json . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (m4 . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (objectivec . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (perl6 . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (r . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (restructuredtext . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (rust . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (systemverilog . (gtags+universal-ctags gtags+exuberant-ctags gtags+pygments))
                                    (windres . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (zephir . (gtags+universal-ctags gtags+exuberant-ctags))
                                    (abap . (gtags+pygments))
                                    (antlr . (gtags+pygments))
                                    (actionscript3 . (gtags+pygments))
                                    (applescript . (gtags+pygments))
                                    (aspectj . (gtags+pygments))
                                    (aspx-cs . (gtags+pygments))
                                    (asymptote . (gtags+pygments))
                                    (autoit . (gtags+pygments))
                                    (bugs . (gtags+pygments))
                                    (bash . (gtags+pygments))
                                    (bat . (gtags+pygments))
                                    (blitzmax . (gtags+pygments))
                                    (boo . (gtags+pygments))
                                    (bro . (gtags+pygments))
                                    (cobolfree . (gtags+pygments))
                                    (cuda . (gtags+pygments))
                                    (ceylon . (gtags+pygments))
                                    (cfm . (gtags+pygments))
                                    (common-lisp . (gtags+pygments))
                                    (coq . (gtags+pygments))
                                    (croc . (gtags+pygments))
                                    (csh . (gtags+pygments))
                                    (cython . (gtags+pygments))
                                    (dart . (gtags+pygments))
                                    (dg . (gtags+pygments))
                                    (duel . (gtags+pygments))
                                    (dylan . (gtags+pygments))
                                    (ecl . (gtags+pygments))
                                    (ec . (gtags+pygments))
                                    (erb . (gtags+pygments))
                                    (elixir . (gtags+pygments))
                                    (evoque . (gtags+pygments))
                                    (fsharp . (gtags+pygments))
                                    (factor . (gtags+pygments))
                                    (fancy . (gtags+pygments))
                                    (fantom . (gtags+pygments))
                                    (felix . (gtags+pygments))
                                    (gas . (gtags+pygments))
                                    (glsl . (gtags+pygments))
                                    (genshi . (gtags+pygments))
                                    (gherkin . (gtags+pygments))
                                    (gnuplot . (gtags+pygments))
                                    (gooddata-cl . (gtags+pygments))
                                    (gosu . (gtags+pygments))
                                    (groovy . (gtags+pygments))
                                    (gst . (gtags+pygments))
                                    (haxe . (gtags+pygments))
                                    (haml . (gtags+pygments))
                                    (haskell . (gtags+pygments))
                                    (hxml . (gtags+pygments))
                                    (hybris . (gtags+pygments))
                                    (idl . (gtags+pygments))
                                    (io . (gtags+pygments))
                                    (ioke . (gtags+pygments))
                                    (jags . (gtags+pygments))
                                    (jade . (gtags+pygments))
                                    (jsp . (gtags+pygments))
                                    (julia . (gtags+pygments))
                                    (koka . (gtags+pygments))
                                    (kotlin . (gtags+pygments))
                                    (llvm . (gtags+pygments))
                                    (lasso . (gtags+pygments))
                                    (literate-haskell . (gtags+pygments))
                                    (livescript . (gtags+pygments))
                                    (logos . (gtags+pygments))
                                    (logtalk . (gtags+pygments))
                                    (moocode . (gtags+pygments))
                                    (mxml . (gtags+pygments))
                                    (mako . (gtags+pygments))
                                    (mason . (gtags+pygments))
                                    (modelica . (gtags+pygments))
                                    (modula2 . (gtags+pygments))
                                    (monkey . (gtags+pygments))
                                    (moonscript . (gtags+pygments))
                                    (mupad . (gtags+pygments))
                                    (myghty . (gtags+pygments))
                                    (nasm . (gtags+pygments))
                                    (nsis . (gtags+pygments))
                                    (nemerle . (gtags+pygments))
                                    (newlisp . (gtags+pygments))
                                    (newspeak . (gtags+pygments))
                                    (nimrod . (gtags+pygments))
                                    (objective-cpp . (gtags+pygments))
                                    (objective-c . (gtags+pygments))
                                    (objective-j . (gtags+pygments))
                                    (octave . (gtags+pygments))
                                    (ooc . (gtags+pygments))
                                    (opa . (gtags+pygments))
                                    (openedge . (gtags+pygments))
                                    (postscript . (gtags+pygments))
                                    (powershell . (gtags+pygments))
                                    (prolog . (gtags+pygments))
                                    (qml . (gtags+pygments))
                                    (rebol . (gtags+pygments))
                                    (rhtml . (gtags+pygments))
                                    (racket . (gtags+pygments))
                                    (ragel . (gtags+pygments))
                                    (redcode . (gtags+pygments))
                                    (robotframework . (gtags+pygments))
                                    (s . (gtags+pygments))
                                    (scala . (gtags+pygments))
                                    (scaml . (gtags+pygments))
                                    (scilab . (gtags+pygments))
                                    (smalltalk . (gtags+pygments))
                                    (smarty . (gtags+pygments))
                                    (snobol . (gtags+pygments))
                                    (sourcepawn . (gtags+pygments))
                                    (spitfire . (gtags+pygments))
                                    (ssp . (gtags+pygments))
                                    (stan . (gtags+pygments))
                                    (tea . (gtags+pygments))
                                    (treetop . (gtags+pygments))
                                    (typescript . (gtags+pygments))
                                    (urbiscript . (gtags+pygments))
                                    (vbnet . (gtags+pygments))
                                    (vgl . (gtags+pygments))
                                    (vala . (gtags+pygments))
                                    (velocity . (gtags+pygments))
                                    (xbase . (gtags+pygments))
                                    (xquery . (gtags+pygments))
                                    (xslt . (gtags+pygments))
                                    (xtend . (gtags+pygments))
                                    )
  "Defines which tagging system to use for which language.

Only gtags, gtags+exuberant-ctags, gtags+universal-ctags and gtags+pygments
are implemented.")

(defvar lazy-thing-selector 'symbol)

(defvar lazy-completions-cache (make-hash-table :test 'equal))

(defvar lazy-project-list (make-hash-table :test 'equal))

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
  "Assert that a lazy project is currently active.

If TRY-GUESSING is t this will guess a project configuration
and ask the user about loading it.

If TRY-GUESSING is nil and no lazy project is active, this
will throw an error.

See also `lazy-guess-alist'."
  (unless lazy-name
    (let* ((continue-prevent-restore t)
           (guessed-alist (when try-guessing
                            (lazy-guess-alist t t))))
      (cond ((and guessed-alist
                  try-guessing
                  (y-or-n-p (concat "Load project " (cadr (assoc 'name guessed-alist)) "? ")))
             (unless (gethash (cadr (assoc 'name guessed-alist)) lazy-project-list nil)
               (lazy-def (cadr (assoc 'name guessed-alist)) guessed-alist))
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

(defun lazy-project-names ()
  (let ((names nil))
    (maphash (lambda (k v) (when k (add-to-list 'names k))) lazy-project-list)
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

(defmacro lazy-with-or-without-marker (marker &rest body)
  `(let ((marker ,marker))
     (if (markerp marker)
         (with-current-buffer (marker-buffer marker)
           (save-excursion
             (goto-char (marker-position marker))
             ,@body))
       ,@body)))

(defmacro lazy-with-marker (marker &rest body)
  `(let ((marker ,marker))
     (if (markerp marker)
         (with-current-buffer (marker-buffer marker)
           (save-excursion
             (goto-char (marker-position marker))
             ,@body)))))

(defun lazy-buffer-lang (&optional buffer)
  (when (buffer-file-name (or buffer (current-buffer)))
    (cadr (assoc (car (last (split-string (buffer-file-name (or buffer (current-buffer))) "\\.")))
                 lazy-src-pattern-table))))

;; ---------------------------------------------------------------------
;; Project Configuration
;; ---------------------------------------------------------------------

(defun lazy-search-projects ()
  "Search for projects by evaluating `lazy-config-save-location' as if it were an elisp file. Falls back to
trying to evaluate `lazy-global-cache-root'/projects.el"
  (cond ((file-exists-p lazy-config-save-location)
         (load lazy-config-save-location))
        ((file-exists-p (expand-file-name "projects.el" (file-name-as-directory (expand-file-name lazy-global-cache-root))))
         (load (expand-file-name "projects.el" (file-name-as-directory (expand-file-name lazy-global-cache-root)))))))

(cl-defun lazy-find-alist (&optional proj-name (inherit t))
  "Get a projects config-alist from the global projects hashmap."
  (when (or proj-name (setq proj-name lazy-name))
    (let* ((child (gethash proj-name lazy-project-list))
           (alist child))
      (while (and (assoc 'parent child)
                  inherit)
        (setq child (gethash (cadr (assoc 'parent child)) lazy-project-list)
              alist (append alist (cl-remove-if (lambda (x) (cl-some (lambda (y) (eq (cl-first x) (cl-first y))) alist)) child))))
      alist)))

(cl-defun lazy-get-config-val (key &optional proj-name (inherit t) (proj-alist nil))
  "Finds the value associated with KEY. A project PROJ can optionally
be specified.

If the third argument INHERIT is non-nil, all parents will queried
for the KEY and the first value that is found is returned.

See also `lazy-var-before-get-functions', `lazy-set-config-val'."
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
  "Set the value associated with KEY to VALUE in config of project NAME.

See also `lazy-get-config-val'."
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
        (puthash proj-name new-alist lazy-project-list)
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
    (when (gethash proj-name lazy-project-list)
      (setq result-alist (lazy-alist-union (gethash proj-name lazy-project-list) result-alist)))
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
             (puthash proj-name alist lazy-project-list)
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
      (with-current-buffer (find-file-noselect (expand-file-name (concat proj-name ".el") (expand-file-name lazy-config-save-location)))
        (save-excursion
          (goto-char (or (lazy-find-project-elisp-configuration-in-buffer proj-name) (point-max)))
          (point-marker))))
     ;; find section to save under in single el file
     ((or (and (stringp lazy-config-save-location)
               (or (file-exists-p (expand-file-name lazy-config-save-location))
                   (write-region "" nil (expand-file-name lazy-config-save-location))
                   t)
               (setq save-location lazy-config-save-location))
          (and (or (file-exists-p (expand-file-name "projects.el" (file-name-as-directory (expand-file-name lazy-global-cache-root))))
                   (write-region "" nil (expand-file-name "projects.el" (file-name-as-directory (expand-file-name lazy-global-cache-root))))
                   t)
               (setq save-location (expand-file-name "projects.el" (file-name-as-directory (expand-file-name lazy-global-cache-root))))))
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
    (lazy-with-marker marker
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
  "Save the current lazy project to disk.

See also `lazy-backend-list'."
  (interactive)
  (lazy-assert-proj)
  (lazy-backend-funcall (lazy-detect-backend)
                        'save lazy-name (lazy-find-alist nil nil)))

(defun lazy-insert ()
  "Insert the current lazy project into the current buffer.

See also `lazy-backend-list'."
  (interactive)
  (lazy-assert-proj)
  (cond ((derived-mode-p 'emacs-lisp-mode 'inferior-emacs-lisp-mode)
         (lazy-backend-funcall 'elisp
                               'insert lazy-name (lazy-find-alist nil nil)))
        ((derived-mode-p 'org-mode)
         (lazy-backend-funcall 'org-mode
                               'insert lazy-name (lazy-find-alist nil nil)))))

(cl-defun lazy-create ()
  "Create a new lazy project interactively.

See also `lazy-backend-list' and `lazy-config-buffer'."
  (interactive)
  (if (and (string-equal (buffer-name (current-buffer)) "*lazy: new project*")
           (gethash 'org-mode lazy-backend-list))
      (progn (kill-buffer)
             (lazy-backend-funcall 'org-mode
                                   'buffer :create))
    (lazy-backend-funcall (lazy-detect-backend)
                          'buffer :create)))

(defun lazy-edit (&optional proj-name)
  "Edit the current lazy project interactively.

See also `lazy-backend-list' and `lazy-config-buffer'."
  (interactive)
  (unless proj-name
    (setq proj-name lazy-name))
  (if (not proj-name)
      (call-interactively 'lazy-create)
    (when (and (string-equal (buffer-name (current-buffer)) "*lazy: edit project*")
               (fboundp 'lazy-org-config-save)
               (not (lazy-get-config-val 'org-file lazy-name))
               (y-or-n-p (concat "Create .org file for " lazy-name "? ")))
      (progn (kill-buffer)
             (lazy-org-config-save lazy-name (lazy-find-alist lazy-name t))))
    (lazy-backend-funcall (lazy-detect-backend proj-name)
                          'buffer :edit proj-name)))




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
  (remhash proj-name lazy-project-list)
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
                              (lazy-get-cache-file symbol (lazy-get-config-val 'parent proj-name nil) t))
                          r)
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
  (let ((cachedir (expand-file-name (concat (or (car-safe (lazy-ancestry proj-name)) proj-name) "/" dirname) lazy-global-cache-root)))
    (unless (file-directory-p cachedir)
      (make-directory cachedir t))
    cachedir))

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
    (add-to-list 'lazy-project-history proj-name)
    (message "Loading project %s done" proj-name)))

(defun lazy-load (&optional proj-name)
  "Load PROJ-NAME or ask the user about which project to load.

See also `lazy-load-project'"
  (interactive)
  (let* ((guessed-alist (lazy-guess-alist))
         (names (let ((ns (lazy-project-names)))
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

See also `lazy-close-files', `lazy-close-friends', `lazy-project-history'
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
        (basedir (file-name-as-directory (or (lazy-get-config-val 'basedir proj-name t proj-alist) default-directory)))
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
  "View projects variables."
  (interactive)
  (unless proj-name
    (lazy-assert-proj)
    (setq proj-name lazy-name))
  (unless proj-name
    (error "No project loaded."))
  (let ((b (get-buffer-create "*lazy: status*")))
    (with-current-buffer b
      (kill-region (point-min) (point-max))
      (dolist (v (append lazy-required-vars lazy-optional-vars))
        (insert (format "%-32s = %s\n" (symbol-name (car v)) (lazy-get-config-val (car v) proj-name t)))))
    (when (not (eq b (current-buffer)))
      (display-buffer b)))
  (when (lazy-get-config-val 'basedir proj-name t)
    (error "Missing basedir in %s" proj-name)))

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

(defun lazy-parse-gtags-conf (&optional gtags-conf)
  "A helper function to generate `lazy-language-tag-systems' and `lazy-src-pattern-table' by parsing
a gtags.conf file. Can be given an specific GTAGS-CONF as argument, otherwise it just uses
`lazy-default-gtags-config'.

It will create a new buffer named lazy-parse-gtags-conf-result and insert the generated defvar
statements in there. You need to manually copy, paste and modify those yourself."
  (unless gtags-conf
    (setq gtags-conf lazy-default-gtags-config))
  (with-temp-buffer
    (insert-file-contents-literally lazy-default-gtags-config)
    ;; - I use gnu globals nomenclature 'parser' inside this function, for what is known as 'system'
    ;; in the other gtags related functions, a parser/system is something like gtags itself, or ctags,
    ;; or pygments
    ;; - activeparser denotes the currently active parser section, it is set while going through the lines
    ;; when a line does not start with whitespaces, it can be something like 'builtin-parser' or 'comman-ctags-maps'
    ;; - parserbins is used to accumulate all langmap definitions, it is a hashmap of hashmaps which gets
    ;; initialized below to contain one hashmap for each possible parser, in the end it should contain stuff
    ;; like ("universal-ctags" . ("c++" . ".cpp" ".h" ".cc" ".hh"))
    ;; - parserbins is the raw data, src-patterns and lang-patterns are the actual results that we want, these contain
    ;; the lists specifying which file pattern belongs to which language, and what systems can parse a language, just
    ;; like in the global lazy-src-pattern-table and lazy-language-tag-systems variables
    (let ((activeparser nil)
          (parserbins (make-hash-table :test 'equal))
          ;; (extension . (lang patterns))
          (src-patterns (make-hash-table :test 'equal))
          ;; (lang . (systems))
          (lang-systems (make-hash-table :test 'equal))
          (parser-systems (make-hash-table :test 'equal)))
      (puthash "builtin-parser" (make-hash-table :test 'equal) parserbins)
      (puthash "common-ctags-maps" (make-hash-table :test 'equal) parserbins)
      (puthash "exuberant-ctags" (make-hash-table :test 'equal) parserbins)
      (puthash "universal-ctags" (make-hash-table :test 'equal) parserbins)
      (puthash "pygments-parser" (make-hash-table :test 'equal) parserbins)
      ;; - go throuhg gtags-conf line by line and look at each line if it is either a new parser, or a langmap definition,
      ;; a parser means we just change activeparser to that new parser, a langmap means we parse what language and what
      ;; extensions it defines and put those in parserbins, in the currently active parser hashmap
      (dolist (line (split-string (buffer-string) "\n" t))
        (unless (string-equal (substring line 0 1) "#")
          (cond ((string-match "[\t ]+:langmap=\\(.*\\)" line)
                 (let ((langmap (match-string 1 line)))
                   (dolist (definition (split-string langmap "," t))
                     (let* ((langext (split-string definition ":" t))
                            (lang (replace-regexp-in-string "\\\\" "" (downcase (first langext))))
                            (ext (second langext))
                            (activebin (gethash activeparser parserbins)))
                       (when activebin
                         (puthash lang ext (gethash activeparser parserbins)))))))
                ((string-match "^\\([^ \t\\|:]+\\)" line)
                 (setq activeparser (match-string 0 line))))))
      ;; - now go through parserbins, and for each parser go through all languages with extension and accumulate
      ;; extensions for each language of all the different parsers in src-patterns, and accumulate the parser systems
      ;; for all languges in lang-systems
      (maphash
       (lambda (parser languages)
         (let ((systems (cond ((string-equal parser "builtin-parser")
                               '(gtags))
                              ((string-equal parser "common-ctags-maps")
                               '(gtags+universal-ctags gtags+exuberant-ctags))
                              ((string-equal parser "exuberant-ctags")
                               '(gtags+exuberant-ctags))
                              ((string-equal parser "universal-ctags")
                               '(gtags+universal-ctags))
                              ((string-equal parser "pygments-parser")
                               '(gtags+pygments))))
               (ordering '(gtags+pygments gtags+exuberant-ctags gtags+universal-ctags gtags)))
           (maphash
            (lambda (raw-lang extensions)
              (let ((ext-list (split-string extensions "\\." t))
                    (lang (replace-regexp-in-string (regexp-quote ".")  ""
                           (replace-regexp-in-string (regexp-quote "++") "pp"
                                                     (replace-regexp-in-string "#" "sharp" raw-lang)))))
                (puthash lang (sort (cl-remove-duplicates (append (gethash lang lang-systems) systems))
                                    (lambda (a b) (<= (cl-position b ordering) (cl-position a ordering)))) lang-systems)
                (dolist (ext ext-list)
                  (let* ((lang-ext (concat lang "-" ext))
                         (ext-regexes (mapcar (lambda (e) (concat ".*" (regexp-quote (concat "." e)))) ext-list))
                         (new-regexes (cl-remove-duplicates (append (cddr (gethash lang-ext src-patterns)) ext-regexes) :test 'equal))
                         (lang-regexes (append (list (make-symbol lang)) new-regexes)))
                    (puthash lang-ext (append (list ext) lang-regexes) src-patterns)))))
            languages)))
       parserbins)
      ;; - create a buffer and insert new defvar statements for the lazy-language-tag-systems and lazy-src-pattern-table globals
      (with-current-buffer (get-buffer-create "lazy-parse-gtags-conf-result")
        (erase-buffer)
        (emacs-lisp-mode)
        (insert "(defvar lazy-language-tag-systems '(")
        (maphash (lambda (lang systems)
                   (insert "(" lang " . " (prin1-to-string systems) ")\n"))
                 lang-systems)
        (insert "))\n\n")
        (insert "(defvar lazy-src-pattern-table '(")
        (maphash (lambda (lang line)
                   (let ((ext (car line))
                         (lang-regexes (cdr line)))
                     (insert "(" (prin1-to-string ext) " . " (prin1-to-string lang-regexes) ")\n")))
                 src-patterns)
        (insert "))\n")
        (indent-region (point-min) (point-max))))))

(defun lazy-find-ctags-executable (ctags-type)
  "Find ctags executable for either exuberant-ctags or universal-ctags, specified by CTAGS-TYPE."
  (or (cond ((eq ctags-type 'universal)
             (or (executable-find "ctags-universal")
                 (executable-find "universal-ctags")))
            (t
             (or (executable-find "ctags-exuberant")
                 (executable-find "exuberant-ctags"))))
      (let* ((executable (or (executable-find "ctags") (executable-find "ctags.exe")))
             (stdout (when executable (shell-command-to-string (concat executable " --version")))))
        (when executable
          (cond ((and (eq ctags-type 'universal)
                      (string-match "Universal.*" stdout))
                 executable)
                ((and (eq ctags-type 'exuberant)
                      (string-match "Exuberant.*" stdout))
                 executable)
                (t
                 executable))))))

(defun lazy-update-tags (&optional proj-name proj-alist files debug)
  "Create or update the projects tags database. The current implementation uses gtags together with
universal-ctags, exuberant-ctags and pygments to generate a tags database. It tries to use those in projects
consisting of multiple languages to generate a tags database that contains all symbols from all languages.

The generated tags is used for project specific completions as well as navigating to defintions of symbols.

This function will ignore GTAGSLABEL, GTAGSROOT and GTAGSCONF and specify those itself without overwriting
them. So that it can generate the tags database in a directory under ~/.lazy and have the tags reference paths
relative the the root (/ under linux, c: under windows) directory, so that the tags database can contain
references from multiple places in the filesystem, and not just the projects directory.

Supply PROJ-NAME and/or PROJ-ALIST to apply this to a project other then the currently loaded one. Supply
FILES to specify for which files to update the tags database for.

With DEBUG true this leaves behind buffer(s) named gtags-0,1,... that contain the output of the gtags
commands executed, and it also writes the gtags commands it executes into the *Messages* buffer.

See also `lazy-setup-tags', `lazy-jump-definition' and `lazy-process-group'"
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
        (ctags-exuberant-executable (lazy-find-ctags-executable 'exuberant))
        (ctags-universal-executable (lazy-find-ctags-executable 'universal))
        (pygments-executable (executable-find "pygmentize"))
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
                ((and (or (eq sys 'gtags)
                          (eq sys 'gtags+exuberant-ctags)
                          (eq sys 'gtags+universal-ctags))
                      gtags-executable
                      global-executable
                      ctags-universal-executable)
                 (cl-return (puthash 'gtags+universal-ctags
                                     (append (list f) (gethash 'gtags+universal-ctags sys-files))
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
                                     sys-files)))
                ((and (or (eq sys 'gtags)
                          (eq sys 'gtags+pygments))
                      gtags-executable
                      global-executable
                      pygments-executable)
                 (cl-return (puthash 'gtags+pygments
                                     (append (list f) (gethash 'gtags+pygments sys-files))
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
                             (let ((c (expand-file-name "~/.globalrc") ))
                               (when (file-exists-p c)
                                 c))
                             (let ((c (expand-file-name "gtags.conf" (lazy-get-config-val 'basedir proj-name nil proj-alist))))
                               (when (file-exists-p c)
                                 c))
                             (when (and lazy-default-gtags-config
                                        (file-exists-p (expand-file-name lazy-default-gtags-config)))
                               (expand-file-name lazy-default-gtags-config))
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
      ;; - the following when section put gtags commands to execute into a hashmap, one for each type
      ;; (gtags, exuberant-ctags, etc ) of system
      (when (gethash 'gtags sys-files)
        (puthash 'gtags
                 (concat "cd " gtags-root cmd-seperator
                         "env GTAGSROOT=" gtags-root " "
                         (when gtags-config (concat "GTAGSCONF=" gtags-config " "))
                         "gtags " gtags-dbpath (when debug " -v") " -i -f - " gtags-arguments cmd-seperator)
                 gtags-commands))
      (when (gethash 'gtags+exuberant-ctags sys-files)
        (puthash 'gtags+exuberant-ctags
                 (concat "cd " gtags-root cmd-seperator
                         "env GTAGSLABEL=exuberant-ctags "
                         (when gtags-config (concat "GTAGSCONF=" gtags-config " "))
                         "GTAGSROOT=" gtags-root " "
                         "gtags " gtags-dbpath (when debug " -v") " -i -f - " gtags-arguments cmd-seperator)
                 gtags-commands))
      (when (gethash 'gtags+universal-ctags sys-files)
        (puthash 'gtags+universal-ctags
                 (concat "cd " gtags-root cmd-seperator
                         "env GTAGSLABEL=universal-ctags "
                         (when gtags-config (concat "GTAGSCONF=" gtags-config " "))
                         "GTAGSROOT=" gtags-root " "
                         "gtags " gtags-dbpath (when debug " -v") " -i -f - " gtags-arguments cmd-seperator)
                 gtags-commands))
      (when (gethash 'gtags+pygments sys-files)
        (puthash 'gtags+pygments
                 (concat "cd " gtags-root cmd-seperator
                         "env GTAGSLABEL=pygments-parser "
                         (when gtags-config (concat "GTAGSCONF=" gtags-config " "))
                         "GTAGSROOT=" gtags-root " "
                         "gtags " gtags-dbpath (when debug " -v") " -i -f - " gtags-arguments cmd-seperator)
                 gtags-commands))
      ;; - I define an ordering so I can control which system is allowed to overwrite what other systems, the
      ;; one that comes first in the order can be overwritten by every following system so that means lower
      ;; position in list is lower priority
      ;; - a system generates entries in the tag database, but two system may actually create conflicting
      ;; entries, although this should only happen rarely, I still want higher quality systems to overwrite lower
      ;; quality ones
      (let* ((ordering (list 'gtags+pygments 'gtags+exuberant-ctags 'gtags+universal-ctags 'gtags))
             (commands (cl-loop for sys in ordering
                                if (gethash sys gtags-commands)
                                collect (gethash sys gtags-commands)))
             (inputs (cl-loop for sys in ordering
                              if (gethash sys gtags-commands)
                              collect (concat (mapconcat #'identity (gethash sys sys-files) "\n") "\n"))))
        (lazy-process-group "gtags" commands inputs 'lazy-update-completions-cache (list proj-name) debug)
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
recieves when it acts as process sentinel.

I implemented it mainly to execute multiple gtags calls in the background, each after the other."
  (unless n (setq n 0))
  (if (and (nth n commands)
           (or (not event)
               (string-equal event "finished\n")))
      (let* ((proc-name (concat name "-" (prin1-to-string n)))
             (shell-file-name (if (eq system-type 'windows-nt) (default-value 'shell-file-name) "/bin/sh"))
             (process (start-process-shell-command proc-name (when debug proc-name) (nth n commands)))
             (input (nth n inputs)))
        (when debug (message "%s" (nth n commands)))
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
  "Setup environment for existing tags database. Force current project by giving PROJ-NAME.

This overwrites any existing GTAGSDBPATH and GTAGSROOT environment variables with `setenv'. It
is called almost always before using any of the related tags functionality, like `lazy-jump-definition'
or `lazy-completions'.

See also `lazy-update-tags'."
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
              (cl-find 'gtags+exuberant-ctags proj-systems)
              (cl-find 'gtags+universal-ctags proj-systems)
              (cl-find 'gtags+pygments proj-systems))
      (let* ((gtags-file (file-truename (expand-file-name "GTAGS" (lazy-get-cache-dir nil proj-name))))
             (gtags-file-alternative (expand-file-name "GTAGS" (lazy-get-config-val 'basedir proj-name))))
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
                              ((facep (quote ,sym)) (find-definition-noselect (quote ,sym) 'defface))
                              (((symbol-plist (quote ,sym)) (find-definition-noselect (quote ,sym) nil))))))
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
                                  (featurep sym)
                                  (symbol-plist sym)))
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
  (let* ((cmd "global --match-part=first -Gq -dc \"\"; global --match-part=first -Gq -sc \"\"")
         (completions (split-string (condition-case nil (shell-command-to-string cmd) (error "")) "\n" t))
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

(defvar lazy-completions-table-for-elisp (completion-table-merge
                                          elisp--local-variables-completion-table
                                          (apply-partially #'completion-table-with-predicate
                                                           obarray
                                                           (lambda (sym)
                                                             (or (boundp sym)
                                                                 (fboundp sym)
                                                                 (facep sym)
                                                                 (featurep sym)
                                                                 (symbol-plist sym)))
                                                           'strict)))
(defun lazy-completions-for-elisp (elisp-string)
  (all-completions elisp-string lazy-completions-table-for-elisp))

(defvar lazy-completions-table-for-path (completion-table-in-turn #'completion--embedded-envvar-table
                                                                  #'completion--file-name-table))
(defun lazy-completions-for-path (elisp-string)
  (all-completions elisp-string lazy-completions-table-for-path))

;; (defvar lazy-completions-table-for-filename #'completion-file-name-table)
;; (defun lazy-completions-for-filename (filename-string)
;;   (all-completions filename-string lazy-completions-table-for-filename))

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
                                                                     (lazy-completions) nil nil nil nil
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

;; (defun lazy-jump-references (word &optional proj-name buffer)
;;   )

;; (defun lazy-jump-callees (word &optional proj-name buffer)
;;   )

;; (defun lazy-jump-callers (word &optional proj-name buffer)
;;   )

;; ---------------------------------------------------------------------
;; Compile
;; ---------------------------------------------------------------------


(defvar lazy-compile-previous-history nil
  "Used to keep track of the previous looked at command from compile history.")
(defvar lazy-compile-entered-string nil
  "The string that was in the minibuffer when the user pressed up and initiated
compile history search for that string.")
(defvar lazy-compile-history nil
  "Used internally in `lazy-compile' to keep a seperate, project specific version
of what `compile-history' normally is.")

(defun lazy-compile-delete-from-history (delete-string)
  "Remove DELETE-STRING item from `lazy-compile-history' or `compile-history'.

See also `lazy-compile-read-command'."
  (let* ((old-history (lazy-get-config-val 'compile-cmd))
         (old-length (length old-history))
         (new-history nil))
    (when (listp old-history)
      (setq new-history (cl-remove-if (lambda (s) (string-equal s delete-string)) old-history))
      (if (eq (length new-history) old-length)
          (setq compile-history (cl-remove-if (lambda (s) (string-equal s delete-string)) compile-history))
        ;; - not only is removing enough, we also need to set the lazy-compile-history, the minibuffer history
        ;; (which is hiding behind minibuffer-history-variable) and also save the new history in the project
        ;; config
        (setq lazy-compile-history new-history)
        (setf (symbol-value minibuffer-history-variable) new-history)
        (lazy-set-config-val 'compile-cmd new-history)))))

;; - both lazy-compile-previous-history-element and lazy-compile-next-history-element work
;; by keeping the 'history search state' in two variables named lazy-compile-entered-string
;; and lazy-compile-previous-history
;; - the prev/next functions are supposed to be mapped to the up/down keys while in minibuffer,
;; the up/down initiates searching through the history just like in fish shell
;; - whenever the user changes the minibuffer contents in any way, lazy-compile-entered-string
;; is reset to nil (this is done by mapping keys to lazy-compile-history-search-reset), then
;; either prev/next function set it to contain the current minibuffer contents, those contents
;; then become the string we are searching for until either the user exits the minibuffer or
;; changes the contents again
;; - to decide when a user has changed the contents of the minibuffer, we also remember what
;; was previously shown in the minibuffer in lazy-compile-previous-history, which is also
;; reset in lazy-compile-history-search-reset
(defun lazy-compile-next-history-element (n)
  "Puts next element of the minibuffer history in the minibuffer.
With argument N, it uses the Nth following element.

This is an altered version of `next-history-element' that searches
through the minibuffer history for whatever the user previously
typed into the minibuffer. If nothing was typed, this behaves just
like `next-history-element'.

This function is used by `lazy-compile' to make its prompt act just
like the fish shell.

See also `lazy-compile-read-command'."
  (interactive "p")

  (let* ((search-point nil)
         (history (symbol-value minibuffer-history-variable))
         (current-string (buffer-substring (minibuffer-prompt-end) (point)))
         (search-string (or lazy-compile-entered-string current-string))
         (history-pos nil))

    ;; - set lazy-compile-entered-string to contain the current minibuffer contents (current-string)
    ;; but only when it has been reset (is nil) before calling this function
    (when (and (> (length current-string) 0)
               (not lazy-compile-entered-string))
      (setq lazy-compile-entered-string current-string))

    ;; - this block handles searching through the history, since we are in the next function
    ;; we search from the current minibuffer-history-position backwards to the start of the history
    ;; - the (and ...) condition makes sure there is a search-string, then searches if the current
    ;; minibuffer contents differ from lazy-compile-previous-history (as described above) OR it
    ;; searches if the cursor is not at the end of the minibuffer but at some point between
    ;; (minibuffer-prompt-end) and (point-max), meaning we also search for the partial string
    ;; between those two points, even when the user has not changed anything (I glanced over that
    ;; above)
    (when (and (> (length search-string) 0)
               (or (and (< (minibuffer-prompt-end) (point))
                        (< (point) (point-max)))
                   (not (string-equal lazy-compile-previous-history current-string))))
      ;; - using cl-position, search for search-string in history, notice that we search from
      ;; (1- minibuffer-history-position), meaning one before the current history item, to the
      ;; beginning of the history, if minibuffer-history-position is nil then we search from the
      ;: end of the history
      (setq history-pos (cl-position search-string history
                                     :end (1- (or minibuffer-history-position (1+ (length history))))
                                     :test (lambda (a b) (string-match-p (concat ".*" (regexp-quote a)) b))
                                     :from-end t))

      ;; - it is confusing for the user if the history search finds the exact string the user entered
      ;; and nothing else, because then nothing changes visually in the minibuffer and the user wonders
      ;; if anything happened at all, thats why we just search a second time for search-string if the found
      ;; history item (elt history history-pos) is exactly equal to search-string
      (when (and history-pos (string-equal (elt history history-pos) search-string))
        (setq history-pos (cl-position search-string history
                                       :end (1- history-pos)
                                       :test (lambda (a b) (string-match-p (concat ".*" (regexp-quote a)) b))
                                       :from-end t)))

      ;; - when we found a history item containing the search string, set n so that goto-history-element
      ;; below we yield the correct history element, also set search-point to t indicating that we want to
      ;; adjust point after updating the minibuffer contents so that it ends up at the end of the search-string
      ;; but in the new minibuffer contents
      ;; - if no history item was found, set n to 0 so that absolutely nothing happens, informing the user that
      ;; no history item matches the current minibuffer contents
      (if history-pos
          (setq n (- (- minibuffer-history-position 1) history-pos)
                search-point t)
        (setq n 0)))

    ;; - this handles the case where we searched and found nothing (n is zero), then it
    ;; inserts either the entered string so that the user sees its compile command not changeing
    ;; and knows there is nothing more to scroll through, or it inserts an empty string if there
    ;; is no entered string which indicates to the user that nothing was found and now he can
    ;; enter something himself
    ;; - notice that the insertion of the entered string seems nonsensical at first, but is
    ;; absolutely neccessary, the entered string is NOT part of the history, so it can't be found
    ;; and must be inserted manually to appear, if we did not do that, the user would always
    ;; 'loose his own command when scrolling through the history for any matches
    ;; - otherwise if n is not zero, then this just uses goto-history-element like the regular
    ;; next-history-element would do
    (cond ((and (zerop n)
                (> (length current-string) 0))
           (progn (kill-whole-line)
                  (insert (or lazy-compile-entered-string ""))
                  (goto-char (point-max))
                  (setq minibuffer-history-position 0)))
          ((and (not (zerop n))
                (> (length current-string) 0)
                (> minibuffer-history-position 0))
           (goto-history-element (- minibuffer-history-position n))))

    ;; - this sets point to be consitent to where it was before initiating search, by just
    ;; searching for the search-string interactivly, because at this time the contents of
    ;; the minibuffer have been updated with the found history element, but point did not
    ;; change
    (when search-point
      (goto-char (minibuffer-prompt-end))
      (re-search-forward (regexp-quote search-string) nil t))

    ;; - last thing to do is update lazy-compile-previous-history before ending the function
    ;; with the current contents of the minibuffer
    ;; - except when we did NOT find a history element and therefore did NOT call goto-history-element
    ;; above, then the previous history should just be the empty string
    (setq lazy-compile-previous-history (buffer-substring (minibuffer-prompt-end) (point-max)))
    (unless (and (not (zerop n))
                 (> (length current-string) 0)
                 (> minibuffer-history-position 0))
      (setq lazy-compile-previous-history ""))))

(defun lazy-compile-previous-history-element (n)
  "Puts previous element of the minibuffer history in the minibuffer.
With argument N, it uses the Nth previous element.

This is an altered version of `previous-history-element' that searches
through the minibuffer history for whatever the user previously
typed into the minibuffer. If nothing was typed, this behaves just
like `previous-history-element'.

This function is used by `lazy-compile' to make its prompt act just
like the fish shell.

See also `lazy-compile-read-command'."
  (interactive "p")

  (let* ((search-point nil)
         (history (symbol-value minibuffer-history-variable))
         (current-string (buffer-substring (minibuffer-prompt-end) (point)))
         (search-string (or lazy-compile-entered-string current-string))
         (history-pos nil))

    ;; - this and the next block are pretty much the same as in the function above, the only real
    ;; difference here is that cl-position uses :start instead of :end and searches from minibuffer-position
    ;; (notice no 1+ there) toward the end of the history, so obviously just the other way around then
    ;; the function above
    (when (and (> (length current-string) 0)
               (not lazy-compile-entered-string))
      (setq lazy-compile-entered-string current-string))

    (when (and (> (length search-string) 0)
               (or (and (< (minibuffer-prompt-end) (point))
                        (< (point) (point-max)))
                   (not (string-equal lazy-compile-previous-history current-string))))
      (setq history-pos (cl-position search-string history
                                     :start (or minibuffer-history-position 1)
                                     :test (lambda (a b) (string-match-p (concat ".*" (regexp-quote a)) b))))

      (when (and history-pos (string-equal (elt history history-pos) search-string))
        (setq history-pos (cl-position search-string history
                                       :start (1+ history-pos)
                                       :test (lambda (a b) (string-match-p (concat ".*" (regexp-quote a)) b)))))

      (if history-pos
          (setq n (- history-pos (- minibuffer-history-position 1))
                search-point t)
        (setq n 0)))

    ;; - there is no insertion or anything like that in this function because this function is for
    ;; when the user presses up, and the user does not expect to find 'his' entered command at the
    ;; end when he presses up so often that nothing to scroll through is left, he just expects the
    ;; scrolling throuhg history to just stop on the last (matching) element, this is why the this
    ;; simple (or ...) is enough
    (or (zerop n)
        (goto-history-element (+ minibuffer-history-position n)))

    ;; - set point so the user gets consistent behaviour, first we almost always put it at point-max,
    ;; because pressing up when point is at minibuffer-prompt-end keeps it there, and that is unfortunate
    ;; if did not put it there on purpose
    ;; - and because we WANT point to be where we put it on purpose, the second thing we do is
    ;; search for search-string in the updated minibuffer contents so that point ends up at the end
    ;; of search-string
    (unless (> (length lazy-compile-previous-history) 0)
      (goto-char (point-max)))
    (when (and search-point
               (> (length search-string) 0))
      (goto-char (minibuffer-prompt-end))
      (re-search-forward (regexp-quote search-string) nil t))

    ;; - again, last thing to do is update our state, the lazy-compile-previous-history to contain
    ;; what is currently in the minibuffer, unless n is zero, then as above, scrolling stopped and
    ;; nothing should change
    (unless (zerop n)
      (setq lazy-compile-previous-history (buffer-substring (minibuffer-prompt-end) (point-max))))))

(defun lazy-compile-history-search-reset ()
  "This is used in keybindings for the `lazy-compile' prompt history search to reset
the search state when the user changes the contents of the minibuffer.

See also `lazy-compile-next-history-element', `lazy-compile-previous-history-element'
and `lazy-compile-read-command'."
  (setq minibuffer-history-position 0
        lazy-compile-previous-history "" ;; (buffer-substring (minibuffer-prompt-end) (point-max))
        lazy-compile-entered-string nil))

(defun lazy-compile-read-command (&optional command)
  "This interactivly reads a compile command from the user, with COMMAND default contents
of the prompt can be specified.

The functions `lazy-compile-next-history-element', `lazy-compile-previous-history-element'
and `lazy-compile-history-search-reset' implement the history search when the user presses
up or down in the prompt spawned by this function.

`lazy-compile-delete-from-history' is used when the user request deletion of a history
element with C-x d

See also `lazy-compile'."
  (let ((ido-enable-replace-completing-read nil)
        (minibuffer-local-completion-map (copy-keymap minibuffer-local-completion-map))
        (lazy-compile-previous-history command)
        (lazy-compile-entered-string nil)
        (saved-default-directory default-directory))
    ;; - I did not find a way to specify my own minibuffer-local-completion-map, so I just make a local copy
    ;; of the existing one and then redefine the keybinds that need to be changed here
    ;; - I bind up/down to lazy-compile-previous/next-history-element so that it searches through history
    ;; - I make a 'catch all' binding with [t] so that all keys that have no explicit binding already cause
    ;; lazy-compile-history-search-reset to be called, so that whenever something changes in the minibuffer,
    ;; the search state is reset
    ;; - I bind return, home, etc because with the [t] catch all binding those are overwritten and stop working
    ;; correctly, so they need to be explicitly bound
    ;; - the bindings for " " and "?" are because by default minibuffer-local-completion-map binds those to
    ;; minibuffer-complete but I want to be able to enter " " or "?" into commands
    ;; - tab, backspace and delete I wanted to reset the search state
    ;; - C-d is bound to kill-whole-line for convenience
    ;; - C-x d is bound so that I can delete the current item from history completely
    (define-key minibuffer-local-completion-map (kbd "<up>") 'lazy-compile-previous-history-element)
    (define-key minibuffer-local-completion-map (kbd "<down>") 'lazy-compile-next-history-element)

    (define-key minibuffer-local-completion-map [t] (lambda () (interactive)
                                                      (unless (and (stringp (this-command-keys))
                                                                   (string-match "[[:cntrl:]]+" (this-command-keys)))
                                                        (self-insert-command 1))
                                                      (lazy-compile-history-search-reset)))

    (define-key minibuffer-local-completion-map (kbd "<return>") 'exit-minibuffer)
    (define-key minibuffer-local-completion-map (kbd "<home>") 'beginning-of-line)
    (define-key minibuffer-local-completion-map (kbd "<end>") 'end-of-line)
    (define-key minibuffer-local-completion-map (kbd "<right>") 'right-char)
    (define-key minibuffer-local-completion-map (kbd "<left>") 'left-char)

    (define-key minibuffer-local-completion-map " " (lambda () (interactive)
                                                      (insert " ")
                                                      (lazy-compile-history-search-reset)))
    (define-key minibuffer-local-completion-map "?" (lambda () (interactive)
                                                      (insert "?")
                                                      (lazy-compile-history-search-reset)))
    (define-key minibuffer-local-completion-map (kbd "<tab>") (lambda () (interactive)
                                                                (call-interactively 'minibuffer-complete)
                                                                (lazy-compile-history-search-reset)))
    (define-key minibuffer-local-completion-map (kbd "<backspace>") (lambda () (interactive)
                                                                      (call-interactively 'delete-backward-char)
                                                                      (lazy-compile-history-search-reset)))
    (define-key minibuffer-local-completion-map (kbd "<delete>") (lambda () (interactive)
                                                                   (call-interactively 'delete-char)
                                                                   (lazy-compile-history-search-reset)))
    (define-key minibuffer-local-completion-map (kbd "C-d") (lambda () (interactive)
                                                              (call-interactively 'kill-whole-line)
                                                              (lazy-compile-history-search-reset)))
    (define-key minibuffer-local-completion-map (kbd "C-x d") (lambda () (interactive)
                                                                (lazy-compile-delete-from-history (buffer-substring (minibuffer-prompt-end) (point-max)))
                                                                (call-interactively 'kill-whole-line)
                                                                (lazy-compile-history-search-reset)))

    (let ((result (completing-read "Lazy Compile: "
                                   (lambda (string pred action)
                                     ;; - this updates the default-directory with any 'cd' commands that can be found in string,
                                     ;; so that when we try to complete filenames we can actually complete them correctly as if
                                     ;; the 'cd' commands in the compile command had already happened
                                     (mapc (lambda (s)
                                             (setq s (replace-regexp-in-string "[;\"]" "" s))
                                             (setq s (replace-regexp-in-string "[ \t]*$" "" s))
                                             (setq s (replace-regexp-in-string "^[ \t]*" "" s))
                                             (let ((dir (expand-file-name s default-directory)))
                                               (when (and (file-exists-p dir)
                                                          (file-directory-p dir))
                                                 (setq-local default-directory dir)
                                                 )))
                                           (split-string string "cd\\|;" t))
                                     ;; - this matches if point is at the end of something that looks like a path or like a elisp
                                     ;; expression and calls complete-with-action with the appropriate completion table accordingly
                                     ;; - notice that when we match part of the string as path, elisp etc, we then apply complete-with-action
                                     ;; to only the match, and not the whole minibuffer contents (string)
                                     (let* ((completion (cond ((string-match "\\(\\(?:\\.\\.?\\|~\\|[^ \t\";]+\\)?\\(?:/[^/;\"]*\\)+\\)$" string)
                                                               ;; - matched path, notice that we use completion-file-name-table although lazy-completions-table-for-path
                                                               ;; exists, but that uses completion--file-name-table, which has problems with how we later change the
                                                               ;; contents of the minibuffer with replace-match (args out of range for some reason, couldn't figure out
                                                               ;; the exact problem)
                                                               (let ((match (match-string 1 string)))
                                                                 (save-match-data
                                                                   (complete-with-action action #'completion-file-name-table match pred))))
                                                              ((string-match "[^$\"][(]\\([^()]+\\)" string)
                                                               ;; - matched elisp expression, use lazy-completions-table-for-elisp
                                                               (let ((match (match-string 1 string)))
                                                                 (save-match-data
                                                                   (complete-with-action action lazy-completions-table-for-elisp match pred))))
                                                              ((or (string-match "\"\\([^/;\"]+\\)$" string)
                                                                   (string-match "[^;]+[; \t]+\\([^/;\" ]+\\)$" string))
                                                               ;; - matched shell command, use (shell--command-completion-data), notice thats a function, and we use
                                                               ;; (nth 2 ...) to get the actual shell-commmand-table, thats just how emacs does it
                                                               (let ((match (match-string 1 string)))
                                                                 (save-match-data
                                                                   (let ((shell-command-table (nth 2 (shell--command-completion-data))))
                                                                     (complete-with-action action (completion-table-merge shell-command-table #'completion-file-name-table) match pred)))))
                                                              (t
                                                               ;; - default case, try everything
                                                               (let ((shell-command-table (nth 2 (shell--command-completion-data))))
                                                                 (complete-with-action action (completion-table-merge shell-command-table #'completion-file-name-table lazy-completions-table-for-elisp) string pred))))))
                                       ;; - when we had a successfull match above, AND the complete-with-action returned a single string (and not a list or one of
                                       ;; those other metadata things it can return), then we replace-match our match in string with the returned completion, meaning
                                       ;; we replace only the relevant part of string that matched above with the completion returned by complete-with-action
                                       ;; - we have something like "ls /tmp/foo" and then we matched the path "/tmp/foo" above, and complete-with-action was called
                                       ;; with "/tmp/foo" as third argument and returned "/tmp/foobar.txt" as completion, then what we do here is replace "/tmp/foo"
                                       ;; with "/tmp/foobar.txt" in "ls /tmp/foo" so that we get "ls /tmp/foobar.txt" as end result
                                       (when (and (stringp completion)
                                                  (match-string 1 string))
                                         (setq completion (replace-match completion nil nil string 1)))
                                       completion))
                                   nil nil command
                                   (if (equal (car lazy-compile-history) command)
                                       '(lazy-compile-history . 1)
                                     'lazy-compile-history))))
      (setq-local default-directory saved-default-directory)
      result)))

(defun lazy-compile-command (command &optional comint)
  "This function is a modified, non-interactive version of `compile'. It takes a
COMMAND, checks whether the command is a shell command or a elisp expression,
and calls either `compilation-start', `eval-expression' or `command-execute'
accordingly.

If COMINT is non-nil the opened *compilation* buffer will be in comint-mode.

See also `lazy-compile'."
  (let ((compilation-ask-about-save (not (called-interactively-p 'interactive)))
        (compilation-read-command (not (called-interactively-p 'interactive))))
    (save-some-buffers (not compilation-ask-about-save)
                       compilation-save-buffers-predicate)
    (setq-default compilation-directory default-directory)
    (cond ((and (not (executable-find command))
                (not (> (length (split-string command " " t)) 1))
                (commandp (read command)))
           (command-execute (read command)))
          ((and (not (executable-find command))
                (string-match "^(" command)
                (functionp (read command)))
           (funcall (eval-expression (read command))))
          ((and (string-match "^(" command)
                (not (executable-find command))
                (condition-case nil (read command) (error nil)))
           (eval-expression (read command)))
          (t
           (compilation-start command comint))))
  command)

(defun lazy-compile ()
  "The lazy version of `compile'. Prompts the user interactively with the last
used compile command or nothing if the current project was never compiled.
The user can enter a new compile command or search through the compile command
history with the up and down arrow keys. The entered compile command can be
a shell command, an elisp expression or elisp command.

After the user presses return this function invokes the minibuffer contents
as shell command or evaluates it as elisp expression. If the compile command
is a shell command it opens a new buffer in compilation-mode and display the
shell commands output, if the compile command is a elisp expression it is just
evaluated with `eval-expression', if it is a elisp command it is executed with
`command-execute'.

The decision if the compile command is a elisp expression or shell command is
made in `lazy-compile-command'.

The compile command history search is implemented in `lazy-compile-read-command',
`lazy-compile-next-history-element' and `lazy-compile-previous-history-element'."
  (interactive)
  (if (and (called-interactively-p 'interactive)
           (not lazy-name))
      (call-interactively 'compile)
    (lazy-assert-proj nil)
    (with-current-buffer (get-buffer (lazy-fib-name))
      (save-buffer))
    (let* ((cmd (lazy-get-config-val 'compile-cmd)))
      (when (and (not (stringp cmd))
                 (or (commandp cmd)
                     (functionp cmd)))
        (setq cmd (prin1-to-string cmd)))
      (when (string-equal (buffer-name (current-buffer)) "*compilation*")
        (call-interactively 'other-window))
      (lazy-with-directory (lazy-get-config-val 'basedir)
                           (cond ((listp cmd)
                                  (let* ((old-history cmd)
                                         (lazy-compile-history (cl-remove-duplicates (append old-history compile-history) :test 'equal :from-end t))
                                         (old-cmd (car lazy-compile-history))
                                         (new-cmd (lazy-compile-command (lazy-compile-read-command old-cmd) (consp current-prefix-arg))))
                                    (unless (string-equal old-cmd new-cmd)
                                      (lazy-set-config-val 'compile-cmd (cl-remove-duplicates (append (list new-cmd) old-history) :test 'equal :from-end t)))))
                                 ((stringp cmd)
                                  (let* ((old-cmd cmd)
                                         (lazy-compile-history (list old-cmd))
                                         (new-cmd (lazy-compile-command (lazy-compile-read-command old-cmd) (consp current-prefix-arg)))
                                         (new-list (list new-cmd old-cmd)))
                                    (unless (string-equal old-cmd new-cmd)
                                      (lazy-set-config-val 'compile-cmd new-list))))
                                 (t
                                  (let ((lazy-compile-history nil)
                                        (new-cmd (lazy-compile-command (lazy-compile-read-command) (consp current-prefix-arg))))
                                    (lazy-set-config-val 'compile-cmd (list new-cmd)))))))))

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
  (when (or proj-alist (gethash proj-name lazy-project-list nil))
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
                   (setq r (append r `(,k))))))
             lazy-project-list)
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
  "Return all buffers that are friendly to the project."
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
                                                buildsystem-file))
                                            buildsystem-files)))
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
                        (mapc (lambda (name) (puthash name (gethash name lazy-project-list) temp-hash)) name-list)
                        temp-hash))
                 lazy-project-list))
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
                        (mapc (lambda (name) (puthash name (gethash name lazy-project-list) temp-hash)) name-list)
                        temp-hash))
                 lazy-project-list))
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
                        (mapc (lambda (name) (puthash name (gethash name lazy-project-list) temp-hash)) name-list)
                        temp-hash))
                 lazy-project-list))
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
                                            `(1 . ,(expand-file-name (or default-directory "."))))
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
                                                   (splitted-path (when (and path (stringp path)) (split-string path "/"))))
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
                               (languages . (((src-patterns)
                                              (let ((languages (lazy-src-pattern-languages src-patterns)))
                                                (when languages
                                                  `(10 . ,languages))))))
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
                                                         `(100 . ,compile-command)))))
                                               ((languages)
                                                (when (cl-some (lambda (lang) (eq lang 'elisp)) languages)
                                                  (when (fboundp 'flycheck-compile)
                                                    `(75 . flycheck-compile))))
                                               ))
                               (vcs . (((basedir)
                                        (let ((r nil))
                                          (loop for f in (directory-files basedir)
                                                if (cl-some (lambda (y)
                                                              (string-equal (cdr y) f))
                                                            lazy-vcs-path)
                                                return `(10 . ,(car (rassoc f lazy-vcs-path))))))))
                               ))

(defun* lazy-guess-alist (&optional ask-basedir ask-name)
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
                                               (error (message "error while guessing %S: %S in %s" sym e (prin1-to-string expr))
                                                      (backtrace)
                                                      (cl-return-from "lazy-guess-alist" nil)))))
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
                     return (let ((already-defined-result (gethash (car pattern-projects) lazy-project-list)))
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
    (let ((projects-el (expand-file-name "projects.el" (file-name-as-directory lazy-global-cache-root))))
      (when (file-exists-p projects-el)
        (load-file projects-el)))))

(provide 'lazy)

;; lazy.el ends here
