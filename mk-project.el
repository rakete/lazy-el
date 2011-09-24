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

(defvar mk-proj-cache-root "~/.mk-project")

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

(defconst mk-proj-fib-name "*file-index*"
  "Buffer name of the file-list cache. This buffer contains a
list of all the files under the project's basedir (minus those
matching ignore-patterns) or, if index-find-cmd is set, the list
of files found by calling the custom find command.  The list is
used by `project-find-file' to quickly locate project files.")

(defconst mk-proj-vcs-path '((git . "'*/.git/*'")
                             (cvs . "'*/.CVS/*'")
                             (svn . "'*/.svn/*'")
                             (bzr . "'*/.bzr/*'")
                             (hg  . "'*/.hg/*'")
                             (darcs . "'*/_darcs/*'"))
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
                                open-friends-cache
                                sourcemarker)
  "Project config vars that are optional.

See also `mk-proj-required-vars' `mk-proj-var-functions' `mk-proj-load-vars'")

(defvar mk-proj-var-functions '((basedir . (lambda (var val)
                                             (expand-file-name val)))
                                (tags-file . (lambda (var val)
                                               (if val
                                                   (expand-file-name val)
                                                 (mk-proj-get-cache-path var))))
                                (file-list-cache . (lambda (var val)
                                                     (if val
                                                         (expand-file-name val)
                                                       (mk-proj-get-cache-path var))))
                                (open-files-cache . (lambda (var val)
                                                      (if val
                                                          (expand-file-name val)
                                                        (mk-proj-get-cache-path var))))
                                (open-friends-cache . (lambda (var val)
                                                        (if val
                                                            (expand-file-name val)
                                                          (mk-proj-get-cache-path var)))))
  "Config vars from `mk-proj-required-vars' and `mk-proj-optional-vars' (except 'name')
can be associated with a function in this association list, which will be
applied to the value of the var before loading the project.

See also `mk-proj-load-vars'.")

(defvar mk-proj-project-var-load-hook '())
(defvar mk-proj-project-var-unload-hook '())


(defvar mk-proj-project-load-hook '())

(defvar mk-proj-project-unload-hook '())

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

(defcustom mk-proj-ack-cmd (if (eq system-type 'windows-nt) "ack.pl" "ack")
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

(defun mk-proj-get-vcs-path ()
  (if mk-proj-vcs
      (cdr (assoc mk-proj-vcs mk-proj-vcs-path))
    nil))

(defun mk-proj-has-univ-arg ()
  (eql (prefix-numeric-value current-prefix-arg) 4))

(defun mk-proj-names ()
  (let ((names nil))
    (maphash (lambda (k v) (add-to-list 'names k)) mk-proj-list)
    names))

(defun mk-proj-use-ido ()
  (and (boundp 'ido-mode) mk-proj-use-ido-selection))

(defun mk-proj-find-cmd-val (context)
  (let ((cmd (ecase context
               ('src   mk-proj-src-find-cmd)
               ('grep  mk-proj-grep-find-cmd)
               ('index mk-proj-index-find-cmd))))
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
  (let (b)
    (dolist (x lst b)
      (when b
        (return-from "mk-proj-any" t))
      (setq b (funcall condp x)))))

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

(defun mk-proj-sourcemarker-create (&optional n)
  "Sourcemarkers are a persitent alternative to emacs markers specifically aimed
at marking lines in source code.

Creating a sourcemarker will collect lines around the current point which will
then be used by `mk-proj-sourcemarker-restore' to restore point regardless of
whether the piece of code has been moved around in the file. It should be even
possible to restore a point if the lines that represent that point in the
sourcemarker have partly changed in the file."
  (interactive)
  (unless n
    (setq n 2))
  (save-excursion
    (save-restriction
      (org-save-outline-visibility
          (show-all)
        ;; return nil if the buffer is not big enough for a mark
        (cond ((save-excursion
                 (end-of-buffer)
                 (< (line-number-at-pos) (+ (* n 2) 1)))
               `(,(progn
                    (beginning-of-buffer)
                    (point-at-bol))
                 ,(buffer-file-name (current-buffer))
                 nil))
              (t
               (progn
                 ;; move point to nearest non-empty line
                 ;; handle end-of-buffer/beginning-of-buffer
                 ;; by reversing the search direction
                 (let ((rev nil))
                   (while (save-excursion
                            (beginning-of-line)
                            (looking-at "^\\s-*$"))
                     (when (save-excursion
                             (end-of-line)
                             (eobp))
                       (setq rev t))
                     (if rev
                         (previous-line-nomark)
                       (next-line-nomark))
                     ))
                 (beginning-of-line)
                 ;; two functions walking up/down from current point collecting lines
                 ;; trimming whitespaces, skipping empty lines, collecting #eobp#/#bobp#
                 ;; when at end/beginning of buffer
                 (flet ((collect-up (m) (save-excursion
                                          (reverse
                                           ;; arg m is number of lines to collect
                                           (loop for i from 1 to m
                                                 collect (progn
                                                           ;; if already at beginning of buffer collect #bobp#
                                                           (if (save-excursion
                                                                 (beginning-of-line)
                                                                 (bobp))
                                                               "#bobp#"
                                                             (progn
                                                               ;; walk up one line
                                                               (previous-line-nomark)
                                                               ;; skip empty lines or none when current line is not empty
                                                               (while (and (save-excursion
                                                                             (beginning-of-line)
                                                                             (looking-at "^\\s-*$"))
                                                                           (not (save-excursion
                                                                                  (beginning-of-line)
                                                                                  (bobp))))
                                                                 (previous-line-nomark))
                                                               ;; check again if skipping empty lines
                                                               ;; brought us to the beginning of the buffer
                                                               ;; also, check if the actual line is empty
                                                               ;; because skipping terminates on bobp as well
                                                               ;; and if it did, we still want to collect the line
                                                               ;; instead of #bobp#
                                                               ;; also, this is the part where the line is collected
                                                               ;; the condition will only be true on empty lines
                                                               ;; (which should have been skipped by now)
                                                               ;; or if we are at bobp
                                                               (if (and (save-excursion
                                                                          (beginning-of-line)
                                                                          (bobp))
                                                                        (save-excursion
                                                                          (beginning-of-line)
                                                                          (looking-at "^\\s-*$")))
                                                                   "#bobp#"
                                                                 (mk-proj-chomp (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))))))))
                        (collect-down (m) (save-excursion
                                            (loop for i from 1 to m
                                                  collect (progn
                                                            (if (save-excursion
                                                                  (end-of-line)
                                                                  (eobp))
                                                                "#eobp#"
                                                              (progn
                                                                (next-line-nomark)
                                                                (while (and (save-excursion
                                                                              (beginning-of-line)
                                                                              (looking-at "^\\s-*$"))
                                                                            (not (save-excursion
                                                                                   (end-of-line)
                                                                                   (eobp))))
                                                                  (next-line-nomark))
                                                                (if (and (save-excursion
                                                                           (end-of-line)
                                                                           (eobp))
                                                                         (save-excursion
                                                                           (beginning-of-line)
                                                                           (looking-at "^\\s-*$")))
                                                                    "#eobp#"
                                                                  (mk-proj-chomp (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))))))))
                   (let ((above (collect-up n))
                         (below (collect-down n))
                         (line (mk-proj-chomp (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
                     `(,(point-at-bol)
                       ,(buffer-file-name (current-buffer))
                       (,above ,line ,below))
                     )))))))))

(defun mk-proj-sourcemarker-restore (m)
  "Restoring point refered to by a sourcemarker M.

This will look at all pieces of code that are a possible match for the sourcemarker
and assign a score to them. The point-at-bol of the best match is then returned in
form of a marker.

See also `mk-proj-sourcemarker-create'."
  (interactive)
  (let* ((m-point (save-excursion
                    (goto-char (nth 0 m))
                    (point-at-bol)))
         (m-file (nth 1 m))
         (m-stamp (nth 2 m))
         (stamp-prev-lines (nth 0 m-stamp))
         (stamp-line (nth 1 m-stamp))
         (stamp-next-lines (nth 2 m-stamp))
         (n (+ (length stamp-next-lines) (length stamp-prev-lines) 1))
         (m (/ (- n 1) 2))
         (c (+ m 1))
         (re-all (let ((s (concat "\\(" (regexp-quote stamp-line) "$\\)")))
                   (loop for i from 0 to (- m 1)
                         do (progn
                              (let ((left (regexp-quote (nth (- (- m 1) i) stamp-prev-lines)))
                                    (right (regexp-quote (nth i stamp-next-lines))))
                                (setq s (concat (unless (or (string-equal left "#eobp#")
                                                            (string-equal left "#bobp#"))
                                                  (concat "\\(" left "$\\)\\|"))
                                                s
                                                (unless (or (string-equal right "#eobp#")
                                                            (string-equal right "#bobp#"))
                                                  (concat "\\|\\(" right "$\\)"))))))
                         finally return s)))
         (re-individual (mapcar (lambda (s) (if (or (string-equal s "#eobp#")
                                                    (string-equal s "#bobp#"))
                                                s
                                              (concat s "$")))
                                (append (mapcar #'regexp-quote stamp-prev-lines)
                                        (list (regexp-quote stamp-line))
                                        (mapcar #'regexp-quote stamp-next-lines))))
         (tokens (append stamp-prev-lines (list (regexp-quote stamp-line)) stamp-next-lines))
         (matches '())
         (line-matches '())
         (k 0)
         (score 0)
         (result nil)
         (buf (find-file-noselect m-file)))
    (with-current-buffer buf
      (save-excursion
        (beginning-of-buffer)
        ;; search lines that match any of the saved stamp lines
        ;; do that until you run out of matches
        (while (re-search-forward re-all nil t)
          (save-excursion
            (beginning-of-line)
            ;; when we found a match we will test this and the following lines (as many as there are in the stamp)
            (loop for j from 0 to (- n 1)
                  do (setq matches (append matches `(,(progn (if (string-match re-all (buffer-substring (point-at-bol) (point-at-eol)))
                                                                 ;; we try to match every individual regexp to the current line and collect triples
                                                                 ;; of ("the k'th regex that matched p" "point-at-bol" "the individual regex that matched")
                                                                 ;; into a list of line-matches
                                                                 ;; it is possible that a stamp contains duplicate lines, thats why we check every line
                                                                 ;; for all regexps
                                                                 (loop for s in re-individual
                                                                       do (setq k (+ k 1))
                                                                       if (or (and (string-equal s "#eobp#") (save-excursion (end-of-line) (eobp)))
                                                                              (and (string-equal s "#bobp#") (save-excursion (beginning-of-line) (bobp)))
                                                                              (string-match s (buffer-substring (point-at-bol) (point-at-eol))))
                                                                       do (progn
                                                                            (setq line-matches (append line-matches `((,k ,(point-at-bol) ,s))))
                                                                            )
                                                                       else
                                                                       do (setq line-matches (append line-matches `((nil ,(point-at-bol) nil)))))
                                                               ;; the else case if the line does not match any regexp (will this ever be reached?)
                                                               (setq line-matches (append line-matches `((nil ,(point-at-bol) nil)))))
                                                             ;; after collect all matches for a line we try to find one that fits the progression of matches we
                                                             ;; accumulated over the previous lines so far
                                                             ;; that means we check matches for the k'th regexp that matched, then we try to return a match
                                                             ;; for the k+1 regexp or a (k+x)<n regexp
                                                             ;; if that fails we return a (nil point-at-bol nil) triple
                                                             (or (let ((last-k (or (nth 0 (car (last (mk-proj-filter (lambda (y) (not (eq (nth 0 y) nil))) matches)))) 0)))
                                                                   (loop for x from n downto (+ last-k 1)
                                                                         until (assoc x line-matches)
                                                                         finally return (assoc x line-matches)))
                                                                 `(nil ,(point-at-bol) nil))))))
                  ;; reseting the counter and all line-matches before going to the next line,
                  ;; we handle skipping lines that are empty as well as a potential eobp
                  do (progn
                       (setq line-matches '()
                             k 0)
                       ;; walk forward one line
                       (unless (or (save-excursion
                                     (end-of-line)
                                     (eobp))
                                   ;; when we expected a bobp in the stamp we can't walk forward
                                   ;; because we are still on the first line that is waiting to be
                                   ;; matched by actual regexps
                                   (string-equal (nth j tokens) "#bobp#"))
                         (next-line-nomark))
                       ;; skip empty lines until we are on a line with something in it or at the eobp
                       (while (and (save-excursion
                                     (beginning-of-line)
                                     (looking-at "^\\s-*$"))
                                   (not (save-excursion
                                          (end-of-line)
                                          (eobp)))
                                   (not (string-equal (nth j tokens) "#bobp#")))
                         (next-line-nomark))))
            ;; at this point matches should be a list of tokens like:
            ;; ("the k'th regex that matched p" "point-at-bol" "the individual regex that matched")
            (message (format "matches: %s" (prin1-to-string matches)))
            ;; now we define individual tests that will be applied to every element of the matches list
            ;; succesivly, each test takes five arguments:
            ;; PREV: the list before the element to which the test is currently applied
            ;; K,P,L: the three parts of the current element
            ;; NEXT: the rest of the list of matches after the current element
            ;; tests can return a score, all scores returned will be summed up and used to measure
            ;; the relevance of the current match
            (let* ((tests '((match-on-marker-point (prev k p l next)
                                                   (when (and (eq (length prev) (length next)) (eq p m-point))
                                                     5))
                            (central-line-matches (prev k p l next)
                                                  (when (and (eq (length prev) (length next)) (eq k c))
                                                    5))
                            (found-the-original-line (prev k p l next)
                                                     (when (and l (string-match l stamp-line))
                                                       2))
                            (full-house (prev k p l next)
                                        (when (and (eq (length prev) (1- n)) (eq (length next) 0))
                                          (let* ((xs (append prev `((,k ,p ,l))))
                                                 (sum (apply #'+ (mapcar (lambda (x) (or (car x) 0)) xs))))
                                            (when (eq sum (apply #'+ (loop for x from 1 to n collect x)))
                                              10))))
                            (pairs (prev k p l next)
                                   (when (and next k (nth 0 (car next)) (eq (- (nth 0 (car next)) k) 1))
                                     1))))
                   (prev '())
                   (current (car matches))
                   (next (cdr matches)))
              ;; go through the list of matches, apply every test, sum up scores
              (eval `(flet ,tests
                       (loop for x = (list prev current next) then (progn (setq prev (append prev `(,current))
                                                                                current (car next)
                                                                                next (cdr next))
                                                                          (list prev current next))
                             do (dolist (test tests)
                                  (let ((score-add (funcall (car test) prev (nth 0 current) (nth 1 current) (nth 2 current) next)))
                                    (when (and score-add (not (eq score-add 0)))
                                      (message (format "%s : %d + %d = %d" (symbol-name (car test)) score score-add (+ score score-add)))
                                      (setq score (+ score score-add)))))
                             until (eq next nil))))))
          (message (format "score: %d" score))
          ;; together with the score, we need the line number where we found this match, so we look at all the tokens
          ;; and find the one thats nearest to the c (c like in center) line, the line the cursor was originally on
          ;; when the sourcemarker was created
          ;; that tokens line number is then set to be our result, if the score is better then the last score
          (let* ((best-k nil)
                 (k nil)
                 (best-point (loop for m in matches
                                   do (setq k (nth 0 m))
                                   if (or (eq k c) (and k best-k (< (abs (- c k)) (abs (- c best-k)))) (eq best-k nil))
                                   do (setq best-k k)
                                   finally return (when (assoc best-k matches)
                                                    (assoc best-k matches)))))
            (when (or (and result (> score (car result))) (eq result nil))
              (setq result `(,score ,best-point))))
          ;; reset for next iteration
          (setq matches '()
                score 0))
        ;; end of iteration, now we just return our result as a marker
        (print result)
        (when result
          (let* ((p (nth 1 (car (last result))))
                 (m (make-marker)))
            ;;`(,p ,buf)
            (set-marker m p buf)))
          ))))

;; (defun test-sourcemarker-create ()
;;   (interactive)
;;   (let (sm)
;;     (save-excursion
;;       (goto-char 12454)
;;       (setq sm (mk-proj-sourcemarker-create)))
;;     (insert (prin1-to-string sm))))


;; (defun test-sourcemarker-restore ()
;;   (interactive)
;;   (setq test-sm '(12448 "/home/lazor/.emacs.d/mk-project/mk-project.el" (("(when b" "(return-from \"mk-proj-any\" t))") "(setq b (funcall condp x)))))" ("(defun* mk-proj-all (condp lst)" "(let ((b t))"))))
;;   (let ((m (mk-proj-sourcemarker-restore test-sm)))
;;     (when (markerp m)
;;       (switch-to-buffer (marker-buffer m))
;;       (goto-char (marker-position m)))))

(defun mk-proj-sourcemarker-p (sm)
  "Test if SM is a sourcemarker. Technically a sourcemarker is just a list with a very
specific layout, so this is not 100% accurate. I consider it good enough until something
breaks."
  (when sm
    (condition-case nil
        (and (numberp (nth 0 sm))
             (file-exists-p (nth 1 sm))
             (listp (nth 2 sm))
             (listp (nth 0 (nth 2 sm)))
             (stringp (nth 1 (nth 2 sm)))
             (listp (nth 2 (nth 2 sm))))
      (error nil))))

(defun mk-proj-visit-sourcemarker ()
  "Restore project sourcemarker and go there."
  (interactive)
  (when mk-proj-sourcemarker
    (let* ((m (mk-proj-sourcemarker-restore mk-proj-sourcemarker)))
      (when (markerp m)
        (switch-to-buffer (marker-buffer m))
        (goto-char (marker-position m))))))

(defun mk-proj-set-sourcemarker-point (&optional p)
  "Update a projects sourcemarker. Not implemented yet."
  (interactive))

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
  (unless proj
    (setq proj mk-proj-name))
  (let ((proj-alist (cond ((listp proj)
                           proj)
                          (t
                           (mk-proj-find-config proj)))))
    (if (assoc key proj-alist)
        (let ((val (cdr (assoc key proj-alist))))
          ;; check for list, (x . y) vs (x y)
          ;; I got annoyed by making this mistake too often
          (if (listp val)
              (car val)
            val))
      (let ((parent (car (cdr (assoc 'parent proj-alist)))))
        (when (and inherit parent)
          (mk-proj-config-val key parent t))))))

(defalias 'mk-proj-config-val 'mk-proj-get-config-val
  "Alias for `mk-proj-get-config-val' to ensure backward compatibility.")

(defun mk-proj-set-config-val (key &optional proj)
  "A so far not implemented way to modify the project configuration
programmatically. Most likely this will only be practical for project
configurations that are stored in org files or something similar."
  )

(defun project-def (proj-name config-alist &optional inherit)
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

  ;; I changed the behaviour of this function to evaluate the config values (when they look like something
  ;; that could be evaluated)
  ;; thats rather nasty because now lists must be quoted or else project definition will fail
  ;; on the other hand it enables my org-mode integration to have its property values evaluated
  ;; EDIT: I think I made it backwards compatible through a condition-case, lets see if anyone complains...
  (let* ((evaluated-config-alist (let ((evaluated-config-alist '()))
                                   (dolist (cv config-alist evaluated-config-alist)
                                     (let* ((key (car cv))
                                            ;; super behaves like a keyword that can be used within a configuration
                                            ;; to refer to the parents value (if inherit has been specified)
                                            ;; I haven't tested this, it is a experimental feature
                                            (super (when inherit (mk-proj-config-val key inherit t)))
                                            (lisp (car (cdr cv)))
                                            (value (condition-case nil (eval lisp) (error lisp))))
                                       (setq evaluated-config-alist (append `((,key ,value)) evaluated-config-alist))))))
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

(defun project-undef (name)
  "Opposite of `project-define'."
  (remhash name mk-proj-list))

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
                                  (val (mk-proj-config-val var proj-name t))
                                  (fn (cdr (assoc var mk-proj-var-functions))))
                              (setf (symbol-value proj-var) (if fn (funcall fn var val) val)))))
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

(defun mk-proj-get-cache-path (symbol)
  (mk-proj-assert-proj)
  (let ((directory (concat mk-proj-cache-root
                           (cond (mk-proj-parent
                                  (let ((a (concat "/" (mk-proj-join "/" (mk-proj-ancestry)) mk-proj-name)))
                                    (if (mk-proj-config-val 'basedir mk-proj-name) a (concat a "/"))))
                                 (t
                                  (concat "/" mk-proj-name "/")))))
        (file (concat (symbol-name symbol))))
    (make-directory directory t)
    (let ((r (concat directory file)))
      (message r))))

(defun mk-proj-join (delimiter strings)
  (reduce (lambda (a b)
            (concatenate 'string a delimiter b))
          strings))

(defun mk-proj-ancestry (&optional name)
  (let* ((current (or name
                      (progn
                        (mk-proj-assert-proj)
                        mk-proj-name)))
         (ancestry `(,name)))
    (while (mk-proj-config-val 'parent current)
      (setq ancestry (cons (mk-proj-config-val 'parent current) ancestry)
            current (mk-proj-config-val 'parent current)))
    ancestry))

(defun mk-proj-load (name)
  (interactive)
  (catch 'project-load
    (let ((oldname mk-proj-name)
          (name (or name
                    (if (mk-proj-use-ido)
                        (ido-completing-read "Project Name (ido): "
                                             (mk-proj-names))
                      (completing-read "Project Name: " (mk-proj-names))))))
      (unless (string= oldname name)
        (project-unload t))
      (let ((proj-config (mk-proj-find-config name)))
        (if proj-config
            (let ((v (mk-proj-load-vars name proj-config)))
              (when v
                (message "Required config value '%s' missing in %s!" (symbol-name v) name)
                (throw 'mk-proj-load t)))
          (message "Project %s does not exist!" name)
          (throw 'mk-proj-load t)))
      (when (not (file-directory-p mk-proj-basedir))
        (message "Base directory %s does not exist!" mk-proj-basedir)
        (throw 'mk-proj-load t))
      (when (and mk-proj-vcs (not (mk-proj-get-vcs-path)))
        (message "Invalid VCS setting!")
        (throw 'mk-proj-load t))
      (message "Loading project %s ..." name)
      (cd mk-proj-basedir)
      (mk-proj-tags-load)
      (mk-proj-fib-init)
      (add-hook 'kill-emacs-hook 'mk-proj-kill-emacs-hook)
      (when mk-proj-startup-hook
        (run-hooks 'mk-proj-startup-hook))
      (run-hooks 'mk-proj-project-load-hook)
      (mk-proj-visit-saved-open-files)
      (mk-proj-visit-saved-open-friends)
      (mk-proj-visit-sourcemarker)
      (message "Loading project %s done" name))))


(defun project-load ()
  "Load a project's settings."
  (interactive)
  (let ((name (if (mk-proj-use-ido)
                  (ido-completing-read "Project Name (ido): " (mk-proj-names))
                (completing-read "Project Name: " (mk-proj-names)))))
    (mk-proj-load name)))

(defun mk-proj-kill-emacs-hook ()
  "Ensure we save the open-files-cache info on emacs exit"
  (when (and mk-proj-name mk-proj-open-files-cache)
    (mk-proj-save-open-file-info)))

(defun project-unload (&optional arg)
  "Unload the current project's settings after running the shutdown hook."
  (interactive "P")
  (when mk-proj-name
    (condition-case nil
        (progn
          (message "Unloading project %s" mk-proj-name)
          (mk-proj-tags-clear)
          (mk-proj-maybe-kill-buffer mk-proj-fib-name)
          (mk-proj-save-open-file-info)
          (mk-proj-save-open-friends-info)
          (and (or (mk-proj-buffers) (mk-proj-friendly-buffers))
               (not arg)
               (y-or-n-p (concat "Close all " mk-proj-name " project files? "))
               (project-close-files)
               (project-close-friends))
          (when mk-proj-shutdown-hook (run-hooks 'mk-proj-shutdown-hook))
          (run-hooks 'mk-proj-project-unload-hook))
      (error nil)))
  (mk-proj-defaults)
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
  "Is the given buffer in our project based on filename? Also detects dired buffers open to basedir/*"
  (let ((file-name (mk-proj-buffer-name buf)))
    (if (and file-name
             (string-match (concat "^" (regexp-quote mk-proj-basedir)) file-name))
        t
      nil)))

(defun mk-proj-buffers ()
  "Get a list of buffers that reside in this project's basedir"
  (let ((buffers nil))
    (dolist (b (buffer-list))
      (when (mk-proj-buffer-p b) (push b buffers)))
    buffers))

(defun project-status (&optional name)
  "View project's variables."
  (interactive)
  (unless name
    (mk-proj-assert-proj)
    (setq name mk-proj-name))
  (if mk-proj-basedir
      (let ((b (get-buffer-create "*project-status*")))
        (with-current-buffer b
          (kill-region (point-min) (point-max))
          (dolist (v (append mk-proj-required-vars mk-proj-optional-vars))
            (insert (format "%-24s = %s\n" (symbol-name v) (mk-proj-config-val v name t)))))
        (when (not (eq b (current-buffer)))
          (switch-to-buffer-other-window b)))
    (message "No project loaded.")))

;; (unless name
;;   (mk-proj-assert-proj)
;;   (setq name mk-proj-name))
;; (let ((msg))
;;   (dolist (v (append mk-proj-required-vars mk-proj-optional-vars))
;;     (setq msg (concat msg (format "%-24s = %s\n" (symbol-name v) (mk-proj-config-val v name t)))))
;;   (message msg))
;; )

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

(defun mk-proj-find-cmd-src-args (src-patterns)
  "Generate the ( -name <pat1> -o -name <pat2> ...) pattern for find cmd"
  (if src-patterns
      (let ((name-expr " \\(")
            (regex-or-name-arg (if mk-proj-patterns-are-regex
                                   "-regex"
                                 "-name")))
        (dolist (pat src-patterns)
          (setq name-expr (concat name-expr " " regex-or-name-arg " \"" pat "\" -o ")))
        (concat (mk-proj-replace-tail name-expr "-o " "") "\\) "))
    ""))

(defun mk-proj-find-cmd-ignore-args (ignore-patterns)
  "Generate the -not ( -name <pat1> -o -name <pat2> ...) pattern for find cmd"
  (if ignore-patterns
      (concat " -not " (mk-proj-find-cmd-src-args ignore-patterns))
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
      (setq find-cmd (concat find-cmd " -not -path " (mk-proj-get-vcs-path))))
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

(defun project-compile (&optional opts)
 "Run the compile command (string or function) for this project."
 (interactive)
 (mk-proj-assert-proj)
 (let ((default-directory mk-proj-basedir))
   (cond ((stringp mk-proj-compile-cmd)
          (when (and (null opts) (called-interactively-p))
            (setq opts (read-string "Compile options: ")))
          (compile (concat mk-proj-compile-cmd " " opts)))
         ((fboundp mk-proj-compile-cmd)
          (cond ((commandp mk-proj-compile-cmd)
                 (call-interactively mk-proj-compile-cmd))
                (opts
                 (funcall mk-proj-compile-cmd opts))
                (t (funcall mk-proj-compile-cmd))))
         (t (message "No compile command defined.")))))

(defun project-compile (&optional opts)
  "Run the compile command for this project."
  (interactive)
  (mk-proj-assert-proj)
  (project-home)
  (if (stringp mk-proj-compile-cmd)
      (if opts
          (funcall 'mk-proj-compile opts)
        (call-interactively 'mk-proj-compile))
    (if (fboundp mk-proj-compile-cmd)
        (if (commandp mk-proj-compile-cmd)
            (call-interactively mk-proj-compile-cmd)
          (funcall mk-proj-compile-cmd)))))

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

(defun mk-proj-fib-init ()
  "Either load the *file-index* buffer from the file cache, or create it afresh."
  (if (and mk-proj-file-list-cache
           (file-readable-p mk-proj-file-list-cache))
      (with-current-buffer (find-file-noselect mk-proj-file-list-cache)
          (with-current-buffer (rename-buffer mk-proj-fib-name)
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            (message "Loading *file-index* from %s" mk-proj-file-list-cache)))
    (project-index)))

(defun mk-proj-fib-clear ()
  "Clear the contents of the fib buffer"
  (let ((buf (get-buffer mk-proj-fib-name)))
    (when buf
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (kill-region (point-min) (point-max))
        (setq buffer-read-only t)))))

(defun mk-proj-fib-cb (process event)
  "Handle failure to complete fib building"
  (cond
   ((string= event "finished\n")
    (with-current-buffer (get-buffer mk-proj-fib-name)
      (setq buffer-read-only t)
      (when mk-proj-file-list-cache
        (write-file mk-proj-file-list-cache)
        (rename-buffer mk-proj-fib-name)))
    (message "Refreshing %s buffer...done" mk-proj-fib-name))
   (t
    (mk-proj-fib-clear)
    (message "Failed to generate the %s buffer!" mk-proj-fib-name))))

(defun project-index ()
  "Regenerate the *file-index* buffer that is used for project-find-file"
  (interactive)
  (mk-proj-assert-proj)
  (when mk-proj-file-list-cache
    (mk-proj-fib-clear)
    (let* ((default-directory (file-name-as-directory mk-proj-basedir))
           (start-dir (if mk-proj-file-index-relative-paths "." mk-proj-basedir))
           (find-cmd (concat "find '" start-dir "' -type f "
                             (mk-proj-find-cmd-src-args mk-proj-src-patterns)
                             (mk-proj-find-cmd-ignore-args mk-proj-ignore-patterns)))
           (proc-name "index-process"))
      (when (mk-proj-get-vcs-path)
        (setq find-cmd (concat find-cmd " -not -path " (mk-proj-get-vcs-path))))
      (setq find-cmd (or (mk-proj-find-cmd-val 'index) find-cmd))
      (with-current-buffer (get-buffer-create mk-proj-fib-name)
        (buffer-disable-undo) ;; this is a large change we don't need to undo
        (setq buffer-read-only nil))
      (message "project-index cmd: \"%s\"" find-cmd)
      (message "Refreshing %s buffer..." mk-proj-fib-name)
      (start-process-shell-command proc-name mk-proj-fib-name find-cmd)
      (set-process-sentinel (get-process proc-name) 'mk-proj-fib-cb))))

(defun mk-proj-fib-matches (regex)
  "Return list of files in *file-index* matching regex.

If regex is nil, return all files. Returned file paths are
relative to the project's basedir."
  (let ((files '()))
    (with-current-buffer mk-proj-fib-name
      (goto-char (point-min))
      (while
          (progn
            (let ((raw-file (mk-proj-normalize-drive-letter
                             (buffer-substring
                              (line-beginning-position) (line-end-position)))))
              (when (> (length raw-file) 0)
                ;; file names in buffer can be absolute or relative to basedir
                (let ((file (if (file-name-absolute-p raw-file)
                                (file-relative-name raw-file mk-proj-basedir)
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

(defun* project-find-file (regex)
  "Find file in the current project matching the given regex.

The files listed in buffer *file-index* are scanned for regex
matches. If only one match is found, the file is opened
automatically. If more than one match is found, prompt for
completion. See also: `project-index', `project-find-file-ido'."
  (interactive "sFind file in project matching: ")
  (mk-proj-assert-proj)
  (unless (get-buffer mk-proj-fib-name)
    (message "Please use project-index to create the index before running project-find-file")
    (return-from "project-find-file" nil))
    (let* ((matches (mk-proj-fib-matches regex))
           (match-cnt (length matches)))
      (cond
       ((= 0 match-cnt)
        (message "No matches for \"%s\" in this project" regex))
       ((= 1 match-cnt )
        (find-file (car matches)))
       (t
        (let ((file (if (mk-proj-use-ido)
                        (ido-completing-read "Multiple matches, pick one (ido): " matches)
                      (completing-read "Multiple matches, pick one: " matches))))
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
  (unless (get-buffer mk-proj-fib-name)
    (message "Please use project-index to create the index before running project-find-file-ido")
    (return-from "project-find-file-ido" nil))
  (let ((file (ido-completing-read "Find file in project matching (ido): "
                                   (mk-proj-fib-matches nil))))
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

(defun mk-proj-get-friends (&optional name)
  (unless name
    (mk-proj-assert-proj)
    (setq name mk-proj-name))
  ;; go through all configs
  ;; collect all projects which have the requested name in their friend list
  ;; remove duplicates and return
  (let ((r '()))
    (maphash (lambda (k c)
               (unless (string-equal k name)
                 (when (mk-proj-any (lambda (f)
                                      (string-equal f name))
                                    (mk-proj-config-val 'friends c))
                   (setq r (append r `(,k)))))) mk-proj-list)
    (remove-duplicates (append r (mk-proj-config-val 'friends name t)) :test #'string-equal)))

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
  (let ((resulting-matches '()))
    (dolist (friend (mk-proj-get-friends) resulting-matches)
      ;; friends can be either project names or single files,
      ;; so first check if the friend is a single file here
      (if (not (stringp friend)) (error "Error in mk-proj-friend-matches, did you quote the friends list?"))
      (if (file-exists-p (expand-file-name friend))
          (if regex
              (when (string-match regex friend) (add-to-list 'resulting-matches friend))
            (add-to-list 'resulting-matches friend))
        ;; if friend is not a single file, it must be a project name
        (let ((friend-matches (mk-proj-get-project-files friend regex)))
          (if (and mk-proj-patterns-are-regex mk-proj-ignore-patterns)
              (dolist (file friend-matches resulting-matches)
                (dolist (pattern mk-proj-ignore-patterns resulting-matches)
                  (if (not (string-match pattern file))
                      (add-to-list 'resulting-matches file)))))
          (setq resulting-matches (append resulting-matches friend-matches)))))))

(defun mk-proj-friendly-buffer-p (buf)
  (let ((file-name (mk-proj-buffer-name buf)))
    (if (and file-name
             (block "friend-loop"
               (dolist (f (mk-proj-get-friends))
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
                         (print friend-basedir)
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

;; (defun mk-proj-kill-emacs-hook-friends ()
;;   (when (and mk-proj-name mk-proj-open-friends-cache)
;;     (mk-proj-save-open-friends-info)))

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
    (dolist (f (mk-proj-get-friends) basedirs)
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

(provide 'mk-project)














;;; mk-project.el ends here
