;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : rb.el
;;;; Authors         : Doug Bates
;;;;                 : Ed Kademan
;;;;                 : Frank Ritter
;;;;                 : David Smith
;;;; Created On      : October 14, 1991
;;;; Last Modified By: David Smith
;;;; Last Modified On: Mon Jun 29 15:04:26 CST 1992
;;;; Version         : 3.41
;;;; 
;;;; Lisp-dir-entry  : S-mode|
;;;;                   Doug Bates, Ed Kademan, Frank Ritter, David Smith|
;;;;                   dsmith@stats.adelaide.edu.au|
;;;;                   Interface to the S/Splus statistical software packages|
;;;;                   92-06-29|
;;;;                   3.4|
;;;;                   /attunga.stats.adelaide.edu.au:pub/S-mode/S-mode3.4.tar.Z
;;;;
;;;; PURPOSE
;;;; 	Interface to the S/Splus statistical software packages
;;;; 
;;;; Copyright 1989,1991,1992 Doug Bates    bates@stat.wisc.edu
;;;;                          Ed Kademan    kademan@stat.wisc.edu
;;;;                          Frank Ritter  ritter@psy.cmu.edu
;;;;                                            (or  @cs.cmu.edu)
;;;;                          David Smith   dsmith@stats.adelaide.edu.au
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The Changelog is at the end of this file.

;;; CREDITS.
;;; Thanks to shiba@shun.isac.co.jp (Ken'ichi "Modal" Shibayama) for
;;;   the indenting code.
;;; Thanks also to maechler@stat.math.ethz.ch (Martin Maechler) for
;;;   suggestions and bug fixes.
;;; S-eval-line-and-next-line is based on a function by Rod Ball 
;;;   (rod@marcam.dsir.govt.nz)
;;;
;;; Also thanks from David Smith to the previous authors for all their
;;; help and suggestions.

;;; BRIEF OVERVIEW
;;; Supports stuctured editing of S (a statistics package)
;;; functions that is integrated with a running S process in a
;;; buffer.  

;;; GENERAL DISCLAIMER
;;; 
;;; This program is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation; either
;;; version 1, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more
;;; details.
;;; 
;;; You should have received a copy of the GNU General Public
;;; License along with this program; if not, write to the Free
;;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;; 
;;; In short: you may use this code any way you like, as long as you
;;; don't charge money for it, remove this notice, or hold anyone liable
;;; for its results.

;;; OVERVIEW OF S MODE
;;; 
;;; S is a statistics package available from Bell Labs
;;; particularly suited for descriptive and exploratory
;;; statistics.  s-mode is built on top of comint (the general
;;; command interpreter mode written by Olin Shivers), and so
;;; comint.el (or comint.elc) should be either loaded or in your
;;; load path when you invoke it.
;;; 
;;; Aside from the general features offered by comint such as
;;; command history editing and job control, inferior S mode
;;; allows you to dump and load S objects into and from external
;;; files, and to display help on functions.  It also provides
;;; name completion while you do these.  For more detailed
;;; information see the documentation strings for S,
;;; inferior-rb-mode, rb-mode, and comint-mode.  There are also
;;; many variables and hooks available for customizing (see
;;; the variables below that have document strings that start
;;; with an "*").

;;; INSTALLATION
;;; Save this file in an appropriate directory and put the following
;;; line in your .emacs:
;;; 
;;;     (autoload 'S "~/elisp/S" "" t)
;;; 
;;; where "~/elisp/S.el" is the path name of this file.  That
;;; way, all you will have to do to get S running is to type
;;; "M-x S" from within emacs. You may also want to change some
;;; options, by putting lines such as the following in your .emacs:
;;; 
;;;     (setq inferior-rb-program "rb") ; command to run S
;;;	(setq rb-version-running "2.3") ; Running the old version
;;;	(setq rb-ask-about-display t) ; Ask for an X-display
;;;     (setq rb-source-directory
;;;	  (expand-file-name "~/rb-Src/"))
;;;	(setq rb-keep-dump-files t)
;;;                  ; Make a directory of backup object source files
;;;
;;; See the section "User changeable variables" below for more options.

;;; GETTING LATER RELEASES OF S MODE
;;; The latest version is available from statlib by sending a
;;; blank message with subject "send index from S" to
;;; statlib@stat.cmu.edu, and following the directions from
;;; there.  Comint is probably already available at your site, 
;;; and already in your load path.  If it is not, you can get it
;;; from archive.cis.ohio-state.edu (login anonymous, passwd id)
;;; in directory /pub/gnu/emacs/elisp-archive/as-is/comint.el.Z
;;; This version has been tested and works with (at least) 
;;; comint-version 2.03.  You probably have copies of comint.el 
;;; on your system.  Copies are also available from ritter@cs.cmu.edu,
;;; and shivers@cs.cmu.edu.
;;;
;;; rb-mode is also available for anonymous FTP from
;;; attunga.stats.adelaide.edu.au in the directory pub/rb-mode. It is
;;; alsa avaliable from the Emacs-lisp archive on
;;; archive.cis.ohio-state.edu.

;;; RELEASE 2.1 INFORMATION
;;;
;;; Improvements since last release (unnumbered of Summer 1990):
;;; * Better description provided of functions loaded.
;;; * Better header for this file.
;;; * rb-directory is now a prescriptive rather than just 
;;;   descriptive variable.  
;;; * better syntax table, so |#; are better recognized and
;;;   commands using them work better.
;;; * we have a version number.
;;;
;;; RELEASE 3.4 INFORMATION
;;; 
;;; * Works with version 3.0 S
;;; * Command-line completion of S object names
;;; * Recognition of attached data frames 
;;; * Dedicated S Help mode
;;; * Tek graphics support
;;; * Several bugfixes and code cleanups
;;; * Texinfo documentation
;;;
;;; Remaining Bugs:
;;; 
;;; * It would be nice to use .Last.value when running S+
;;; * It would be nice to use S VERSION when running S+
;;; Until the end of August 1992, please report bugs to me at
;;; dsmith@stats.adelaide.edu.au. After this date, mail to that address
;;; will not be answered for some time; please contact Frank Ritter
;;; (Frank_Ritter@SHAMO.SOAR.CS.CMU.EDU) or any of the other authors then
;;; (please CC: to me as well though -- you never know, I might just
;;; answer!) Comments, suggestions, words of praise and large cash
;;; donations are also more than welcome.

;;; Inits and provides
;;;=====================================================
;;;

(require 'comint)
(require 'comint-extra)
(autoload 'comint-isearch "comint-isearch" 
	  "Isearch for comint [full documentation when loaded]" t)
(provide 'rb)

(defconst rb-mode-version "3.41" 
  "Version of rb-mode currently loaded.")

;; this will appear for just a short while, but it's a
;; chance to teach...
(message 
 (concat (substitute-command-keys
	  "Type \\[describe-mode] for help on rb-mode version ")
	 rb-mode-version))



;;; User changeable variables
;;;=====================================================
;;; Users note: Variables with document strings starting
;;; with a * are the ones you can generally change safely, and
;;; may have to upon occasion.

;;; System dependent variables

(defvar inferior-rb-program "rb"
  "*Program name for invoking an inferior S.")

(defvar inferior-rb-args nil
  "*String of arguments passed to the S process on startup if the name of
the S program is `rb'.")

(defvar rb-version-running "3.0"
  "Version of S being run.")
;;; The value of this variable affects the
;;; default values of the following variables:
;;; 
;;;	 inferior-rb-help-command
;;;	 inferior-rb-search-list-command
;;;	 rb-dump-error-re
;;; 
;;; Modifications to these variables are made at *load* time (provided, of
;;; course, they have not already been given values), hence changing the
;;; value of rb-version-running after this package is loaded will have no
;;; effect.
;;; 
;;; Currently the string \"3.0\" is the only value of this variable with
;;; any real meaning; in this case the defaults are set to comply with the
;;; August '91 (3.0) version of S/Splus, defaults which also work for
;;; version 2.3. Any other value than \"3.0\" sets the defaults to comply
;;; with the 1988 version of S/Splus.")
;;;
;;; Please reserve the following values as special:
;;;   "3.0"    Version 3.0 (August '91) of S/Splus
;;;   "2.3"    Version 2.3 of S/Splus
;;;   "old"    Any older version

(defvar rb (assoc inferior-rb-program '(("rb") ("rb")))
  "Set to t if Splus is being used instead of vanilla S")
;;; Used for setting default values of other variables, and hence
;;; has no effect after S.el has been loaded.

(defvar inferior-rb-prompt "\\(\\+\\|[a-zA-Z0-9() ]*>\\) *"
  "The regular expression inferior S mode uses for recognizing prompts
Do not anchor to bol with `^'.")

(defvar inferior-rb-primary-prompt "[a-zA-Z0-9() ]*> *"
  "Regular expression used by rb-mode to detect the primary prompt.
Do not anchor to bol with `^'.")

;;; Initialising the environment

(defvar rb-ask-for-rb-directory t
  "*If non-nil, the process directory will be requested each time S is run")

(defvar rb-ask-about-display nil
  "*If non-nil, asks for a value for the DISPLAY environment
variable, to make X-windows work with S")

(defvar X-displays-list '("unix:0.0")
  "List of strings that are candidates for the DISPLAY environment variable.")

(defvar rb-directory (file-name-as-directory (getenv "HOME"))
  "*The directory S is run from.  It must end in a slash.
Provided as a default if rb-ask-for-rb-directory is non-nil.")

;;; Editing functions

(defvar rb-insert-function-templates t
  "*Boolean flag specifying action when editing a non-existent object.
If t, then when the text of a dumped object contains rb-dumped-missing-re,
then it will be replaced by rb-function-template.")

  ;; By K.Shibayama 5.14.1992

(defvar rb-indent-level 2
  "*Indentation of S statements with respect to containing block.")

(defvar rb-brace-imaginary-offset 0
  "*Imagined indentation of a S open brace that actually follows a statement.")

(defvar rb-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context.")

(defvar rb-continued-statement-offset 2
  "*Extra indent for lines not starting new statements.")

(defvar rb-continued-brace-offset 0
  "*Extra indent for substatements that start with open-braces.
This is in addition to rb-continued-statement-offset.")

(defvar rb-arg-function-offset 2
  "*Extra indent for internal substatements of function `foo' that called
in `arg=foo(...)' form. 
If not number, the statements are indented at open-parenthesis following foo.")

(defvar rb-expression-offset 4
  "*Extra indent for internal substatements of `expression' that specified
in `obj <- expression(...)' form. 
If not number, the statements are indented at open-parenthesis following 
`expression'.")

(defvar rb-auto-newline nil
  "*Non-nil means automatically newline before and after braces
inserted in S code.")

(defvar rb-tab-always-indent t
  "*Non-nil means TAB in S mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")
   
(defvar rb-default-style 'GNU
  "*The default value of rb-style")

(defvar rb-style rb-default-style
  "*The buffer specific S indentation style.")

;;; Dump files

(defvar rb-source-directory "/tmp/"
  "*Directory in which to place dump files.  
The directory generated by rb-source-directory-generator (if it is
non-nil) is used preferentially, and the value of rb-source-directory
is used only of the generated directory can not be written or
created.")

(defvar rb-source-directory-generator nil
  "*Function which, when called with no args, will return a directory
name (ending in a slash) into which S objects should be dumped. If this is
nil of the directory does not exist and cannot be created, the value of
rb-source-directory is used.")
;;; Possible value:
;;; '(lambda () (file-name-as-directory 
;;;	      (expand-file-name (concat (car rb-search-list) "/.Src"))))
;;; This always dumps to a sub-directory (".Src") of the current S
;;; working directory (i.e. first elt of search list)

(defvar rb-dump-filename-template (concat (user-login-name) ".%s.rb")
  "*Template for filenames of dumped objects.
%s is replaced by the object name.")
;;; This gives filenames like `user.foofun.rb', so as not to clash with
;;; other users if you are using a shared directory. Other alternatives:
;;; "%s.rb" ; Don't bother uniquifying if using your own directory(ies)
;;; "dump" ; Always dump to a specific filename. This makes it impossible
;;;          to edit more than one object at a time, though.
;;; (make-temp-name "scr.") ; Another way to uniquify

(defvar rb-keep-dump-files nil
  "*If nil, delete dump files ater use. Otherwise, never delete.")
;;; Boolean flag which determines what to do with the dump files
;;; generated by \\[rb-dump-object-into-edit-buffer], as follows:
;;; 
;;; 	If nil: dump files are deleted after each use, and so appear
;;; only transiently. The one exception to this is when a loading error
;;; occurs, in which case the file is retained until the error is
;;; corrected and the file re-loaded.
;;; 
;;; 	If non-nil: dump files are not deleted, and backups are kept
;;; as usual.  This provides a simple method for keeping an archive of S
;;; functions in text-file form.
;;; 
;;; Auto-save is always enabled in dump-file buffers to enable recovery
;;; from crashes.

(defvar rb-function-template " function( )\n{\n\n}\n"
  "Function template used when editing nonexistent objects. 
The edit buffer will contain the object name in quotes, followed by
\"<-\", followed by this string.")

;;; Interacting with the S process

(defvar rb-execute-in-process-buffer nil
  "*If non-nil, the rb-execute- commands output to the process buffer.
Otherwise, they get their own temporary buffer.")

(defvar rb-eval-visibly-p nil
  "*If non-nil, the rb-eval- commands display the text to be evaluated 
in the process buffer.")

(defvar rb-tek-mode nil
  "*Grab Tek Graphics?
Toggle with \\[rb-tek-mode-toggle].")

(defvar rb-tek-possible-graph-prompts "Selection: "
  "Prompts that might follow TEK graphics. 
If S mode seems to lock up when grabbing graphics, it probably means
you need something else in here. Your prompt is assumed: you don't
need to include it. Separate options with \\|")

(defvar rb-tek-pause-for-graphics (not (string= (getenv "TERM") "xterm"))
  "If t, wait for a key to be pressed before returning to text mode.
Use this option when graphics and text share the same screen.")

;;; Help mode

(defvar rb-help-sec-keys-alist 
  '((?a . "ARGUMENTS:") 
    (?b . "BACKGROUND:") (?B . "BUGS:")
    (?d . "DETAILS:") (?D . "DESCRIPTION:")
    (?e . "EXAMPLES:") 
    (?n . "NOTE:") (?o . "OPTIONAL ARGUMENTS:") (?r . "REQUIRED ARGUMENTS:") 
    (?R . "REFERENCES:") 
    (?s . "SIDE EFFECTS:") (?S . "SEE ALSO:") (?u . "USAGE:") (?v . "VALUE:"))
  "Alist of (key . string) pairs for use in section searching.")
;;; `key' indicates the keystroke to use to search for the section heading
;;; `string' in an S help file. `string' is used as part of a
;;; regexp-search, and so specials should be quoted.

;;; Hooks

(defvar rb-mode-hook '()
  "*Hook for customizing S mode each time it is entered.")

(defvar rb-mode-load-hook '()
  "*Hook to call when S.el is loaded.")

(defvar rb-pre-run-hook nil
  "*Hook to call before starting up S.
Good for setting up your directory.")
;; You can put something like:
;; (setq rb-directory (file-name-as-directory (concat (getenv "HOME") "/S")))
;; in your ~/.emacs file and S will always start up in your ~/S directory.
;; Alternatively, you can get S to start up in the directory you start 
;; Emacs from by putting this in your .emacs:
;; (setq rb-pre-run-hook '((lambda () (setq rb-directory default-directory))))



;;; System variables
;;;=====================================================
;;; Users note: You will rarely have to change these 
;;; variables.

(defvar rb-change-sp-regexp
  "\\(attach(\\([^)]\\|$\\)\\|detach(\\|collection(\\|library(\\)"
  "The regexp for matching the S commands that change the search path.")

(defvar rb-function-pattern
  (concat
   "\\(" ; EITHER
   "\\s\"" ; quote
   "\\(\\sw\\|\\s_\\)+" ; symbol
   "\\s\"" ; quote
   "\\s-*\\(<-\\|_\\)\\(\\s-\\|\n\\)*" ; whitespace, assign, whitespace/nl
   "function\\s-*(" ; function keyword, parenthesis
   "\\)\\|\\(" ; OR
   "\\<\\(\\sw\\|\\s_\\)+" ; symbol
   "\\s-*\\(<-\\|_\\)\\(\\s-\\|\n\\)*" ; whitespace, assign, whitespace/nl
   "function\\s-*(" ; function keyword, parenthesis
   "\\)")
  "The regular expression for matching the beginning of an S function.")

(defvar rb-source-modes '(rb-mode)
  "A list of modes used to determine if a buffer contains S source code.")
;;; If a file is loaded into a buffer that is in one of these major modes, it
;;; is considered an S source file.  The function rb-load-file uses this to
;;; determine defaults.

(defvar inferior-rb-load-command "source(\"%s\")\n"
  "Format-string for building the S command to load a file.")
;;; This format string should use %s to substitute a file name
;;; and should result in an S expression that will command the inferior S
;;; to load that file.

(defvar inferior-rb-dump-command "dump(\"%s\",file=\"%s\")\n"
  "Format-string for building the S command to dump an object into a file.")
;;; Use first %s to substitute an object name
;;;     second %s substitutes the dump file name.

(defvar inferior-rb-help-command 
  (if rb
      "help(\"%s\",pager=\"cat\",window=F)\n" 
    "help(\"%s\")\n")
  "Format-string for building the S command to ask for help on an object.")
;;; This format string should use %s to substitute an object name.

(defvar inferior-rb-search-list-command "search()\n"
  "S command that prints out the search list.")
;;; i.e. The list of directories and (recursive) objects that S uses when
;;; it searches for objects.

(defvar inferior-rb-names-command "names(%s)\n"
  "Format string for S command to extract names from an object.")
;;; %s is replaced by the object name -- usually a list or data frame

(defvar inferior-rb-objects-command 
  (if (string= rb-version-running "3.0")
      "objects(%d)"
    "ls()")
  "Format string for S command to get a list of objects at position %d")
;;; Don't include a newline at the end! Used in rb-execute-objects

(defvar rb-dumped-missing-re "\nDumped\n\\'"
  "If a dumped object's buffer matches this re, then it is replaced
by rb-function-template.")

(defvar rb-dump-error-re 
  (if (string= rb-version-running "3.0") "\nDumped\n\\'" "[Ee]rror")
  "Regexp used to detect an error when loading a file.")

(defvar rb-error-buffer-name " *rb-errors*"
  "Name of buffer to keep error messages in.")

(defvar rb-loop-timeout 20000
  "Integer specifying how many loops rb-mode will wait for the prompt for
before signalling an error.")

(defvar rb-search-list nil
  "The list of directories and (recursive) objects to search for S objects.")

(defvar rb-sl-modtime-alist nil
  "Alist of modtimes for all S directories accessed this session.")

(defvar rb-sp-change nil
  "This symbol flags a change in the S search path.")

(defvar rb-prb-load-dir/file nil
  "This symbol saves the (directory . file) pair used in the last
rb-load-file command.  Used for determining the default in the next one.")

(defvar inferior-rb-get-prompt-command "options()$prompt\n"
  "Command to find the value of the current S prompt.")

(defvar rb-temp-buffer-p nil
  "*Flags whether the current buffer is a temporary buffer created by rb-mode.
Such buffers will be killed by \\[rb-quit] or \\[rb-cleanup].
Source buffers and help buffers have this flag set.
This is a buffer-local variable.")
(make-variable-buffer-local 'rb-temp-buffer-p)

(defvar rb-local-variables-string "

# Local Variables:
# mode:S
# rb-temp-buffer-p:t
# End:
")

(defvar rb-style-alist 
'((GNU (rb-indent-level . 2)
       (rb-continued-statement-offset . 2)
       (rb-brace-offset . 0)
       (rb-arg-function-offset . 4)
       (rb-expression-offset . 2))
  (BSD (rb-indent-level . 8)
       (rb-continued-statement-offset . 8)
       (rb-brace-offset . -8)
       (rb-arg-function-offset . 0)
       (rb-expression-offset . 8))
  (K&R (rb-indent-level . 5)
       (rb-continued-statement-offset . 5)
       (rb-brace-offset . -5)
       (rb-arg-function-offset . 0)
       (rb-expression-offset . 5))
  (C++ (rb-indent-level . 4)
       (rb-continued-statement-offset . 4)
       (rb-brace-offset . -4)
       (rb-arg-function-offset . 0)
       (rb-expression-offset . 4)))
"Predefined formatting styles for S code")

(defvar rb-tek-simple-prompt nil
  "Explicit version of primary S prompt.")



;;; rb-mode helper functions and code
;;;=====================================================
;;;

(defvar inferior-rb-mode-map nil)
(if inferior-rb-mode-map
    nil
  (setq inferior-rb-mode-map (full-copy-sparse-keymap comint-mode-map))
  (define-key inferior-rb-mode-map "\r" 'inferior-rb-send-input)
  (define-key inferior-rb-mode-map "\eP" 'comint-msearch-input)
  (define-key inferior-rb-mode-map "\eN" 'comint-psearch-input)
  (define-key inferior-rb-mode-map "\C-c\C-b" 'comint-msearch-input-matching)
  (define-key inferior-rb-mode-map "\eS" 'comint-next-similar-input)
  (define-key inferior-rb-mode-map "\er" 'comint-isearch)
  (define-key inferior-rb-mode-map "\C-c\C-l" 'rb-load-file)
  (define-key inferior-rb-mode-map "\C-x`" 'rb-parse-errors)
  (define-key inferior-rb-mode-map "\C-c\C-d" 'rb-dump-object-into-edit-buffer)
  (define-key inferior-rb-mode-map "\C-c\C-h" 'rb-display-help-on-object)
  (define-key inferior-rb-mode-map "\C-c\C-t" 'rb-tek-mode-toggle)
  (define-key inferior-rb-mode-map "\C-c\C-q" 'rb-quit)
  (define-key inferior-rb-mode-map "\C-c\C-e" 'rb-execute)
  (define-key inferior-rb-mode-map "\C-c\C-s" 'rb-execute-search)
  (define-key inferior-rb-mode-map "\C-c\C-x" 'rb-execute-objects)
  (define-key inferior-rb-mode-map "\C-c\C-a" 'rb-execute-attach)
  (define-key inferior-rb-mode-map "\C-c\C-z" 'rb-abort)       ; these mask in
  (define-key inferior-rb-mode-map "\C-c\C-o" 'rb-kill-output) ; comint-m-map
  (define-key inferior-rb-mode-map "\C-c\C-v" 'rb-view-at-bottom)
  (define-key inferior-rb-mode-map "\t" 'rb-complete-object-name)) 

(defvar rb-mode-syntax-table nil "Syntax table for rb-mode.")
(if rb-mode-syntax-table
    nil
  (setq rb-mode-syntax-table (make-syntax-table c-mode-syntax-table))
  (modify-syntax-entry ?# "<" rb-mode-syntax-table)  ; now an open comment
  (modify-syntax-entry ?\n ">" rb-mode-syntax-table) ; close comment
  (modify-syntax-entry ?_ "." rb-mode-syntax-table)  
  (modify-syntax-entry ?. "w" rb-mode-syntax-table)  ; making S names same as
  (modify-syntax-entry ?$ "w" rb-mode-syntax-table)  ; words makes coding easier
  (modify-syntax-entry ?* "." rb-mode-syntax-table)
  (modify-syntax-entry ?< "." rb-mode-syntax-table)
  (modify-syntax-entry ?> "." rb-mode-syntax-table)
  (modify-syntax-entry ?/ "." rb-mode-syntax-table))


(defvar inferior-rb-mode-hook '()
  "*Hook for customizing inferior S mode.
Called after inferior-rb-mode is entered and variables have been initialised.")


;;;
;;; Starting up
;;;

(defun S ()
  "Run an inferior S process, input and output via buffer *S*.
If there is a process already running in *S*, just switch to that buffer.
Takes the program name from the variable inferior-rb-program.
The S program name is used to make a symbol name such as `inferior-rb-args'.
If that symbol is a variable its value is used as a string of arguments
when invoking S.
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive)
  (if (not (comint-check-proc "*S*"))
      (let* ((symbol-string
              (concat "inferior-" inferior-rb-program "-args"))
             (switches-symbol (intern-soft symbol-string))
             (switches
              (if (and switches-symbol (boundp switches-symbol))
                  (symbol-value switches-symbol))))
        (run-hooks 'rb-pre-run-hook)
	(if rb-ask-for-rb-directory (rb-set-directory))
	(if rb-ask-about-display (rb-set-display))
	(set-buffer
         (if switches
             (inferior-rb-make-comint switches)
           (inferior-rb-make-comint)))
        (inferior-rb-mode)
        (inferior-rb-wait-for-prompt)
        (goto-char (point-max))
	(setq rb-sl-modtime-alist nil)
	(rb-tek-get-simple-prompt)
	(rb-get-search-list)))
  (switch-to-buffer "*S*"))

(defun rb-set-directory nil
  "Interactively set rb-directory."
  (setq rb-directory
	(expand-file-name
	 (file-name-as-directory
	  (read-file-name
	   "From which directory? " rb-directory rb-directory t)))))

(defun rb-set-display nil
  "Interactively set DISPLAY variable for S process"
  (let* ((matches (append (mapcar
			   '(lambda (envelt)
			      (if (string-match "^DISPLAY=\\(.*\\)$" envelt)
				  (substring envelt (match-beginning 1) (match-end 1))))
			   process-environment)
			  (list (getenv "DISPLAY"))))
	 (initial (eval (cons 'or matches))))
    (setq process-environment 
	  (comint-update-env
	   process-environment
	   (list (concat
		  "DISPLAY="
		  (completing-read 
		   "Which X-display? "
		   (mapcar 'list X-displays-list)
		   nil
		   nil
		   initial)))))))

;;; define two commands consistent with other comint modes, run-s &
;;; run-rb.
(fset 'run-s (fset 'run-rb (symbol-function 'S)))

(defun inferior-rb-mode () 
  "Major mode for interacting with an inferior S process.  
Runs an S interactive job as a subprocess of Emacs, with I/O through an
Emacs buffer.  Variable inferior-rb-program controls which S
is run.

Commands are sent to the S process by typing them, and pressing
\\[inferior-rb-send-input]. Pressing \\[rb-complete-object-name] completes known
object names. Other keybindings for this mode are:

\\{inferior-rb-mode-map}

When editing S objects, the use of \\[rb-load-file] is advocated.
rb-load-file keeps source files (if rb-keep-dump-files is non-nil) in
the directory specified by rb-source-directory-generator, with the
filename chosen according to rb-dump-filename-template. When a file is
loaded, rb-mode parses error messages and jumps to the appropriate file
if errors occur. The rb-eval- commands do not do this.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-rb-mode-hook (in that order).

You can send text to the inferior S process from other buffers containing
S source. The key bindings of these commands can be found by typing 
^h m (help for mode) in the other buffers.
    rb-eval-region sends the current region to the S process.
    rb-eval-buffer sends the current buffer to the S process.
    rb-eval-function sends the current function to the S process.
    rb-eval-line sends the current line to the S process.
    rb-beginning-of-function and rb-end-of-function move the point to
        the beginning and end of the current S function.
    rb-switch-to-rb switches the current buffer to the S process buffer.
    rb-switch-to-end-of-rb switches the current buffer to the S process
        buffer and puts point at the end of it.

    rb-eval-region-and-go, rb-eval-buffer-and-go,
        rb-eval-function-and-go, and rb-eval-line-and-go switch to the S
        process buffer after sending their text.

    rb-dump-object-into-edit-buffer moves an S object into a temporary file
        and buffer for editing
    rb-load-file sources a file of commands to the S process.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Crosshatches start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp (concat "^" inferior-rb-prompt))
  (setq major-mode 'inferior-rb-mode)
  (setq mode-name "Inferior S")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-rb-mode-map)
  (set-syntax-table rb-mode-syntax-table)
  (setq comint-input-sentinel 'rb-search-path-tracker)
  (setq comint-get-old-input 'inferior-rb-get-old-input)
  (make-local-variable 'scroll-step)
  (setq scroll-step 4)
  (make-local-variable 'input-ring-size)
  (setq input-ring-size 50)
  (run-hooks 'inferior-rb-mode-hook))

;;; This function is a modification of make-comint from the comint.el
;;; code of Olin Shivers.
(defun inferior-rb-make-comint (&rest switches)
  (let* ((name "S")
         (buffer (get-buffer-create (concat "*" name "*")))
         (proc (get-buffer-process buffer)))
    ;; If no process, or nuked process, crank up a new one and put buffer in
    ;; comint mode. Otherwise, leave buffer and existing process alone.
    (cond ((or (not proc) (not (memq (process-status proc) '(run stop))))
           (save-excursion
             (set-buffer buffer)
             (setq default-directory rb-directory)
             (comint-mode)) ; Install local vars, mode, keymap, ...
           (comint-exec buffer name inferior-rb-program nil switches)))
    buffer))

(defun inferior-rb-send-input ()
  "Sends the command on the current line to the S process."
  (interactive)
  (comint-send-input)
  (if (and rb-sp-change
           (inferior-rb-wait-for-prompt))
      (progn
        (rb-get-search-list)
        (setq rb-sp-change nil))
    ;; Is this TEK graphics output?
    (if rb-tek-mode 
	(progn
	  (require 'rb-tek)
	  (rb-tek-snarf-graphics)))))

(defun inferior-rb-get-old-input ()
  "Returns the S command surrounding point."
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at inferior-rb-prompt))
	(rb-error "No command on this line."))
    (if (looking-at inferior-rb-primary-prompt) nil
	(re-search-backward (concat "^" inferior-rb-primary-prompt)))
    (comint-skip-prompt)
    (let (command
	   (beg (point)))
      (end-of-line)
      (setq command (buffer-substring beg (point)))
      (forward-line 1)
      (while (and (looking-at inferior-rb-prompt) 
		  (not (looking-at inferior-rb-primary-prompt)))
	;; looking at secondary prompt
	(comint-skip-prompt)
	(setq beg (point))
	(end-of-line)
	(setq command (concat command " " (buffer-substring beg (point))))
	(forward-line 1))
      command)))

(defun rb-error (msg)
  "Something bad has happened. Display the S buffer, and cause an error 
displaying MSG."
  (display-buffer (process-buffer (get-process "S")))
  (error msg))
		      
(defun inferior-rb-wait-for-prompt ()
  "Wait until the S process is ready for input."
  (let* ((cbuffer (current-buffer))
         (sprocess (get-process "S"))
         (sbuffer (process-buffer sprocess))
         r
	 (timeout 0))
    (set-buffer sbuffer)
    (while (progn
	     (if (not (eq (process-status sprocess) 'run))
		 (rb-error "S process has died unexpectedly.")
	       (if (> (setq timeout (1+ timeout)) rb-loop-timeout)
		   (rb-error "Timeout waiting for prompt. Check inferior-rb-prompt or rb-loop-timeout."))
	       (accept-process-output)
	       (goto-char (point-max))
	       (beginning-of-line)
	       (setq r (looking-at inferior-rb-prompt))
	       (not (or r (looking-at ".*\\?\\s *"))))))
    (goto-char (point-max))
    (set-buffer cbuffer)
    (symbol-value r)))

(defun rb-dump-object-into-edit-buffer (object)
  "Edit an S object in its own buffer.  Without a prefix argument,
this simply finds the file pointed to by rb-dump-filename. If this file
does not exist, or if a prefix argument is given, a dump() command is
sent to the S process to generate the source buffer."
  (interactive (rb-read-object-name "Object to edit: "))
  (let* ((filename (concat (if rb-source-directory-generator 
			       (funcall rb-source-directory-generator) 
			     rb-source-directory)
			   (format rb-dump-filename-template object)))
         (complete-dump-command (format inferior-rb-dump-command
                                        object filename))
	 (old-buff (get-file-buffer filename)))
    (if rb-source-directory-generator
	(let ((the-dir (file-name-directory filename)))
	  ;; If the directory doesn't exist, create if possible and approved.
	  (if (not (file-writable-p filename)) ; Can't create file
	      (if (and (not (file-exists-p the-dir)) ; No such directory
		       (file-writable-p	; Can we create dir in parent?
			(file-name-directory (directory-file-name the-dir)))
		       (y-or-n-p	; Approved
			(format "Directory %s does not exist. Create it? " the-dir))) ; and we want to create it
		  (make-directory (directory-file-name the-dir))
		(setq filename (concat rb-source-directory 
				       (format rb-dump-filename-template object)))))))
    ;; Try and find a buffer or filename before asking S
    (catch 'found-text
      (if (not current-prefix-arg)
	  (cond 
	   (old-buff 
	    (pop-to-buffer old-buff)
	    (message "Popped to edit buffer.")
	    (throw 'found-text nil))
	   ((file-exists-p filename) 
	    (find-file-other-window filename)
	    (message "Read %s" filename)
	    (throw 'found-text nil))))
      (rb-command complete-dump-command)
      (let ((old-buff (get-file-buffer filename)))
	(if old-buff
	    (kill-buffer old-buff)))	;make sure we start fresh
      ;; Generate a buffer with the dumped data
      (find-file-other-window filename)
      (rb-mode)
      (auto-save-mode 1)		; Auto save in this buffer
      (setq rb-temp-buffer-p t)		; Flag as a temp buffer
      (if rb-insert-function-templates
	  (progn 
	    (goto-char (point-max))
	    (if (re-search-backward rb-dumped-missing-re nil t)
		(replace-match rb-function-template t t))
	    (goto-char (point-min))))	;It might be nice to go between braces here
      ;; Insert the local variables stuff
      (save-excursion
	(goto-char (point-max))
	(insert rb-local-variables-string)
	(if rb-keep-dump-files nil
	  (set-buffer-modified-p nil)))
      (message "Dumped in %s" filename)
      (if rb-keep-dump-files nil 
	  (delete-file filename))) ; In case buffer is killed
    (setq rb-prb-load-dir/file
	  (cons (file-name-directory filename)
		(file-name-nondirectory filename)))))

(defun rb-read-object-name (p-string)
  (let* ((default (rb-read-object-name-default))
         (prompt-string (if default
                            (format "%s(default %s) " p-string default)
                          p-string))
         (rb-object-list (rb-get-object-list))
         (spec (completing-read prompt-string rb-object-list)))
    (list (cond
           ((string= spec "") default)
           (t spec)))))

(defun rb-read-object-name-default ()
 (save-excursion
   ;; The following line circumvents an 18.57 bug in following-char
   (if (eobp) (backward-char 1)) ; Hopefully buffer is not empty!
   ;; Get onto a symbol
   (catch 'nosym ; bail out if there's no symbol at all before point
     (while (/= (char-syntax (following-char)) ?w)
       (if (bobp) (throw 'nosym nil) (backward-char 1)))
     (let* 
	 ((end (progn (forward-sexp 1) (point)))
	  (beg (progn (backward-sexp 1) (point))))
       (buffer-substring beg end)))))

(defun rb-object-names (dir)
  "Return alist of S object names in directory (or object) DIR"
  (if (string-match "^/" dir) 
      (mapcar 'list (directory-files dir))
    ;;It might be an object name; try to get names
    (let ((tbuffer (generate-new-buffer "names-list"))
	  (objname dir)
	  names)
      (save-excursion
	(set-buffer tbuffer)
	(buffer-flush-undo tbuffer)
	(rb-command (format inferior-rb-names-command objname) tbuffer)
	(goto-char (point-min))
	(if (not (looking-at "\\s-*\\[1\\]"))
	    (setq names nil)
	  (goto-char (point-max))
	  (while (re-search-backward "\"\\([^\"]*\\)\"" nil t)
	    (setq names (cons (buffer-substring (match-beginning 1)
						(match-end 1)) names))))
	(kill-buffer tbuffer))
      (mapcar 'list names))))

(defun rb-resynch nil
"Reread all directories/objects in rb-search-list to form completions."
 (interactive)
 (setq rb-sl-modtime-alist nil)
 (rb-get-search-list))
       
(defun rb-extract-onames-from-alist (dir) 
"Extract the object names for directory (or object) DIR from rb-sl-modtime-alist
generating a new set if the directory has been recently modified."
  (let* ((assoc-res (assoc dir rb-sl-modtime-alist))
	 (data-cell (cdr assoc-res))
	 (last-mtime (car data-cell))
	 (new-mtime (rb-dir-modtime dir))
	 (old-objs (cdr data-cell)))
    (if (equal new-mtime last-mtime) old-objs
      (setcar data-cell new-mtime)
      (setcdr data-cell (rb-object-names dir)))))

(defun rb-dir-modtime (dir)
"Return the last modtime if dir is a directory, and nil otherwise."
;; Attached dataframes return a modtime of nil. It probably wouldn't be
;; too difficult to find the modtime of the actual object by searching for 
;; it along rb-search-list, but one hardly ever modifies dataframes after
;; they're attached, and I couldn't be bothered anyway.
  (if (string-match "^/" dir) 
      (nth 5 (file-attributes dir))))

(defun rb-get-search-list ()
  "Get the list of directories and (recursive) objects that S searches
when it looks for objects."
  (save-excursion
  (let ((tbuffer (generate-new-buffer "search-list"))
	dir-assoc
        dir)
    (setq rb-search-list nil)
    (buffer-flush-undo tbuffer)
    (set-buffer tbuffer)
    (rb-command inferior-rb-search-list-command tbuffer)
    (goto-char (point-max))
    (while (re-search-backward "\"\\([^\"]*\\)\"" nil t)
      (setq dir (buffer-substring (match-beginning 1) (match-end 1)))
      (if (and (string-match "^[^/]" dir)
	       (file-directory-p (concat rb-directory dir)))
          (setq dir (concat rb-directory dir)))
      (setq rb-search-list (cons dir rb-search-list))
      (setq dir-assoc (assoc dir rb-sl-modtime-alist))
      (if (not dir-assoc)
	  (let (conselt)
	    (setq conselt (cons dir
				(cons (rb-dir-modtime dir)
				      (rb-object-names dir))))
	    (setq rb-sl-modtime-alist (cons conselt rb-sl-modtime-alist)))))
    (kill-buffer tbuffer))))

(defun rb-get-object-list ()
  "Return the alist of current S object names."
;;; suitable for use with completing-read
  (rb-get-object-list-r rb-search-list))

(defun rb-get-object-list-r (s-list)
  "Return the alist of current S object names, recursive version.
rb-LIST is the search list of directories (or objects) for S." 
  (let* ((dir (car s-list))
         (dir-list (cdr s-list)))
    (if (null dir)
        nil
      (append (rb-extract-onames-from-alist dir)
              (rb-get-object-list-r dir-list)))))

(defun rb-command (com &optional buf visible)
  "Send the S process command COM and delete the output
from the S process buffer.  If an optional second argument BUF exists
save the output in that buffer. If optional third arg VISIBLE is
non-nil, both the command and the output appear in the S process
buffer."
  (let* ((cbuffer (current-buffer))
         (sprocess (get-process "S"))
         sbuffer
	 start-of-output
	 point-holder)
    (if sprocess nil (error "No S process running!"))
    (setq sbuffer (process-buffer sprocess))
    (set-buffer sbuffer)
    (setq point-holder (point-marker))
    (goto-char (marker-position (process-mark sprocess)))
    (beginning-of-line)
    (if (looking-at inferior-rb-primary-prompt) nil
      (goto-char (marker-position point-holder))
      (rb-error 
       "S process not ready. Finish your command before trying again."))
    (if visible
	(progn
	  (goto-char (marker-position (process-mark sprocess)))
	  (insert-before-markers com) ))
    (setq start-of-output (marker-position (process-mark sprocess)))
    (process-send-string sprocess com)
    (while (progn
             (accept-process-output sprocess)
             (goto-char (marker-position (process-mark sprocess)))
             (beginning-of-line)
	     (if (< (point) start-of-output) (goto-char start-of-output))
	     (not (looking-at inferior-rb-primary-prompt))))
    (if buf
        (append-to-buffer buf start-of-output (point)))
    (if visible (goto-char (marker-position (process-mark sprocess)))
      (delete-region start-of-output
		     (marker-position (process-mark sprocess)))
      (goto-char (marker-position point-holder)))
    (set-buffer cbuffer)))

(defun rb-eval-visibly (text &optional invisibly)
  "Evaluate TEXT in the S process buffer as if it had been typed in.
If optional secod arg INVISIBLY is non-nil, don't echo commands. If 
if is a string, just include that string.
Waits for prompt after each line of input, so won't break on large texts."
  (let* ((cbuffer (current-buffer))
         (sprocess (get-process "S"))
         (sbuffer (process-buffer sprocess))
	 start-of-output
	 com pos)
    (set-buffer sbuffer)
    (goto-char (marker-position (process-mark sprocess)))
    (setq comint-last-input-end (point-marker))
    (if (stringp invisibly)
	(insert-before-markers (concat "*** " invisibly " ***\n")))
    (while (> (length text) 0)
      (setq pos (string-match "\n\\|$" text))
      (setq com (concat (substring text 0 pos) "\n"))
      (setq text (substring text (min (length text) (1+ pos))))
      (goto-char (marker-position (process-mark sprocess)))
      (if invisibly nil (insert-before-markers com))
      (setq start-of-output (marker-position (process-mark sprocess)))
      (process-send-string sprocess com)
      (while (progn
	       (accept-process-output sprocess)
	       (goto-char (marker-position (process-mark sprocess)))
	       (beginning-of-line)
	       (if (< (point) start-of-output) (goto-char start-of-output))
	       (not (looking-at inferior-rb-prompt)))))
    (goto-char (marker-position (process-mark sprocess)))
    (set-buffer cbuffer)))

(defun rb-execute (command &optional invert buff message)
  "Send a command to the S process.
A newline is automatically added to COMMAND. Prefix arg (or second arg INVERT)
means invert the meaning of rb-execute-in-process-buffer. If INVERT is 'buffer,
output is forced to go to the process buffer.
If the output is going to a buffer, name it *BUFF*. This buffer is erased
before use. Optional fourth arg MESSAGE is text to print at the top of the
buffer (defaults to the command if BUFF is not given.)"
  (interactive "sCommand: \nP")
  (let ((the-command (concat command "\n"))
	(buff-name (concat "*" (or buff "rb-output") "*"))
	(in-pbuff (if invert (or (eq invert 'buffer) 
				 (not rb-execute-in-process-buffer))
		    rb-execute-in-process-buffer)))
    (if in-pbuff 
	(rb-eval-visibly the-command)
      (with-output-to-temp-buffer buff-name
	(if message (princ message)
	  (if buff nil
	      ;; Print the command in the buffer if it has not been
	      ;; given a special name
	    (princ "> ")
	    (princ the-command)))
	(rb-command the-command (get-buffer buff-name) nil))
      (save-excursion
	(set-buffer (get-buffer buff-name))
	(setq rb-temp-buffer-p t)))))

(defun rb-execute-in-tb nil
  "Like rb-execute, but always evaluates in temp buffer."
  (interactive)
  (let ((rb-execute-in-process-buffer nil))
    (call-interactively 'rb-execute)))

(defun rb-execute-objects (posn)
  "Send the objects() command to the S process.
By default, gives the objects at position 1.
A prefix argument toggles the meaning of rb-execute-in-process-buffer.
A prefix argument of 2 or more means get objects for that position.
A negative prefix argument gets the objects for that position
  and toggles rb-execute-in-process-buffer as well."
  (interactive "P")
  (let* ((num-arg (if (listp posn) 
		      (if posn -1 1)
		    (prefix-numeric-value posn)))
	(the-posn (if (< num-arg 0) (- num-arg) num-arg))
	(invert (< num-arg 0))
	(the-command (format inferior-rb-objects-command the-posn))
	(the-message (concat ">>> Position "
			     the-posn
			     " ("
			     (nth (1- the-posn) rb-search-list)
			     ")\n")))
    (rb-execute the-command invert "S objects" the-message)))

(defun rb-execute-search (invert)
  "Send the search() command to the S process."
  (interactive "P")
  (rb-execute "search()" invert "S search list"))

(defun rb-execute-attach (dir &optional posn)
  "Attach a directory in the S process with the attach() command.
When used interactively, user is prompted for DIR to attach and
prefix argument is used for POSN (or 2, if absent.) 
Doesn't work for data frames."
  (interactive "DAttach directory: \nP")
  (rb-execute (concat "attach(\"" 
		     (directory-file-name (expand-file-name dir))
		     "\""
		     (if posn (concat "," (prefix-numeric-value posn)))
		     ")") 'buffer))

(defun rb-view-at-bottom ()
  "Move to the end of the buffer, and place cursor on bottom line of window."
  (interactive)
  (goto-char (point-max))
  (recenter -1))

(defun rb-kill-output ()
  "Kill all output from last S command."
  ;; A version of comint-kill-output that doesn't nuke the prompt.
  (interactive)
  (let* ((sprocess (get-process "S"))
	(pmark (process-mark sprocess))
	(oldpoint (point-marker)))
    (goto-char pmark)
    (re-search-backward inferior-rb-primary-prompt)
    (kill-region comint-last-input-end (point))
    (insert "*** output flushed ***\n")
    (goto-char oldpoint)
    (recenter -1)))

(defun rb-load-file (filename)
  "Load an S source file into an inferior S process."
  (interactive (comint-get-source "Load S file: "
                                  rb-prb-load-dir/file
                                  rb-source-modes
                                  nil))
  (catch 'give-up	       	; In case we don't want to load after all
    (let ((buff (get-file-buffer filename))
	  tbuffer-p)
      (if buff		    	; Buffer exists
	  (save-excursion
	    (set-buffer buff)
	    (setq tbuffer-p rb-temp-buffer-p)
	    (if (buffer-modified-p buff) ; Buff exists and has changed
		;; save BUFF, but don't make a backup
		;; if we're about to delete it
		(if tbuffer-p		; i.e. a result from a dump command
		    (save-buffer (if rb-keep-dump-files 1 0))
		  ;; Better check if it's just any old buffer
		  (if (y-or-n-p (format "Buffer %s modified. Save it? "
					(buffer-name buff)))
		      (save-buffer)
		    ;; Maybe we should just give up here ...
		    (message
		     "Using current disk version (don't say I didn't warn you!)")))
	      ;; Buffer hasn't changed lately, might need to write
	      ;; it back if the file is gone
	      (if tbuffer-p
		  (if (y-or-n-p 
		       "Buffer hasn't changed. Really load it into S? ")
		      (if (file-exists-p (buffer-file-name buff)) nil
			(set-buffer-modified-p t) ; so save will work
			(save-buffer 0))
		    (message "No load performed.")
		    (throw 'give-up nil))))))
      (setq rb-prb-load-dir/file
	    (cons (file-name-directory filename)
		  (file-name-nondirectory filename)))
      (let ((errbuffer (get-buffer-create rb-error-buffer-name)))
	(save-excursion 
	  (set-buffer errbuffer)
	  (erase-buffer)
	  (rb-command (format inferior-rb-load-command filename) errbuffer)
	  (goto-char (point-max))
	  (if (re-search-backward rb-dump-error-re nil t)
	      (progn
		(message "Errors: Use %s to find error." 
			 (substitute-command-keys 
			  "\\<inferior-rb-mode-map>\\[rb-parse-errors]"))
		;; This load failed, so set buffer as modified so the
		;; user will be warned if he tries to kill it
		(if buff
		    (progn
		      (set-buffer buff)
		      (set-buffer-modified-p t)))) 
	    (message "Load successful.")
	    (if (and tbuffer-p (not rb-keep-dump-files)) 
		(delete-file filename)))))))
  (rb-switch-to-rb t))

(defun rb-parse-errors (showerr)
  "Jump to error in last loaded S source file.
With prefix argument, only shows the errors S reported."
  (interactive "P")
  (let ((errbuff (get-buffer rb-error-buffer-name)))
    (if (not errbuff)
	(error "You need to do a load first!")
      (set-buffer errbuff)
      (goto-char (point-max))
      (if 
	  (re-search-backward
	   "^\\(Syntax error: .*\\) at line \\([0-9]*\\), file \\(.*\\)$"
	   nil
	   t)
	  (let* ((filename (buffer-substring (match-beginning 3) (match-end 3))) 
		 (fbuffer (get-file-buffer filename)) 
		 (linenum (string-to-int (buffer-substring (match-beginning 2) (match-end 2))))
		 (errmess (buffer-substring (match-beginning 1) (match-end 1))))
	    (if showerr 
		(display-buffer errbuff)
	      (if fbuffer nil
		(setq fbuffer (find-file-noselect filename))
		(save-excursion
		  (set-buffer fbuffer)
		  (rb-mode))) 
	      (pop-to-buffer fbuffer)
	      (goto-line linenum))
	    (princ errmess t))
	(message "Not a syntax error.")
	(display-buffer errbuff)))))

      
(defun rb-search-path-tracker (str)
  "Check if input STR changed the search path."
;;; This function monitors user input to the inferior S process so that
;;; emacs can keep the rb-search-list up to date.  Completing-read uses this
;;; list indirectly when it prompts for help or for an object to dump.
  (if (string-match rb-change-sp-regexp str)
      (setq rb-sp-change t)))

(defun rb-cleanup ()
  "Delete all of rb-mode's temporary buffers and files
(if rb-keep-dump-files is nil) leaving you in the S process buffer.
Auto-save files and S help buffers are also deleted. Buffers whose
contents do not match with S's idea of the objects value *usually*
have the modified flag set, and you will be warned before such buffers
are killed. The exception to this is buffers which were saved in a
prbious session before being loaded into S, and then read this
session.

It's a good idea to run this before you quit. It is run automatically by 
\\[rb-quit]."
  (interactive)
  (if (yes-or-no-p "Delete all temporary files and buffers? ")
      (progn
	(mapcar '(lambda (buf)
		   (set-buffer buf)
		   (let ((fname (buffer-file-name buf))
			 (asfnm buffer-auto-save-file-name))
		     (if rb-temp-buffer-p
			 (progn
			   (kill-buffer buf)
			   (if (or (buffer-name buf)
				   rb-keep-dump-files)
			       ;; Don't do anything if buffer was not
			       ;; killed or dump files are kept
			       nil 
			     ;; if the file exists, it stays! (consider
			     ;; dumping an object with an existing file)
;;;				  (if (and fname (file-exists-p fname))
;;;				      (delete-file fname))
			     ;; Auto-save files can go, since they're
			     ;; only associated with modified buffers
			     (if (and asfnm (file-exists-p asfnm))
				 (delete-file asfnm)))))))
		(buffer-list))
	(rb-switch-to-rb nil))))

(defun rb-quit ()
  "Issue the q() command to S, and clean up."
  (interactive)
  (let ((sprocess (get-process "S")))
    (if (not sprocess) (error "No S process running."))
    (if (yes-or-no-p "Really quit from S? ")
	(save-excursion 
	  (rb-cleanup)
	  (rb-switch-to-rb nil)
	  (goto-char (marker-position (process-mark sprocess)))
	  (insert "q()\n")
	  (process-send-string sprocess "q()\n")))))

(defun rb-abort ()
  "Kill the S process, without executing .Last or terminating devices.
If you want to finish your session, use \\[rb-quit] instead."
;;; Provided as a safety measure over the default binding of C-c C-z in 
;;; comint-mode-map. 
  (interactive)
  (ding)
  (message "WARNING: q() will not be executed and graphics devices won't finish properly!")
  (sit-for 5)
  (if (yes-or-no-p "Still abort? ")
      (comint-quit-subjob)
    (message "Good move.")))
      


;;;
;;; Tek terminal Graphics support
;;;

(defun rb-tek-mode-toggle nil
  "Toggle rb-tek-mode.
Resets the rb-tek-simple-prompt when rb-tek-mode is turned on."
  (interactive)
  (message (if (setq rb-tek-mode (not rb-tek-mode))
	       "Tek mode is now ON." 
	     "Tek mode is now OFF."))
  (if rb-tek-mode (rb-tek-get-simple-prompt)))

(defun rb-tek-get-simple-prompt nil
  "Find the exact version of the current prompt."
  (interactive)
  (let ((tbuffer (generate-new-buffer "*rb-exact-prompt*")))
    (buffer-flush-undo tbuffer)
    (set-buffer tbuffer)
    (rb-command inferior-rb-get-prompt-command tbuffer)
    (goto-char (point-max))
    (re-search-backward "\"\\([^\"]*\\)\"" nil t)
    (setq rb-tek-simple-prompt
	  (buffer-substring (match-beginning 1) (match-end 1)))
    (kill-buffer tbuffer)))

;;; 25/6/92 dsmith
;;; Rest of code moved to rb-tek.el



;;; S mode
;;;======================================================
;;;

(defvar rb-mode-map nil)
(if rb-mode-map
    nil
  (setq rb-mode-map (make-sparse-keymap))
  (define-key rb-mode-map "\C-c\C-r"    'rb-eval-region)
  (define-key rb-mode-map "\C-c\M-r" 'rb-eval-region-and-go)
  (define-key rb-mode-map "\C-c\C-b"    'rb-eval-buffer)
  (define-key rb-mode-map "\C-c\M-b" 'rb-eval-buffer-and-go)
  (define-key rb-mode-map "\C-c\C-f"    'rb-eval-function)
  (define-key rb-mode-map "\C-c\M-f" 'rb-eval-function-and-go)
  (define-key rb-mode-map "\M-\C-x"  'rb-eval-function)
  (define-key rb-mode-map "\C-c\C-n"     'rb-eval-line-and-next-line)
  (define-key rb-mode-map "\C-c\C-j"    'rb-eval-line)
  (define-key rb-mode-map "\C-c\M-j" 'rb-eval-line-and-go)
  (define-key rb-mode-map "\M-\C-a"  'rb-beginning-of-function)
  (define-key rb-mode-map "\M-\C-e"  'rb-end-of-function)
  (define-key rb-mode-map "\C-c\C-y"    'rb-switch-to-rb)
  (define-key rb-mode-map "\C-c\C-z" 'rb-switch-to-end-of-rb)
  (define-key rb-mode-map "\C-c\C-l"    'rb-load-file)
  (define-key rb-mode-map "\C-c\C-h"    'rb-display-help-on-object)
  (define-key rb-mode-map "\C-c\C-d" 'rb-dump-object-into-edit-buffer)
  (define-key rb-mode-map "\C-c\C-e" 'rb-execute-in-tb)
  (define-key rb-mode-map "\M-\t"    'rb-complete-object-name)
  (define-key rb-mode-map "{" 'rb-electric-brace)
  (define-key rb-mode-map "}" 'rb-electric-brace)
  (define-key rb-mode-map "\e\C-h" 'rb-mark-function)
  (define-key rb-mode-map "\e\C-q" 'rb-indent-exp)
  (define-key rb-mode-map "\177" 'backward-delete-char-untabify)
  (define-key rb-mode-map "\t" 'rb-indent-command)
)

(defun rb-mode ()
  "Major mode for editing S source.

\\{rb-mode-map}

Customization: Entry to this mode runs the hooks in rb-mode-hook.

You can send text to the inferior S process from other buffers containing
S source.
    rb-eval-region sends the current region to the S process.
    rb-eval-buffer sends the current buffer to the S process.
    rb-eval-function sends the current function to the S process.
    rb-eval-line sends the current line to the S process.
    rb-beginning-of-function and rb-end-of-function move the point to
        the beginning and end of the current S function.
    rb-switch-to-rb switches the current buffer to the S process buffer.
    rb-switch-to-end-of-rb switches the current buffer to the S process
        buffer and puts point at the end of it.

    rb-eval-region-and-go, rb-eval-buffer-and-go,
        rb-eval-function-and-go, and rb-eval-line-and-go switch to the S
        process buffer after sending their text.

    rb-load-file sources a file of commands to the S process.
    rb-make-function inserts a function template in the buffer.

\\[rb-indent-command] indents for S code. 
\\[backward-delete-char-untabify] converts tabs to spaces as it moves back.
Comments are indented in a similar way to Emacs-lisp mode:
       `###'     beginning of line
       `##'      the same level of indentation as the code
       `#'       the same column on the right, or to the right of such a
                 column if that is not possible.(default value 40). 
                 \\[indent-for-comment] command automatically inserts such a
                 `#' in the right place, or aligns such a comment if it is 
                 already inserted.
\\[rb-indent-exp] command indents each line of the S grouping following point.

Variables controlling indentation style:
 rb-tab-always-indent
    Non-nil means TAB in S mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 rb-auto-newline
    Non-nil means automatically newline before and after braces inserted in S 
    code.
 rb-indent-level
    Indentation of S statements within surrounding block.
    The surrounding block's indentation is the indentation of the line on 
    which the open-brace appears.
 rb-continued-statement-offset
    Extra indentation given to a substatement, such as the then-clause of an 
    if or body of a while.
 rb-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to rb-continued-statement-offset.
 rb-brace-offset
    Extra indentation for line if it starts with an open brace.
 rb-arg-function-offset 
    Extra indent for internal substatements of function `foo' that called
    in `arg=foo(...)' form. 
   If not number, the statements are indented at open-parenthesis following 
   `foo'.
 rb-expression-offset
    Extra indent for internal substatements of `expression' that specified
    in `obj <- expression(...)' form. 
    If not number, the statements are indented at open-parenthesis following 
    `expression'.
 rb-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.

Furthermore, \\[rb-set-style] command enables you to set up predefined rb-mode 
indentation style. At present, predefined style are `BSD', `GNU', `K&R' `C++'
 (quoted from C language style)."
  (interactive)
  (setq major-mode 'rb-mode)
  (setq mode-name "S")
  (use-local-map rb-mode-map)
  (set-syntax-table rb-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'rb-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'rb-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (run-hooks 'rb-mode-hook))

;;; Emacs will set the mode for a file based on the file's header.
;;; The mode name is indicated by putting it between -*- on the top line. 
;;; (Other commands can go here too, see an Emacs manual.)
;;; For a file you also load, you will want a leading # (comment to S)
;;; Emacs will downcase the name of the mode, e.g., S, so we must provide
;;; s-mode in lower case too.  That is, "#-*-rb-*-" invokes s-mode and not rb-mode.
(fset 's-mode 'rb-mode)

(defun rb-eval-region (start end toggle &optional message)
  "Send the current region to the inferior S process.
With prefix argument, toggle meaning of rb-eval-visibly-p."
  (interactive "r\nP")
  (let ((visibly (if toggle (not rb-eval-visibly-p) rb-eval-visibly-p)))
    (if visibly
	(rb-eval-visibly (buffer-substring start end))
      (rb-eval-visibly (buffer-substring start end)
		      (or message "Eval region")))))

(defun rb-eval-region-and-go (start end vis)
  "Send the current region to the inferior S and switch to the process buffer.
Arg has same meaning as for rb-eval-region."
  (interactive "r\nP")
  (rb-eval-region start end vis)
  (rb-switch-to-rb t))

(defun rb-eval-buffer (vis)
  "Send the current buffer to the inferior S process.
Arg has same meaning as for rb-eval-region."
  (interactive "P")
  (rb-eval-region (point-min) (point-max) vis "Eval buffer"))

(defun rb-eval-buffer-and-go (vis)
  "Send the current buffer to the inferior S and switch to the process buffer.
Arg has same meaning as for rb-eval-region."
  (interactive)
  (rb-eval-buffer vis)
  (rb-switch-to-rb t))

(defun rb-eval-function (vis)
  "Send the current function to the inferior S process.
Arg has same meaning as for rb-eval-region."
  (interactive "P")
  (save-excursion
    (rb-end-of-function)
    (let ((end (point)))
      (rb-beginning-of-function)
      (princ (concat "Loading: " (rb-extract-word-name)) t)
      (rb-eval-region (point) end vis 
		     (concat "Eval function " (rb-extract-word-name))))))

(defun rb-eval-function-and-go (vis)
  "Send the current function to the inferior S process and switch to
the process buffer. Arg has same meaning as for rb-eval-region."
  (interactive "P")
  (rb-eval-function vis)
  (rb-switch-to-rb t))

(defun rb-eval-line (vis)
  "Send the current line to the inferior S process.
Arg has same meaning as for rb-eval-region."
  (interactive "P")
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (princ (concat "Loading line: " (rb-extract-word-name) " ...") t)
      (rb-eval-region (point) end vis "Eval line"))))

(defun rb-eval-line-and-go (vis)
  "Send the current line to the inferior S process and switch to the
process buffer. Arg has same meaning as for rb-eval-region."
  (interactive "P")
  (rb-eval-line vis)
  (rb-switch-to-rb t))

(defun rb-eval-line-and-next-line ()
  "Evaluate the current line visibly and move to the next line."
  ;; From an idea by Rod Ball (rod@marcam.dsir.govt.nz)
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (rb-eval-visibly (buffer-substring (point) end))))
  (next-line 1))

(defun rb-beginning-of-function nil
  "Leave the point at the beginning of the current S function."
  (interactive)
  (let ((init-point (point))
	beg end done)
    (if (search-forward "(" nil t) (forward-char 1))
    ;; in case we're sitting in a function header
    (while (not done)
      (if 
	  (re-search-backward rb-function-pattern (point-min) t)
	  nil
	(goto-char init-point)
	(error "Point is not in a function."))
      (setq beg (point))
      (forward-list 1) ; get over arguments
      (forward-sexp 1) ; move over braces
      (setq end (point))
      (goto-char beg)
      ;; current function must begin and end around point  
      (setq done (and (>= end init-point) (<= beg init-point))))))

(defun rb-end-of-function nil
  "Leave the point at the end of the current S function."
  (interactive)
  (rb-beginning-of-function)
  (forward-list 1) ; get over arguments
  (forward-sexp 1) ; move over braces
  )

(defun rb-extract-word-name ()
  "Get the word you're on."
  (save-excursion
    (re-search-forward "\\<\\w+\\>" nil t)
    (buffer-substring (match-beginning 0) (match-end 0))))

(defun rb-switch-to-rb (eob-p)
  "Switch to the inferior S process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (cond ((comint-check-proc "*S*")
         (pop-to-buffer "*S*")
         (cond (eob-p
                (goto-char (point-max)))))
        (t
         (message "No inferior S process")
         (ding))))

(defun rb-switch-to-end-of-rb nil
  "Switch to the end of the inferior S process buffer."
  (interactive)
  (rb-switch-to-rb t))

(defun rb-make-function ()
  "Insert a function template."
  (interactive)
  (insert "fu <- function()\n{\n\t\n}\n")
  (forward-line -2)
  (end-of-line))

(defun rb-complete-object-name (&optional listcomp)
  ;;Based on lisp-complete-symbol
  "Perform completion on S object preceding point.  The object is
compared against those objects known by rb-get-object-list and any
additional characters up to ambiguity are inserted.  Completion only
works on globally-known objects (including elements of attached data
frames), and thus is most suitable for interactive command-line entry,
and not so much for function editing since local objects (e.g.
argument names) aren't known.

Use \\[rb-resynch] to re-read the names of the attached directories.
This is done automatically (and transparently) if a directory is
modified, so the most up-to-date list of object names is always
available. However attached dataframes are *not* updated, so this
command may be necessary if you modify an attached dataframe.

If ARG is non-nil, no completion is attempted, but the available
completions are listed.

If the character proceding point is not a symbol element,
indent-for-tab-command is run."
  (interactive "P")
  (if (memq (char-syntax (preceding-char)) '(?w ?_)) 
      (let* ((end (point))
	     (buffer-syntax (syntax-table))
	     (beg (unwind-protect
		      (save-excursion
			(set-syntax-table rb-mode-syntax-table)
			(backward-sexp 1)
			(point))
		    (set-syntax-table buffer-syntax)))
	     (full-prefix (buffer-substring beg end))
	     ;; See if we're indexing a list with `$'
	     (pattern full-prefix)
	     components
	     (listname (if (string-match "\\(.+\\)\\$\\(\\sw\\|\\s_\\)*$"
					 full-prefix) 
			   (progn
			     (setq pattern 
				   (if (not (match-beginning 2)) ""
				     (substring full-prefix
						(match-beginning 2)
						(match-end 2))))
			     (substring full-prefix (match-beginning 1)
					(match-end 1)))))
	     (completion (try-completion pattern
					 (if listname
					     (setq components
						   (rb-object-names listname))
					   (rb-get-object-list)))))
	(if listcomp (setq completion full-prefix))
	(cond ((eq completion t)
	       (message "[sole completion]"))
	      ((null completion)
	       (message "Can't find completion for \"%s\"" full-prefix)
	       (ding))
	      ((not (string= pattern completion))
	       (delete-region 
		(if listname (+ beg (length listname) 1) beg)
		end)
	       (insert completion))
	      (t 
	       (message "Making completion list...")
	       (let ((list (all-completions pattern
					    (if listname components
					      (rb-get-object-list)))))
		 (with-output-to-temp-buffer " *Completions*"
		   (display-completion-list list)))
	       (message "Making completion list...%s" "done"))))
    (indent-for-tab-command)))

;;; S code formatting functions

(defun rb-comment-indent ()
  (if (looking-at "###")
      (current-column)
    (if (looking-at "##")
	(let ((tem (rb-calculate-indent)))
	  (if (listp tem) (car tem) tem))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
	   comment-column))))

(defun rb-electric-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (if rb-auto-newline (progn (rb-indent-line) (newline) t) nil)))
	(progn
	  (insert last-command-char)
	  (rb-indent-line)
	  (if rb-auto-newline
	      (progn
		(newline)
		;; (newline) may have done auto-fill
		(setq insertpos (- (point) 2))
		(rb-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun rb-indent-command (&optional whole-exp)
  "Indent current line as S code, or in some cases insert a tab character.
If rb-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as S
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (rb-indent-line))
	    beg end)
	(save-excursion
	  (if rb-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (backward-up-list 1)
	  (forward-list 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt)))
    (if (and (not rb-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (rb-indent-line))))

(defun rb-indent-line ()
  "Indent current line as S code.
Return the amount the indentation changed by."
  (let ((indent (rb-calculate-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  (t
	   (skip-chars-forward " \t")
	   (if (looking-at "###")
	       (setq indent 0))
	   (if (and (looking-at "#") (not (looking-at "##")))
	       (setq indent comment-column)
	     (if (eq indent t) (setq indent 0))
	     (if (listp indent) (setq indent (car indent)))
	     (cond ((and (looking-at "else\\b")
			 (not (looking-at "else\\s_")))
		    (setq indent (save-excursion
				   (rb-backward-to-start-of-if)
				   (current-indentation))))
		   ((= (following-char) ?})
		    (setq indent (- indent rb-indent-level)))
		   ((= (following-char) ?{)
		    (setq indent (+ indent rb-brace-offset)))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  
      ;; Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun rb-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as S code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     ;; Line is at top level.  May be data or function definition,
	     0)   ; Unless it starts a function body
	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char containing-sexp)
	     (let ((bol (save-excursion (beginning-of-line) (point))))
	       (cond ((and (numberp rb-arg-function-offset)
			    (re-search-backward "=[ \t]*\\s\"*\\(\\w\\|\\s_\\)+\\s\"*[ \t]*" bol t))
		      (forward-sexp -1)
		      (+ (current-column) rb-arg-function-offset))
		     ((and (numberp rb-expression-offset)
			   (re-search-backward "<-[ \t]*expression[ \t]*" bol t))
		      (forward-sexp -1)
		      (+ (current-column) rb-expression-offset))
		     (t
		      (progn (goto-char (1+ containing-sexp))
			     (current-column))))))
	    (t
	     ;; Statement level.  Is it a continuation or a new statement?
	     ;; Find prbious non-comment character.
	     (goto-char indent-point)
	     (rb-backward-to-noncomment containing-sexp)
	     ;; Back up over label lines, since they don't
	     ;; affect whether our line is a continuation.
	     (while (eq (preceding-char) ?\,)
	       (rb-backward-to-start-of-continued-exp containing-sexp)
	       (beginning-of-line)
	       (rb-backward-to-noncomment containing-sexp))
	     ;; Now we get the answer.
	     (if (rb-continued-statement-p)
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  rb-continued-statement-offset  more than the
		 ;; prbious line of the statement.
		 (progn
		   (rb-backward-to-start-of-continued-exp containing-sexp)
		   (+ rb-continued-statement-offset (current-column)
		      (if (save-excursion (goto-char indent-point)
					  (skip-chars-forward " \t")
					  (eq (following-char) ?{))
			  rb-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position following last unclosed open.
	       (goto-char containing-sexp)
	       ;; Is line first statement after an open-brace?
	       (or
		 ;; If no, find that first statement and indent like it.
		 (save-excursion
		   (forward-char 1)
		   (while (progn (skip-chars-forward " \t\n")
				 (looking-at "#"))
		     ;; Skip over comments following openbrace.
		     (forward-line 1))
		   ;; The first following code counts
		   ;; if it is before the line we want to indent.
		   (and (< (point) indent-point)
			(current-column)))
		 ;; If no prbious statement,
		 ;; indent it relative to line brace is on.
		 ;; For open brace in column zero, don't let statement
		 ;; start there too.  If rb-indent-level is zero,
		 ;; use rb-brace-offset + rb-continued-statement-offset instead.
		 ;; For open-braces not the first thing in a line,
		 ;; add in rb-brace-imaginary-offset.
		 (+ (if (and (bolp) (zerop rb-indent-level))
			(+ rb-brace-offset rb-continued-statement-offset)
		      rb-indent-level)
		    ;; Move back over whitespace before the openbrace.
		    ;; If openbrace is not first nonwhite thing on the line,
		    ;; add the rb-brace-imaginary-offset.
		    (progn (skip-chars-backward " \t")
			   (if (bolp) 0 rb-brace-imaginary-offset))
		    ;; If the openbrace is preceded by a parenthesized exp,
		    ;; move to the beginning of that;
		    ;; possibly a different line
		    (progn
		      (if (eq (preceding-char) ?\))
			  (forward-sexp -1))
		      ;; Get initial indentation of the line we are on.
		      (current-indentation))))))))))

(defun rb-continued-statement-p ()
  (let ((eol (point)))
    (save-excursion
      (cond ((memq (preceding-char) '(nil ?\, ?\; ?\} ?\{ ?\]))
	     nil)
	    ((bolp))
	    ((= (preceding-char) ?\))
	     (forward-sexp -2)
	     (looking-at "if\\b[ \t]*(\\|function\\b[ \t]*(\\|for\\b[ \t]*(\\|while\\b[ \t]*("))
	    ((progn (forward-sexp -1) 
		    (and (looking-at "else\\b\\|repeat\\b")
			 (not (looking-at "else\\s_\\|repeat\\s_"))))
	     (skip-chars-backward " \t")
	     (or (bolp)
		 (= (preceding-char) ?\;)))
	    (t
	     (progn (goto-char eol)
		    (skip-chars-backward " \t")
		    (or (and (> (current-column) 1)
			     (save-excursion (backward-char 1)
					     (looking-at "[-:+*/_><=]")))
			(and (> (current-column) 3)
			     (progn (backward-char 3)
				    (looking-at "%[^ \t]%"))))))))))

(defun rb-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (setq opoint (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq stop (or (not (looking-at "#")) (<= (point) lim)))
      (if stop (goto-char opoint)
	(beginning-of-line)))))

(defun rb-backward-to-start-of-continued-exp (lim)
  (if (= (preceding-char) ?\))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun rb-backward-to-start-of-if (&optional limit)
  "Move to the start of the last ``unbalanced'' if."
  (or limit (setq limit (save-excursion (beginning-of-defun) (point))))
  (let ((if-level 1)
	(case-fold-search nil))
    (while (not (zerop if-level))
      (backward-sexp 1)
      (cond ((looking-at "else\\b")
	     (setq if-level (1+ if-level)))
	    ((looking-at "if\\b")
	     (setq if-level (1- if-level)))
	    ((< (point) limit)
	     (setq if-level 0)
	     (goto-char limit))))))

(defun rb-mark-function ()
  "Put mark at end of S function, point at beginning."
  (interactive)
  (push-mark (point))
  (rb-end-of-function)
  (push-mark (point))
  (rb-beginning-of-function))

(defun rb-indent-exp ()
  "Indent each line of the S grouping following point."
  (interactive)
  (let ((indent-stack (list nil))
	(contain-stack (list (point)))
	(case-fold-search nil)
	restart outer-loop-done inner-loop-done state ostate
	this-indent last-sexp
	at-else at-brace
	(opoint (point))
	(next-depth 0))
    (save-excursion
      (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp)) (not outer-loop-done))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq innerloop-done nil)
	(while (and (not innerloop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (and (car (cdr (cdr state)))
		   (>= (car (cdr (cdr state))) 0))
	      (setq last-sexp (car (cdr (cdr state)))))
	  (if (or (nth 4 ostate))
	      (rb-indent-line))
	  (if (nth 4 state)
	      (and (rb-indent-line)
		   (setcar (nthcdr 4 state) nil)))
	  (if (or (nth 3 state))
	      (forward-line 1)
	    (setq innerloop-done t)))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  ;; If this line had ..))) (((.. in it, pop out of the levels
	  ;; that ended anywhere in this line, even if the final depth
	  ;; doesn't indicate that they ended.
	  (while (> last-depth (nth 6 state))
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  ;; Add levels for any parens that were started in this line.
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  contain-stack (cons nil contain-stack)
		  last-depth (1+ last-depth)))
	  (if (null (car contain-stack))
	      (setcar contain-stack (or (car (cdr state))
					(save-excursion (forward-sexp -1)
							(point)))))
	  (forward-line 1)
	  (skip-chars-forward " \t")
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Line is on an existing nesting level.
		;; Lines inside parens are handled specially.
		(if (/= (char-after (car contain-stack)) ?{)
		    (setq this-indent (car indent-stack))
		  ;; Line is at statement level.
		  ;; Is it a new statement?  Is it an else?
		  ;; Find last non-comment character before this line
		  (save-excursion
		    (setq at-else (looking-at "else\\W"))
		    (setq at-brace (= (following-char) ?{))
		    (rb-backward-to-noncomment opoint)
		    (if (rb-continued-statement-p)
			;; Preceding line did not end in comma or semi;
			;; indent this line  rb-continued-statement-offset
			;; more than prbious.
			(progn
			  (rb-backward-to-start-of-continued-exp (car contain-stack))
			  (setq this-indent
				(+ rb-continued-statement-offset (current-column)
				   (if at-brace rb-continued-brace-offset 0))))
		      ;; Preceding line ended in comma or semi;
		      ;; use the standard indent for this level.
		      (if at-else
			  (progn (rb-backward-to-start-of-if opoint)
				 (setq this-indent (current-indentation)))
			(setq this-indent (car indent-stack))))))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (rb-calculate-indent
			   (if (car indent-stack)
			       (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))))
	    ;; Adjust line indentation according to its contents
	    (if (= (following-char) ?})
		(setq this-indent (- this-indent rb-indent-level)))
	    (if (= (following-char) ?{)
		(setq this-indent (+ this-indent rb-brace-offset)))
	    ;; Put chosen indentation into effect.
	    (or (= (current-column) this-indent)
		(= (following-char) ?\#)
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to this-indent)))
	    ;; Indent any comment following the text.
	    (or (looking-at comment-start-skip)
		(if (re-search-forward comment-start-skip (save-excursion (end-of-line) (point)) t)
		    (progn (indent-for-comment) (beginning-of-line)))))))))
; (message "Indenting S expression...done")
  )

;; Predefined styles
(defun rb-set-style (&optional style)
  "Set up the rb-mode style variables from the rb-style variable or if
  STYLE argument is given, use that.  It makes the S indentation style 
  variables buffer local."

  (interactive)

  (let ((rb-styles (mapcar 'car rb-style-alist)))
	
    (if (interactive-p)
	(setq style
	      (let ((style-string ; get style name with completion
		     (completing-read
		      (format "Set S mode indentation style to (default %s): "
			      rb-default-style)
		      (vconcat rb-styles)
		      (function (lambda (arg) (memq arg rb-styles)))
		      )))
		(if (string-equal "" style-string)
		    rb-default-style
		  (intern style-string))
		)))
    
    (setq style (or style rb-style)) ; use rb-style if style is nil
    
    (make-local-variable 'rb-style)
    (if (memq style rb-styles)
	(setq rb-style style)
      (error (concat "Bad S style: " style))
      )
    (message "rb-style: %s" rb-style)
      
    ; finally, set the indentation style variables making each one local
    (mapcar (function (lambda (rb-style-pair)
			(make-local-variable (car rb-style-pair))
			(set (car rb-style-pair)
			     (cdr rb-style-pair))))
	    (cdr (assq rb-style rb-style-alist)))
    rb-style))



;;; rb-help-mode
;;;======================================================
;;;

(defvar rb-help-mode-map nil "Keymap for S help mode.")
(defvar rb-help-mode-hook nil "Functions to call when entering more mode. ")

(defvar rb-help-sec-map nil "Sub-keymap for S help mode.")

(defun rb-skip-to-help-section nil
  "Jump to a section heading of a help buffer. The section selected is
determined by the command letter used to invoke the command, as indicated
by rb-help-sec-keys-alist. Use \\[rb-describe-sec-map] to see which keystrokes
find which sections."
  (interactive)
  (let ((old-point (point)))
    (goto-char (point-min))
    (let ((the-sec (cdr (assoc last-command-char rb-help-sec-keys-alist))))
      (if (not the-sec) (error "Invalid section key: %c" last-command-char)
	(if (re-search-forward (concat "^" the-sec) nil t) nil
	    (message "No %s section in this help. Sorry." the-sec)
	    (goto-char old-point))))))

(defun rb-skip-to-next-section nil
  "Jump to next section in S help buffer."
  (interactive)
  (let ((case-fold-search nil))
    (if (re-search-forward "^[A-Z. ---]+:$" nil t) nil
      (message "No more sections."))))

(defun rb-skip-to-prbious-section nil
  "Jump to prbious section in S help buffer."
  (interactive)
  (let ((case-fold-search nil))
    (if (re-search-backward "^[A-Z. ---]+:$" nil t) nil
      (message "No prbious section."))))

(defun rb-describe-help-mode nil
"Display help for rb-mode"
 (interactive)
 (describe-function 'rb-help-mode))

(defun rb-kill-buffer-and-go nil
  "Kill the current buffer and switch back to S"
  (interactive)
  (kill-buffer (current-buffer))
  (rb-switch-to-rb nil))

(defun rb-describe-sec-map nil
  "Display help for the `s' key."
  (interactive)
  (describe-function 'rb-skip-to-help-section)
  (save-excursion
    (set-buffer "*Help*")
    (goto-char (point-max))
    (insert "\n\nCurrently defined keys are:

Keystroke    Section
---------    -------\n")
    (mapcar '(lambda (cs) (insert "    " (car cs) "        " (cdr cs) "\n")) rb-help-sec-keys-alist)
    (insert "\nFull list of key definitions:\n" (substitute-command-keys "\\{rb-help-sec-map}"))))

(defun rb-find-help-file (p-string)
  (let* ((default (rb-read-object-name-default))
         (prompt-string (if default
                            (format "%s(default %s) " p-string default)
                          p-string))
	 (help-files-list (rb-get-help-files-list))
         (spec (completing-read prompt-string help-files-list)))
    (list (cond
           ((string= spec "") default)
           (t spec)))))

(defun rb-get-help-files-list nil
  (mapcar 'list
	  (apply 'append
		 (mapcar '(lambda (dirname)
			    (if (file-directory-p dirname) 
				(directory-files dirname)))
			 (mapcar '(lambda (str) (concat str "/.Help"))
				 rb-search-list)))))
	  
(if rb-help-sec-map
    nil
  (setq rb-help-sec-map (make-keymap))
  (mapcar '(lambda (key) 
	    (define-key rb-help-sec-map (char-to-string key) 
	      'rb-skip-to-help-section))
	    (mapcar 'car rb-help-sec-keys-alist))
  (define-key rb-help-sec-map "?" 'rb-describe-sec-map)
  (define-key rb-help-sec-map ">" 'end-of-buffer)
  (define-key rb-help-sec-map "<" 'beginning-of-buffer)
)

(defun rb-display-help-on-object (object)
  "Display the help page for OBJECT in the *Help* buffer. 
If prefix arg is given, forces a query of the S process for the help
file.  Otherwise just pops to an existing buffer if it exists."
  (interactive (rb-find-help-file "Help on: "))
  (let* ((hb-name (concat "*help(" object ")*"))
	 (old-hb-p (get-buffer hb-name))
	 (tbuffer (get-buffer-create hb-name)))
    (set-buffer tbuffer)
    (if (or (not old-hb-p) current-prefix-arg)
	;; Ask S for the help file
	(progn
	  (setq rb-temp-buffer-p t)		; Flag as a temp buffer
	  (delete-region (point-min) (point-max))
	  (rb-help-mode)
	  (rb-command (format inferior-rb-help-command object) tbuffer)
	  (rb-nuke-help-bs)
	  (goto-char (point-min))))
    (let (nodocs)
      (save-excursion
	(goto-char (point-min))
	(setq nodocs 
	      (re-search-forward "\\`No documentation available.*$" nil t))
	(if nodocs
	    (progn
	      (princ (buffer-substring (match-beginning 0) (match-end 0)) t)
	      ;; Avoid using 'message here -- may be %'s in string
	      (ding)
	      (kill-buffer tbuffer))
	  (if (eq major-mode 'rb-help-mode) (switch-to-buffer tbuffer)
	    (pop-to-buffer tbuffer)))))))

;;; This function is a modification of nuke-nroff-bs in man.el from the
;;; standard emacs 18 lisp library.
(defun rb-nuke-help-bs ()
  (interactive "*")
  ;; Nuke underlining and overstriking (only by the same letter)
  (goto-char (point-min))
  (while (search-forward "\b" nil t)
    (let* ((preceding (char-after (- (point) 2)))
           (following (following-char)))
      (cond ((= preceding following)
             ;; x\bx
             (delete-char -2))
            ((= preceding ?\_)
             ;; _\b
             (delete-char -2))
            ((= following ?\_)
             ;; \b_
             (delete-region (1- (point)) (1+ (point)))))))
  ;; Crunch blank lines
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n\n*" nil t)
    (replace-match "\n\n"))
  ;; Nuke blanks lines at start.
  (goto-char (point-min))
  (skip-chars-forward "\n")
  (delete-region (point-min) (point)))

(if rb-help-mode-map
    nil
  (setq rb-help-mode-map (make-keymap))
  (suppress-keymap rb-help-mode-map)  
  (define-key rb-help-mode-map " " 'scroll-up)
  (define-key rb-help-mode-map "b" 'scroll-down)
  (define-key rb-help-mode-map "q" 'rb-switch-to-end-of-rb)
  (define-key rb-help-mode-map "\177" 'scroll-down) ; DEL
  (define-key rb-help-mode-map "s" rb-help-sec-map)
  (define-key rb-help-mode-map "h" 'rb-display-help-on-object)
  (define-key rb-help-mode-map "r" 'rb-eval-region)
  (define-key rb-help-mode-map "n" 'rb-skip-to-next-section)
  (define-key rb-help-mode-map "p" 'rb-skip-to-prbious-section)
  (define-key rb-help-mode-map "/" 'isearch-forward)
  (define-key rb-help-mode-map ">" 'end-of-buffer)
  (define-key rb-help-mode-map "<" 'beginning-of-buffer)
  (define-key rb-help-mode-map "x" 'rb-kill-buffer-and-go)
  (define-key rb-help-mode-map "?" 'rb-describe-help-mode)
  (define-key rb-help-mode-map "\C-c\C-r"    'rb-eval-region)
  (define-key rb-help-mode-map "\C-c\M-r" 'rb-eval-region-and-go)
  (define-key rb-help-mode-map "\C-c\C-f"    'rb-eval-function)
  (define-key rb-help-mode-map "\M-\C-x"  'rb-eval-function)
  (define-key rb-help-mode-map "\C-c\M-f" 'rb-eval-function-and-go)
  (define-key rb-help-mode-map "\C-c\C-j"    'rb-eval-line)
  (define-key rb-help-mode-map "\C-c\M-j" 'rb-eval-line-and-go)
  (define-key rb-help-mode-map "\M-\C-a"  'rb-beginning-of-function)
  (define-key rb-help-mode-map "\M-\C-e"  'rb-end-of-function)
  (define-key rb-help-mode-map "\C-c\C-y"    'rb-switch-to-rb)
  (define-key rb-help-mode-map "\C-c\C-z" 'rb-switch-to-end-of-rb)
  (define-key rb-help-mode-map "\C-c\C-l"    'rb-load-file)
  (define-key rb-help-mode-map "\C-c\C-h"    'rb-display-help-on-object))

;;; Largely ripped from more-mode.el,
;;;  by Wolfgang Rupprecht wolfgang@mgm.mit.edu

(defun rb-help-mode ()
  "Mode for viewing S help files.
Use SPC and DEL to page back and forth through the file.
Use `s' to jump to a particular section; `s ?' for help.
Use `q' to return to your S session; `x' to kill this buffer first.
The usual commands for evaluating S source are available.
Other keybindings are as follows:
\\{rb-help-mode-map}"
  (interactive)
  (setq major-mode 'rb-help-mode)
  (setq mode-name "Rb Help")
  (use-local-map rb-help-mode-map)
  (run-hooks rb-help-mode-hook))

(run-hooks 'rb-mode-load-hook)


;;; Rbision notes:
;;  Release 2.1 on October 14, 1991 to statlib@stat.cmu.edu, 
;;     and to the elisp archives at OSU (brennan@dg-rtp.dg.com (Dave Brennan))
;;  and announced on internet.s-news, netnews.gnu.emacs.sources, & 
;;    andrew.programs.S
;; -------------------------------------------------------
;;     Jul 26          1991  Frank Ritter
;;   * added rb-mode-load-hook & rb-pre-run-hook
;;     and testing by neilc@research.att.com
;;     Jul 9           1991  Frank Ritter
;;   * Changed rb-command to use a register rather than 
;;       the kill ring.
;;   * Better file header, comments now at 60 col so 
;;       mailers wont' eat them.
;;   * Better rb-extract-word-name.
;;   * Added rb-mode-version variable
;;   * Changed syntax table to read |#; appropriately
;;
;; Wed Nov 28 11:03:50 1990  Ed Kademan  (kademan at hermes)
;;   * Make the rb-mode-syntax-table a slightly modified
;;       version of the c-mode-syntax-table instead of a
;;       version of the one for lisp.
;; 
;; Sat Nov 10 12:41:52 1990  Ed Kademan  (kademan at hermes)
;;   * Made run-rb and run-s commands synonymous with the
;;       function S.
;; 
;; Fri Oct 19 12:41:52 1990  Ed Kademan  (kademan at hermes)
;;   * Made rb-directory a user modifiable variable.  S will
;;       run from that directory.
;; 
;; Thu Oct 18 12:41:52 1990  Ed Kademan  (kademan at hermes)
;;   * Added function rb-nuke-help-bs to clean up nroff
;;       style text in the S help buffer.  This function is
;;       a modification of nuke-nroff-bs from man.el.
;; -------------------------------------------------------
;; Unnumbered version released dated Thu Jun 14 09:56:56 CDT 1990
;;
;; Fri Jan 17 1992 Dave Smith (dsmith@stats.adelaide.edu.au)
;;   * Help mode for reading files. When asking for an object to
;;     run help on, completion is over those help files that exist.
;;   * Added object name completion, and made rb-get-object-list
;;     efficient enough to make it worthwile.
;;   * Error parsing for loaded files
;;   * Better customization of file-names, with sensible defaults
;;   * Sensible buffer names for object buffers
;;   * Corrected definition for `.' in syntax table
;;   * Improved (and simplified) rb-read-object-name-default
;;   * Included pager='cat' to default help-command specification
;;   * Added a call to run-hook for rb-pre-run-hook
;;   * Changed keymaps to conform with GNU guidelines
;;     (i.e. no \C-letter bindings)
;;   * rb-command has a new third argument, visible
;;
;; Tue May 27 1992 Dave Smith (dsmith@stats.adelaide.edu.au)
;;   * now copes with dynamically changing prompts (reported by Doug Bates)
;;
;; Thu May 29 1992 Dave Smith (dsmith@stats.adelaide.edu.au)
;;   * Added rb-execute, modified rb-execute-* to use it.
;;
;; Mon Jun 22 1992 dsmith
;;   * Added rb-mode editing commands written by Ken'ichi Shibayama
;;     (shiba@isac.co.jp). A big win. 
;;   * Removed the redundant argument to rb-switch-to-end-of-rb
;;   * rb-function-pattern improved
;;   * added rb-eval-visibly, rb-eval-visibly-p and modified rb-eval-*
;;     to use them
;;   * added rb-eval-line-and-next-line
;;   * eval commands can now echo in the process buffer
;;   * added rb-kill-output and rb-view-at-bottom
;;   * added a binding for comint-isearch and autoloaded it
;;   * added rb-execute-in-tb. rb-parse-errors now takes prefix arg.
;;
;; Thu Jun 25 1992 dsmith
;;   * Moved some doctrings to comments (Frank Ritter)
;;   * The Tek stuff now lives in a separate file (Frank Ritter)
;;   * Fiddly C-c ESC M-. bindings in S mode and Help mode moved
;;       to C-c M-. bindings (Martin Maechler)
;;   * rb-execute-objects now uses variable inferior-rb-objects-command
;;       whose value depends on S version. (Ken'ichi Shibayama)
;;   * Symbols given uniform prefixes: rb- or inferior-rb (Frank Ritter)

