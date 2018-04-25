# rev-mode
A GNU Emacs interpreter mode for the rev language of evolutionary bayesian inference

This mode aims at interpreting `rev` code for use in `revbayes` under Emacs. It has been partially modified from the  `essh` and `julia-mode` modes for syntax highlight (feature pending, actually), auto-completion (pending, too), and command redirection in order to provide a development environment for `revbayes`. Please see the original code in the `s-mode` directory for further details and attribution.

At this moment it just runs, provided taht you include the following in your `.emacs` file or wherever appropriate:

```lisp
(add-to-list 'load-path "/path/to/rev-mode")
(require 'rev-mode)                                                    ;;
(defun rev-mode-sh-hook ()                                             ;;
  (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-rev)        ;;
  (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-rev)        ;;
  (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-rev)          ;;
  (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-rev-and-step) ;;
  (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-rev))      ;;
(add-hook 'sh-mode-hook 'rev-mode-sh-hook)
;; setup files ending in “.rev” to open in rev-mode
(add-to-list 'auto-mode-alist '("\\.rev\\'" . rev-mode))
```

## How to use

At present it does not run automatically so first you will need to open a `.rev` file (it will be recognized as `rev-mode`); this unfortunately opens a terminal buffer that is unnecessary but also harmless as all the code redirection goes directly to the `rb` process. you will need to open a RevBayes process with `run-rev`; this will open a command line interpreter of the `rb` program. Afterwards just use the keystroke combinations described above.
