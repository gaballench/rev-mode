# rev-mode
A GNU Emacs interpreter mode for the rev language of evolutionary bayesian inference

This mode aims at interpreting `rev` code for use in `revbayes` under Emacs. It has been partially modified from the  `essh` and `julia-mode` modes for syntax highlight (feature pending, actually), auto-completion (pending, too), and command redirection in order to provide a development environment for `revbayes`. Please see the original code in the `s-mode` directory for further details and attribution.

At this moment it just runs, provided taht you include the following in your `.emacs` file or wherever appropriate:

```lisp
(add-to-list 'load-path "/path/to/rev-mode")
(require 'rev-mode)
;; setup files ending in “.rev” to open in rev-mode
(add-to-list 'auto-mode-alist '("\\.rev\\'" . rev-mode))
```

## How to use

An earlier version required the user to open explicitly `run-rev` in order to redirect code to the console, but this is no longer necessary; it is automatically tested during code redirection whether there is an `rb` process active, it not, it opens a new process. The only thing the user needs to do is to open a file and use any of the key combinations for code redirection.
