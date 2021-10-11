# FreeBASIC mode for GNU Emacs

This is an Emacs major mode for the [FreeBASIC programming language](https://freebasic.net/).
It's functional, fairly bug-free (except missing keywords), but always a work in progress.
It is written from scratch, so may not follow certain conventions.

## Installing

`require` it after downloading it and placing it in your `load-path`. E.g. in your `.emacs`:
```elisp
(add-to-list 'load-path "/path/to/fb-mode")  ; Directory containing fb-mode.el
(require 'fb-mode)
```

You likely will want to customise the `fb-emph-face` (used for `:` and `_` tokens - I couldn't find an existing suitable face) for your colour theme.

## Features

- Syntax highlighting
  - Declared variable names are highlighted, thanks to parsing of variable declarations and sub/function declarations and header lines instead of simplistic keyword highlighting
- Indentation
  - Full support for lines continued with `_`
  - Amount of indentation is controlled by the `fb-indent-level` variable
- Commenting/uncommenting, and `fill-paragraph` support for comments
- C-c C-h to lookup a symbol (default under point) in the manual in a web browser tab (note, doesn't work for many builtins)
- F5 to run `compile`, with a suitable default command (`fbc <file>`, `make` or `scons`)
  - `compile-mode` support for `fbc` error/warning messages so you can jump to the line
- `beginning-of-defun`/`end-of-defun` (C-M-a/C-M-e) support
- C-M-j to split the current line at point (possibly in middle of a string or comment), adding a _ line continuation
and indenting.

## Not yet included

- Syntax highlighting of builtin functions and types; some keywords like `data` are also missing.
- Keyword/builtin completion
- Jumping to definitions (tags)
- Menubar items

## Alternative

[freebasic-mode](https://github.com/z80lives/freebasic-mode) by z80lives is an older, simpler Emacs mode for FreeBASIC.
The two modes are fairly distinct in their feature sets.

freebasic-mode highlights based on a near-complete (though out of date) list of all FB keywords and builtin functions.
It has functions/keys to compile and (Unix only) run the file, and menu bar items.
However, it currently has no support for line continuations, minimal support for multiline comments (can only highlight them), and suffers various bugs in syntax highlighting and indentation.
