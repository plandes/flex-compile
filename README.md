# Flexible Evaluation and Compilation [![MELPA badge][melpa-badge]][melpa-link] [![MELPA stable badge][melpa-stable-badge]][melpa-stable-link] [![Travis CI Build Status][travis-badge]][travis-link]

  [melpa-link]: https://melpa.org/#/compile-flex
  [melpa-stable-link]: https://stable.melpa.org/#/compile-flex
  [melpa-badge]: https://melpa.org/packages/compile-flex-badge.svg
  [melpa-stable-badge]: https://stable.melpa.org/packages/compile-flex-badge.svg
  [travis-link]: https://travis-ci.org/plandes/compile-flex
  [travis-badge]: https://travis-ci.org/plandes/compile-flex.svg?branch=master

Run, evaluate and compile functionality for a variety of different languages
and modes.  The specific "compilation" method is different across each add-on
library, which are called *flexible compilers*.  For example, for ESS and
Clojure you can evaluate a specific file and/or evaluate a specfic expression
via a REPL.  For running a script or starting a `make` an async process is
started.

The top level library `compile-flex` library provides a plugin architecture for
add-on libraries, which include:
* compile-flex-make
* compile-flex-beanshell
* compile-flex-script
* compile-flex-maven
* compile-flex-clojure
* compile-flex-scala
* compile-flex-command
* compile-flex-python
* compile-flex-ess


## Configuration

Add the following to your `~/.emacs` file:
```lisp
(require 'compile-flex)
```
This loads the file and creates global key bindings.

For each specific *flexible* compilation method you want:
```lisp
(eval-after-load
    "compile-flex"
  '(progn
     (require 'compile-flex-make)
     (require 'compile-flex-script)
     (require 'compile-flex-clojure)
     (require 'compile-flex-scala)
     (require 'compile-flex-command)
     (require 'compile-flex-python)
     (require 'compile-flex-ess)))
```

### Key Bindings

I use the following key bindings since they clobber Emacs functions I don't
use:
```lisp
;; switch compiler; clobbers `mark-page'
(global-set-key "\C-x\C-p" 'flex-compiler-activate)
(global-set-key "\C-x\C-u" 'flex-compile-compile)
(global-set-key "\C-x\C-y" 'flex-compile-clean)
(global-set-key "\C-x\C-i" 'flex-compile-run-or-set-config)
;; clobbers `delete-blank-lines'
(global-set-key "\C-x\C-o" 'flex-compile-eval)
```


## Usage

Most flexible compilers define a specific source file called the *config* file
in *compile flex* parlance.

There are the operations (also included are the [given](#key-bindings) key
bindings):
* **Choose a Compiler** (`C-x C-p` or `M-x flex-compiler-activate`):
  select/activate a flex compiler.
* **Compile** (`C-x C-u` or `M-x flex-compile-compile`): This is the default
  *make something* command.  For make it invokes the first target, for REPL
  languages like Clojure and ESS it evaluates a `.clj` or `.r` file.
* **Run** (`C-x C-i` or `M-x flex-compile-run-or-set-config`): This starts
  invokes a `run` target for make, starts the REPL for REPL type languages.
* **Evaluate** (`C-x C=o` or `M-x flex-compile-eval`): This invokes the
  compiler's evaluation functionality.  For REPL based languages, this
  evaluates the current form and stores the result in the kill buffer.
* **Clean** (`C-x C-y` or `M-x flex-compile-clean`): This invokes the `clean`
  target for make and kills the REPL for REPL based compilers.
* **Set Config File** (`C-u 1 C-x C-i`): This sets the *config* file, which is the
  `Makefile`, `.clj`, `.r`, `.sh` file etc. *compile*, run or interpret.  Note
  that the prefix `C-u` isn't needed for some compilers like the `script`
  compiler.
* **Go to Config File** (`C-u C-x C-i` or `C-u M-x
  flex-compile-run-or-set-config`): This pops the *config* file/buffer to the
  current buffer.

Each compiler also has configuration setting ability, which is invoked with the
`C-u` universal argument to the compile `C-x C-u` invocation per the
aforementioned `flex-compile-run-or-set-config`.  You can remap the

To select which *compilation* (flex compiler) library you'd like to use with
`C-x C-p` and then auto complete the library name.



## License

Copyright © 2017 Paul Landes

GNU Lesser General Public License, Version 2.0
