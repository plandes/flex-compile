# Flexible Evaluation and Compilation

[![MELPA badge][melpa-badge]][melpa-link]
[![MELPA stable badge][melpa-stable-badge]][melpa-stable-link]
[![Travis CI Build Status][travis-badge]][travis-link]

Run, evaluate and compile functionality for a variety of different languages
and modes.  The specific "compilation" method is different across each add-on
library, which are called *flexible compilers*.  For example, for ESS and
Clojure you can evaluate a specific file and/or evaluate a specfic expression
via a REPL.  For running a script or starting a `make` an async process is
started.

The top level library `flex-compile` library provides a plugin architecture for
[add-on libraries](#compilers), which include:
* [flex-compile-make](#make)
* flex-compile-beanshell
* flex-compile-script
* flex-compile-maven
* flex-compile-clojure
* flex-compile-scala
* flex-compile-command
* flex-compile-python
* flex-compile-ess


<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [Configuration](#configuration)
    - [Key Bindings](#key-bindings)
- [Usage](#usage)
- [Compilers](#compilers)
    - [Make](#make)
- [License](#license)

<!-- markdown-toc end -->


## Configuration

Add the following to your `~/.emacs` file:
```lisp
(require 'flex-compile)
```
This loads the file and creates global key bindings.

For each specific *flexible* compilation method you want:
```lisp
(eval-after-load
    "flex-compile"
  '(progn
     (require 'flex-compile-make)
     (require 'flex-compile-script)
     (require 'flex-compile-clojure)
     (require 'flex-compile-scala)
     (require 'flex-compile-command)
     (require 'flex-compile-python)
     (require 'flex-compile-ess)))
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

Most flexible compilers (any subclass of `config-flex-compiler`) define a
specific source file called the *config* file.

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
* **Set Config File** (`C-u 1 C-x C-i`): This sets the *config* file, which is
  the `Makefile`, `.clj`, `.r`, `.sh` file etc. *compile*, run or interpret.
  Note that the prefix `C-u` isn't needed for some compilers like the `script`
  compiler.
* **Go to Config File** (`C-u C-x C-i` or `C-u M-x
  flex-compile-run-or-set-config`): This pops the *config* file/buffer to the
  current buffer.

Each compiler also has configuration setting ability, which is invoked with the
`C-u` universal argument to the compile `C-x C-u` invocation per the
aforementioned `flex-compile-run-or-set-config`.


## Compilers

You can write your own compilers as add-on plugins.  However, there are many
that come with this package.


### Make

This compiler invokes make as an asynchronous process in a buffer.  The first
target, `run` target, and `clean` target are invoked respectfully with
*compile*, *run* and *clean* Emacs commands (see [usage](#usage)).


## License

Copyright © 2017 Paul Landes

GNU Lesser General Public License, Version 2.0


<!-- links -->
[melpa-link]: https://melpa.org/#/flex-compile
[melpa-stable-link]: https://stable.melpa.org/#/flex-compile
[melpa-badge]: https://melpa.org/packages/flex-compile-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/flex-compile-badge.svg
[travis-link]: https://travis-ci.org/plandes/flex-compile
[travis-badge]: https://travis-ci.org/plandes/flex-compile.svg?branch=master
