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


<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [Introduction](#introduction)
    - [Motivation](#motivation)
- [Configuration](#configuration)
    - [Key Bindings](#key-bindings)
- [Usage](#usage)
- [Compilers](#compilers)
    - [Make](#make)
    - [Command](#command)
    - [Beanshell](#beanshell)
    - [Script](#script)
    - [Clojure](#clojure)
    - [Scala](#scala)
    - [Python](#python)
    - [Ess](#ess)
    - [XML Validation](#xml-validation)
    - [Choice Program](#choice-program)
- [Changelog](#changelog)
- [License](#license)

<!-- markdown-toc end -->

## Introduction

The general idea is the keybindings to compile you get use to are "switched" to
whatever specific problem you're working on.  For example, if you're compiling
with a makefile, use the *make* compiler to initiate an make process.  If
you're working with a REPL based langauage (i.e. [flex-compile-clojure],
[flex-compile-scala] etc) instead a buffer or expression is evaluated.


### Motivation

Many of the "compilers" (i.e. the [flex-compile-command] and
[flex-compile-script]) don't do much more than invoke a function or execute a
script.  However, when jumping between several development tasks the strength
of the library comes from easily switching between compilers using the same
keybindings and finger muscle memory to invoke them.


## Configuration

Add the following to your `~/.emacs` file:
```emacs-lisp
(require 'flex-compile)
```
This loads the library and creates global key bindings.


### Key Bindings

I use the following key bindings since they clobber Emacs functions I don't
use:
```emacs-lisp
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
* **Evaluate** (`C-x C-o` or `M-x flex-compile-eval`): This invokes the
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

The top level library `flex-compile` library provides a plugin architecture for
add-on libraries, which include:
* [flex-compile-make]
* [flex-compile-command]
* [flex-compile-beanshell]
* [flex-compile-script]
* [flex-compile-clojure]
* [flex-compile-scala]
* [flex-compile-python]
* [flex-compile-ess]
* [flex-compile-xml-validate]
* [flex-compile-choice-program]

You can write your own compilers as add-on plugins.  However, there are many
that come with this package.


### Make

This compiler invokes make as an asynchronous process in a buffer.  The first
target, `run` target, and `clean` target are invoked respectfully with
*compile*, *run* and *clean* Emacs commands (see [usage](#usage)).


### Command

This "compiler" is more of a convenience to invoke an Emacs Lisp function or
form.  This is handy for functions that you end up invoking over and over with
`M-x` (i.e. `cider-test-run-ns-tests`).  See [motivation](#motivation).

### Beanshell

Compiler and environment for evaluting and running [Beanshell].


### Script

This compiler runs a script with optional arguments in an async buffer.
See [motivation](#motivation).


### Clojure

This is a REPL based compiler that allows for evaluation Clojure buffers,
expressions and starting the REPL using [Cider].


### Scala

This is a REPL based compiler that allows for evaluation Scala buffers,
expressions the Scala REPL and compilation with [sbt].


### Python

This is a compiler that allows for evaluation Python buffers and expressions
using [python mode].


### Ess

This is a compiler to evaluate R code with [Emacs Speaks Statistics].


### XML Validation

Implementation compiler for XML validation using command line [xmllint] command
line tool.


### Choice Program

Prompt and more easily invoke choice/action based programs using the
[Choice Program] Emacs library.


## Changelog

An extensive changelog is available [here](CHANGELOG.md).


## License

Copyright © 2017 Paul Landes

GNU Lesser General Public License, Version 2.0


<!-- links -->
[flex-compile-make]: #make
[flex-compile-command]: #command
[flex-compile-beanshell]: #beanshell
[flex-compile-script]: #script
[flex-compile-maven]: #maven
[flex-compile-clojure]: #clojure
[flex-compile-scala]: #scala
[flex-compile-python]: #python
[flex-compile-ess]: #ess
[flex-compile-xml-validate]: #xml-validation
[flex-compile-choice-program]: #choice-program

[Beanshell]: http://www.beanshell.org
[Cider]: https://github.com/clojure-emacs/cider
[sbt]: http://www.scala-sbt.org
[python mode]: https://github.com/fgallina/python.el
[Emacs Speaks Statistics]: https://ess.r-project.org
[Choice Program]: https://github.com/plandes/choice-program
[xmllint]: http://xmlsoft.org/xmllint.html

[melpa-link]: https://melpa.org/#/flex-compile
[melpa-stable-link]: https://stable.melpa.org/#/flex-compile
[melpa-badge]: https://melpa.org/packages/flex-compile-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/flex-compile-badge.svg
[travis-link]: https://travis-ci.org/plandes/flex-compile
[travis-badge]: https://travis-ci.org/plandes/flex-compile.svg?branch=master
