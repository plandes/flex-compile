# Flexible Evaluation and Compilation

[![MELPA badge][melpa-badge]][melpa-link]
[![MELPA stable badge][melpa-stable-badge]][melpa-stable-link]
[![Build Status][build-badge]][build-link]


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
- [Usage](#usage)
    - [Special Cases](#special-cases)
- [Compiler Configuration](#compiler-configuration)
    - [Revealing The Interactive Buffer](#revealing-the-interactive-buffer)
- [Compilers](#compilers)
    - [Choice program](#choice-program)
    - [CLI python file](#cli-python-file)
    - [Clojure](#clojure)
    - [Comint](#comint)
    - [Command](#command)
    - [Do nothing](#do-nothing)
    - [Emacs speaks statistics](#emacs-speaks-statistics)
    - [Make](#make)
    - [Org mode](#org-mode)
    - [Python](#python)
    - [Script](#script)
    - [XML](#xml)
- [Invoking a Compiler Programmatically](#invoking-a-compiler-programmatically)
- [Writing Your Own Compiler](#writing-your-own-compiler)
- [Changelog](#changelog)
- [License](#license)

<!-- markdown-toc end -->

## Introduction

The general idea is the keybindings to compile you get use to are "switched" to
whatever specific problem you're working on.  For example, if you're compiling
with a makefile, use the *make* compiler to initiate an make process.  If
you're working with a REPL based langauage (i.e. [flex-compile-python],
[flex-compile-clojure] etc) instead a buffer or expression is evaluated.


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
(flex-compile-init)
;; recommended to bind functions globally but not necessary
(flex-compile-key-bindings)
```
This loads the library and creates global key bindings.


## Usage

Most flexible compilers (any subclass of `config-flex-compiler`) define a
specific source file called the *config* file.

There are the operations (also included are the [given](#key-bindings) key
bindings):
* **Choose a Compiler** (`C-x C-p` or `M-x flex-compiler-do-activate`):
  select/activate a flex compiler.
* **Set Configuration** (`C-u C-x C-u` or `C-u M-x flex-compiler-do-compile`):
  This allows the configuration of the compiler.  In most compilers the first
  step is to select the property, then the configuration for that property is
  prompted.  In some cases a specific property is set, like the [Make](#make)
  compiler jumps to the target.  All
  properties are always available for configuration with `C-u 0 C-x C-u`.  
* **Select or Get Information** (`M-x flex-compiler-do-list`): This displays a
  list of available compilers that are selectable with RET or provide
  documentation and configuration with `?`.  In addition, `e` configures the
  compiler.
* **Prompt for Config File** (`C-u 1 C-x C-u` or `C-u 1 M-x
  flex-compiler-do-compile`): This the file the compiler will use.  For example,
  the [Script](#script) compiler will run selected script file and display the
  output on compile.  For REPL compilers, like the [Python](#python) copiler,
  the starting directory can also be set (see **Set Configuration**)..
* **Set Config File** (`C-u 1 C-x C-i` or `C-u 1
  flex-compiler-do-run-or-set-config`): This sets the *config* file to compile, run
  or interpret.  The term *config* is a nomenclature and examples include
  `Makefile`, `.clj`, `.r`, `.sh` files.
* **Go to Config File** (`C-u C-x C-i` or `C-u M-x
  flex-compiler-do-run-or-set-config`): This displays the *config* file/buffer to
  the current buffer.  For some compilers, this also displays the interactive
  (i.e. REPL) buffer.  See the [reveal
  buffer](#revealing-the-interactive-buffer) section for more information, and
  specifically how to force show the buffer.
* **Compile** (`C-x C-u` or `M-x flex-compiler-do-compile`): This is the default
  *make something* command.  For make it invokes the first target, for REPL
  languages like Clojure and ESS it evaluates a `.clj` or `.r` file.
* **Run** (`C-x C-i` or `M-x flex-compiler-do-run-or-set-config`): This starts
  invokes a `run` target for make, starts the REPL for REPL type languages.
* **Evaluate** (`C-x C-o` or `M-x flex-compiler-do-eval`): This invokes the
  compiler's evaluation functionality.  For REPL based languages, this
  evaluates the current form and stores the result in the kill buffer.
* **Clean** (`C-x C-y` or `M-x flex-compiler-do-clean`): This invokes the `clean`
  target for make and kills the REPL for REPL based compilers.
* **Generate Compiler Docs** (`M-x flex-compiler-doc-show`): this generates the
  documentation given in the [compilers](#compilers) section (verbatim).

Each compiler also has configuration setting ability, which is invoked with the
`C-u` universal argument to the compile `C-x C-u` invocation per the
aforementioned `flex-compiler-do-run-or-set-config`.


### Special Cases

Some compilers are configured differently for the default configuration key
binding.  For example the [make](#make) compiler sets the make target defined
in the make file, and in this case uses `C-u 0 C-u` to set configuration
properties.


## Compiler Configuration

This package uses a configuration system that moves the responsibility out of
the specific compilers for configuration.  This package extends from the
`config-manage` framework in the [buffer manage] library by extending and
building configuration meta data.  For example, the `M-x flex-compiler-list`
lists available compiler with `?` providing information and `e` configuring the
compiler.


### Revealing The Interactive Buffer

One of the features of the *flex compiler* library is it provides specific
behavior on example where and how to display buffers, which is a compiler
configuration.  These buffers are usually interactive buffers, i.e. REPL
buffers.  Any compiler that extends `single-buffer-flex-compiler` has this
capability, which include the following:

* [flex-compile-make]
* [flex-compile-command]
* [flex-compile-cli]
* [flex-compile-script]
* [flex-compile-clojure]
* [flex-compile-python]
* [flex-compile-ess]
* [flex-compile-xml-validate]
* [flex-compile-choice-program]


The two properties for these compilers include:

* **Buffer New Mode**: used when the interactive buffer is created.
* **Buffer Exists Mode**: used when the interactive buffer is already exists.


These can be set to the *Global* settings, which means to take it from the
customzied variables `flex-compile-single-buffer-display-buffer-new-mode` and
`flex-compile-single-buffer-display-buffer-exists-mode`.  These variables and
the compiler level properties can be set to one of:

* **Switch to Buffer** means to first pop then switch to the buffer.
* **Display Buffer** means to show the buffer in a different window.
* **Next Frame Otherwise Switch** means to use the next frame if there are
  multiple frames, otherwise pop and switch to the buffer.
* **Next Frame Otherwise Display** means to use the next frame if there are
  multiple frames, otherwise show buffer.
* **Next Frame Skip Switch** means to do nothing there are multiple frames,
  otherwise pop and switch to the buffer.
* **Next Frame Skip Display** means to do nothing there are multiple frames,
  otherwise display the buffer.
* **Never** means to never show the buffer.


The decision of where to show a buffer (or not) happens either when the
interative buffer is created or during a compilation.  In many cases you might
not want to ever show the buffer, so you could set both buffer `new` and
`exist` properties to *never*.  In this case, you still force the interactive
buffer with `C-u 2 C-x C-i` or `C-u 2 M-x flex-compiler-run-or-set-config`.


## Compilers

Concrete instances of *flexible* compilers that provide a common interface.
Each is an implementation of glue code to the respective compilation method.

Note that all compilers that extend from `conf-file-flex-compiler`, which
include `make`, `script`, `xml-validate`, `org-mode`, `python`, `clojure`, and
`ess` have their `start-directory` property unset each time the `config-file`
is set.

This documentation was generated with `M-x flex-compiler-doc-show`.


### Choice program

Prompt and more easily invoke choice/action based programs using the
[Choice Program](https://github.com/plandes/choice-program) Emacs library.

Properties:
  * Program: An instance of `choice-program`.
  * Action: The action to invoke on the program.
  * Buffer Exists Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-exists-mode`.
  * Buffer New Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-new-mode`.
  * Kill Buffer Clean: If non-nil kill the buffer on clean.


### CLI python file

Provides support for user input for action mnemonics and options using Python
programs that use the
[Zensols action CLI](https://plandes.github.io/util/doc/command-line.html).

This compiler gets the command line metadata as a list of actions and their
respective positional and option arguments.  It this prompts the user with
documentation associated, first the action, then the action's arguments.
Finally, it constructs the command line and executes the Python program with
the arguments.

Properties:
  * Action: The action to invoke on the program.
  * Config File: The file to use for *configuring* the compiler.
  * Arguments: The arguments to give to the script.
  * Cache Metadata: 
    Whether or not to cache the Python program's CLI metadata.
  * Buffer Exists Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-exists-mode`.
  * Buffer New Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-new-mode`.
  * Kill Buffer Clean: If non-nil kill the buffer on clean.
  * Start Directory: The directory for starting the compilation.


### Clojure

This is a REPL based compiler that allows for evaluation Clojure
buffers, expressions and starting the REPL using
[Cider](https://github.com/clojure-emacs/cider).

The Clojure compiler connects using two Cider modes: the default `jack-in`
mode or connecting to a host and port remotely with `cider-connect`.  You can
switch betwee these two methods with the [given keybindings](#key-bindings):

  `M-x 1 C-u C-x C-u`

See documetation with `M-h f flex-compiler-query-eval` method for more
inforamtion (and current binding).

Todo: support multiple Cider buffers as this implementation currently does
not.

Properties:
  * Config File: The file to use for *configuring* the compiler.
  * Buffer Exists Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-exists-mode`.
  * Buffer New Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-new-mode`.
  * Kill Buffer Clean: If non-nil kill the buffer on clean.
  * Connect Mode: Defines how to connect to a Clojure REPL.
  * Repl Port: The port running the REPL; default: 32345
  * Output Clear: Whether or not to clear comint buffer after a compilation.
  * Prompt Kill Repl Buffer: If non-`nil` then prompt to kill a REPL buffer on clean.
  * Repl Buffer Start Timeout: Number of seconds as an integer to wait to start before giving up (and not
    displaying).
  * Repl Buffer Start Wait: Number of seconds (as a float) to wait before issuing any first command to the
    REPL.
  * Start Directory: The directory for starting the compilation.


### Comint

Send text to any running `comint` buffer.
This is useful for entering a command in a shell, SQL etc buffer that otherwise
requires switching back and forth between buffers, which is a hassle.

Properties:
  * Config File: The file to use for *configuring* the compiler.
  * Buffer: The buffer to insert the `content` slot.
  * Content: The string to insert in the buffer referred by the `buffer` slot.
  * Start Directory: The directory for starting the compilation.


### Command

This "compiler" is more of a convenience to invoke an Emacs Lisp function or
form.  This is handy for functions that you end up invoking over and over with
`M-x` (i.e. `cider-test-run-ns-tests`).  See [motivation](#motivation).

Properties:
  * Sexp: The symbol expression to evaluate.


### Do nothing

A no-op compiler for the disabled state.


### Emacs speaks statistics

This is a REPL based compiler to evaluate R code with
[Emacs Speaks Statistics](https://ess.r-project.org) .

Properties:
  * Config File: The file to use for *configuring* the compiler.
  * Buffer Exists Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-exists-mode`.
  * Buffer New Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-new-mode`.
  * Kill Buffer Clean: If non-nil kill the buffer on clean.
  * Output Clear: Whether or not to clear comint buffer after a compilation.
  * Prompt Kill Repl Buffer: If non-`nil` then prompt to kill a REPL buffer on clean.
  * Repl Buffer Start Timeout: Number of seconds as an integer to wait to start before giving up (and not
    displaying).
  * Repl Buffer Start Wait: Number of seconds (as a float) to wait before issuing any first command to the
    REPL.
  * Start Directory: The directory for starting the compilation.


### Make

This compiler invokes make as an asynchronous process in a
buffer.  The first target, `run` target, and `clean` target are
invoked respectfully with *compile*, *run* and *clean* Emacs
commands (see [usage](#usage)).

This is a special compiler in it's configuration.  Instead of
setting properties, the default configuration mechanism is to set
the make target instead.  If you want to set a flex compiler
property, use `\C-u 0 \C-u`.

When setting the configuration file the target property is unset.

Properties:
  * Config File: The file to use for *configuring* the compiler.
  * Target: The make file target to satisfy.
  * Run Target: The target used to run or test as the secondary compilation functionality.
  * Buffer Exists Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-exists-mode`.
  * Buffer New Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-new-mode`.
  * Kill Buffer Clean: If non-nil kill the buffer on clean.
  * Start Directory: The directory for starting the compilation.


### Org mode

This compiler exports [Org mode](https://orgmode.org) to external formats and
then shows the output in the browser.  Only HTML is currently supported.

Properties:
  * Config File: The file to use for *configuring* the compiler.
  * Export Fn: The Org mode export function.
  * Open File: Whether to open the file after exported.
  * Output Directory: The output directory.
  * Frame Focus Command: 
    The command to refocus the Emacs frame after rendering the output (browser).
  * Frame Focus Delay: Seconds before refocusing the Emacs frame after redering the output.
  * Start Directory: The directory for starting the compilation.


### Python

This is a REPL based compiler that allows for evaluation Python buffers and
expressions using [python mode](https://github.com/fgallina/python.el).

Properties:
  * Config File: The file to use for *configuring* the compiler.
  * Buffer Exists Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-exists-mode`.
  * Buffer New Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-new-mode`.
  * Kill Buffer Clean: If non-nil kill the buffer on clean.
  * Output Clear: Whether or not to clear comint buffer after a compilation.
  * Prompt Kill Repl Buffer: If non-`nil` then prompt to kill a REPL buffer on clean.
  * Repl Buffer Start Timeout: Number of seconds as an integer to wait to start before giving up (and not
    displaying).
  * Repl Buffer Start Wait: Number of seconds (as a float) to wait before issuing any first command to the
    REPL.
  * Start Directory: The directory for starting the compilation.


### Script

This compiler runs a script with optional arguments in an async buffer.
See [motivation](#motivation).

Properties:
  * Config File: The file to use for *configuring* the compiler.
  * Buffer Exists Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-exists-mode`.
  * Buffer New Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-new-mode`.
  * Kill Buffer Clean: If non-nil kill the buffer on clean.
  * Arguments: The arguments to give to the script.
  * Start Directory: The directory for starting the compilation.


### XML

Implementation compiler for XML validation using command line
[xmllint](http://xmlsoft.org/xmllint.html) command line tool.

Properties:
  * Config File: The file to use for *configuring* the compiler.
  * Schema File: Location of the schema file to validate against.
  * Buffer Exists Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-exists-mode`.
  * Buffer New Mode: Compiler instance of `flex-compile-single-buffer-display-buffer-new-mode`.
  * Kill Buffer Clean: If non-nil kill the buffer on clean.
  * Start Directory: The directory for starting the compilation.


## Invoking a Compiler Programmatically

There might be instances where it's necessary to execute other actions along
with a compilation.  If you know Emacs Lisp, you can create your own function
to invoke other compilers, and then use the [Command](#command) compiler to
invoke it as a command (an interactive function), such as:

```lisp
(defun clean-and-call-python-compiler ()
  "Invoke `make clean', then call the Python compiler."
  (interactive)
  (shell-command "make clean")
  (let ((this (flex-compiler-by-name "python")))
    (flex-compiler-compile this)))
```


## Writing Your Own Compiler

Writing your own flexible compiler is pretty easy if you know how to write
Emacs object oriented programs.  Even if you don't, you should be able to find
a compiler already written that follows your use case among those concerete
(see [compilers](#compilers)) that are already given in this package.  You can
also submit an issue (see the [contributing instructions](CONTRIBUTING.md)).


## Changelog

An extensive changelog is available [here](CHANGELOG.md).


## License

Copyright © 2017-2020 Paul Landes

GNU Lesser General Public License, Version 2.0


<!-- links -->
[flex-compile-make]: #make
[flex-compile-command]: #command
[flex-compile-cli]: #cli-python-file
[flex-compile-script]: #script
[flex-compile-clojure]: #clojure
[flex-compile-python]: #python
[flex-compile-ess]: #ess
[flex-compile-xml-validate]: #xml-validation
[flex-compile-choice-program]: #choice-program

[melpa-link]: https://melpa.org/#/flex-compile
[melpa-stable-link]: https://stable.melpa.org/#/flex-compile
[melpa-badge]: https://melpa.org/packages/flex-compile-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/flex-compile-badge.svg
[build-badge]: https://github.com/plandes/flex-compile/workflows/CI/badge.svg
[build-link]: https://github.com/plandes/flex-compile/actions

[buffer manage]: https://github.com/plandes/buffer-manage
