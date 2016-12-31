# Flexible Compilation [![MELPA badge][melpa-badge]][melpa-link] [![MELPA stable badge][melpa-stable-badge]][melpa-stable-link] [![Travis CI Build Status][travis-badge]][travis-link]

  [melpa-link]: https://melpa.org/#/compile-flex
  [melpa-stable-link]: https://stable.melpa.org/#/compile-flex
  [melpa-badge]: https://melpa.org/packages/compile-flex-badge.svg
  [melpa-stable-badge]: https://stable.melpa.org/packages/compile-flex-badge.svg
  [travis-link]: https://travis-ci.org/plandes/compile-flex
  [travis-badge]: https://travis-ci.org/plandes/compile-flex.svg?branch=master




## Usage

This isn't useful by itself, you need to extend but subclassing eieio (Emacs
Lisp) objects.  See the [buffer shell](https://github.com/plandes/bshell)
project for an example.  This is doen by extending Emacs `eieio` objects and
can be done in ~90 lines of code
(example: [buffer shell](https://github.com/plandes/bshell)).

To create your own managed buffer set you must extend `buffer-entry` that
defines behavior for each created managed buffer.  You must also extend the
manager itself `compile-flexr`.


## License

Copyright © 2017 Paul Landes

GNU Lesser General Public License, Version 2.0
