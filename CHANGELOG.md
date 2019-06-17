# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).


## [Unreleased]

## [0.4] - 2019-06-17
### Added
- New configuration system that moves the responsibility out of the specific
  compilers for configuration.  Configuration meta data has been added to
  [Buffer Manage] and *flex compile* extends that frame work to configure each
  compiler.


## [0.3] - 2019-06-15
### Added
- Option to start REPLs and script/argument based compilers in a configured
  directory.

### Changed
- Refactoring of persistent framework to be more inline with `config-manage`
  dependency.

### Removed
- Support for Scala.  After the major refactoring it's non-trivial to retrofit
  it to the new framework and I'm currently no longer writing Scala.  I'm glad
  to work with anyone willing to do this work.


## [0.2] - 2017-11-07
### Added
- Added [Choice Program] compiler.

### Changed
- Fixed `buffer-manage` clobbering with compile warning issues.


## [0.1] - 2017

### Added
- Choice program compiler.
- Fixed compiler configuration merge with new registered compilers bug.

### Changed
- More documentation.


### Changed
- Optionally prompt for evaluate form using `flex-compile-eval`.  Before this
  function would obligate the user to provide a confirmed for to execute.
- Fix native order for the Python compiler setup on compile.

## [0.1] - 2017-09-17
First major release.


[Unreleased]: https://github.com/plandes/flex-compile/compare/v0.4...HEAD
[0.4]: https://github.com/plandes/flex-compile/compare/v0.3...v0.4
[0.3]: https://github.com/plandes/flex-compile/compare/v0.2...v0.3
[0.2]: https://github.com/plandes/flex-compile/compare/v0.1...v0.2
[0.1]: https://github.com/plandes/flex-compile/compare/772d70f...v0.1

<!-- links -->
[Choice Program]: https://github.com/plandes/choice-program
[Buffer Manage]: https://github.com/plandes/buffer-manage
