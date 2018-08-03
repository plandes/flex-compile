# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).


## [Unreleased]

### Added
- Option to start REPLs and script/argument based compilers in a configured
  directory.

### Changed
- Refactoring of persistent framework to be more inline with `config-manage`
  dependency.


## [0.2] - 2017-11-07
### Added
- Added [Choice Program] compiler.

### Changed
- Fixed `buffer-manage` clobbering with compile warning issues.


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


[Unreleased]: https://github.com/plandes/flex-compile/compare/v0.2...HEAD
[0.2]: https://github.com/plandes/flex-compile/compare/v0.1...v0.2
[0.1]: https://github.com/plandes/flex-compile/compare/772d70f...v0.1

<!-- links -->
[Choice Program]: https://github.com/plandes/choice-program
