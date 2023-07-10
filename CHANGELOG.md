# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.1.2] - 2023-07-10
### Changed
- removed Compat
- upgraded Setfield dependency to include version 1

## [1.1.1] - 2022-07-01
### Added
- in case of a MethodError, a `WhereTraitsMethodError` is thrown instead which adds
  detailed information about the available traits.
- `UndefVarError` is thrown if variables are used within the traits definition which are
  obvious typos.
- added documentation fo WhereTraitsMethodError

## [1.1.0] - 2022-04-09

### Added
- `@traits_order` macro was added for resolving ambiguity among traits definition in a explicit way
- nice error reporting in case a traits ambiguity is found, including concrete `@traits_order` suggestion
- the underlying ambiguity handling was implemented respectively

### Changed
- updated dependency `StructEquality` to version 2.0
- dropped support for Julia version < 1.6 for maintenance simplicity and because 1.6 replaced 1.0 as the new long term release

## [1.0.0] - 2020-11-30
### Added
- official registered package at Julia General registry

### Changed
- Internal representation changed to use one storage per normal dispatch signature (previously it was on global storage for all) in order to have less method overwrite warnings.
- switched dependency from  ProxyInterface (singular, now deprecated) to ProxyInterfaces (plural)
- all dependencies are now registered in Julia General registry, hence custom registry no longer needed for CICD.

## [0.5.3] - 2020-07-27
### Added
- GithubActions for CICD
- Documentation using Documenter.jl
- License
- Codecoverage

### Changed
- renamed from "Traits.jl" to "WhereTraits.jl"

## [0.5.3] - 2020-05-04
### Fixed
- bugs in internal dispatch

### Changed
- README.md now reflects the new refactored version

### Added
- README.md now mentions `@traits_show_implementation`

## [0.5.2] - 2020-05-02
### Changed
- refactored and split internal code into two additional submodules for better readability
- switched dependency from ASTParser to ExprParsers library

## [0.5.1] - 2020-03-06
initial release
