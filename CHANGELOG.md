# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
