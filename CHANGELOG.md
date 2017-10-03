# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]

## [1.0.3] - 2017-10-02

### Fixed

- defs no longer makes a var from &

### Added

- sentinel and sentinels

- add :do, :when-not, :while-not, :when-let and :while-let modifiers to for and for-likes

### Deprecated

- :into as a for modifier

### Un-Deprecated

- Resurrect forv, for-map and forcatv

## [1.0.2] - 2017-09-14

### Added

- Support for defining metadata in defs

## [1.0.1] - 2017-09-10

### Added

- Variation of core/for that accepts the modifier :into.

- Also add :into capability to forcat.

### Deprecated

- forv, for-map and forcatv

## [1.0.0-alpha3] - 2017-08-13

### Breaking change

- Remove core segment from namespace (madstap.comfy.core => madstap.comfy)
