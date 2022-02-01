# Changelog for `cleff`

## Unreleased

### Changed

- Changed parameter order of `Handling` class from `e es esSend` to `esSend e es`
- Relaxed fundep of `Handling` to `esSend -> e es` (HO combinators may require `TypeApplication` more often)

### Added

- `Trustworthy` flags for non-internal modules

## 0.1.0.0

- Initial API
