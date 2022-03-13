# Changelog for `cleff`

## Unreleased

## 0.3.2.0 (2022-03-13)

### Changed

- Slight performance improvements
- `(:>)` is now a typeclass by itself instead of a type synonym

## 0.3.1.0 (2022-02-28)

### Added

- `makeEffect` is now capable of generating sending functions for operations using concrete `Eff`s for the monad type

### Removed

- Dependency on `rec-smallarray`

## 0.3.0.1 (2022-02-21)

Clarify changelog: new features that are listed "Unreleased" in 0.3.0.0 changelog are in fact *released*

## 0.3.0.0 (2022-02-21)

### Changed

- **[BREAKING]** Introduces an `OnException` primitive for `Mask` that replaces `Bracket` and `BracketOnError`
  (note that this only affects the effect datatype; there is still `bracket` and `bracketOnError` *functions* with the same semantics)
- `runError` and `mapError` are slightly (but observably) faster now

### Added

- `freshEnumToState` for `Fresh`
- `onException` and `bracketOnError_` for `Mask`

## 0.2.1.0 (2022-02-13)

### Added

- Lifted convenience instances of `Bounded`, `Num`, `Fractional`, `Floating` and `IsString` for `Eff`
- `MonadZip` instance from the `MonadComprehensions` extension for `Eff`
- `runFreshAtomicCounter` for `Fresh`
- `inputToReader`, `mapInput` and `bindInput` for `Input`
- `mapOutput` and `bindOutput` for `Output`
- `runStateIORef`, `runStateMVar` and `runStateTVar` for `State`

## 0.2.0.0 (2022-02-06)

### Changed

- **[BREAKING]** Changed parameter order of `Handling` class from `e es esSend` to `esSend e es`
- **[BREAKING]** Relaxed fundep of `Handling` to `esSend -> e es` (HO combinators may require `TypeApplication` more often)
- Moved `Data.*` modules to `Cleff.Internal.*` so as not to pollute common namespaces

### Added

- `Trustworthy` flags for non-internal modules
- `sendVia` for sending an effect operation along a transformation between effect stacks
- `raiseUnder`, `raiseNUnder`, `raiseUnderN`, `raiseNUnderN` for introducing effects under other effects in the effect stack
- `runWriterBatch` as a more efficient `Writer` interpreter that writes `listen`ed values in batch instead of in real time

## 0.1.0.0 (2022-01-31)

- Initial API
