# Changelog for `cleff`

## Unreleased

### Added

- Convenience lifted instances of `Bounded`, `Num`, `Fractional`, `Floating` and `IsString` for `Eff`
- `MonadZip` instance from the `MonadComprehensions` extension for `Eff`
- `freshEnumToState` and `runFreshAtomicCounter` for `Fresh`
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
