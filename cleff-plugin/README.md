# `cleff-plugin`

This GHC typechecking plugin disambiguates obvious usages of effects for the extensible effect framework `cleff`.

## Usage

This plugin works with GHC 8.6 through 9.2 and `cleff ^>= 0.3.1`. To use the plugin:

1. Add this plugin as your package's dependency.
    - If you use `stack`, then you also need to add these lines your `stack.yaml`:
      ```yaml
      extra-deps:
      - cleff-plugin-0.1.0.0
      ```
2. Enable the plugin by adding the following GHC option to your project file:
    ```
    ghc-options: -fplugin=Cleff.Plugin
    ```

## What it does

When using `cleff`, the following code would not compile:

```haskell
action :: '[State Int, State String] :>> es => Eff es ()
action = do
  x <- get
  put (x + 1)
-- • Could not deduce (Cleff.Internal.Rec.Elem (State s0) es)
--     arising from a use of ‘get’
```

This is because GHC is unable to determine which `State` effect we're trying to operate on, although the only viable choice is `State Int`. We had to write:

```haskell
action :: '[State Int, State String] :>> es => Eff es ()
action = do
  x <- get @Int
  put (x + 1)
```

This is quite unwieldy. This plugin tells GHC extra information so code like this can typecheck without requiring manually annotating which effect to use.

## References

This plugin's design is largely inspired by [`polysemy-plugin`](https://hackage.haskell.org/package/polysemy-plugin), which is in turn based on [`simple-effects`](https://hackage.haskell.org/package/simple-effects).

Refer to [`docs/plugin-spec.md`](../docs/plugin-spec.md) for details on the disambiguation algorithm this typecheck plugin uses.
