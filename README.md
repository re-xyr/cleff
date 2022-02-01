# `cleff` - fast and concise extensible effects

[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/re-xyr/cleff/build)](https://github.com/re-xyr/cleff/actions/workflows/build.yaml)
[![Hackage](https://img.shields.io/hackage/v/cleff)](https://hackage.haskell.org/package/cleff)

`cleff` is an extensible effects library for Haskell, with a focus on the balance of performance, expressiveness and ease of use. It provides a set of predefined effects that you can conveniently reuse in your program, as well as low-boilerplate mechanisms for defining and interpreting new domain-specific effects on your own.

## Overview

Different from [many](`polysemy`) [previous](`fused-effects`) [libraries](`freer-simple`), `cleff` does not use techniques like Freer monads or monad transformers. Instead, the `Eff` monad is essentially a `ReaderT IO`, which provides predictable semantics and reliable performance. The only caveat is that `cleff` does not support nondeterminism and continuations in the `Eff` monad - but after all, [most effects libraries has broken nondeterminism support](https://github.com/polysemy-research/polysemy/issues/246), and we encourage users to wrap another monad transformer with support of nondeterminism (e.g. `ListT`) over the main `Eff` monad in such cases.

### Performance

`cleff`'s `Eff` monad is essentially implemented as a `ReaderT IO`. This concrete formulation [allows more GHC optimizations to fire][alexis-talk], and brings lower performance overhead. This is first done by [`eff`], and then [`effectful`]; it proved to work, so we followed this path.

[In microbenchmarks](#benchmarks), `cleff` outperforms [`polysemy`], and is slightly behind [`effectful`]. However, note that `effectful` and `cleff` have very different design principles. While `effectful` prioritizes performance over anything else (by [providing static dispatch](https://github.com/arybczak/effectful/blob/master/effectful-core/src/Effectful/Reader/Static.hs)), `cleff` focuses on balancing expressivity and performance. If you would like minimal performance overhead, consider [`effectful`].

### Low-boilerplate

`cleff` supports user-defined effects and provides simple yet flexible API for that. Users familiar with [`polysemy`], [`freer-simple`] or [`effectful`] will find it very easy to get along with `cleff`. `cleff`'s effect interpretation API include:

- Arbitrary lifting and subsumption of effects
- Interpreting and reinterpreting, without needing to distinguish first-order and higher-order interpreters like `polysemy`
- *Translation* of effects, i.e. handling an effect in terms of a simple transformation into another effect, as seen in `polysemy`'s `rewrite` and `freer-simple`'s `translate`

### Predictable semantics

Traditional effect libraries have many surprising behaviors, such as [`mtl` reverts state when an error is thrown][alexis-talk-2], and [more so when interacting with `IO`][readert]. By implementing `State` and `Writer` as `IORef` operations, and `Error` as `Exceptions`, `cleff` is able to interact well with `IO` and provide semantics that are predictable in the presence of concurrency and exceptions. Moreover, any potentially surprising behavior is carefully documented for each effect.

### Higher-order effects

*Higher-order* effects are effects that take monadic computations. They are often useful in real world applications, as examples of higher-order effect operations include `local`, `catchError` and `mask`. Implementing higher-order effects is often tedious, or even not supported in some effect libraries. `polysemy` is the first library that aims to provide easy higher-order effects mechanism with its [`Tactics`](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/Polysemy.html#g:16) API. Following its path, `cleff` provides a set of combinators that can be used to implement higher-order effects. These combinators are as expressive as `polysemy`'s, and are also easier to use correctly.

## Example

This is the code that defines `Teletype` effect. It only takes 20 lines to define the effect and two interpretations, one using stdio and another reading from and writing to a list:

```haskell
import Cleff
import Cleff.Input
import Cleff.Output
import Cleff.State
import Data.Maybe (fromMaybe)

-- Effect definition
data Teletype :: Effect where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()
makeEffect ''Teletype

-- Effect Interpretation via IO
runTeletypeIO :: IOE :> es => Eff (Teletype ': es) a -> Eff es a
runTeletypeIO = interpretIO \case
  ReadTTY    -> getLine
  WriteTTY s -> putStrLn s

-- Effect interpretation via other pure effects
runTeletypePure :: [String] -> Eff (Teletype ': es) w -> Eff es [String]
runTeletypePure tty = fmap (reverse . snd)
  . runState [] . outputToListState
  . runState tty . inputToListState
  . reinterpret2 \case
    ReadTTY -> fromMaybe "" <$> input
    WriteTTY msg -> output msg

-- Using the effect

echo :: Teletype :> es => Eff es ()
echo = do
  x <- readTTY
  if null x then pure ()
    else writeTTY x >> echo

echoPure :: [String] -> [String]
echoPure input = runPure $ runTeletypePure input echo

main :: IO ()
main = runIOE $ runTeletypeIO echo
```

See [`example/`](https://github.com/re-xyr/cleff/tree/master/example/) for more examples.

## Benchmarks

These are the results of the [effect-zoo](https://github.com/ocharles/effect-zoo) microbenchmarks, compiled by GHC 8.10.7. Keep in mind that these are *very short and synthetic programs*, and may or may not tell the accurate performance characteristics of different effect libraries in real use:

- `big-stack`: ![big-stack benchmark result](https://raw.githubusercontent.com/re-xyr/cleff/master/docs/img/effect-zoo-big-stack.png)
- `countdown`: ![countdown benchmark result](https://raw.githubusercontent.com/re-xyr/cleff/master/docs/img/effect-zoo-countdown.png)
- `file-sizes`: ![file-sizes benchmark result](https://raw.githubusercontent.com/re-xyr/cleff/master/docs/img/effect-zoo-file-sizes.png)
- `reinterpretation`: ![reinterpretation benchmark result](https://raw.githubusercontent.com/re-xyr/cleff/master/docs/img/effect-zoo-reinterpretation.png)

## References

These are the useful resources that inspired this library's design and implementation.

Papers:

- [Extensible Effect: An Alternative to Monad Transformers](https://okmij.org/ftp/Haskell/extensible/exteff.pdf) by Oleg Kiselyov, Amr Sabry, and Cameron Swords.
- [Freer Monads, More Extensible Effects](https://okmij.org/ftp/Haskell/extensible/more.pdf) by Oleg Kiselyov, and Hiromi Ishii.

Libraries:

- [`eff`] by Alexis King and contributors.
- [`effectful`] by Andrzej Rybczak and contributors.
- [`freer-simple`] by Alexis King and contributors.
- [`polysemy`] by Sandy Maguire and contributors.

Talks:

- [Effects for Less][alexis-talk] by Alexis King.
- [Unresolved challenges of scoped effects, and what that means for `eff`][alexis-talk-2] by Alexis King.

Blog posts:

- [Asynchronous Exception Handling in Haskell](https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/) by Michael Snoyman.
- [Polysemy: Mea Culpa](https://reasonablypolymorphic.com/blog/mea-culpa/) by Sandy Maguire.
- [Polysemy Internals: The Effect-Interpreter Effect](https://reasonablypolymorphic.com/blog/tactics/) by Sandy Maguire.
- [ReaderT design pattern][readert] by Michael Snoyman.
- [Safe exception handling](https://www.fpcomplete.com/haskell/tutorial/exceptions/) by Michael Snoyman.

[`polysemy`]: https://hackage.haskell.org/package/polysemy
[`fused-effects`]: https://hackage.haskell.org/package/fused-effects
[`effectful`]: https://github.com/arybczak/effectful
[`eff`]: https://github.com/hasura/eff
[`freer-simple`]: https://hackage.haskell.org/package/freer-simple
[alexis-talk]: https://www.youtube.com/watch?v=0jI-AlWEwYI
[alexis-talk-2]: https://www.twitch.tv/videos/1163853841
[readert]: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
