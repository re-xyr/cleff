# cleff

> Note: this library is still WIP! At the same time, you can preview it [at the package candidate](https://hackage.haskell.org/package/cleff-0.1.0.0/candidate).

`cleff` is an extensible effects library for Haskell. It provides a set of predefined effects that you can conveniently reuse in your program, as well as mechanisms for defining and interpreting new domain-specific effects on your own.

- `cleff` supports **higher-order** effects, like `bracket`, `local` and `mask`, and provides a set of easy-to-use combinators to interpret them.
- `cleff` is **fast**. It is essentially built on top of a `ReaderT IO`, which admits many compiler optimizations.
- `cleff` requires **little boilerplate** to interpret an effect; the interface is similar to that of [`freer-simple`] and [`polysemy`], but sometimes even simpler.
- `cleff` takes inspiration from [`freer-simple`], [`polysemy`] and [`effectful`]. If you have used any of them, it would be very easy to get along with `cleff`.

## Rationale

We have [a][`polysemy`] [bunch][`fused-effects`] [of][`effectful`] [effect][`eff`] [libraries][`freer-simple`] out there, why another? To put it simply: `cleff` is an attempt of implementing an expressive effect system, with good ergonomics and a unified API, and without sacrificing much performance.

In particular, `cleff` uses a `ReaderT IO` as the underlying representation of the `Eff` monad. With this representation, [more optimizations are possible][alexis-talk], and thus brings hope for lower performance overhead. The [`effectful`] library already uses the approach, and proved it to be true; so we follow this path. Indeed, this means that we lose nondeterminism and continuations in the `Eff` monad - but after all, [most effects libraries has broken nondeterminism support](https://github.com/polysemy-research/polysemy/issues/246), and you could always wrap another monad transformer with support of nondeterminism (e.g. `ListT`) over the main `Eff` monad.

However, `cleff` is also like [`polysemy`], in the sense that it supports very flexible and user-friendly effect interpretation. This includes support for [arbitrary effect lifting and subsumption](https://hackage.haskell.org/package/cleff-0.1.0.0/candidate/docs/Cleff.html#g:4), as well as [interpreting higher-order effects](https://hackage.haskell.org/package/cleff-0.1.0.0/candidate/docs/Cleff.html#g:6), with arguably even less boilerplate than `polysemy`.

[In terms of performance](#benchmarks), `cleff` outperforms `polysemy` in microbenchmarks, and often being comparable to `effectful`. However, note that `effectful` and `cleff` have very different design principles. While `effectful` prioritizes performance (by [providing static dispatch](https://github.com/arybczak/effectful/blob/master/effectful-core/src/Effectful/Reader.hs)), `cleff` more focuses on smoothing higher-order effect interpretation and providing user-friendly interpretation combinators, and finally have a unified effect interface that maximizes the ease of use. If you would like minimal performance overhead, please still consider [`effectful`].

In conclusion, `cleff` is an effect library that tries to find a good balance between simplicity, performance, and expressivity.

## Example

The classical `Teletype` effect:

```haskell
import Cleff
import Cleff.Input
import Cleff.Output
import Cleff.State
import Data.Maybe (fromMaybe)

data Teletype :: Effect where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()
makeEffect ''Teletype

runTeletypeIO :: IOE :> es => Eff (Teletype ': es) a -> Eff es a
runTeletypeIO = interpretIO \case
  ReadTTY    -> getLine
  WriteTTY s -> putStrLn s

runTeletypePure :: [String] -> Eff (Teletype ': es) w -> Eff es [String]
runTeletypePure tty = fmap (reverse . snd)
  . runState [] . outputToListState
  . runState tty . inputToListState
  . reinterpret2 \case
    ReadTTY -> fromMaybe "" <$> input
    WriteTTY msg -> output msg

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

These are the useful resourses that inspired this library.

Libraries:

- [`eff`] by Alexis King and contributors.
- [`effectful`] by Andrzej Rybczak and contributors.
- [`freer-simple`] by Alexis King and contributors.
- [`polysemy`] by Sandy Maguire and contributors.

Talks:

- [Effects for Less][alexis-talk] by Alexis King.
- [Unresolved challenges of scoped effects, and what that means for `eff`](https://www.twitch.tv/videos/1163853841) by Alexis King.

Blog posts:

- [Polysemy: Mea Culpa](https://reasonablypolymorphic.com/blog/mea-culpa/) by Sandy Maguire.
- [Polysemy Internals: The Effect-Interpreter Effect](https://reasonablypolymorphic.com/blog/tactics/) by Sandy Maguire.
- [ReaderT design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) by Michael Snoyman.
- [Safe exception handling](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/) by Michael Snoyman.

[`polysemy`]: https://hackage.haskell.org/package/polysemy
[`fused-effects`]: https://hackage.haskell.org/package/fused-effects
[`effectful`]: https://github.com/arybczak/effectful
[`eff`]: https://github.com/hasura/eff
[`freer-simple`]: https://hackage.haskell.org/package/freer-simple
[alexis-talk]: https://www.youtube.com/watch?v=0jI-AlWEwYI
