# cleff

> **Warning: WIP.**

`cleff` is an extensible effects library for Haskell. It provides a set of predefined effects that you can conveniently reuse in your program, as well as mechanisms for defining and interpreting new domain-specific effects on your own.

- `cleff` supports **higher-order** effects, like `bracket`, `local` and `mask`, and provides a set of easy-to-use combinators to interpret them.
- `cleff` is **reasonably fast**. It is essentially built on top of a `ReaderT IO`, which admits many compiler optimizations.
- `cleff` requires **little boilerplate** to interpret an effect; the interface is similar to that of [`effectful`] and [`polysemy`], but sometimes even simpler.
- `cleff` is **principled** and **easy to understand**. Its core implementation is less than 200 lines of code and hardly uses any hacks or low-level primitives.
- `cleff` takes inspiration from [`effectful`], [`freer-simple`] and [`polysemy`]. If you have used any of them, it would be very easy to get along with `cleff`.

## Rationale

[Just as `effectful` asked](https://github.com/arybczak/effectful#motivation), we have [a][`polysemy`] [bunch][`fused-effects`] [of][`effectful`] [effect][`eff`] [libraries][`freer-simple`] out there, why another?

To put it simply: we need both performance and good ergonomics for effect interpretation. For the best in each, respectively:

- [`eff`] is *very fast* because it is based on low-level GHC primitives, but the development has stalled because of (alas) [unresolved difficulties](https://www.reddit.com/r/haskell/comments/pywuqg/unresolved_challenges_of_scoped_effects_and_what/), and the primitives are not yet merged into GHC's master branch.
- [`polysemy`] has best-of-class support of higher-order effect interpretation. However, the library is *slow*. This may not matter that much in applications that are IO bound, but still a problem elsewhere.

If we give up continuations (thus, nondeterminism), and use a concrete monad (particularly, a `ReaderT IO`), there will be resonable performance. After all, [most effects libraries has broken nondeterminism support](https://github.com/polysemy-research/polysemy/issues/246), and you could always wrap another monad transformer with support of nondeterminism over the main `Eff` monad.

[`effectful`] already does that. However, it focused mostly on good performance instead of easy interpretation.

`effectful` has a notion of *static* and *dynamic* effects, where dynamic effects are the normal extensible effects, while static ones directly call primitive operations and cannot be interpreted in different ways, as a sacrifice of effect interoperability to ensure performance. `effectful`'s underlying `ReaderT IO` is parameterized by a *mutable* variable that stores various effect handling information, which makes it not only very painful to think about threaded operations, but also largely restricts the effect interpretation interface, and operations like `listen` is not achievable without dealing with the library internals.

`cleff` uses a simpler `ReaderT Env IO a`, where the `Env` is an immutable map that stores all effect handlers, i.e. implementation of effect operations. For example, `Reader`'s effect handler is basically equivalent to two function definitions, `ask` and `local`. The simplicity of this implementation admits very flexible effect interpretation mechanism (sometimes even more flexible than `polysemy`), while still preserves resonable performance. In microbenchmarks, `cleff` performed scaringly similar to `effectful`, only being ~2x slower than it in the most synthetic `countdown` benchmark, and that's even when `effectful` uses a non-reinterpretable static effect, i.e. effectively primitive operations.

In conclusion, `cleff` is an effect library that tries to find a good balance between implementation simplicity, performance, ease of use, and expressivity.

## Example

> Attribution: These examples are shamelessly adapted from the [`polysemy`] README.

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

see [`cleff/example`](https://github.com/re-xyr/cleff/tree/master/cleff/example/) for more examples.

## Microbenchmarks

These are the results of the [effect-zoo](https://github.com/ocharles/effect-zoo) microbenchmark, compiled by GHC 8.10.7. Keep in mind that these are *very short and synthetic programs*, and may or may not tell the correct performance characteristics of different effect libraries in real use:

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

- [Effects for Less](https://www.youtube.com/watch?v=0jI-AlWEwYI) by Alexis King.
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
