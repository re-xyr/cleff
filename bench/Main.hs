import           BenchCountdown
import           BenchPyth
import           Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bgroup "countdown"
    [ bench "sp/shallow" $ nf countdownSp 10000000
    , bench "sp/deep" $ nf countdownSpDeep 10000000
    , bench "ev/shallow" $ nf countdownEv 10000000
    , bench "ev/deep" $ nf countdownEvDeep 10000000
    ]
  , bgroup "pyth"
    [ bench "sp" $ nf pythSp 200
    , bench "ev" $ nf pythEv 200
    ]
  ]
