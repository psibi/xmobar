{-# LANGUAGE RecordWildCards #-}

import Control.Monad.Reader
import Data.IORef (newIORef)
import Data.Time
import Gauge
import Xmobar
import Xmobar.Plugins.Monitors.Common.Run
import Xmobar.Plugins.Monitors.Common.Types
import Xmobar.Plugins.Monitors.Cpu

data BenchmarkInput = BenchmarkInput
  { cpuInput :: CpuArguments,
    dateInput :: DateInput
  }

data DateInput = DateInput
  { diFormat :: String,
    diZone :: TimeZone
  }

main :: IO ()
main = do
  cpuParams <- mkCpuArgs
  time <- getCurrentTime
  zone <- getTimeZone time
  let benchInput =
        BenchmarkInput
          { cpuInput = cpuParams,
            dateInput =
              DateInput
                { diFormat = "D: %B %d %A W%V",
                  diZone = zone
                }
          }
  defaultMain $ normalBench benchInput
  where
    normalBench args =
      [ bgroup "Cpu Benchmarks" $ normalCpuBench (cpuInput args),
        bgroup "Date Benchmarks" $ dateBench (dateInput args)
      ]

runMonitor :: MConfig -> Monitor a -> IO a
runMonitor config r = runReaderT r config

mkCpuArgs :: IO CpuArguments
mkCpuArgs = getArguments ["-L", "3", "-H", "50", "--normal", "green", "--high", "red", "-t", "Cpu: <total>%"]

-- | The action which will be benchmarked
cpuAction :: CpuArguments -> IO String
cpuAction = runCpu

cpuBenchmark :: CpuArguments -> Benchmarkable
cpuBenchmark cpuParams = nfIO $ cpuAction cpuParams

normalCpuBench :: CpuArguments -> [Benchmark]
normalCpuBench args = [bench "CPU normal args" (cpuBenchmark args)]

dateBenchmark :: DateInput -> Benchmarkable
dateBenchmark params = nfIO $ date (diFormat params)

dateWithTimezoneBenchmark :: DateInput -> Benchmarkable
dateWithTimezoneBenchmark params = nfIO $ dateWithTimeZone (diZone params) (diFormat params)

dateBench :: DateInput -> [Benchmark]
dateBench args =
  [ bench "Date" (dateBenchmark args),
    bench "DateWithTimeZone" (dateWithTimezoneBenchmark args)
  ]
