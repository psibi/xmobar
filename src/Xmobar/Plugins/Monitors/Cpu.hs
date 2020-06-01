-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Cpu
-- Copyright   :  (c) 2011, 2017 Jose Antonio Ortega Ruiz
--                (c) 2007-2010 Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A cpu monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Cpu (startCpu) where

import Xmobar.Plugins.Monitors.Common
import qualified Data.ByteString.Char8 as B
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Console.GetOpt
import Control.Monad.Reader
import System.IO
import Xmobar.Plugins.Monitors.Common.Parsers

newtype CpuOpts = CpuOpts
  { loadIconPattern :: Maybe IconPattern
  }

defaultOpts :: CpuOpts
defaultOpts = CpuOpts
  { loadIconPattern = Nothing
  }

options :: [OptDescr (CpuOpts -> CpuOpts)]
options =
  [ Option "" ["load-icon-pattern"] (ReqArg (\x o ->
     o { loadIconPattern = Just $ parseIconPattern x }) "") ""
  ]

cpuConfig :: IO MConfig
cpuConfig = mkMConfig "Cpu: <total>%" cpuConfigParameters

cpuConfigParameters :: [String]
cpuConfigParameters = ["bar","vbar","ipat","total","user","nice","system","idle","iowait"]

totalCpuParameters :: Int
totalCpuParameters = length cpuConfigParameters

type CpuDataRef = IORef [Int]

cpuData :: IO [Int]
cpuData = cpuParser `fmap` B.readFile "/proc/stat"

cpuParser :: B.ByteString -> [Int]
cpuParser = map (read . B.unpack) . tail . B.words . head . B.lines

parseCpu :: CpuDataRef -> IO [Float]
parseCpu cref =
    do a <- readIORef cref
       b <- cpuData
       writeIORef cref b
       let dif = zipWith (-) b a
           tot = fromIntegral $ sum dif
           percent = map ((/ tot) . fromIntegral) dif
       return percent

formatCpu :: CpuOpts -> [Float] -> Monitor [String]
formatCpu _ [] = return $ replicate 8 ""
formatCpu opts xs = do
  let t = sum $ take 3 xs
  -- b <- showPercentBar (100 * t) t
  -- v <- showVerticalBar (100 * t) t
  -- d <- showIconPattern (loadIconPattern opts) t
  ps <- showPercentsWithColors [t]
  return $ replicate 8 (head ps)

runCpu :: CpuDataRef -> [String] -> Monitor String
runCpu cref argv =
    do cpuValue <- io (parseCpu cref)
       t <- getConfigValue template
       opts <- io $ parseOptsWith options defaultOpts argv
       -- io $ hPutStrLn stderr "debug"
       -- io $ hPutStrLn stderr (show argv)
       l <- formatCpu opts cpuValue
       -- io $ hPutStrLn stderr (show cpuValue)
       -- io $ hPutStrLn stderr (show l)
       parseTemplate l

getTemplate :: Monitor [String]
getTemplate = do
  t <- getConfigValue template
  v <- io $ runP templateParser t
  pure $ map mid v

mid :: (a,b,c) -> b
mid (_,b,_) = b

test :: [String] -> IO [String]
test arguments = do
  config <- cpuConfig
  runReaderT getTemplate config

helperTemplate :: [String] -> [String]
helperTemplate arguments = case getOpt Permute [] arguments of
                             (_,n,_) -> n

startCpu :: [String] -> Int -> (String -> IO ()) -> IO ()
startCpu arguments refreshRate cb = do
  cref <- newIORef []
  -- goo <- test arguments
  -- hPutStrLn stderr ("startCPU")  
  -- hPutStrLn stderr (show arguments)
  -- hPutStrLn stderr (show goo)
  -- hPutStrLn stderr (show $ helperTemplate arguments)
  runM arguments cpuConfig (runCpu cref) refreshRate cb
