-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Top
-- Copyright   :  (c) 2010, 2011, 2012, 2013, 2014, 2018 Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
--  Process activity and memory consumption monitors
--
-----------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

module Xmobar.Plugins.Monitors.Top (startTop, topMemConfig, runTopMem) where

import Xmobar.Plugins.Monitors.Common

import Control.Exception (SomeException, handle)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode), hGetLine, withFile)
import System.Posix.Unistd (SysVar(ClockTick), getSysVar)

import Foreign.C.Types

maxEntries :: Int
maxEntries = 10

intStrs :: [String]
intStrs = map show [1..maxEntries]

topMemConfig :: IO MConfig
topMemConfig = mkMConfig "<both1>"
                 [ k ++ n | n <- intStrs , k <- ["name", "mem", "both"]]

topConfig :: IO MConfig
topConfig = mkMConfig "<both1>"
              ("no" : [ k ++ n | n <- intStrs
                               , k <- [ "name", "cpu", "both"
                                      , "mname", "mem", "mboth"]])

foreign import ccall "unistd.h getpagesize"
  c_getpagesize :: CInt

pageSize :: Float
pageSize = fromIntegral c_getpagesize / 1024

processes :: IO [FilePath]
processes = fmap (filter isPid) (getDirectoryContents "/proc")
  where isPid = (`elem` ['0'..'9']) . head

statWords :: [String] -> [String]
statWords line@(x:pn:ppn:xs) =
  if last pn == ')' then line else statWords (x:(pn ++ " " ++ ppn):xs)
statWords _ = replicate 52 "0"

getProcessData :: FilePath -> IO [String]
getProcessData pidf =
  handle ign $ withFile ("/proc" </> pidf </> "stat") ReadMode readWords
  where readWords = fmap (statWords . words) . hGetLine
        ign = const (return []) :: SomeException -> IO [String]

memPages :: [String] -> String
memPages fs = fs!!23

ppid :: [String] -> String
ppid fs = fs!!3

skip :: [String] -> Bool
skip fs = length fs < 24 || memPages fs == "0" || ppid fs == "0"

handleProcesses :: ([String] -> a) -> IO [a]
handleProcesses f =
  fmap (foldl' (\a p -> if skip p then a else f p : a) [])
       (processes >>= mapM getProcessData)

showInfo :: String -> String -> Float -> Monitor [String]
showInfo nm sms mms = do
  mnw <- getConfigValue maxWidth
  mxw <- getConfigValue minWidth
  let lsms = length sms
      nmw = mnw - lsms - 1
      nmx = mxw - lsms - 1
      rnm = if nmw > 0 then padString nmw nmx " " True "" nm else nm
  mstr <- showWithColors' sms mms
  both <- showWithColors' (rnm ++ " " ++ sms) mms
  return [nm, mstr, both]

processName :: [String] -> String
processName = drop 1 . init . (!!1)

sortTop :: [(String, Float)] -> [(String, Float)]
sortTop =  sortBy (flip (comparing snd))

type MemInfo = (String, Float)

meminfo :: [String] -> MemInfo
meminfo fs = (processName fs, pageSize * parseFloat (fs!!23))

meminfos :: IO [MemInfo]
meminfos = handleProcesses meminfo

showMemInfo :: Float -> MemInfo -> Monitor [String]
showMemInfo scale (nm, rss) =
  showInfo nm (showWithUnits 3 1 rss) (100 * rss / sc)
  where sc = if scale > 0 then scale else 100

showMemInfos :: [MemInfo] -> Monitor [[String]]
showMemInfos ms = mapM (showMemInfo tm) ms
  where tm = sum (map snd ms)

runTopMem :: [String] -> Monitor String
runTopMem _ = do
  mis <- io meminfos
  pstr <- showMemInfos (sortTop mis)
  parseTemplate $ concat pstr

type Pid = Int
type TimeInfo = (String, Float)
type TimeEntry = (Pid, TimeInfo)
type Times = [TimeEntry]
type TimesRef = IORef (Times, UTCTime)

timeMemEntry :: [String] -> (TimeEntry, MemInfo)
timeMemEntry fs = ((p, (n, t)), (n, r))
  where p = parseInt (head fs)
        n = processName fs
        t = parseFloat (fs!!13) + parseFloat (fs!!14)
        (_, r) = meminfo fs

timeMemEntries :: IO [(TimeEntry, MemInfo)]
timeMemEntries = handleProcesses timeMemEntry

timeMemInfos :: IO (Times, [MemInfo], Int)
timeMemInfos = fmap res timeMemEntries
  where res x = (sortBy (comparing fst) $ map fst x, map snd x, length x)

combine :: Times -> Times -> Times
combine _ [] = []
combine [] ts = ts
combine l@((p0, (n0, t0)):ls) r@((p1, (n1, t1)):rs)
  | p0 == p1 && n0 == n1 = (p0, (n0, t1 - t0)) : combine ls rs
  | p0 <= p1 = combine ls r
  | otherwise = (p1, (n1, t1)) : combine l rs

take' :: Int -> [a] -> [a]
take' m l = let !r = tk m l in length l `seq` r
  where tk 0 _ = []
        tk _ [] = []
        tk n (x:xs) = let !r = tk (n - 1) xs in x : r

topProcesses :: TimesRef -> Float -> IO (Int, [TimeInfo], [MemInfo])
topProcesses tref scale = do
  (t0, c0) <- readIORef tref
  (t1, mis, len) <- timeMemInfos
  c1 <- getCurrentTime
  let scx = realToFrac (diffUTCTime c1 c0) * scale
      !scx' = if scx > 0 then scx else scale
      nts = map (\(_, (nm, t)) -> (nm, min 100 (t / scx'))) (combine t0 t1)
      !t1' = take' (length t1) t1
      !nts' = take' maxEntries (sortTop nts)
      !mis' = take' maxEntries (sortTop mis)
  writeIORef tref (t1', c1)
  return (len, nts', mis')

showTimeInfo :: TimeInfo -> Monitor [String]
showTimeInfo (n, t) =
  getConfigValue decDigits >>= \d -> showInfo n (showDigits d t) t

showTimeInfos :: [TimeInfo] -> Monitor [[String]]
showTimeInfos = mapM showTimeInfo

runTop :: TimesRef -> Float -> [String] -> Monitor String
runTop tref scale _ = do
  (no, ps, ms) <- io $ topProcesses tref scale
  pstr <- showTimeInfos ps
  mstr <- showMemInfos ms
  parseTemplate $ show no : concat (zipWith (++) pstr mstr) ++ repeat "N/A"

startTop :: [String] -> Int -> (String -> IO ()) -> IO ()
startTop a r cb = do
  cr <- getSysVar ClockTick
  c <- getCurrentTime
  tref <- newIORef ([], c)
  let scale = fromIntegral cr / 100
  runM a topConfig (runTop tref scale) r cb
