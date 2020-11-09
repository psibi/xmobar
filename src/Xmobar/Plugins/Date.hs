{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Date
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A date plugin for Xmobar
--
-- Usage example: in template put
--
-- > Run Date "%a %b %_d %Y <fc=#ee9a00> %H:%M:%S</fc>" "Mydate" 10
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Date (Date(..), date, dateWithTimeZone) where

import Xmobar.Run.Exec

#if ! MIN_VERSION_time(1,5,0)
import System.Locale
#endif
import Data.Time

data Date = Date String String Int
          | DateWithTimeZone String String Int
    deriving (Read, Show)

instance Exec Date where
    alias (Date _ a _) = a
    alias (DateWithTimeZone _ a _) = a
    run   (Date f _ _) = date f
    run   (DateWithTimeZone f _ _) = do
                     t <- getCurrentTime
                     zone <- getTimeZone t
                     dateWithTimeZone zone f
    rate  (Date _ _ r) = r
    rate  (DateWithTimeZone _ _ r) = r
    start (Date f _ r) cb = doEveryTenthSeconds r $ date f >>= cb
    start (DateWithTimeZone f _ r) cb = do
      t <- getCurrentTime
      zone <- getTimeZone t
      go zone
     where
      go zone = doEveryTenthSeconds r $ dateWithTimeZone zone f >>= cb

date :: String -> IO String
date format = fmap (formatTime defaultTimeLocale format) getZonedTime

dateWithTimeZone :: TimeZone -> String -> IO String
dateWithTimeZone timezone format = do
  time <- getCurrentTime
  let zonedTime = utcToZonedTime timezone time
  pure $ formatTime defaultTimeLocale format zonedTime
