{-#LANGUAGE RecordWildCards#-}
{-#LANGUAGE ScopedTypeVariables#-}
------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Plugins.Monitors.Parsers
-- Copyright: (c) 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Dec 02, 2018 04:49
--
--
-- Parsing template strings
--
------------------------------------------------------------------------------


module Xmobar.Plugins.Monitors.Common.Parsers ( runP
                                              , skipRestOfLine
                                              , getNumbers
                                              , getNumbersAsString
                                              , getAllBut
                                              , getAfterString
                                              , skipTillString
                                              , parseTemplate
                                              , parseTemplate'
                                              , parseOptsWith
                                              , templateParser
                                              , helperTemp
                                              , runExportTemplate
                                              , minParseTemplate
                                              ) where

import Xmobar.Plugins.Monitors.Common.Types

import Control.Applicative ((<$>))
import qualified Data.Map as Map
import System.Console.GetOpt (ArgOrder(Permute), OptDescr, getOpt)
import Text.ParserCombinators.Parsec
import System.IO

runP :: Parser [a] -> String -> IO [a]
runP p i =
    case parse p "" i of
      Left _ -> return []
      Right x  -> return x

getAllBut :: String -> Parser String
getAllBut s =
    manyTill (noneOf s) (char $ head s)

getNumbers :: Parser Float
getNumbers = skipMany space >> many1 digit >>= \n -> return $ read n

getNumbersAsString :: Parser String
getNumbersAsString = skipMany space >> many1 digit >>= \n -> return n

skipRestOfLine :: Parser Char
skipRestOfLine =
    do many $ noneOf "\n\r"
       newline

getAfterString :: String -> Parser String
getAfterString s =
    do { try $ manyTill skipRestOfLine $ string s
       ; manyTill anyChar newline
       } <|> return ""

skipTillString :: String -> Parser String
skipTillString s =
    manyTill skipRestOfLine $ string s

-- | Parses the output template string
templateStringParser :: Parser (String,String,String)
templateStringParser =
    do { s <- nonPlaceHolder
       ; com <- templateCommandParser
       ; ss <- nonPlaceHolder
       ; return (s, com, ss)
       }
    where
      nonPlaceHolder = fmap concat . many $
                       many1 (noneOf "<") <|> colorSpec <|> iconSpec

-- | Recognizes color specification and returns it unchanged
colorSpec :: Parser String
colorSpec = try (string "</fc>") <|> try (
            do string "<fc="
               s <- many1 (alphaNum <|> char ',' <|> char '#')
               char '>'
               return $ "<fc=" ++ s ++ ">")

-- | Recognizes icon specification and returns it unchanged
iconSpec :: Parser String
iconSpec = try (do string "<icon="
                   i <- manyTill (noneOf ">") (try (string "/>"))
                   return $ "<icon=" ++ i ++ "/>")

-- | Parses the command part of the template string
templateCommandParser :: Parser String
templateCommandParser =
    do { char '<'
       ; com <- many $ noneOf ">"
       ; char '>'
       ; return com
       }

-- | Combines the template parsers
templateParser :: Parser [(String,String,String)]
templateParser = many templateStringParser --"%")

trimTo :: Int -> String -> String -> (Int, String)
trimTo n p "" = (n, p)
trimTo n p ('<':cs) = trimTo n p' s
  where p' = p ++ "<" ++ takeWhile (/= '>') cs ++ ">"
        s = drop 1 (dropWhile (/= '>') cs)
trimTo 0 p s = trimTo 0 p (dropWhile (/= '<') s)
trimTo n p s = let p' = takeWhile (/= '<') s
                   s' = dropWhile (/= '<') s
               in
                 if length p' <= n
                 then trimTo (n - length p') (p ++ p') s'
                 else trimTo 0 (p ++ take n p') s'

-- | Takes a list of strings that represent the values of the exported
-- keys. The strings are joined with the exported keys to form a map
-- to be combined with 'combine' to the parsed template. Returns the
-- final output of the monitor, trimmed to MaxTotalWidth if that
-- configuration value is positive.
parseTemplate :: [String] -> Monitor String
parseTemplate l =
    do t <- getConfigValue template
       e <- getConfigValue export
       w <- getConfigValue maxTotalWidth
       ell <- getConfigValue maxTotalWidthEllipsis
       let m = Map.fromList . zip e $ l
       s <- parseTemplate' t m
       let (n, s') = if w > 0 && length s > w
                     then trimTo (w - length ell) "" s
                     else (1, s)
       -- io $ hPutStrLn stderr (show e)
       -- io $ hPutStrLn stderr (show t)
       -- io $ hPutStrLn stderr "parseTemplate"
       return $ if n > 0 then s' else s' ++ ell

minParseTemplate :: PureConfig -> [String] -> [(String, String, String)] -> [(String, [(String, String,String)])] -> IO String
minParseTemplate PureConfig{..} l s1 exp =
    do let t = pTemplate
           e = pExport
           w = pMaxTotalWidth
           ell = pMaxTotalWidthEllipsis
       let m = let expSnds :: [([(String, String, String)], String)]  = zip (map snd exp) l
               in Map.fromList . zip (map fst exp) $ expSnds
       s <- minCombine m s1
       let (n, s') = if w > 0 && length s > w
                     then trimTo (w - length ell) "" s
                     else (1, s)
       -- io $ hPutStrLn stderr (show e)
       -- io $ hPutStrLn stderr (show t)
       -- io $ hPutStrLn stderr "parseTemplate"
       return $ if n > 0 then s' else s' ++ ell

helperTemp :: PureConfig -> IO [(String, String, String)]
helperTemp PureConfig{..} = runP templateParser pTemplate

runExportTemplate :: [String] -> IO [(String, [(String, String,String)])]
runExportTemplate [] = pure []
runExportTemplate (x:xs) = do
  s <- runP templateParser x
  rem <- runExportTemplate xs
  pure $ (x,s):rem

-- minParseTemplate' :: String -> Map.Map String String -> IO String
-- minParseTemplate' t m =
--     do s <- runP templateParser t
--        -- io $ hPutStrLn stderr (show s)
--        -- io $ hPutStrLn stderr ("parseToo")
--        minCombine m s

-- | Parses the template given to it with a map of export values and combines
-- them
parseTemplate' :: String -> Map.Map String String -> Monitor String
parseTemplate' t m =
    do s <- io $ runP templateParser t
       -- io $ hPutStrLn stderr (show s)
       -- io $ hPutStrLn stderr ("parseToo")
       combine m s

-- | Given a finite "Map" and a parsed template t produces the
-- | resulting output string as the output of the monitor.
combine :: Map.Map String String -> [(String, String, String)] -> Monitor String
combine _ [] = return []
combine m ((s,ts,ss):xs) =
    do next <- combine m xs
       str <- case Map.lookup ts m of
         Nothing -> return $ "<" ++ ts ++ ">"
         Just  r -> let f "" = r; f n = n; in f <$> parseTemplate' r m
       return $ s ++ str ++ ss ++ next

minCombine :: Map.Map String ([(String, String, String)], String) -> [(String, String, String)] -> IO String
minCombine _ [] = return []
minCombine m ((s,ts,ss):xs) =
    do next <- minCombine m xs
       str <- case Map.lookup ts m of
         Nothing -> return $ "<" ++ ts ++ ">"
         Just (s,r) -> let f "" = r; f n = n; in f <$> minCombine m s
       pure $ s ++ str ++ ss ++ next

-- | Try to parse arguments from the config file and apply them to Options.
parseOptsWith
    :: [OptDescr (opts -> opts)]  -- ^ Options that are specifiable
    -> opts                       -- ^ Default options to use as a fallback
    -> [String]                   -- ^ Actual arguments given
    -> IO opts
parseOptsWith options defaultOpts argv =
    case getOpt Permute options argv of
        (o, _, []  ) -> pure $ foldr id defaultOpts o
        (_, _, errs) -> ioError . userError $ concat errs
