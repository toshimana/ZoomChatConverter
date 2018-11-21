{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.ParserCombinators.Parsec
import System.Environment
import Data.Char
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX
import Data.Maybe

data Chat = Line String | Timestamp UTCTime String deriving Show

convertUTCTime :: String -> Maybe UTCTime
convertUTCTime = parseTimeM True defaultTimeLocale "%H:%M:%S"

convertFromNominalDiffTime :: NominalDiffTime -> String
convertFromNominalDiffTime = formatTime defaultTimeLocale "%H:%M:%S" . posixSecondsToUTCTime

chat :: Parser [Chat]
chat = endBy line eol

line :: Parser Chat
line = with_header <|> txt

with_header :: Parser Chat
with_header = do
    ts <- timestamp
    (Line l) <- txt
    return (Timestamp ts l)

timestamp :: Parser UTCTime
timestamp = do
    a <- many (satisfy isDigit)
    char ':'
    b <- many (satisfy isDigit)
    char ':'
    c <- many (satisfy isDigit)
    let utc = convertUTCTime $ concat [a, ":", b, ":", c]
    return (fromJust utc)

txt :: Parser Chat
txt = do
    a <- many (noneOf "\r\n")
    return (Line a)


eol = try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"

convertDiffTime :: UTCTime -> Chat -> String
convertDiffTime _ (Line s) = s
convertDiffTime base (Timestamp time s) =
    let dt = diffUTCTime time base in
    (convertFromNominalDiffTime dt) ++ s

main :: IO ()
main = do
    args <-getArgs
    print args
    cs <- readFile $ args !! 0
    let time = fromJust $ convertUTCTime (args !! 2)
    case (parse chat "" cs) of
        Left err -> print err
        Right result -> do
            let difftimes = map (convertDiffTime time) result
            writeFile (args !! 1) (unlines difftimes)


