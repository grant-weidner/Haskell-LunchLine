{-# LANGUAGE FlexibleContexts #-}

module Parser where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)

parseC :: Parsec.Parsec String () (Char, Maybe String, Maybe Int)
parseC = do
    command <- Parsec.char 'c'
    Parsec.spaces
    letters <- Parsec.many1 Parsec.letter
    Parsec.spaces
    digits <- read <$> Parsec.many1 Parsec.digit
    return (command, Just letters, Just digits)

parseP = parseSingleCharComand 'p'
parseT = parseSingleCharComand 't'
parseS = parseSingleCharComand 's'
parseD = parseSingleCharComand 'd'
parseR = parseSingleCharComand 'r'

parseSingleCharComand :: Char -> Parsec.Parsec String () (Char, Maybe String, Maybe Int)
parseSingleCharComand c = do
    command <- Parsec.char c
    Parsec.spaces
    return (command, Nothing, Nothing)

parseU :: Parsec.Parsec String () (Char, Maybe String, Maybe Int)
parseU = do
    command <- Parsec.char 'u'
    Parsec.spaces
    digits <- read <$> Parsec.many1 Parsec.digit
    return (command, Just digits)
    return (command, Nothing, Just digits)
    
parse rule text = Parsec.parse rule "(source)" text

error_example = do
    let result = parse (parseC <|> parseP) "p 21313 " 
    case result of
        Right _ -> putStrLn "success!"
        Left _ -> putStrLn "whoops, error"