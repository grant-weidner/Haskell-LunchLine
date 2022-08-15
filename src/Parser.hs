{-# LANGUAGE FlexibleContexts #-}

module Parser where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)

data Command = CommandC String Int
             | CommandP  
             | CommandT 
             | CommandU Int  
             | CommandS 
             | CommandR 
             | CommandD 

parseC :: Parsec.Parsec String () Command
parseC = do
    command <- Parsec.char 'c'
    Parsec.spaces
    letters <- Parsec.many1 Parsec.letter
    Parsec.spaces
    digits <- read <$> Parsec.many1 Parsec.digit
    return $ CommandC letters digits

parseSingleCharComand :: Char -> Parsec.Parsec String () Command
parseSingleCharComand c = do
    command <- Parsec.char c
    Parsec.spaces
    case command of
        'p' -> return CommandP
        't' -> return CommandT
        's' -> return CommandS
        'd' -> return CommandD
        'r' -> return CommandR

parseU :: Parsec.Parsec String () Command
parseU = do
    command <- Parsec.char 'u'
    Parsec.spaces
    digits <- read <$> Parsec.many1 Parsec.digit
    return (command, Just digits)
    return $ CommandU digits
    
parse rule text = Parsec.parse rule "(source)" text

parser = (  parseC 
        <|> parseSingleCharComand 'p'
        <|> parseSingleCharComand 't' 
        <|> parseU 
        <|> parseSingleCharComand 's'
        <|> parseSingleCharComand 'r' 
        <|> parseSingleCharComand 'd'
         )

error_example = do
    let result = parse (parseC <|> parseSingleCharComand 'p') "p 21313 " 
    case result of
        Right _ -> putStrLn "success!"
        Left _ -> putStrLn "whoops, error"