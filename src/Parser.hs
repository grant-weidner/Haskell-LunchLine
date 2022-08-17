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

parseSingleCharComand :: Char -> Command -> Parsec.Parsec String () Command
parseSingleCharComand c m = do
    command <- Parsec.char c
    Parsec.spaces
    pure m

parseU :: Parsec.Parsec String () Command
parseU = do
    command <- Parsec.char 'u'
    Parsec.spaces
    digits <- read <$> Parsec.many1 Parsec.digit
    return $ CommandU digits
    
parse rule text = Parsec.parse rule "(source)" text

parser = (  parseC 
        <|> parseSingleCharComand 'p' CommandP
        <|> parseSingleCharComand 't' CommandT
        <|> parseU
        <|> parseSingleCharComand 's' CommandS
        <|> parseSingleCharComand 'r' CommandR
        <|> parseSingleCharComand 'd' CommandD
         )

error_example = do
    let result = parse (parseC <|> parseSingleCharComand 'p' CommandP) "p 21313 " 
    case result of
        Right _ -> putStrLn "success!"
        Left _ -> putStrLn "whoops, error"