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

parseP :: Parsec.Parsec String () (Char, Maybe String, Maybe Int)
parseP = do
    command <- Parsec.char 'p'
    Parsec.spaces
    return (command, Nothing, Nothing)

parseT :: Parsec.Parsec String () (Char, Maybe String, Maybe Int)
parseT = do
    command <- Parsec.char 't'
    Parsec.spaces
    return (command, Nothing, Nothing)
    
parse rule text = Parsec.parse rule "(source)" text

error_example = do
    let result = parse (parseC <|> parseP) "p 21313 " 
    case result of
        Right _ -> putStrLn "success!"
        Left _ -> putStrLn "whoops, error"