module ObjLoader ( loadOBJ ) where

import Control.Monad
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Data.Char
import Data.Either
import Data.Maybe

type Config = Map.Map String [String]

ident :: Parser String
ident = do
  c  <- letter <|> char '_'
  cs <- many (letter <|> digit <|> char '_')
  return (c:cs)
  <?> "identifier"

comment :: Parser ()
comment = do
  char '#'
  skipMany (noneOf "\r\n")
  <?> "comment"

eol :: Parser ()
eol = do
  oneOf "\n\r"
  return ()
  <?> "eol"

item :: Parser (String, [String])
item = do
  key <- ident
  skipMany space
  value <- manyTill anyChar (try eol <|> try comment <|> eof)
  return (key, [value])

line :: Parser (Maybe (String, [String]))
line = do
  skipMany space
  try (comment >> return Nothing) <|> liftM Just item

file :: Parser [(String, [String])]
file = do
  lines <- many line
  return (catMaybes lines)

-- | Data.Map k, [v] 
-- | (Right result) <- loadOBJ "obj/blender.obj"
-- | let Just v  = Data.Map.lookup "v"  result
-- |     Just vn = Data.Map.lookup "vn" result
-- |     Just f  = Data.Map.lookup "f"  result
loadOBJ :: SourceName -> IO (Either ParseError Config)
loadOBJ name = do
  result <- parseFromFile file name
  return $ case result of
    Left err -> Left err
    Right xs -> Right (listToMap xs)

listToMap :: [(String, [String])] -> Config
listToMap ((k,v):xs) = Map.insertWith (++) k v (listToMap xs)
listToMap []         = Map.empty
