module Dubstep where

-- import Text.Parsec.Char
import Data.Either (fromRight)
import Text.Parsec
import Data.List.Split ( splitOn )

{-
songDecoder "WUBWEWUBAREWUBWUBTHEWUBCHAMPIONSWUBMYWUBFRIENDWUB" -> "WE ARE THE CHAMPIONS MY FRIEND"
-}

songDecoder :: String -> String
songDecoder str =
  unwords $
  fromRight [] $ 
  parse (many (skipMany (try $ string "WUB") *> manyTill anyChar (many1 $ try $ string "WUB"))) "" (str ++ "WUB")

songDecoder' :: String -> String
songDecoder' = unwords . filter (not . null) . splitOn "WUB"