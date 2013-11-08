{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections #-}
{-# LANGUAGE {-LOL-} Trustworthy #-}
module Acme.LOLCAT where

import System.Random
import System.IO.Unsafe
import Data.Text (Text)
import qualified Data.Text as T
-- import Data.Attoparsec.Text hiding (I)
-- import qualified Data.Attoparsec as A

import Text.Parsec
--import Text.Parsec.Text
import Text.Parsec.String

import Data.String
import Data.Monoid
import Control.Applicative ((<$>),(<*))
import Control.Arrow
import Data.Char
import Control.Monad


import Acme.LOLCAT.IO


{-# NOINLINE pick #-}
pick :: [a] -> a
pick m = (m!!) . unsafePerformIO $ randomRIO (0, length m - 1)



variants x = cycle x


replaceOne :: Parser' String -> String -> String -> Either String String
replaceOne pat to s | Right s' <- parsed = Right s'
                    | otherwise          = Left s
    where repl (pref,r) = pref ++ to ++ drop (length $ pref ++ r) s
          parsed = repl <$> myrun (find pat) s

myrun p s = runP p ' ' "" s



replace :: Parser' String -> [String] -> String -> String
replace pat tos str = repl tos $ Right str
    where repl (t:ts) (Right s) = repl ts $ replaceOne pat t s
          repl _ (Left s) = s


translateT src = last $ scanl f src rules
    where f s (pat,repls) = replace pat (cycle repls) s

-- translate :: FromToText s => s -> s
-- translate = fromText . translateT . toText


class FromToText a where
    fromText :: Text -> a
    toText :: a -> Text

instance FromToText Text where
    fromText = id
    toText = id

instance FromToText String where
    fromText = T.unpack
    toText = T.pack


type Parser' = Parsec String Char

find :: Parser' String -> Parser' (String,String)
find pat = try (("",) <$> pat)
           <|> do c <- anyChar
                  putState c
                  first (c:) <$> find pat


wend = notFollowedBy letter

word s = do c <- getState
            guard . not . isAlpha $ c
            string s <* wend


wEnds s = string s <* wend


instance IsString (Parser' String) where
    fromString = string


rules :: [(Parser' String, [String])]

rules = [
  ("what",                      ["wut", "whut"]),
  (wEnds "you",	 		["yu", "yous", "yoo", "u"]),
  ("cture",			["kshur"]),
  ("have",			["has", "hav", "haz a"]),
  ("unless",			["unles"]),
  (wEnds "the",			["teh"]),
  ("more",			["moar"]),
  ("my",			["muh", "mah"]),
  ("are",			["r", "is", "ar"]),
  ("eese",			["eez"]),
  ("ph",			["f"]),
  (wEnds "as",			["az"]),
  ("seriously",			["srsly"]),
  (wEnds "er",			["r"]),
  ("sion",			["shun"]),
  ("just",			["jus"]),
  (wEnds "ose",			["oze"]),
  ("eady",			["eddy"]),
  (wEnds "om" <|> wEnds "ome",	["um"]),
  (wEnds "of",			["of", "ov", "of"]),
  ("uestion",			["wesjun"]),
  ("want",			["wants"]),
  (wEnds "ead",			["edd"]),
  ("ucke",			["ukki", "ukke"]),
  ("sion",			["shun"]),
  ("eak",			["ekk"]),
  ("age",			["uj"]),
  ("like",			["likes", "liek"]),
  ("love",			["loves", "lub", "lubs", "luv"]),
  (word "is",			["ar teh","ar"]),
  (wEnds "nd",			["n"]),
  ("who",			["hoo"]),
  ("'",				[""]),
  (wEnds "ese",			["eez"]),
  ("outh",			["owf"]),
  ("scio",			["shu"]),
  ("esque",			["esk"]),
  ("ture",			["chur"]),
  (word "too" <|> word "to",	["to", "t", "2", "to", "t"]),
  ("tious",			["shus"]),
  (wEnds "sure",		["shur"]),
  (wEnds "tty",			["tteh"]),
  ("were",			["was"]),
  (wEnds "ok",			["k", "kay"]),
  (word "a",			[""]),
  ("ym",			["im"]),
  (wEnds "thy",			["fee"]),
  (letter >> string "ly" <* letter,	["li"]), -- \wly\w, wrong
  ("que" <* letter,		["kwe"]),                -- que\w
  ("oth",			["udd"]),                          -- only start?
  ("ease",			["eez"]),
  (wEnds "ing",			["in", "ins", "ng", "ing"]),
  ("your",			["yur", "ur", "yore", "yoar"]),
  (wEnds "ove",			["oov", "ove", "uuv", "uv", "oove"]),
  ("for",			["for", "4", "fr", "fur", "for", "foar"]),
  ("thank",			["fank", "tank", "thx", "thnx"]),
  ("good",			["gud", "goed", "guud", "gude", "gewd"]),
  ("really",			["rly", "rily", "rilly", "rilley"]),
  ("world",			["wurrld", "whirld", "wurld", "wrld"]),
  (word "i'm" <|> word "im",	["im"]),

   -- ("(?!e)ight",			["ite"]),
   ("tion",			["shun"]),          --    ("(?!ues)tion",["shun"])
   ("you're" <|> "youre" <|> "you are",	["yore", "yr"]), -- ("you'?re",["yore", "yr"])

   -- ("\\boh\\b(?!.*hai)",			["o", "ohs"]),
   (word "oh",			["o", "ohs"]),   -- ???

   -- ("can\\si\\s(?:ple(?:a|e)(?:s|z)e?)?\\s?have\\sa" ,			["i can has"]),

   -- ("(?:hello | \\bhi\\b | \\bhey\\b | howdy | \\byo\\b) ,?"    ,			["oh hai,"]),
   (word "hello" <|> word "hi"
    <|> word "hey" <|> word "howdy" <|> word "yo",	["oh hai"]),

   -- ("(?:god|allah|buddah?|diety)",			["ceiling cat"])
   (word "god",			["ceiling cat"])

 ]

