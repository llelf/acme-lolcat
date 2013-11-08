{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections #-}
{-# LANGUAGE {-LOL-} Trustworthy #-}
module Acme.LOLCAT where

import System.Random
import System.IO.Unsafe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.Text hiding (I)
import qualified Data.Attoparsec as A
import Data.String
import Data.Monoid
import Control.Applicative
import Control.Arrow

import Acme.LOLCAT.IO


{-# NOINLINE pick #-}
pick :: [a] -> a
pick m = (m!!) . unsafePerformIO $ randomRIO (0, length m - 1)



variants x = cycle x


replaceOne :: Parser Text -> Text -> Text -> Either Text Text
replaceOne pat to s | Right s' <- parsed = Right s'
                    | otherwise          = Left s
    where repl (pref,r) = pref <> to <> T.drop (T.length $ pref <> r) s
          parsed = repl <$> parseOnly (find pat) s


replace :: Parser Text -> [Text] -> Text -> Text
replace pat tos str = repl tos $ Right str
    where repl (t:ts) (Right s) = repl ts $ replaceOne pat t s
          repl _ (Left s) = s


translate src = last $ scanl f src rules
    where f s (pat,repls) = replace pat (cycle repls) s




find :: Parser Text -> Parser (Text,Text)
find pat = ("",) <$> pat
           <|> do c <- anyChar
                  first (T.cons c) <$> find pat



class ToBS a where toBS :: a -> BS.ByteString
instance ToBS String where toBS = BS.pack


tr :: IsString s => s -> s
tr = id

wbound = endOfInput <|> (space >> return ())

word w = tail <|> (space >> tail)
    where tail = string w <* wbound

rules = [
  ("what",                      ["wut", "whut"]),
  ("you" <* wbound, 		["yu", "yous", "yoo", "u"]),
  ("cture",			["kshur"]),
  ("have",			["has", "hav", "haz a"]),
  ("unless",			["unles"]),
  ("the" <* wbound,		["teh"]),
  ("more",			["moar"]),
  ("my",			["muh", "mah"]),
  ("are",			["r", "is", "ar"]),
  ("eese",			["eez"]),
  ("ph",			["f"]),
  ("as" <* wbound,		["az"]),
  ("seriously",			["srsly"]),
  ("er" <* wbound,		["r"]),
  ("sion",			["shun"]),
  ("just",			["jus"]),
  ("ose" <* wbound,		["oze"]),
  ("eady",			["eddy"]),
  ("ome?" <* wbound,		["um"]),
  ("of" <* wbound,		["of", "ov", "of"]),
  ("uestion",			["wesjun"]),
  ("want",			["wants"]),
  ("ead" <* wbound,		["edd"]),
  ("ucke",			["ukki", "ukke"]),
  ("sion",			["shun"]),
  ("eak",			["ekk"]),
  ("age",			["uj"]),
  ("like",			["likes", "liek"]),
  ("love",			["loves", "lub", "lubs", "luv"]),
  (word "is",			["ar teh","ar"]),
  ("nd" <* wbound,		["n"]),
  ("who",			["hoo"]),
  ("'",				[""]),
  ("ese" <* wbound,		["eez"]),
  ("outh",			["owf"]),
  ("scio",			["shu"]),
  ("esque",			["esk"]),
  ("ture",			["chur"]),
  ("\\btoo?\\b",		["to", "t", "2", "to", "t"]),
  ("tious",			["shus"]),
  ("sure\\b",			["shur"]),
  ("tty\\b",			["tteh"]),
  ("were",			["was"]),
  ("ok" <* wbound,		["k", "kay"]),
  (word "a",			[""]),
  ("ym",			["im"]),
  ("thy" <* wbound,		["fee"]),
  (letter *> string "ly" <* letter,	["li"]), -- \wly\w, wrong
  ("que" <* letter,		["kwe"]),                -- que\w
  ("oth",			["udd"]),                          -- only start?
  ("ease",			["eez"]),
  ("ing" <* wbound,		["in", "ins", "ng", "ing"]),
  ("your",			["yur", "ur", "yore", "yoar"]),
  ("ove" <* wbound,		["oov", "ove", "uuv", "uv", "oove"]),
  ("for",			["for", "4", "fr", "fur", "for", "foar"]),
  ("thank",			["fank", "tank", "thx", "thnx"]),
  ("good",			["gud", "goed", "guud", "gude", "gewd"]),
  ("really",			["rly", "rily", "rilly", "rilley"]),
  ("world",			["wurrld", "whirld", "wurld", "wrld"]),
  (("i'm" <|> "im") <* wbound,			["im"]),

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

