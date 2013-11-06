{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE {-LOL-} Trustworthy #-}
module Acme.LOLCAT where

import System.Random
import System.IO.Unsafe
import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.Text hiding (I)
import qualified Data.Attoparsec as A
--import Control.Applicative
import Acme.LOLCAT.IO
import Data.Array
import Control.Monad
import Data.Monoid
import Control.Applicative
import Data.List (unfoldr)
import Data.Maybe



{-# NOINLINE pick #-}
pick :: [a] -> a
pick m = (m!!) . unsafePerformIO $ randomRIO (0, length m - 1)


p1 = string "can" <* string " a"


--subAll 


sub s to | Right r <- parseOnly p1 s = Just $ T.replace r to s
         | otherwise = Nothing





class ToBS a where toBS :: a -> BS.ByteString
instance ToBS String where toBS = BS.pack


tr :: IsString s => s -> s
tr = id

wbound = endOfInput <|> (space >> return ())

word w = tail <|> (space >> tail)
    where tail = string w <* wbound

language = [
  ("what", ["wut", "whut"]),
  ("you" <* wbound, ["yu", "yous", "yoo", "u"]),
  ("cture",  ["kshur"]),
  ("have",   ["has", "hav", "haz a"]),
  ("unless", ["unles"]),
  ("the" <* wbound, ["teh"]),
  ("more", ["moar"]),
  ("my", ["muh", "mah"]),
  ("are", ["r", "is", "ar"]),
   ("eese", ["eez"]),
   ("ph", ["f"]),
   ("as" <* wbound, ["az"]),
  ("seriously", ["srsly"]),
   ("er" <* wbound, ["r"]),
  ("sion", ["shun"]),
   ("just", ["jus"]),
  ("ose" <* wbound, ["oze"]),
   ("eady", ["eddy"]),
  ("ome?" <* wbound, ["um"]),
  ("of" <* wbound, ["of", "ov", "of"]),
 ("uestion", ["wesjun"]),
   ("want", ["wants"]),
 ("ead" <* wbound, ["edd"]),
  ("ucke", ["ukki", "ukke"]),
  ("sion", ["shun"]),
   ("eak", ["ekk"]),
  ("age", ["uj"]),
  ("like", ["likes", "liek"]),
  ("love", ["loves", "lub", "lubs", "luv"]),
  (word "is", ["ar teh","ar"]),
 ("nd" <* wbound, ["n"]),
  ("who", ["hoo"]),
 ("'", [""]),
 ("ese" <* wbound, ["eez"]),
 ("outh", ["owf"]),
 ("scio", ["shu"]),
 ("esque", ["esk"]),
   ("ture", ["chur"]),
  ("\\btoo?\\b", ["to", "t", "2", "to", "t"]),
   ("tious", ["shus"]),
 ("sure\\b", ["shur"]),
   ("tty\\b", ["tteh"]),
 ("were", ["was"]),
  ("ok" <* wbound, ["k", "kay"]),
  (word "a", [""]),
 ("ym", ["im"]),
 ("thy" <* wbound, ["fee"]),
 (letter *> string "ly" <* letter, ["li"]), -- \wly\w, wrong
 ("que" <* letter, ["kwe"]),                -- que\w
 ("oth", ["udd"]),
 ("ease", ["eez"]),
  ("ing" <* wbound, ["in", "ins", "ng", "ing"]),
   ("your", ["yur", "ur", "yore", "yoar"]),
  ("ove" <* wbound, ["oov", "ove", "uuv", "uv", "oove"]),
  ("for", ["for", "4", "fr", "fur", "for", "foar"]),
   ("thank", ["fank", "tank", "thx", "thnx"]),
   ("good", ["gud", "goed", "guud", "gude", "gewd"]),
   ("really", ["rly", "rily", "rilly", "rilley"]),
   ("world", ["wurrld", "whirld", "wurld", "wrld"]),
   (("i'm" <|> "im") <* wbound, ["im"]),

   -- ("(?!e)ight", ["ite"]),
   ("tion", ["shun"]),          --    ("(?!ues)tion", ["shun"])
   ("you're" <|> "youre" <|> "you are", ["yore", "yr"]), -- ("you'?re", ["yore", "yr"])

   -- ("\\boh\\b(?!.*hai)", ["o", "ohs"]),
   (word "oh", ["o", "ohs"]),   -- ???

   -- ("can\\si\\s(?:ple(?:a|e)(?:s|z)e?)?\\s?have\\sa" , ["i can has"]),

   -- ("(?:hello | \\bhi\\b | \\bhey\\b | howdy | \\byo\\b) ,?"    , ["oh hai,"]),
   (word "hello" <|> word "hi" <|> word "hey" <|> word "howdy" <|> word "yo",
    ["oh hai,"]),

   -- ("(?:god|allah|buddah?|diety)", ["ceiling cat"])
   (word "god", ["ceiling cat"]),

  (string "", [])
 ]

