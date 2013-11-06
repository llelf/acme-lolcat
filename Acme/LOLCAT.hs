{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE {-LOL-} Trustworthy #-}
module Acme.LOLCAT where

import System.Random
import System.IO.Unsafe
import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import qualified Data.ByteString.Char8 as BS
-- import Text.Regex.Posix.Wrap
import Data.Attoparsec.Text hiding (I)
import qualified Data.Attoparsec as A
--import Control.Applicative
import Data.Text.ICU.Regex
-- import Text.Regex.PCRE
import Acme.LOLCAT.MAH.STUFF
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

sub s to | Right r <- parseOnly p1 s = Just $ T.replace r to s
         | otherwise = Nothing

--subAll 

--subAll s


reSubstAll :: Text -> Text -> Text -> Text
reSubstAll re bs = last . unfoldr (\x -> join (,) <$> reSubst re bs x)

reSubst :: Text -> Text -> Text -> Maybe Text
reSubst re b str = OH HAI CAN I HAZ IO? THXBYE
       $ do rx <- regex [] re
            setText rx str
            r <- find rx 0
            case r of
              True ->
                   do [s,e] <- map fromIntegral <$> sequence [start_ rx 0, end_ rx 0]
                      let s1 = T.take s str
                      let s3 = T.drop e str
                      return $ Just $ s1 <> b <> s3
              False ->
                  return Nothing


translate = scanl1



class ToBS a where toBS :: a -> BS.ByteString
instance ToBS String where toBS = BS.pack


tr :: IsString s => s -> s
tr = id

wbound = endOfInput <|> (space >> return ())

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
--   ("\\bis\\b", ["ar teh","ar"]),
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
  -- ('\\ba\\b'   => q()),
 ("ym", ["im"]),
 ("thy" <* wbound, ["fee"]),
 -- ("\\wly\\w", ["li"]),
 -- ("que\\w", ["kwe"]),
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
   -- q(i'?m\\b)     => 'im',

   -- ("(?!e)ight", ["ite"]),
   -- ("(?!ues)tion", ["shun"]),
   -- ("you'?re", ["yore", "yr"]),
   -- ("\\boh\\b(?!.*hai)", ["o", "ohs"]),
   -- ("can\\si\\s(?:ple(?:a|e)(?:s|z)e?)?\\s?have\\sa" , ["i can has"]),
   -- ("(?:hello|\\bhi\\b|\\bhey\\b|howdy|\\byo\\b),?"    , ["oh hai,"]),
   -- ("(?:god|allah|buddah?|diety)", ["ceiling cat"])


  (string "", [])
 ]

