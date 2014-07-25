-- | Module:      Acme.LOLCAT
--  Copyright:   (c) 2013 Anton Nikishaev
--  License:     BSD
--  Maintainer:  me@lelf.lu
--
--
-- LOLCAT translator.
--
-- Roughly based on <https://metacpan.org/pod/Acme::LOLCAT Acme::LOLCAT>

{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections #-}
{-# LANGUAGE Trustworthy #-}

module Acme.LOLCAT (translate, KindaText()) where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.Text()

import Control.Applicative ((<$>),(<*),(<*>))
import Control.Arrow
import Control.Monad
import Data.String (IsString(..))
import Data.Monoid
import Data.Char

import System.Random
import qualified System.Random.Shuffle as SHU

import Acme.LOLCAT.IO


{-# NOINLINE shuffle #-}
shuffle xs = do io <- OH HAI I CAN HAZ IO? THXBYE
                SHU.shuffle' xs (length xs) (io newStdGen)

-- | Given a list, return a infinity list of its elements in random order
variants xs = shuffle xs ++ variants xs
variants' xs = cycle xs



replaceOne :: Parser String -> Text -> Text -> Either Text Text
replaceOne pat to s | Right s' <- parsed = Right s'
                    | otherwise          = Left s
    where repl (pref,r) = pref <> to <> T.drop (T.length pref + length r) s
          parsed = repl <$> myrun (find pat) s

myrun p s = runP p Punct "" s



replace :: Parser String -> [Text] -> Text -> Text
replace pat tos str = repl (cycle tos) $ Right str
    where repl (t:ts) (Right s) = repl ts $ replaceOne pat t s
          repl _ (Left s) = s


translateT src = last $ scanl f src rules
    where f s (pat,repls) = replace pat (variants repls) s


class KindaText a where
    fromText :: Text -> a
    toText :: a -> Text

instance KindaText Text where
    fromText = id
    toText = id

instance KindaText String where
    fromText = T.unpack
    toText = T.pack

translate :: KindaText s => s -> s
translate = fromText . translateT . toText



type Parser = Parsec Text ParserState
data ParserState = Word | Punct deriving Eq


type Pattern = Parser String

instance IsString (Parser String) where
    fromString = string

find :: Parser String -> Parser (Text,String)
find pat = try (("",) <$> pat)
           <|> do c <- anyChar
                  putState $ if isLetter c then Word else Punct
                  first (T.cons c) <$> find pat


wend = notFollowedBy letter

word s = do Punct <- getState
            string s <* wend

inside s = do Word <- getState
              string s <* letter


wordPuncts w = (++) <$> word w <*> many1 space

wEnds s = string s <* wend



rules :: [(Parser String, [Text])]

rules = [
 ("what",                       ["wut", "whut"]),
 (wEnds "you",		        ["yu", "yous", "yoo", "u"]),
 ("cture",			["kshur"]),
 ("unless",			["unles"]),
 (wEnds "the",	         	["teh"]),
 ("more",			["moar"]),
 ("my",				["muh", "mah"]),
 ("are",			["r", "is", "ar"]),
 (word "a",			[""]),
 ("eese",			["eez"]),
 ("catamorphi",			["cat-a-murrphi"]),
 ("morphi",			["murrphi"]),
 ("ph",				["f"]),
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
 (word "want",			["wants"]),
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
 (wEnds "sure",			["shur"]),
 (wEnds "tty",			["tteh"]),
 ("were",			["was"]),
 (wEnds "ok",			["k", "kay"]),
 ("ym",				["im"]),
 (wEnds "thy",			["fee"]),
 (inside "ly",			["li"]),
 ("que" <* letter,		["kwe"]),
 ("oth",			["udd"]), -- ?
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

 -- ("(?!e)ight",		["ite"]),
 ("tion",			["shun"]),             -- ("(?!ues)tion",["shun"])
 ("you're" <|> "youre" <|> "you are",
				["yore", "yr"]),

-- ("\\boh\\b(?!.*hai)",	["o", "ohs"]),
 (word "oh",			["o", "ohs"]), -- ???

-- ("can" <+spaces, [""]),

 (join<$>sequence [wordPuncts "can",
                   wordPuncts "i",
                   wordPuncts "have",
                   word "a"],	["i can has"]),

 ("have",			["has", "hav", "haz a"]),

 (word "hello" <|> word "hi"
    <|> word "hey" <|> word "howdy" <|> word "yo",
				["oh hai"]),

 (word "god",			["ceiling cat"])

 ]

