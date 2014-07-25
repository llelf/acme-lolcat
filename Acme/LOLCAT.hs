-- | Module:      Acme.LOLCAT
--  Copyright:   (c) 2013 Antonio Nikishaev
--  License:     BSD
--  Maintainer:  me@lelf.lu
--
--
-- LOLCAT translator.
--
-- Roughly based on Perl's <https://metacpan.org/pod/Acme::LOLCAT Acme::LOLCAT>

{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections #-}
{-# LANGUAGE Trustworthy #-}

module Acme.LOLCAT (translate, KindaText()) where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec hiding (string)
import Text.Parsec.Text ()
import qualified Text.Parsec as Parsec (string)

import Control.Applicative ((<$>),(<*),(<*>))
import Control.Arrow
import Control.Monad
import Data.String (IsString(..))
import Data.Monoid
import Data.Char

import System.Random
import qualified System.Random.Shuffle as SHU (shuffle')

import Acme.LOLCAT.IO


{-# NOINLINE shuffle #-}
shuffle xs = do io <- OH HAI I CAN HAZ IO? THXBYE
                SHU.shuffle' xs (length xs) (io newStdGen)


-- | Given a list, return a infinity list of its elements in random order
variants xs = shuffle xs ++ variants xs
variants' xs = cycle xs


-- | Replace the part that matched by pattern PAT with TO in S.
-- Return Right NEW-STRING or Left ORIGINAL if nothing matches.
replaceOne :: Pattern -> Text -> Text -> Either Text Text
replaceOne pat to s | Right s' <- parsed = Right s'
                    | otherwise          = Left s
    where repl (pref,r) = pref <> to <> T.drop (T.length pref + T.length r) s
          parsed = repl <$> myRun (find pat) s


-- | Run parser
myRun :: Parser a -> Text -> Either ParseError a
myRun p s = runP p Punct "" s


-- | Replace PAT with TOS (cycling) in STR
replace :: Pattern -> [Text] -> Text -> Text
replace pat tos str = repl (cycle tos) $ Right str
    where repl (t:ts) (Right s) = repl ts $ replaceOne pat t s
          repl _ (Left s)       = s



translateT :: Text -> Text
translateT src = T.toUpper $ last $ scanl f src rules
    where f s (pat,repls) = replace pat (variants repls) s



-- | Text/String-like things
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


type Pattern = Parser Text

instance IsString Pattern where
    fromString = string . T.pack


-- | Given a parser for X returns a new parser for (HEAD,X)
-- where HEAD is matched string's part before X.
-- 
-- Weird? This is Acme.*.
find :: Pattern -> Parser (Text,Text)
find pat = try (("",) <$> pat)
           <|> do c <- anyChar
                  putState $ if isLetter c then Word else Punct
                  first (T.cons c) <$> find pat


-- | word S exactly
word s = do Punct <- getState
            string s <* wend

-- | S is inside word
inside s = do Word <- getState
              string s <* letter

-- | word S and then any punctuations
wordPuncts w = (<>) <$> word w <*> many1' space

-- | word ending with S
wEnds s = string s <* wend


wend = notFollowedBy letter
many1' = fmap T.pack . many1

string :: Text -> Parser Text
string = fmap T.pack . Parsec.string . T.unpack




rules :: [(Pattern, [Text])]

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

 -- XXX Text
 -- (join<$>sequence [wordPuncts "can",
 --                   wordPuncts "i",
 --                   wordPuncts "have",
 --                   word "a"],
 --                                ["i can has"]),

 ("have",			["has", "hav", "haz a"]),

 (word "hello" <|> word "hi"
    <|> word "hey" <|> word "howdy" <|> word "yo",
				["oh hai"]),

 (word "god",			["ceiling cat"])

 ]

