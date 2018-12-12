module Parsers where

import           Data.Text       (Text)
import           Data.Void       (Void)
import           Text.Megaparsec (Parsec)
import           Text.Megaparsec as MP

type Parser = Parsec Void Text

type ParseError = MP.ParseError (MP.Token Text) Void
