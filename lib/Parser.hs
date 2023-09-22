{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Parser (parse, raw) where

import           CabalASTTypes.Types       (CabalField, CabalStanza,
                                            CabalTopLevel (TLComment, TLEOF, TLField, TLRaw, TLStanza, TLWhitespace),
                                            Comment (MkComment),
                                            CommentLine (MkCommentLine),
                                            LineEnder (LineEndOfFile, POSIXLineEnd, WinLineEnd),
                                            Raw (MkRaw), Whitespace,
                                            WhitespaceEnd, fromStrictTuple,
                                            toStrictTuple, type (!!!!) ((:!!)))
import           Control.Monad.Combinators (manyTill_)
import           Data.Bifunctor            (first)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.Char                 (isSpace)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vec
import           Data.Void                 (Void)
import           FlatParse.Basic           (Result (OK), failed, many,
                                            runParser, (<|>))
import           FlatParse.Basic.Base      (eof)
import qualified FlatParse.Basic.Base      as FPB
import           FlatParse.Basic.Parser    (Parser)
import           FlatParse.Basic.Text      (anyChar, satisfy)



-- ** DEBUG IMPORTS
-- IF THIS IS NOT EMPTY, THE MAINTAINER SCREWED UP.

-- ** END DEBUG IMPORTS





-- | In theory, we only need to export this. The parser for outside access.

parse :: ByteString -> Vector CabalTopLevel
parse = (\case
            OK a _ -> a
            _   -> Vec.empty) . runParser cabalParser





-- | Let's use a minimal parser type, since we already have "Raw" as a way to dump errors.

type CabalParser = Parser Void






-- | Convenience to get around representation issues for ByteString

stringToByteStringUTF8 :: String -> ByteString
stringToByteStringUTF8 = T.encodeUtf8 . T.pack





{- | The actual parser that does things.

It manyTill_s the parse list until it reaches EOF,
and then packages the EOF and parseList together,
which is unfortunately unperformant due to list append
being strict in the left argument.

-}

cabalParser :: CabalParser (Vector CabalTopLevel)
cabalParser
    =   Vec.fromList . (\(a,b) -> a <> [b])
    <$>     parseList
        `manyTill_`
            parseTLEOF
  where
    parseList = tlWhitespace
        <|> tlComment
        <|> tlRaw

    parseTLEOF = TLEOF <$ eof





{- | Runs whiteSpaceEnd to get rid of whitespace,
then adds a TLWhitespace constructor to render it top-level. -}

tlWhitespace :: CabalParser CabalTopLevel
tlWhitespace
    = TLWhitespace <$> whiteSpaceEnd





{- | Produces a strict tuple of all the parsed whitespace,
then a representation of the specific line end, including
potential end of file. -}

whiteSpaceEnd :: CabalParser WhitespaceEnd
whiteSpaceEnd = first stringToByteStringUTF8 . toStrictTuple
    <$> satisfy isSpace `manyTill_` lineEnd





{- | Detects a line end, whether it be Windows format,
POSIX format, or an end of file, and returns
the specific kind of line end. -}

lineEnd :: CabalParser LineEnder
lineEnd =
    ( anyChar >>= \case
            '\n' -> pure POSIXLineEnd
            '\r' -> anyChar >>= \case
                    '\n'    -> pure WinLineEnd
                    _       -> failed
            _ -> failed
        ) <|> (LineEndOfFile <$ eof)





{- | Parses comments on the top level, by adding
a TLComment constructor to the yield of parseCommentLine. -}

tlComment :: CabalParser CabalTopLevel
tlComment = TLComment <$> parseCommentLine





{- | Generates a comment line by parsing out the whitespace,
storing it, then runs a parser for the comment. -}

parseCommentLine :: CabalParser CommentLine
parseCommentLine
    = MkCommentLine
        <$> manyWhitespace
        <*> parseComment





{- | Helper function that extracts whitespace without appending
a line end to it. -}

manyWhitespace :: CabalParser Whitespace
manyWhitespace =
    stringToByteStringUTF8 <$> many (satisfy isSpace)





{- | The actual parser that parses the comment, probably
more complex than intended. -}

parseComment :: CabalParser Comment
parseComment =
    flip (:). pure <$> anyChar <*> anyChar >>= \case
        line | line /= "--" -> failed
        u -> MkComment . first (stringToByteStringUTF8 . (u <>))
            <$> takeLineW





{- | Top-level raw parser, just calls raw, then slaps
a top-level constructor onto it. -}

tlRaw :: CabalParser CabalTopLevel
tlRaw = TLRaw <$> raw





{- | Parses any data until the end of the line.
Used as a catch-all for parsing errors and so on. -}

raw :: CabalParser Raw
raw = MkRaw . first stringToByteStringUTF8 <$> takeLineW





{- | Takes a full line, then staples the sort of end of line
onto it as part of a strict tuple. -}

takeLineW :: CabalParser (String !!!! LineEnder)
takeLineW = toStrictTuple
    <$> anyChar `manyTill_` lineEnd
