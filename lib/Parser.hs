{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Parser (parse) where

import           CabalASTTypes.Types    (CabalField, CabalStanza,
                                         CabalTopLevel (TLComment, TLEOF, TLField, TLRaw, TLStanza, TLWhitespace),
                                         CommentLine (MkCommentLine),
                                         Comment (MkComment),
                                         LineEnder (LineEndOfFile, LinuxLineEnd, WinLineEnd),
                                         Raw (MkRaw), Whitespace, WhitespaceEnd,
                                         type (!!!!) ((:!!)))
import           FlatParse.Basic.Base qualified as FPB
import           Data.Bifunctor         (first)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.Vector            (Vector)
import qualified Data.Vector            as Vec
import           Data.Void              (Void)
import           FlatParse.Basic        (Result (OK), many, runParser,
                                        (<|>), failed)
import           FlatParse.Basic.Base   (eof)
import           FlatParse.Basic.Parser (Parser)
import           FlatParse.Basic.Text   (anyChar, satisfy)
import Data.Char (isSpace)

-- | In theory, we only need to export this. The parser for outside access.

parse :: ByteString -> Vector CabalTopLevel
parse = (\case
            OK a _ -> a
            _   -> Vec.empty) . runParser cabalParser

-- | Let's use a minimal parser type, since we already have "Raw" as a way to dump errors.

type CabalParser = Parser Void

cabalParser :: CabalParser (Vector CabalTopLevel)
cabalParser = Vec.fromList
    <$> many (     tlComment
               <|> tlWhitespace
               <|> tlRaw
               <|> (TLEOF <$ eof)
             )

tlComment :: CabalParser CabalTopLevel
tlComment = TLComment <$> parseCommentLine

parseCommentLine :: CabalParser CommentLine
parseCommentLine = do
    ws <- manyWhitespace
    comment <- parseComment
    pure $! MkCommentLine ws comment

parseComment :: CabalParser Comment
parseComment = do
    bs <- FPB.take 2
    if bs /= "--" then failed
        else MkComment . first read <$> takeLineW

manyWhitespace :: CabalParser Whitespace
manyWhitespace =
    read <$> many (satisfy isSpace)

tlWhitespace :: CabalParser CabalTopLevel
tlWhitespace =
    TLWhitespace <$> whiteSpaceEnd

whiteSpaceEnd :: CabalParser WhitespaceEnd
whiteSpaceEnd = first read <$> process
  where
    process =
        lineEnd <|> do
            anyChar >>= \u -> satisfy isSpace >>
                first (u :) <$> process

tlRaw :: CabalParser CabalTopLevel
tlRaw = TLRaw <$> raw

takeLineW :: CabalParser (String !!!! LineEnder)
takeLineW =
    lineEnd <|> do
        r <- anyChar
        first (r :) <$> takeLineW

lineEnd :: CabalParser (String !!!! LineEnder)
lineEnd =
    anyChar >>= \case
        '\n' -> pure $! [] :!! LinuxLineEnd
        '\r' -> anyChar >>= \case
                    '\n'    -> pure $! [] :!! WinLineEnd
                    _       -> failed
        _ -> failed

raw :: CabalParser Raw
raw = MkRaw . first read <$> takeLineW