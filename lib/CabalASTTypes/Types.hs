{-# LANGUAGE ExplicitNamespaces #-}
module CabalASTTypes.Types
    (   
        type (!!!!) ((:!!))
    ,   toStrictTuple
    ,   fromStrictTuple
    ,   Whitespace
    ,   WhitespaceEnd
    ,   LineEnder (POSIXLineEnd, WinLineEnd, LineEndOfFile)
    ,   CommentLine (MkCommentLine)
    ,   Comment (MkComment)
    ,   Raw (MkRaw)
    ,   CabalTopLevel
        (
            TLRaw
        ,   TLComment
        ,   TLWhitespace
        ,   TLStanza
        ,   TLField
        ,   TLEOF
        )
    ,   CabalStanza
    ,   CabalField
    ) where

import Data.ByteString (ByteString)
import Data.Bifunctor (Bifunctor, first, second, bimap)

-- | Convenience strict tuple.

data a !!!! b = !a :!! !b
    deriving (Eq, Show)

toStrictTuple :: ( a , b ) -> a !!!! b
toStrictTuple (!a,!b) = a :!! b

fromStrictTuple :: a !!!! b -> ( a , b )
fromStrictTuple (a :!! b) = (a,b) 

instance Bifunctor (!!!!) where
    bimap :: (a -> b) -> (c -> d) -> a !!!! c -> b !!!! d
    bimap f g (a :!! b) = f a :!! g b

    first :: (a -> b) -> a !!!! c -> b !!!! c
    first f (a :!! b) = f a :!! b

    second :: (b -> c) -> a !!!! b -> a !!!! c
    second g (a :!! b) = a :!! g b

-- | Type synonym for ByteString, no need to parse.

type Whitespace = ByteString

-- | Necessary type synonym if we want to exact parse, since Cabal
-- won't care about the end char, but we do.

type WhitespaceEnd = ByteString !!!! LineEnder

-- | Presents a representation of LineEnders

data LineEnder
    = POSIXLineEnd
    | WinLineEnd
    | LineEndOfFile
    deriving (Eq, Show)

-- | Data structure for comments that comprise a line, since
-- comment lines will be used by both top-levels and stanzas.

data CommentLine = MkCommentLine !Whitespace !Comment
    deriving (Eq, Show)

-- | Data structure for comments overall, since comments can
-- comprise a line

newtype Comment = MkComment (ByteString !!!! LineEnder)
    deriving (Eq, Show)

-- | Represents a length of data the parser could not parse.
-- Ideally, for all valid cabal files, this should not exist.

newtype Raw = MkRaw (ByteString !!!! LineEnder)
    deriving (Eq, Show)

-- | Top-level representation of a cabal file.

data CabalTopLevel
    = TLRaw !Raw
    | TLComment !CommentLine
    | TLWhitespace !WhitespaceEnd
    | TLStanza !CabalStanza
    | TLField !CabalField
    | TLEOF
    deriving (Eq, Show)

data CabalStanza
    deriving (Eq, Show)

data CabalField
    deriving (Eq, Show)
