{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Lua.LexerUtils where

import           Control.DeepSeq (NFData(..))
import           Data.Char (ord)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word8)

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (Applicative(..))
#endif

import           Language.Lua.Token

-- | Lua token with position information.
data Lexeme a = Lexeme
  { ltokToken :: LToken
  , ltokPos   :: a
  , ltokText  :: Text
  } deriving (Show,Eq)

instance Functor Lexeme where
  fmap f l = l { ltokPos = f (ltokPos l) }

ltokEOF :: Lexeme SourcePos
ltokEOF = Lexeme
  { ltokText  = Text.empty
  , ltokToken = LTokEof
  , ltokPos   = SourcePos "" (-1) (-1) (-1)
  }

lexerEOF :: Mode -> [Lexeme SourcePos]
lexerEOF mode =
  case mode of
    QuoteMode (AlexInput start rest) _ True ->
      [ Lexeme{ltokToken=LTokUntermComment, ltokPos=start, ltokText=rest}
      , ltokEOF ]
    QuoteMode (AlexInput start rest) _ False ->
      [ Lexeme{ltokToken=LTokUntermString, ltokPos=start, ltokText=rest}
      ,ltokEOF ]
    _ -> [ltokEOF]


-- | Type of alex actions
type Action =
  Int                   {- ^ lexeme length                -} ->
  AlexInput             {- ^ current input                -} ->
  Mode                  {- ^ lexer mode                   -} ->
  (Mode, Maybe (Lexeme SourcePos)) {- ^ updated mode, emitted lexeme -}

-- | Start lexing a long-quoted string literal
enterString :: Action
enterString len inp _ = (QuoteMode inp len False, Nothing)

-- | Start lexing a long-quoted comment
enterLongComment :: Action
enterLongComment len inp _ = (QuoteMode inp (len - 2) True, Nothing)

-- | Start lexing a single-line comment
enterComment :: Action
enterComment _ inp _ = (CommentMode inp, Nothing)

-- | Construct a lexeme spanning multiple matches
longToken ::
  AlexInput {- ^ starting position         -} ->
  SourcePos {- ^ position of ending lexeme -} ->
  Int       {- ^ length of ending lexeme   -} ->
  LToken    {- ^ token for lexeme          -} ->
  Lexeme SourcePos
longToken (AlexInput start text) posn len t = Lexeme
  { ltokToken = t
  , ltokPos   = start
  , ltokText  = str
  }
  where
  commentLength = sourcePosIndex posn - sourcePosIndex start + len
  str = Text.take commentLength text

-- | The closing delimiter for long-quoted lexemes must be the same length as
-- the opening delimiter. This predicate checks if the currently match
-- delimiter is the right length.
endStringPredicate ::
  Mode      {- ^ lexer mode                    -} ->
  AlexInput {- ^ input stream before the token -} ->
  Int       {- ^ length of the token           -} ->
  AlexInput {- ^ input stream after the token  -} ->
  Bool      {- ^ is expected ending long-quote -}
endStringPredicate mode _ len _ =
  case mode of
    QuoteMode _ startlen _ -> len == startlen
    _                      -> False

-- | Action called at the end of a lexer-sub mode.
endMode :: Action
endMode len (AlexInput posn _) mode = (NormalMode, Just lexeme)
  where
  lexeme =
    case mode of
      CommentMode inp             -> longToken inp posn len LTokComment
      QuoteMode   inp _ isComment -> longToken inp posn len
                                   $ if isComment then LTokComment
                                                  else LTokSLit
      NormalMode -> error "endMode: internal lexer error"

-- | Simplest action emitting a lexeme for the current match
tok :: LToken -> Action
tok lexeme len (AlexInput posn s) mode = (mode, Just t)
  where
  t = Lexeme
        { ltokToken = lexeme
        , ltokPos   = posn
        , ltokText  = Text.take len s
        }

-- | Drop the first line of a Lua file when it starts with a '#'
dropSpecialComment :: Text -> Text
dropSpecialComment text
  | "#" `Text.isPrefixOf` text = Text.dropWhile (/='\n') text
  | otherwise = text
-- Newline is preserved in order to ensure that line numbers stay correct

-- | This function drops whitespace and comments from a list of lexemes
-- in order to make it suitable for parsing.
dropWhiteSpace :: [Lexeme a] -> [Lexeme a]
dropWhiteSpace = filter (not . isWhite . ltokToken)
  where
  isWhite LTokWhiteSpace = True
  isWhite LTokComment    = True
  isWhite _              = False


-- | The type of locations in a source file
data SourcePos = SourcePos
  { sourcePosName :: String
  , sourcePosIndex, sourcePosLine, sourcePosColumn :: {-# UNPACK #-}!Int
  }
  deriving (Show,Eq)

instance NFData SourcePos where
  rnf (SourcePos _ _ _ _) = ()

startPos :: String -> SourcePos
startPos n = SourcePos n 0 1 1

-- This is unused but alex needs the definition to exist
alexInputPrevChar :: a -> ()
alexInputPrevChar _ = ()

-- | Attempt to retrieve the next representative element for the character
-- at the head of the input string. Returns an advanced 'AlexInput'
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AlexInput p text) =
  do (c,text') <- Text.uncons text
     let p' = move p c
         x = fromIntegral (min 127 (ord c))
     x `seq` p' `seq` return (x, AlexInput p' text')

-- | Update a 'SourcePos' for a particular matched character
move :: SourcePos -> Char -> SourcePos
move (SourcePos name index line column) c =
  case c of
    '\t' -> SourcePos name (index+1) line (((column + 7) `div` 8) * 8 + 1)
    '\n' -> SourcePos name (index+1) (line + 1) 1
    _    -> SourcePos name (index+1) line (column + 1)


-- | The remaining input text annotated with the starting position
data AlexInput = AlexInput
  { input_pos :: !SourcePos
  , input_text :: !Text
  }

-- | Lexer mode
data Mode
  = NormalMode
  | CommentMode AlexInput -- input at beginning of comment
  | QuoteMode AlexInput -- input at beginning of long-quote
              Int       -- delim length
              Bool      -- is comment
                -- ^ start delimlen iscomment
