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
data LLexeme = LLexeme
  { ltokToken :: LToken
  , ltokPos   :: SourcePos
  , ltokText  :: Text
  } deriving (Show,Eq)

ltokEOF :: LLexeme
ltokEOF = LLexeme { ltokText   = ""
               , ltokToken = LTokEof
               , ltokPos    = SourcePos "" (-1) (-1) (-1)
               }

isEOF :: LLexeme -> Bool
isEOF x = ltokToken x == LTokEof


-- The matched token and the starting position
type Action = Int -> AlexInput -> Mode -> (Mode, Maybe LLexeme)

enterString :: Action
enterString len (AlexInput posn t) _ = (QuoteMode posn t len False, Nothing)

enterLongComment :: Action
enterLongComment len (AlexInput posn t) _ = (QuoteMode posn t (len - 2) True, Nothing)

enterComment :: Action
enterComment _ (AlexInput p t) _ = (CommentMode p t, Nothing)

endComment :: Action
endComment len (AlexInput posn _) mode =
  case mode of
    CommentMode start text -> (NormalMode, Just t)
      where
      commentLength = sourcePosIndex posn - sourcePosIndex start + len
      str = Text.take commentLength text
      t = LLexeme { ltokToken = LTokComment
               , ltokPos   = start
               , ltokText  = str
               }
    _ -> error "endComment: internal lexer error"

endStringPredicate ::
  Mode ->
  AlexInput {- ^ input stream before the token -} ->
  Int       {- ^ length of the token           -} ->
  AlexInput {- ^ input stream after the token  -} ->
  Bool
endStringPredicate (QuoteMode _ _ startlen _) _ len _ = len == startlen
endStringPredicate _ _ _ _ = error "endStringPredicate called from wrong mode"

endString :: Action
endString len (AlexInput posn _) mode =
  case mode of
    QuoteMode start text _ isComment -> (NormalMode, Just t)
      where
      stringLength = sourcePosIndex posn - sourcePosIndex start + len
      str = Text.take stringLength text
      lexeme | isComment = LTokComment
             | otherwise = LTokSLit
      t = LLexeme { ltokPos = start, ltokToken = lexeme, ltokText = str }
    _ -> error "endString: internal lexer error"

tok :: LToken -> Action
tok lexeme len (AlexInput posn s) mode = (mode, Just t)
  where
  t = LLexeme { ltokToken = lexeme
           , ltokPos   = posn
           , ltokText  = Text.take len s
           }

-- | Drop the first line of a Lua file when it starts with a '#'
dropSpecialComment :: Text -> Text
dropSpecialComment text
  | "#" `Text.isPrefixOf` text = Text.dropWhile (/='\n') text
  | otherwise = text
-- Newline is preserved in order to ensure that line numbers stay correct

dropWhiteSpace :: [LLexeme] -> [LLexeme]
dropWhiteSpace = filter (not . isWhite . ltokToken)
  where
  isWhite LTokWhiteSpace = True
  isWhite LTokComment    = True
  isWhite _              = False


data SourcePos = SourcePos
                  { sourcePosName :: String
                  , sourcePosIndex, sourcePosLine, sourcePosColumn
                       :: {-# UNPACK #-}!Int
                  }
  deriving (Show,Eq)

instance NFData SourcePos where
  rnf (SourcePos _ _ _ _) = ()

startPos :: String -> SourcePos
startPos n = SourcePos n 0 1 1

-- This is unused but alex needs the definition to exist
alexInputPrevChar :: a -> ()
alexInputPrevChar _ = ()

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AlexInput p text) =
  do (c,text') <- Text.uncons text
     let p' = move p c
         x = byteForChar c
     x `seq` p' `seq` return (x, AlexInput p' text')

move :: SourcePos -> Char -> SourcePos
move (SourcePos name index line column) c =
  case c of
    '\t' -> SourcePos name (index+1) line (((column + 7) `div` 8) * 8 + 1)
    '\n' -> SourcePos name (index+1) (line + 1) 1
    _    -> SourcePos name (index+1) line (column + 1)

------------------------------------------------------------------------
-- Embed all of unicode, kind of, in a single byte!
------------------------------------------------------------------------

byteForChar :: Char -> Word8
byteForChar c = fromIntegral (min 127 (ord c))
  -- map non-ascii to a single non-ascii byte
  -- the Lua lexer doesn't distinguish between
  -- any of the non-ascii bytes


type Lexer a = Mode -> (Mode, a)

data AlexInput = AlexInput { input_pos :: !SourcePos
                           , input_text :: !Text
                           }

data Mode
  = NormalMode
  | CommentMode SourcePos Text
  | QuoteMode SourcePos -- start
              Text
              Int       -- delim length
              Bool      -- is comment
                -- ^ start delimlen iscomment
