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
data LexToken = LexToken
  { ltokToken  :: Token
  , ltokRange  :: SourceRange
  , ltokLexeme :: Text
  } deriving (Show,Eq)

-- | Type of alex actions
type Action =
  AlexInput                  {- ^ state at the start of the lexeme -} ->
  AlexInput                  {- ^ state at the end   of the lexeme -} ->
  Int                        {- ^ number of characters in the lexeme -} ->
  Mode                       {- ^ lexer mode -} ->
  (Mode, [LexToken])         {- ^ updated mode, lexemes    -}

-- | The remaining input text annotated with the starting position
data AlexInput = AlexInput
  { input_pos  :: {-# UNPACK #-} !SourcePos
  , input_prev :: {-# UNPACK #-} !SourcePos
  , input_text :: {-# UNPACK #-} !Text
  }

-- | Lexer mode
data Mode
  = NormalMode

  | StringMode StringMode [SourceRange] AlexInput
    -- ^ string type, errors, input at start

  | CommentMode AlexInput
    -- ^ Single line comment. Input at beginning of comment

  | QuoteMode AlexInput -- input at beginning of long-quote
              Int       -- delim length
              Bool      -- is comment
                -- ^ start delimlen iscomment

data StringMode = SingleQuote | DoubleQuote


-- | This is called when we encounter the end of a line before seeing
-- the closing character for a string.
unterminatedString :: Action
unterminatedString _inp1 inp2 _len mode =
  case mode of
    StringMode _strTy _errs inp0 -> ( NormalMode
                                    , [ longToken inp0 inp2 TokUntermString ]
                                    )
    _ -> error "[bug] unterminatedString outside a string."


-- | An unknown character in "normal mode"
invalidChar :: Action
invalidChar inp1 _ _ _ =
  ( NormalMode
  , [ LexToken { ltokToken  = TokUnexpected
               , ltokRange  = singleRange (input_pos inp1)
               , ltokLexeme = Text.take 1 (input_text inp1)
               }
    ]
  )

-- | A a bad escape withing a string
invalidEsc :: Action
invalidEsc inp1 inp2 _ mode =
  case mode of
    StringMode m errs inp0 -> (StringMode m (err : errs) inp0, [])
      where err = SourceRange { sourceFrom = input_pos inp1
                              , sourceTo   = input_prev inp2
                              }
    _ -> error "[bug] invalidEsc outside a string."

checkEOF :: Mode -> AlexInput -> [LexToken]
checkEOF mode AlexInput { input_prev = end } =
  case mode of
    NormalMode {}         -> []
    CommentMode {}        -> []

    QuoteMode inp _ True  -> ret TokUntermComment  inp
    QuoteMode inp _ _     -> ret TokUntermString   inp
    StringMode _ _ inp     -> ret TokUntermString inp

  where
  ret t AlexInput { input_pos = start, input_text = rest } =
    [ LexToken { ltokToken  = t
               , ltokRange  = SourceRange { sourceFrom = start, sourceTo = end }
               , ltokLexeme = rest
               } ]

-- | Start lexing a long-quoted string literal
enterLongString :: Action
enterLongString inp _ len _ = (QuoteMode inp len False, [])

-- | Start lexing of a string literal
enterString :: StringMode -> Action
enterString sm inp _ _ _ = (StringMode sm [] inp, [])

-- | Start lexing a long-quoted comment
enterLongComment :: Action
enterLongComment inp _ len _ = (QuoteMode inp (len - 2) True, [])

-- | Start lexing a single-line comment
enterComment :: Action
enterComment inp _ _ _ = (CommentMode inp, [])

-- | Construct a lexeme spanning multiple matches
longToken ::
  AlexInput {- ^ input from the mode       -} ->
  AlexInput {- ^ current input             -} ->
  Token     {- ^ token for lexeme          -} ->
  LexToken
longToken AlexInput { input_pos  = start, input_text = text }
          AlexInput { input_prev = end }
          t =
  LexToken
    { ltokToken  = t
    , ltokRange  = SourceRange { sourceFrom = start, sourceTo = end }
    , ltokLexeme = Text.take lexLen text
    }
  where
  lexLen = 1 + sourcePosIndex end - sourcePosIndex start

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
endMode _ inp2 _ mode = (NormalMode, [lexeme])
  where
  lexeme =
    case mode of
      StringMode _ err inp        -> longToken inp inp2
                                   $ if null err then TokSLit
                                                 else TokUnexpected
      CommentMode inp             -> longToken inp inp2 TokComment
      QuoteMode   inp _ isComment -> longToken inp inp2
                                   $ if isComment then TokComment
                                                  else TokSLit
      NormalMode -> error "endMode: internal lexer error"

-- | Simplest action emitting a lexeme for the current match
tok :: Token -> Action
tok token inp1 inp2 len mode = (mode, [t])
  where
  t = LexToken
        { ltokToken  = token
        , ltokRange  = SourceRange { sourceFrom = input_pos inp1
                                   , sourceTo   = input_prev inp2
                                   }
        , ltokLexeme = Text.take len (input_text inp1)
        }

-- | Drop the first line of a Lua file when it starts with a '#'
dropSpecialComment :: Text -> Text
dropSpecialComment text
  | "#" `Text.isPrefixOf` text = Text.dropWhile (/='\n') text
  | otherwise = text
-- Newline is preserved in order to ensure that line numbers stay correct

-- | This function drops whitespace and comments from a list of lexemes
-- in order to make it suitable for parsing.
dropWhiteSpace :: [LexToken] -> [LexToken]
dropWhiteSpace = filter (not . isWhite . ltokToken)
  where
  isWhite TokWhiteSpace = True
  isWhite TokComment    = True
  isWhite _             = False


--------------------------------------------------------------------------------
-- Positions and Ranges


-- | The type of locations in a source file
data SourcePos = SourcePos
  { sourcePosName :: String
  , sourcePosIndex, sourcePosLine, sourcePosColumn :: {-# UNPACK #-}!Int
  }
  deriving (Show,Eq)

instance NFData SourcePos where
  rnf (SourcePos _ _ _ _) = ()


data SourceRange = SourceRange
  { sourceFrom :: !SourcePos, sourceTo :: !SourcePos
  } deriving (Show,Eq)

instance NFData SourceRange where
  rnf (SourceRange _ _) = ()

singleRange :: SourcePos -> SourceRange
singleRange p = SourceRange { sourceFrom = p, sourceTo = p }


startPos :: String -> SourcePos
startPos n = SourcePos n 0 1 1

showPos :: SourcePos -> String
showPos p = show (sourcePosLine p) ++ ":" ++ show (sourcePosColumn p)

showRange :: SourceRange -> String
showRange p = showPos (sourceFrom p) ++ "--" ++ showPos (sourceTo p)



--------------------------------------------------------------------------------
-- Scanner

-- This is unused because we don't use regular expressions with
-- "left contexts" (see Alex docs).   It is still in the code though,
-- so we provide this stub.
alexInputPrevChar :: a -> ()
alexInputPrevChar _ = ()

-- | Attempt to retrieve the next representative element for the character
-- at the head of the input string. Returns an advanced 'AlexInput'
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte AlexInput { input_pos = p, input_text = text } =
  do (c,text') <- Text.uncons text
     let p'  = move p c
         x   = fromIntegral (min 127 (ord c))
         inp = AlexInput { input_prev = p, input_pos = p', input_text = text' }
     x `seq` inp `seq` return (x, inp)

-- | Update a 'SourcePos' for a particular matched character
move :: SourcePos -> Char -> SourcePos
move (SourcePos name index line column) c =
  case c of
    '\t' -> SourcePos name (index+1) line (((column + 7) `div` 8) * 8 + 1)
    '\n' -> SourcePos name (index+1) (line + 1) 1
    _    -> SourcePos name (index+1) line (column + 1)


