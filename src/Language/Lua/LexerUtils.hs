{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Lua.LexerUtils where

import           Control.DeepSeq (NFData(..))
import           Control.Monad (ap, liftM)
import           Data.Char (isAscii, ord)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))
import           Data.Word (Word8)

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (Applicative(..))
#endif

import           Language.Lua.Token

-- | Lua token with position information.
data LTok = LTok
  { ltokLexeme :: LToken
  , ltokPos   :: SourcePos
  , ltokText  :: Text
  } deriving (Show,Eq)

ltokEOF :: LTok
ltokEOF = LTok { ltokText   = ""
               , ltokLexeme = LTokEof
               , ltokPos    = SourcePos "" (-1) (-1) (-1)
               }

isEOF :: LTok -> Bool
isEOF x = ltokLexeme x == LTokEof


-- The matched token and the starting position
type Action result = Text -> SourcePos -> Lexer result

data R a = R { rState :: !AlexState, rResult :: a}
newtype Lexer a = Lexer { unAlex :: AlexState -> R a }

getState :: Lexer AlexState
getState = Lexer $ \s -> R s s

getMode :: Lexer Mode
getMode = fmap alex_mode getState

setMode :: Mode -> Lexer (Maybe LTok)
setMode mode = Lexer $ \s -> R s{alex_mode=mode} Nothing

enterString :: Action (Maybe LTok)
enterString t posn =
  do inp <- fmap alex_inp getState
     setMode (QuoteMode posn (t<>inp) (Text.length t - 2) False)

enterLongComment :: Action (Maybe LTok)
enterLongComment t posn =
  do inp <- fmap alex_inp getState
     setMode (QuoteMode posn (t<>inp) (Text.length t - 4) True)

enterComment :: Action (Maybe LTok)
enterComment t p =
  do inp <- fmap alex_inp getState
     setMode (CommentMode p (t<>inp))

endComment :: Action (Maybe LTok)
endComment s posn =
  do CommentMode start text <- getMode
     _ <- setMode NormalMode
     let commentLength = sourcePosIndex posn - sourcePosIndex start + Text.length s
         str = Text.take commentLength text
     return (Just LTok { ltokLexeme = LTokComment
                       , ltokPos   = start
                       , ltokText  = str
                       })

testAndEndString :: Action (Maybe LTok)
testAndEndString s posn = do
  let endlen = Text.length s - 2
  QuoteMode start text startlen isComment <- getMode
  if startlen /= endlen
    then do let c = Text.head s
            file <- getFile
            let p = move posn c
            setInput (p, Text.drop (sourcePosIndex p) file)
            return Nothing
    else do _ <- setMode NormalMode
            let stringLength = sourcePosIndex posn - sourcePosIndex start + Text.length s
                str = Text.take stringLength text
                t | isComment = LTokComment
                  | otherwise = LTokSLit
            return $ Just LTok { ltokPos = posn, ltokLexeme = t, ltokText = str }


tok :: LToken -> Action (Maybe LTok)
tok t s posn = return (Just LTok { ltokLexeme = t
                                 , ltokPos   = posn
                                 , ltokText  = s })

-- | Drop the first line of a Lua file when it starts with a '#'
dropSpecialComment :: Text -> Text
dropSpecialComment text
  | "#" `Text.isPrefixOf` text = Text.dropWhile (/='\n') text
  | otherwise = text
-- Newline is preserved in order to ensure that line numbers stay correct

dropWhiteSpace :: [LTok] -> [LTok]
dropWhiteSpace = filter (not . isWhite . ltokLexeme)
  where
  isWhite x = x == LTokWhiteSpace || x == LTokComment



------------------------------------------------------------------------
-- Custom Lexer wrapper
------------------------------------------------------------------------

data SourcePos = SourcePos
                  { sourcePosName :: String
                  , sourcePosIndex, sourcePosLine, sourcePosColumn
                       :: {-# UNPACK #-}!Int
                  }
  deriving (Show,Eq)

instance NFData SourcePos where
  rnf (SourcePos _ _ _ _) = ()

type AlexInput = (SourcePos,     -- current position
                  Text)          -- input text

startPos :: String -> SourcePos
startPos n = SourcePos n 0 1 1

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (p,text) =
  do (c,text') <- Text.uncons text
     let p' = move p c
     return (byteForChar c, (p',text'))

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
byteForChar c
  | isAscii c = fromIntegral (ord c)
  | otherwise = 0   -- map non-ascii to a single non-ascii byte
                    -- the Lua lexer doesn't distinguish between
                    -- any of the non-ascii bytes

data AlexState = AlexState
      { alex_pos  :: !SourcePos  -- ^ position at current input location
      , alex_inp  :: !Text
      , alex_mode :: !Mode       -- ^ the lexer mode
      , alex_file :: !Text       -- ^ the whole file
      }

data Mode
  = NormalMode
  | CommentMode SourcePos Text
  | QuoteMode SourcePos -- start
              Text
              Int       -- delim length
              Bool      -- is comment
                -- ^ start delimlen iscomment

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Text -> Lexer LTok -> [LTok]
runAlex name input (Lexer f) = go s0
  where
  s0 = AlexState
        { alex_pos = startPos name
        , alex_inp = input
        , alex_mode = NormalMode
        , alex_file = input
        }
  go s = case f s of
           R s' x | isEOF x -> [x]
                  | otherwise -> x : go s'

getInput :: Lexer AlexInput
getInput
 = Lexer $ \s@AlexState{alex_pos=pos,alex_inp=inp} -> R s (pos,inp)

getFile :: Lexer Text
getFile = Lexer $ \s -> R s (alex_file s)

eofError :: SourcePos -> Text -> LToken -> Lexer LTok
eofError posn text t =
  do setInput (posn, Text.empty)
     _ <- setMode NormalMode
     return LTok
       { ltokLexeme = t
       , ltokPos   = posn
       , ltokText  = text
       }

setInput :: AlexInput -> Lexer ()
setInput (posn,text) = Lexer $ \s -> R s{alex_pos=posn,alex_inp=text} ()

instance Functor Lexer where
  fmap = liftM

instance Applicative Lexer where
  pure a = Lexer $ \s -> R s a
  (<*>) = ap

instance Monad Lexer where
  return = pure
  m >>= k  = Lexer $ \s ->
              case unAlex m s of
                R s' a -> unAlex (k a) s'


