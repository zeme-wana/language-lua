{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Lua.LexerUtils where

import           Control.DeepSeq (NFData(..))
import           Control.Monad (ap, liftM, guard)
import           Data.Char (isAscii, ord)
import           Data.Text (Text)
import qualified Data.Text as Text
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

newtype Lexer a = Lexer { unAlex :: Text -> AlexState -> (AlexState, a) }

getState :: Lexer AlexState
getState = Lexer $ \_ s -> (s, s)

getMode :: Lexer Mode
getMode = fmap alex_mode getState

setMode :: Mode -> Lexer (Maybe LTok)
setMode mode = Lexer $ \_ s -> (s{alex_mode=mode},Nothing)

enterString :: Action (Maybe LTok)
enterString t posn = setMode (QuoteMode posn (Text.length t - 2) False)

enterLongComment :: Action (Maybe LTok)
enterLongComment t posn = setMode (QuoteMode posn (Text.length t - 4) True)

enterComment :: Action (Maybe LTok)
enterComment _ p = setMode (CommentMode p)

endComment :: Action (Maybe LTok)
endComment s posn =
  do CommentMode start <- getMode
     _ <- setMode NormalMode
     text <- getFile
     let str = Text.take (sourcePosIndex posn - sourcePosIndex start + Text.length s)
             $ Text.drop (sourcePosIndex start) text
     return (Just LTok { ltokLexeme = LTokComment
                       , ltokPos   = start
                       , ltokText  = str
                       })

testAndEndString :: Action (Maybe LTok)
testAndEndString s posn = do
  let endlen = Text.length s - 2
  QuoteMode start startlen isComment <- getMode
  if startlen /= endlen
    then do let c = Text.head s
            setSourcePos (move posn c)
            return Nothing
    else do _ <- setMode NormalMode
            text <- getFile
            let str = Text.take (sourcePosIndex posn - sourcePosIndex start + Text.length s)
                    $ Text.drop (sourcePosIndex start) text
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
  do guard (sourcePosIndex p < Text.length text)
     let c  = Text.index text (sourcePosIndex p)
         p' = move p c
     return (byteForChar c, (p',text))

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
      { alex_pos :: !SourcePos  -- ^ position at current input location
      , alex_mode :: Mode       -- ^ the lexer mode
      }

data Mode
  = NormalMode
  | CommentMode SourcePos
  | QuoteMode SourcePos -- start
              Int       -- delim length
              Bool      -- is comment
                -- ^ start delimlen iscomment

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Text -> Lexer a -> a
runAlex name input (Lexer f) = snd (f input s)
  where
  s = AlexState
        { alex_pos = startPos name
        , alex_mode = NormalMode
        }

getInput :: Lexer AlexInput
getInput
 = Lexer $ \inp s@AlexState{alex_pos=pos} -> (s, (pos,inp))

getFile :: Lexer Text
getFile = Lexer $ \inp s -> (s, inp)

eofError :: SourcePos -> LToken -> Lexer LTok
eofError posn t =
  do text <- getFile
     return LTok
       { ltokLexeme = t
       , ltokPos   = posn
       , ltokText  = Text.drop (sourcePosIndex posn) text
       }

setSourcePos :: SourcePos -> Lexer ()
setSourcePos posn = Lexer $ \_ s -> (s{alex_pos=posn}, ())

instance Functor Lexer where
  fmap = liftM

instance Applicative Lexer where
  pure a = Lexer $ \_ s -> (s,a)
  (<*>) = ap

instance Monad Lexer where
  return = pure
  m >>= k  = Lexer $ \t s ->
              case unAlex m t s of
                (s',a) -> unAlex (k a) t s'


