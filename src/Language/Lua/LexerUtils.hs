{-# LANGUAGE CPP #-}
module Language.Lua.LexerUtils where

import           Control.DeepSeq (NFData(..))
import           Control.Monad (ap, liftM)
import           Data.Char (isAscii, ord)
import           Data.Word (Word8)

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (Applicative(..))
#endif

import           Language.Lua.Token

-- | Lua token with position information.
type LTok = (LToken, SourcePos)

type Action result = AlexInput -> Int -> Lexer result
newtype Lexer a = Lexer { unAlex :: AlexState -> Either (String,SourcePos) (AlexState, a) }

getMode :: Lexer Mode
getMode = Lexer $ \s@AlexState{alex_mode=mode} -> Right (s, mode)

setMode :: Mode -> Lexer ()
setMode mode = Lexer $ \s -> Right (s{alex_mode=mode},())

putInputBack :: String -> Lexer ()
putInputBack str = Lexer $ \s -> Right (s{alex_inp=str ++ alex_inp s}, ())

enterString :: Action (Maybe LTok)
enterString (posn,_,_) len =
  do let n = len - 2
     setMode (QuoteMode posn n False "")
     return Nothing

enterLongComment :: Action (Maybe LTok)
enterLongComment (posn,_,_) len =
  do let n = len - 4
     setMode (QuoteMode posn n True "")
     return Nothing

enterComment :: Action (Maybe LTok)
enterComment _ _ = do
  setMode CommentMode
  return Nothing

addCharToString :: Action (Maybe LTok)
addCharToString (_,_,s) _ = do
  mode <- getMode
  case mode of
    QuoteMode posn len isComment body ->
      setMode (QuoteMode posn len isComment (head s : body))
    _ -> error "lexer broken: addCharToString"
  return Nothing

endComment :: Action (Maybe LTok)
endComment _ _ = do
  setMode NormalMode
  return Nothing

testAndEndString :: Action (Maybe LTok)
testAndEndString (_,_,s) len = do
  let endlen = len-2
  QuoteMode posn startlen isComment val <- getMode
  if startlen /= endlen
    then do setMode (QuoteMode posn startlen isComment (head s : val))
            putInputBack (tail (take len s))
            return Nothing
    else do setMode NormalMode
            if isComment
              then return Nothing
              else do
                let eqs = replicate startlen '='
                return (Just (LTokSLit ("["++eqs++"["++reverse val++"]"++eqs++"]"), posn))


-- Helper to make LTokens with string value (like LTokNum, LTokSLit etc.)
tokWValue :: (String -> LToken) -> AlexInput -> Int -> Lexer (Maybe LTok)
tokWValue f (posn,_,s) len = return (Just (f (take len s), posn))

tok :: LToken -> AlexInput -> Int -> Lexer (Maybe LTok)
tok t (posn,_,_) _ = return (Just (t, posn))

-- | Drop the first line of a Lua file when it starts with a '#'
dropSpecialComment :: String -> String
dropSpecialComment ('#':xs) = dropWhile (/='\n') xs
dropSpecialComment xs = xs
-- Newline is preserved in order to ensure that line numbers stay correct

------------------------------------------------------------------------
-- Custom Lexer wrapper
------------------------------------------------------------------------

data SourcePos = SourcePos String -- ^ filename
                       {-# UNPACK #-}!Int  -- ^ line number
                       {-# UNPACK #-}!Int  -- ^ column number
  deriving (Show,Eq)

instance NFData SourcePos where
  rnf (SourcePos _ _ _) = ()

type AlexInput = (SourcePos,     -- current position,
                  Char,         -- previous char
                  String)       -- current input string

startPos :: String -> SourcePos
startPos n = SourcePos n 1 1

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (_,_,[]) = Nothing
alexGetByte (p,_,c:cs) = Just (byteForChar c,(p',c,cs))
  where p' = move p c

move :: SourcePos -> Char -> SourcePos
move (SourcePos name line column) c =
  case c of
    '\t' -> SourcePos name line (((column + 7) `div` 8) * 8 + 1)
    '\n' -> SourcePos name (line + 1) 1
    _    -> SourcePos name line (column + 1)

------------------------------------------------------------------------
-- Embed all of unicode, kind of, in a single byte!
------------------------------------------------------------------------

byteForChar :: Char -> Word8
byteForChar c
  | isAscii c = fromIntegral (ord c)
  | otherwise = 0   -- map non-ascii to a single non-ascii byte
                    -- the Lua lexer doesn't distinguish between
                    -- any of the non-ascii bytes

data AlexState = AlexState {
        alex_pos :: !SourcePos,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: Char,      -- the character before the input
        alex_mode :: Mode

    }

data Mode
  = NormalMode
  | CommentMode
  | QuoteMode SourcePos -- ^ start
              Int       -- ^ delim length
              Bool      -- ^ is comment
              String    -- ^ body

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> String -> Lexer a -> Either (String,SourcePos) a
runAlex name input (Lexer f)
   = case f (AlexState {alex_pos = startPos name,
                        alex_inp = input,
                        alex_chr = '\n',

                        alex_mode = NormalMode
                        }) of Left e -> Left e
                              Right ( _, a) -> Right a


throwErrorAt :: SourcePos -> String -> Lexer a
throwErrorAt pos message = Lexer $ \_ -> Left (message, pos)

setInput :: AlexInput -> Lexer ()
setInput (pos,c,inp)
 = Lexer $ \s -> case s{alex_pos=pos,alex_chr=c,alex_inp=inp} of
                  s'@(AlexState{}) -> Right (s', ())

getInput :: Lexer AlexInput
getInput
 = Lexer $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_inp=inp} ->
        Right (s, (pos,c,inp))

instance Functor Lexer where
  fmap = liftM

instance Applicative Lexer where
  pure a = Lexer $ \s -> Right (s,a)
  (<*>) = ap

instance Monad Lexer where
  return = pure
  m >>= k  = Lexer $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
