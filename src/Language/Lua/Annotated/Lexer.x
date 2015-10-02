{

{-# OPTIONS_GHC -w #-}

module Language.Lua.Annotated.Lexer
  ( llex
  , llexNamed
  , llexFile
  , LTok
  , SourcePos(..)
  ) where

import           Control.Applicative (Applicative(..))
import           Control.Monad (ap, liftM, forM_, when)
import           Data.Char (GeneralCategory(..),generalCategory,isAscii,isSpace)
import           Data.Word (Word8)
import           Control.DeepSeq (NFData(..))

import           Language.Lua.Token

}

$letter      = [a-zA-Z_]                 -- first letter of variables
$identletter = [a-zA-Z_0-9]              -- letters for rest of variables

$digit    = 0-9                          -- decimal digits
$hexdigit = [0-9a-fA-F]                  -- hexadecimal digits

$dqstr    = . # [ \" \\ ]       -- valid character in a string literal with dquotes
$sqstr    = . # [ \' \\ ]       -- valid character in a string literal with quotes
$longstr  = [ . \n ]                     -- valid character in a long string

-- escape characters
@charesc   = \\ ([ntvbrfa\\"'] | $digit{1,3} | x$hexdigit{2} | u\{$hexdigit+\} | \n | z $white*)

@digits    = $digit+
@hexdigits = $hexdigit+

@mantpart = (@digits \. @digits) | @digits \. | \. @digits
@exppart  = [eE][\+\-]? @digits

@hexprefix   = 0x | 0X
@mantparthex = (@hexdigits \. @hexdigits) | @hexdigits \. | \. @hexdigits
@expparthex  = [pP][\+\-]? @hexdigits

tokens :-

    <0> $white+  ;

    -- keywords
    <0> "and"      { tok LTokAnd }
    <0> "break"    { tok LTokBreak }
    <0> "do"       { tok LTokDo }
    <0> "else"     { tok LTokElse }
    <0> "elseif"   { tok LTokElseIf }
    <0> "end"      { tok LTokEnd }
    <0> "false"    { tok LTokFalse }
    <0> "for"      { tok LTokFor }
    <0> "function" { tok LTokFunction }
    <0> "goto"     { tok LTokGoto }
    <0> "if"       { tok LTokIf }
    <0> "in"       { tok LTokIn }
    <0> "local"    { tok LTokLocal }
    <0> "nil"      { tok LTokNil }
    <0> "not"      { tok LTokNot }
    <0> "or"       { tok LTokOr }
    <0> "repeat"   { tok LTokRepeat }
    <0> "return"   { tok LTokReturn }
    <0> "then"     { tok LTokThen }
    <0> "true"     { tok LTokTrue }
    <0> "until"    { tok LTokUntil }
    <0> "while"    { tok LTokWhile }

    -- identifiers
    <0> $letter $identletter* { tokWValue LTokIdent }

    -- number literals
    <0> @digits                              { tokWValue LTokNum }
    <0> @digits @exppart                     { tokWValue LTokNum }
    <0> @mantpart @exppart?                  { tokWValue LTokNum }
    <0> @hexprefix @hexdigits                { tokWValue LTokNum }
    <0> @hexprefix @hexdigits @expparthex    { tokWValue LTokNum }
    <0> @hexprefix @mantparthex @expparthex? { tokWValue LTokNum }

    -- string literals
    <0> \"($dqstr|@charesc)*\" { tokWValue LTokSLit }
    <0> \'($sqstr|@charesc)*\' { tokWValue LTokSLit }

    -- long strings
    <0> \[ =* \[            { enterString }
    <state_string> \] =* \] { testAndEndString }
    <state_string> $longstr { addCharToString }

    -- comments
    <0> "--" \[ =* \[       { enterLongComment }
    <0> "--"                { enterComment }
    <state_comment> .       ;
    <state_comment> \n      { endComment }

    -- operators
    <0> "+"   { tok LTokPlus }
    <0> "-"   { tok LTokMinus }
    <0> "*"   { tok LTokStar }
    <0> "/"   { tok LTokSlash }
    <0> "//"  { tok LTokDSlash }
    <0> "%"   { tok LTokPercent }
    <0> "^"   { tok LTokExp }
    <0> "#"   { tok LTokSh }
    <0> "=="  { tok LTokEqual }
    <0> "~="  { tok LTokNotequal }
    <0> "<="  { tok LTokLEq }
    <0> ">="  { tok LTokGEq }
    <0> "<"   { tok LTokLT }
    <0> ">"   { tok LTokGT }
    <0> "="   { tok LTokAssign }
    <0> "("   { tok LTokLParen }
    <0> ")"   { tok LTokRParen }
    <0> "{"   { tok LTokLBrace }
    <0> "}"   { tok LTokRBrace }
    <0> "["   { tok LTokLBracket }
    <0> "]"   { tok LTokRBracket }
    <0> "::"  { tok LTokDColon }
    <0> ";"   { tok LTokSemic }
    <0> ":"   { tok LTokColon }
    <0> ","   { tok LTokComma }
    <0> "."   { tok LTokDot }
    <0> ".."  { tok LTokDDot }
    <0> "..." { tok LTokEllipsis }
    <0> "&"   { tok LTokAmpersand }
    <0> "|"   { tok LTokPipe }
    <0> "~"   { tok LTokTilde }
    <0> "<<"  { tok LTokDLT }
    <0> ">>"  { tok LTokDGT }

{

getMode :: Alex Mode
getMode = Alex $ \s@AlexState{alex_mode=mode} -> Right (s, mode)

setMode :: Mode -> Alex ()
setMode mode = Alex $ \s -> Right (s{alex_mode=mode},())

putInputBack :: String -> Alex ()
putInputBack str = Alex $ \s -> Right (s{alex_inp=str ++ alex_inp s}, ())

enterString :: AlexAction LTok
enterString (posn,_,s) len =
  do let n = len - 2
     setMode (QuoteMode posn n False "")
     monadScan'

enterLongComment :: AlexAction LTok
enterLongComment (posn,_,s) len =
  do let n = len - 4
     setMode (QuoteMode posn n True "")
     monadScan'

enterComment :: AlexAction LTok
enterComment _ _ = do
  setMode CommentMode
  monadScan'

addCharToString :: AlexAction LTok
addCharToString (posn,_,s) _ = do
  mode <- getMode
  case mode of
    QuoteMode posn len isComment body ->
      setMode (QuoteMode posn len isComment (head s : body))
    _ -> throwErrorAt posn "addCharToString: bad mode"
  monadScan'

endComment :: AlexAction LTok
endComment _ _ = do
  setMode NormalMode
  monadScan'

testAndEndString :: AlexAction LTok
testAndEndString (_,_,s) len = do
  let endlen = len-2
  QuoteMode posn startlen isComment val <- getMode
  if startlen /= endlen
    then do setMode (QuoteMode posn startlen isComment (head s : val))
            putInputBack (tail (take len s))
            monadScan'
    else do setMode NormalMode
            if isComment
              then monadScan'
              else do
                let eqs = replicate startlen '='
                return (LTokSLit ("["++eqs++"["++reverse val++"]"++eqs++"]"), posn)

-- | Lua token with position information.
type LTok = (LToken, SourcePos)

type AlexAction result = AlexInput -> Int -> Alex result

-- Helper to make LTokens with string value (like LTokNum, LTokSLit etc.)
tokWValue :: (String -> LToken) -> AlexInput -> Int -> Alex LTok
tokWValue tok (posn,_,s) len = return (tok (take len s), posn)

tok :: LToken -> AlexInput -> Int -> Alex LTok
tok t (posn,_,_) _ = return (t, posn)

monadScan' :: Alex LTok
monadScan' = do
  inp <- getInput
  sc <- getStartCode
  case alexScan inp sc of
    AlexEOF -> do mode <- getMode
                  case mode of
                    QuoteMode start _ True _ -> throwErrorAt start "unterminated comment"
                    QuoteMode start _ False _ -> throwErrorAt start "unterminated string"
                    _ -> return (LTokEof, SourcePos "" (-1) (-1))
    AlexError (pos,ch,_) -> throwErrorAt pos ("at char " ++ [ch])
    AlexSkip  inp' len -> do
        setInput inp'
        monadScan'
    AlexToken inp' len action -> do
        setInput inp'
        action inp len

scanner :: String -> String -> Either (String,SourcePos) [LTok]
scanner name str = runAlex name str loop
  where loop = do
          t    <- monadScan'
          case fst t of
             LTokEof -> return [t]
             _ -> do toks <- loop
                     return (t:toks)

-- | Drop the first line of a Lua file when it starts with a '#'
dropSpecialComment :: String -> String
dropSpecialComment ('#':xs) = dropWhile (/='\n') xs
dropSpecialComment xs = xs
-- Newline is preserved in order to ensure that line numbers stay correct

-- | Lua lexer with default @=<string>@ name.
llex :: String {- ^ chunk -} -> Either (String,SourcePos) [LTok]
llex chunk = llexNamed chunk "=<string>"

-- | Lua lexer with explicit name.
llexNamed ::
  String {- ^ chunk -} ->
  String {- ^ name -} ->
  Either (String,SourcePos) [LTok]
llexNamed chunk name = scanner name (dropSpecialComment chunk)

-- | Run Lua lexer on a file.
llexFile :: FilePath -> IO (Either (String,SourcePos) [LTok])
llexFile fp = fmap (`llexNamed` fp) (readFile fp)

------------------------------------------------------------------------
-- Custom Alex wrapper
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
  where p' = alexMove p c

alexMove :: SourcePos -> Char -> SourcePos
alexMove (SourcePos name line column) c =
  case c of
    '\t' -> SourcePos name line (((column + 7) `div` 8) * 8 + 1)
    '\n' -> SourcePos name (line + 1) 1
    _    -> SourcePos name line (column + 1)

------------------------------------------------------------------------
-- Embed all of unicode, kind of, in a single byte!
------------------------------------------------------------------------

byteForChar :: Char -> Word8
byteForChar c
  | c <= '\6' = non_graphic
  | isAscii c = fromIntegral (ord c)
  | otherwise = case generalCategory c of
                  LowercaseLetter       -> lower
                  OtherLetter           -> lower
                  UppercaseLetter       -> upper
                  TitlecaseLetter       -> upper
                  DecimalNumber         -> digit
                  OtherNumber           -> digit
                  ConnectorPunctuation  -> symbol
                  DashPunctuation       -> symbol
                  OtherPunctuation      -> symbol
                  MathSymbol            -> symbol
                  CurrencySymbol        -> symbol
                  ModifierSymbol        -> symbol
                  OtherSymbol           -> symbol
                  Space                 -> space
                  ModifierLetter        -> other
                  NonSpacingMark        -> other
                  SpacingCombiningMark  -> other
                  EnclosingMark         -> other
                  LetterNumber          -> other
                  OpenPunctuation       -> other
                  ClosePunctuation      -> other
                  InitialQuote          -> other
                  FinalQuote            -> other
                  _                     -> non_graphic
  where
  non_graphic     = 0
  upper           = 1
  lower           = 2
  digit           = 3
  symbol          = 4
  space           = 5
  other           = 6

data AlexState = AlexState {
        alex_pos :: !SourcePos,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
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

runAlex :: String -> String -> Alex a -> Either (String,SourcePos) a
runAlex name input (Alex f)
   = case f (AlexState {alex_pos = startPos name,
                        alex_inp = input,
                        alex_chr = '\n',

                        alex_mode = NormalMode
                        }) of Left e -> Left e
                              Right ( _, a) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either (String,SourcePos) (AlexState, a) }

throwErrorAt :: SourcePos -> String -> Alex a
throwErrorAt pos message = Alex $ \s -> Left (message, pos)

getStartCode :: Alex Int
getStartCode = Alex $ \s@AlexState{alex_mode=mode} -> Right (s, modeCode mode)

modeCode :: Mode -> Int
modeCode mode =
  case mode of
    NormalMode -> 0
    CommentMode -> state_comment
    QuoteMode{} -> state_string

setInput :: AlexInput -> Alex ()
setInput (pos,c,inp)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_inp=inp} of
                  s@(AlexState{}) -> Right (s, ())

getInput :: Alex AlexInput
getInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_inp=inp} ->
        Right (s, (pos,c,inp))

instance Functor Alex where
  fmap = liftM

instance Applicative Alex where
  pure a = Alex $ \s -> Right (s,a)
  (<*>) = ap

instance Monad Alex where
  return = pure
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'

}
