{

{-# OPTIONS_GHC -w #-}

module Language.Lua.Annotated.Lexer
  ( llex
  , llexNamed
  , llexNamedWithWhiteSpace
  , llexFile
  , LTok(..)
  , ltokEOF
  , SourcePos(..)
  , dropWhiteSpace
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Language.Lua.Token
import           Language.Lua.LexerUtils

}

$letter      = [a-zA-Z_]                -- first letter of variables
$identletter = [a-zA-Z_0-9]             -- letters for rest of variables

$digit    = 0-9                         -- decimal digits
$hexdigit = [0-9a-fA-F]                 -- hexadecimal digits

$dqstr    = . # [ \" \\ ]               -- valid character in a string literal with dquotes
$sqstr    = . # [ \' \\ ]               -- valid character in a string literal with quotes
$longstr  = [ . \n ]                    -- valid character in a long string

-- escape characters
@charesc   = \\ ([ntvbrfa\\"'] | $digit{1,3} | x$hexdigit{2} | u\{$hexdigit+\} | \n | z $white*)

@digits    = $digit+
@hexdigits = $hexdigit+

@mantpart = @digits | @digits \. @digits | @digits \. | \. @digits
@exppart  = [eE][\+\-]? @digits

@hexprefix   = 0x | 0X
@mantparthex = @hexdigits | @hexdigits \. @hexdigits | @hexdigits \. | \. @hexdigits
@expparthex  = [pP][\+\-]? @hexdigits

tokens :-

    <0> $white+    { tok LTokWhiteSpace }

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
    <0> $letter $identletter* { tok LTokIdent }

    -- number literals
    <0> @mantpart @exppart?                  { tok LTokNum }
    <0> @hexprefix @mantparthex @expparthex? { tok LTokNum }

    -- string literals
    <0> \"($dqstr|@charesc)*\" { tok LTokSLit }
    <0> \'($sqstr|@charesc)*\' { tok LTokSLit }

    -- long strings
    <0> \[ =* \[            { enterString }
    <state_string> \] =* \] { testAndEndString }
    <state_string> $longstr ;

    -- comments
    <0> "--" \[ =* \[       { enterLongComment }
    <0> "--"                { enterComment }
    <state_comment> .*\n?   { endComment }

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

getStartCode :: Lexer Int
getStartCode = fmap modeCode getMode

modeCode :: Mode -> Int
modeCode mode =
  case mode of
    NormalMode -> 0
    CommentMode{} -> state_comment
    QuoteMode{} -> state_string

monadScan' :: Lexer LTok
monadScan' = do
  (pos,text) <- getInput
  sc <- getStartCode
  case alexScan (pos,text) sc of
    AlexEOF -> do mode <- getMode
                  case mode of
                    QuoteMode start _ True -> eofError start LTokUntermComment
                    QuoteMode start _ False -> eofError start LTokUntermString
                    _ -> return ltokEOF
    AlexError (pos',_) ->
      do setSourcePos (move pos (Text.head text))
         return LTok { ltokToken = LTokUnexpected
                     , ltokPos = pos
                     , ltokText = Text.take 1 text}
    AlexSkip  (pos',_) len ->
      do setSourcePos pos'
         monadScan'
    AlexToken (pos',_) len action ->
      do setSourcePos pos'
         let str = Text.take len (Text.drop (sourcePosIndex pos) text)
         maybe monadScan' return =<< action str pos

scanner :: String -> Text -> [LTok]
scanner name str = runAlex name str loop
  where loop = do
          t <- monadScan'
          case t of
             _ | isEOF t -> return [t]
             t -> fmap (t:) loop

-- | Lua lexer with default @=<string>@ name.
llex :: Text {- ^ chunk -} -> [LTok]
llex = llexNamed "=<string>"


-- | Lua lexer with explicit name.
llexNamed ::
  String {- ^ name -} ->
  Text   {- ^ chunk -} ->
  [LTok]
llexNamed name chunk = dropWhiteSpace
                     $ scanner name
                     $ dropSpecialComment chunk

-- | Lua lexer with explicit name, preseves white space and comments.
llexNamedWithWhiteSpace ::
  String {- ^ name -} ->
  Text   {- ^ chunk -} ->
  [LTok]
llexNamedWithWhiteSpace name chunk = scanner name (dropSpecialComment chunk)



-- | Run Lua lexer on a file.
llexFile :: FilePath -> IO [LTok]
llexFile fp = fmap (llexNamed fp) (Text.readFile fp)

}
