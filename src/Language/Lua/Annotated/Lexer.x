{

{-# OPTIONS_GHC -w #-}
{-# LANGUAGE BangPatterns #-}

module Language.Lua.Annotated.Lexer
  ( llex
  , llexNamed
  , llexNamedWithWhiteSpace
  , llexFile
  , Lexeme(..)
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

    <0> \'                   { enterSingleString }
    <state_sstring> @charesc ;
    <state_sstring> $sqstr   ;
    <state_sstring> \'       { endMode }

    -- string literals
    <0> \"                   { enterDoubleString }
    <state_dstring> @charesc ;
    <state_dstring> $dqstr   ;
    <state_dstring> \"       { endMode }

    -- long strings
    <0> \[ =* \[              { enterLongString }
    <state_lstring> \] =* \] / { endStringPredicate } { endMode }
    <state_lstring> $longstr  ;

    -- comments
    <0> "--" \[ =* \[       { enterLongComment }
    <0> "--"                { enterComment }
    <state_comment> .*      { endMode }

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

    <0,state_sstring,state_dstring,state_lstring>
       . | \n    { unexpectedChar }

{

-- | Map a lexer 'Mode' an an alex state code.
modeCode :: Mode -> Int
modeCode mode =
  case mode of
    NormalMode    -> 0
    CommentMode{} -> state_comment
    QuoteMode{}   -> state_lstring
    SingleQuoteMode{} -> state_sstring
    DoubleQuoteMode{} -> state_dstring

scanner' :: AlexInput -> Mode -> [Lexeme SourcePos]
scanner' inp mode =
  case alexScanUser mode inp (modeCode mode) of
    AlexEOF                   -> abortMode Nothing mode ++ [ltokEOF]
    AlexError _               -> error "language-lua lexer internal error"
    AlexSkip inp' _           -> scanner' inp' mode
    AlexToken inp' len action ->
       case action len inp mode of
         (mode', ts) -> ts ++ scanner' inp' mode'

scanner :: String -> Text -> [Lexeme SourcePos]
scanner name str = scanner' (AlexInput (startPos name) str) NormalMode

-- | Lua lexer with default @=<string>@ name.
llex :: Text {- ^ chunk -} -> [Lexeme SourcePos]
llex = llexNamed "=<string>"


-- | Lua lexer with explicit name.
llexNamed ::
  String {- ^ name -} ->
  Text   {- ^ chunk -} ->
  [Lexeme SourcePos]
llexNamed name chunk = dropWhiteSpace (llexNamedWithWhiteSpace name chunk)


-- | Lua lexer with explicit name, preseves white space and comments.
llexNamedWithWhiteSpace ::
  String {- ^ name -} ->
  Text   {- ^ chunk -} ->
  [Lexeme SourcePos]
llexNamedWithWhiteSpace name chunk = scanner name (dropSpecialComment chunk)


-- | Run Lua lexer on a file.
llexFile :: FilePath -> IO [Lexeme SourcePos]
llexFile fp = fmap (llexNamed fp) (Text.readFile fp)

}
