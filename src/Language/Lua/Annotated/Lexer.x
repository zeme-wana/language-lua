{

{-# OPTIONS_GHC -w #-}
{-# LANGUAGE BangPatterns #-}

module Language.Lua.Annotated.Lexer
  ( llex
  , llexNamed
  , llexNamedWithWhiteSpace
  , llexFile
  , LexToken(..)
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

    <0> $white+    { tok TokWhiteSpace }

    -- keywords
    <0> "and"      { tok TokAnd }
    <0> "break"    { tok TokBreak }
    <0> "do"       { tok TokDo }
    <0> "else"     { tok TokElse }
    <0> "elseif"   { tok TokElseIf }
    <0> "end"      { tok TokEnd }
    <0> "false"    { tok TokFalse }
    <0> "for"      { tok TokFor }
    <0> "function" { tok TokFunction }
    <0> "goto"     { tok TokGoto }
    <0> "if"       { tok TokIf }
    <0> "in"       { tok TokIn }
    <0> "local"    { tok TokLocal }
    <0> "nil"      { tok TokNil }
    <0> "not"      { tok TokNot }
    <0> "or"       { tok TokOr }
    <0> "repeat"   { tok TokRepeat }
    <0> "return"   { tok TokReturn }
    <0> "then"     { tok TokThen }
    <0> "true"     { tok TokTrue }
    <0> "until"    { tok TokUntil }
    <0> "while"    { tok TokWhile }

    -- identifiers
    <0> $letter $identletter* { tok TokIdent }

    -- number literals
    <0> @mantpart @exppart?                  { tok TokNum }
    <0> @hexprefix @mantparthex @expparthex? { tok TokNum }

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
    <0> "+"   { tok TokPlus }
    <0> "-"   { tok TokMinus }
    <0> "*"   { tok TokStar }
    <0> "/"   { tok TokSlash }
    <0> "//"  { tok TokDSlash }
    <0> "%"   { tok TokPercent }
    <0> "^"   { tok TokExp }
    <0> "#"   { tok TokSh }
    <0> "=="  { tok TokEqual }
    <0> "~="  { tok TokNotequal }
    <0> "<="  { tok TokLEq }
    <0> ">="  { tok TokGEq }
    <0> "<"   { tok TokLT }
    <0> ">"   { tok TokGT }
    <0> "="   { tok TokAssign }
    <0> "("   { tok TokLParen }
    <0> ")"   { tok TokRParen }
    <0> "{"   { tok TokLBrace }
    <0> "}"   { tok TokRBrace }
    <0> "["   { tok TokLBracket }
    <0> "]"   { tok TokRBracket }
    <0> "::"  { tok TokDColon }
    <0> ";"   { tok TokSemic }
    <0> ":"   { tok TokColon }
    <0> ","   { tok TokComma }
    <0> "."   { tok TokDot }
    <0> ".."  { tok TokDDot }
    <0> "..." { tok TokEllipsis }
    <0> "&"   { tok TokAmpersand }
    <0> "|"   { tok TokPipe }
    <0> "~"   { tok TokTilde }
    <0> "<<"  { tok TokDLT }
    <0> ">>"  { tok TokDGT }

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

scanner' :: AlexInput -> Mode -> [LexToken SourcePos]
scanner' inp mode =
  case alexScanUser mode inp (modeCode mode) of
    AlexEOF                   -> abortMode Nothing mode ++ [ltokEOF]
    AlexError _               -> error "language-lua lexer internal error"
    AlexSkip inp' _           -> scanner' inp' mode
    AlexToken inp' len action ->
       case action len inp mode of
         (mode', ts) -> ts ++ scanner' inp' mode'

scanner :: String -> Text -> [LexToken SourcePos]
scanner name str = scanner' (AlexInput (startPos name) str) NormalMode

-- | Lua lexer with default @=<string>@ name.
llex :: Text {- ^ chunk -} -> [LexToken SourcePos]
llex = llexNamed "=<string>"


-- | Lua lexer with explicit name.
llexNamed ::
  String {- ^ name -} ->
  Text   {- ^ chunk -} ->
  [LexToken SourcePos]
llexNamed name chunk = dropWhiteSpace (llexNamedWithWhiteSpace name chunk)


-- | Lua lexer with explicit name, preseves white space and comments.
llexNamedWithWhiteSpace ::
  String {- ^ name -} ->
  Text   {- ^ chunk -} ->
  [LexToken SourcePos]
llexNamedWithWhiteSpace name chunk = scanner name (dropSpecialComment chunk)


-- | Run Lua lexer on a file.
llexFile :: FilePath -> IO [LexToken SourcePos]
llexFile fp = fmap (llexNamed fp) (Text.readFile fp)

}
