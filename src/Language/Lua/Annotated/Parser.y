{
{-# LANGUAGE RankNTypes #-}

module Language.Lua.Annotated.Parser
  ( parseText
  , parseNamedText
  , parseFile
  , Parser
  , chunk
  , exp
  , stat
  ) where

import           Control.Monad (liftM,ap)
import           Prelude hiding (LT,GT,EQ,exp)
import           Data.Text (Text)
import qualified Data.Text.IO as Text

import           Language.Lua.Token           (LToken(..))
import           Language.Lua.Annotated.Lexer (SourcePos(..), LLexeme(..), llexNamed)
import           Language.Lua.Annotated.Syntax

}

%tokentype    { LLexeme }
%token
'+'           { LLexeme { ltokToken = LTokPlus      } }
'-'           { LLexeme { ltokToken = LTokMinus     } }
'*'           { LLexeme { ltokToken = LTokStar      } }
'/'           { LLexeme { ltokToken = LTokSlash     } }
'//'          { LLexeme { ltokToken = LTokDSlash    } }
'%'           { LLexeme { ltokToken = LTokPercent   } }
'^'           { LLexeme { ltokToken = LTokExp       } }
'#'           { LLexeme { ltokToken = LTokSh        } }
'=='          { LLexeme { ltokToken = LTokEqual     } }
'~='          { LLexeme { ltokToken = LTokNotequal  } }
'<='          { LLexeme { ltokToken = LTokLEq       } }
'>='          { LLexeme { ltokToken = LTokGEq       } }
'<'           { LLexeme { ltokToken = LTokLT        } }
'>'           { LLexeme { ltokToken = LTokGT        } }
'&'           { LLexeme { ltokToken = LTokAmpersand } }
'~'           { LLexeme { ltokToken = LTokTilde     } }
'|'           { LLexeme { ltokToken = LTokPipe      } }
'>>'          { LLexeme { ltokToken = LTokDGT       } }
'<<'          { LLexeme { ltokToken = LTokDLT       } }
'='           { LLexeme { ltokToken = LTokAssign    } }
'('           { LLexeme { ltokToken = LTokLParen    } }
')'           { LLexeme { ltokToken = LTokRParen    } }
'{'           { LLexeme { ltokToken = LTokLBrace    } }
'}'           { LLexeme { ltokToken = LTokRBrace    } }
'['           { LLexeme { ltokToken = LTokLBracket  } }
']'           { LLexeme { ltokToken = LTokRBracket  } }
'::'          { LLexeme { ltokToken = LTokDColon    } }
';'           { LLexeme { ltokToken = LTokSemic     } }
':'           { LLexeme { ltokToken = LTokColon     } }
','           { LLexeme { ltokToken = LTokComma     } }
'.'           { LLexeme { ltokToken = LTokDot       } }
'..'          { LLexeme { ltokToken = LTokDDot      } }
'...'         { LLexeme { ltokToken = LTokEllipsis  } }
'and'         { LLexeme { ltokToken = LTokAnd       } }
'break'       { LLexeme { ltokToken = LTokBreak     } }
'do'          { LLexeme { ltokToken = LTokDo        } }
'else'        { LLexeme { ltokToken = LTokElse      } }
'elseif'      { LLexeme { ltokToken = LTokElseIf    } }
'end'         { LLexeme { ltokToken = LTokEnd       } }
'false'       { LLexeme { ltokToken = LTokFalse     } }
'for'         { LLexeme { ltokToken = LTokFor       } }
'function'    { LLexeme { ltokToken = LTokFunction  } }
'goto'        { LLexeme { ltokToken = LTokGoto      } }
'if'          { LLexeme { ltokToken = LTokIf        } }
'in'          { LLexeme { ltokToken = LTokIn        } }
'local'       { LLexeme { ltokToken = LTokLocal     } }
'nil'         { LLexeme { ltokToken = LTokNil       } }
'not'         { LLexeme { ltokToken = LTokNot       } }
'or'          { LLexeme { ltokToken = LTokOr        } }
'repeat'      { LLexeme { ltokToken = LTokRepeat    } }
'return'      { LLexeme { ltokToken = LTokReturn    } }
'then'        { LLexeme { ltokToken = LTokThen      } }
'true'        { LLexeme { ltokToken = LTokTrue      } }
'until'       { LLexeme { ltokToken = LTokUntil     } }
'while'       { LLexeme { ltokToken = LTokWhile     } }
numeral       { LLexeme { ltokToken = LTokNum       } }
literalString { LLexeme { ltokToken = LTokSLit      } }
ident         { LLexeme { ltokToken = LTokIdent     } }

%monad { Parser }
%lexer { (>>=) lexerP } { LLexeme { ltokToken = LTokEof } }
%error { errorP }

-- local a=b(nil)() is one statement
-- local a=b;(nil)() is two statements
-- therefore EXP '('
-- f()(nil)() is one statement
-- f();(nil)() is two statements
-- therefore STAT PREFIX
%nonassoc STAT    EXP
%nonassoc PREFIX  '('

%left 'or'
%left 'and'
%left '<' '<=' '>' '>=' '==' '~='
%left '|'
%left '~'
%left '&'
%left '<<' '>>'
%right '..'
%left '+' '-'
%left '*' '/' '//' '%'
%nonassoc 'not' '#' NEG COMPLEMENT
%right '^'

%name chunk block
%name exp exp
%name stat stat

%%

opt(p)
  : p { Just $1 }
  |   { Nothing }

many   (p) : revMany(p)   { reverse $1 }
revMany(p) : revMany(p) p { $2 : $1    }
           |              { []         }

sepBy    (p,sep) : sepBy1(p,sep)          { $1         }
                 |                        { []         }
sepBy1   (p,sep) : revSepBy1(p,sep)       { reverse $1 }
revSepBy1(p,sep) : p                      { [$1]       }
                 | revSepBy1(p,sep) sep p { $3 : $1    }

block ::                    { Block SourcePos              }
  : many(stat) opt(retstat) { Block (blockAnn $1 $2) $1 $2 }

retstat ::                           { [Exp SourcePos] }
  : 'return' sepBy(exp,',') opt(';') { $2              }

stat ::                                                   { Stat SourcePos                }
  : ';'                                                   { sl $1 EmptyStat               }
  | varlist '=' explist                                   { sp (head $1) Assign $1 $3     }
  | functioncall %prec STAT                               { sp $1 FunCall $1              }
  | '::' name '::'                                        { sl $1 Label $2                }
  | 'break'                                               { sl $1 Break                   }
  | 'goto' name                                           { sl $1 Goto $2                 }
  | 'local' namelist opt(assign)                          { sl $1 LocalAssign $2 $3       }

  ------- block structures -------------------------------
  | 'function' funcname funcbody 'end'                    { sl $1 FunAssign $2 $3         }
  | 'local' 'function' name funcbody 'end'                { sl $1 LocalFunAssign $3 $4    }
  | 'repeat' block 'until' exp                            { sl $1 Repeat $2 $4            }
  | 'do' block 'end'                                      { sl $1 Do $2                   }
  | 'while' exp 'do' block 'end'                          { sl $1 While $2 $4             }
  | 'if' exp 'then' block many(elseif) opt(else) 'end'    { sl $1 If (($2,$4):$5) $6      }
  | 'for' name '=' exp ',' exp opt(step) 'do' block 'end' { sl $1 ForRange $2 $4 $6 $7 $9 }
  | 'for' namelist 'in' explist 'do' block 'end'          { sl $1 ForIn $2 $4 $6          }

  ------- error cases for block structures ---------------
  | 'function' funcname funcbody error                    {% noEndP $1 }
  | 'local' 'function' name funcbody error                {% noEndP $1 }
  | 'repeat' block error                                  {% noEndP $1 }
  | 'do' block error                                      {% noEndP $1 }
  | 'while' exp 'do' block error                          {% noEndP $1 }
  | 'if' exp 'then' block many(elseif) opt(else) error    {% noEndP $1 }
  | 'for' name '=' exp ',' exp opt(step) 'do' block error {% noEndP $1 }
  | 'for' namelist 'in' explist 'do' block error          {% noEndP $1 }

elseif : 'elseif' exp 'then' block { ($2,$4) }
else   : 'else' block { $2 }
step   : ',' exp { $2 }
assign : '=' explist { $2 }

varlist  : sepBy1(var,  ',') { $1 }
explist  : sepBy1(exp,  ',') { $1 }
namelist : sepBy1(name, ',') { $1 }

prefixexp ::                  { PrefixExp SourcePos }
  : var                       { sp $1 PEVar $1      }
  | functioncall %prec PREFIX { sp $1 PEFunCall $1  }
  | '(' exp ')'               { sl $1 Paren $2      }

functioncall ::               { FunCall SourcePos         }
  : prefixexp            args { sp $1 NormalFunCall $1 $2 }
  | prefixexp methodname args { sp $1 MethodCall $1 $2 $3 }

funcname ::                               { FunName SourcePos      }
  : name many(dottedname) opt(methodname) { sp $1 FunName $1 $2 $3 }

dottedname : '.' name  { $2 }
methodname : ':' name  { $2 }

var ::                    { Var SourcePos          }
  : name                  { sp $1 VarName $1       }
  | prefixexp '[' exp ']' { sp $1 Select $1 $3     }
  | prefixexp '.' name    { sp $1 SelectName $1 $3 }

exp ::                     { Exp SourcePos               }
  : 'nil'                  { sl $1 Nil                   }
  | 'false'                { sl $1 Bool False            }
  | 'true'                 { sl $1 Bool True             }
  | numeral                { sl $1 Number (ltokText $1) }
  | literalString          { sl $1 String (ltokText $1) }
  | '...'                  { sl $1 Vararg                }
  | functiondef            { sp $1 EFunDef $1            }
  | prefixexp %prec EXP    { sp $1 PrefixExp $1          }
  | tableconstructor       { sp $1 TableConst $1         }

  | exp '+' exp   { sp $1 Binop (sl $2 Add   ) $1 $3 }
  | exp '-' exp   { sp $1 Binop (sl $2 Sub   ) $1 $3 }
  | exp '*' exp   { sp $1 Binop (sl $2 Mul   ) $1 $3 }
  | exp '/' exp   { sp $1 Binop (sl $2 Div   ) $1 $3 }
  | exp '//' exp  { sp $1 Binop (sl $2 IDiv  ) $1 $3 }
  | exp '^' exp   { sp $1 Binop (sl $2 Exp   ) $1 $3 }
  | exp '%' exp   { sp $1 Binop (sl $2 Mod   ) $1 $3 }
  | exp '..' exp  { sp $1 Binop (sl $2 Concat) $1 $3 }
  | exp '<'  exp  { sp $1 Binop (sl $2 LT    ) $1 $3 }
  | exp '<=' exp  { sp $1 Binop (sl $2 LTE   ) $1 $3 }
  | exp '>'  exp  { sp $1 Binop (sl $2 GT    ) $1 $3 }
  | exp '>=' exp  { sp $1 Binop (sl $2 GTE   ) $1 $3 }
  | exp '==' exp  { sp $1 Binop (sl $2 EQ    ) $1 $3 }
  | exp '~=' exp  { sp $1 Binop (sl $2 NEQ   ) $1 $3 }
  | exp 'and' exp { sp $1 Binop (sl $2 And   ) $1 $3 }
  | exp 'or'  exp { sp $1 Binop (sl $2 Or    ) $1 $3 }
  | exp '&' exp   { sp $1 Binop (sl $2 BAnd  ) $1 $3 }
  | exp '|' exp   { sp $1 Binop (sl $2 BOr   ) $1 $3 }
  | exp '~' exp   { sp $1 Binop (sl $2 BXor  ) $1 $3 }
  | exp '<<' exp  { sp $1 Binop (sl $2 ShiftL) $1 $3 }
  | exp '>>' exp  { sp $1 Binop (sl $2 ShiftR) $1 $3 }

  | '-' exp %prec NEG        { sl $1 Unop (sl $1 Neg)        $2 }
  | '~' exp %prec COMPLEMENT { sl $1 Unop (sl $1 Complement) $2 }
  | 'not' exp                { sl $1 Unop (sl $1 Not)        $2 }
  | '#'  exp                 { sl $1 Unop (sl $1 Len)        $2 }

args ::                    { FunArg SourcePos               }
  : '(' sepBy(exp,',') ')' { sl $1 Args $2                  }
  | tableconstructor       { sp $1 TableArg $1              }
  | literalString          { sl $1 StringArg (ltokText $1) }

functiondef ::          { FunDef SourcePos }
  : 'function' funcbody 'end' { sl $1 FunDef $2  }
  | 'function' funcbody error {% noEndP $1 }

funcbody ::                     { FunBody SourcePos                  }
  : '(' parlist ')' block { sl $1 FunBody (fst $2) (snd $2) $4 }

parlist ::              { ([Name SourcePos],Maybe SourcePos) }
  : parnames1 ',' '...' { (reverse $1,Just (ltokPos $3) )     }
  | parnames1           { (reverse $1,Nothing)               }
  | '...'               { ([],Just (ltokPos $1))              }
  |                     { ([],Nothing)                       }

parnames1 ::           { [Name SourcePos] }
  : name               { [$1]             }
  | parnames1 ',' name { $3 : $1          }

tableconstructor ::                 { Table SourcePos          }
  : '{'                         '}' { sl $1 Table []           }
  | '{' fieldlist opt(fieldsep) '}' { sl $1 Table (reverse $2) }

fieldlist ::                  { [TableField SourcePos] }
  : fieldlist fieldsep field  { $3 : $1                }
  | field                     { [$1]                   }

fieldsep :: { () }
  : ','     { () }
  | ';'     { () }

field ::                { TableField SourcePos   }
  : '[' exp ']' '=' exp { sl $1 ExpField $2 $5   }
  | name        '=' exp { sp $1 NamedField $1 $3 }
  |                 exp { sp $1 Field $1         }

name ::   { Name SourcePos            }
  : ident { sl $1 Name (ltokText $1) }

{

data Parser a = Parser
  { runP :: forall r. (SourcePos -> String -> r) -> (a -> [LLexeme] -> r) -> [LLexeme] -> r }

runParser :: Parser a -> [LLexeme] -> Either (String,SourcePos) a
runParser p = runP p (\pos e -> Left (e,pos)) (\x _ -> Right x)

instance Functor     Parser where fmap    = liftM
instance Applicative Parser where pure x  = Parser $ \_ k -> k x
                                  (<*>)   = ap
instance Monad       Parser where return  = pure
                                  m >>= f = Parser $ \e k ->
                                            runP m e $ \a ->
                                            runP (f a) e k

errorP :: LLexeme -> Parser a
errorP LLexeme { ltokPos = pos, ltokToken = t } =
  Parser $ \e _ _ -> e pos ("unexpected " ++ show t)

noEndP :: LLexeme -> Parser a
noEndP LLexeme { ltokPos = pos, ltokToken = t } =
  Parser $ \e _ _ -> e pos ("unterminated " ++ show t)

lexerP :: Parser LLexeme
lexerP = Parser $ \ _ k (l:ls) -> k l ls

sl :: LLexeme -> (SourcePos -> a) -> a
sl x f = f (ltokPos x)

sp :: Annotated p => p SourcePos -> (SourcePos -> a) -> a
sp x f = f (ann x)

blockAnn :: [Stat SourcePos] -> Maybe [Exp SourcePos] -> SourcePos
blockAnn xs mbys =
  case map ann xs ++ maybe [] (map ann) mbys of
    a : _ -> a
    _     -> SourcePos "" 0 1 1

showPos :: SourcePos -> String
showPos p = "line: " ++ show (sourcePosLine p) ++
            " column: " ++ show (sourcePosColumn p)

-- | Runs Lua lexer before parsing. Use @parseNamedText stat "name"@ to parse
-- statements, and @parseText exp "name"@ to parse expressions.
parseNamedText ::
  Parser a ->
  String {- ^ name -} ->
  Text {- ^ chunk -} ->
  Either (String, SourcePos) a
parseNamedText p n xs =
  case runParser p (llexNamed n xs) of
    Left (msg,pos) ->
      Left ("parser error: " ++ show msg, pos)
    Right chunk -> Right chunk

-- | Runs Lua lexer before parsing. Use @parseText stat@ to parse
-- statements, and @parseText exp@ to parse expressions.
parseText ::
  Parser a ->
  Text {- ^ chunk -} ->
  Either (String, SourcePos) a
parseText p = parseNamedText p "=<string>"

-- | Parse a Lua file. You can use @parseText chunk@ to parse a file from a string.
parseFile :: FilePath -> IO (Either (String, SourcePos) (Block SourcePos))
parseFile fp = fmap (parseNamedText chunk fp) (Text.readFile fp)

}
