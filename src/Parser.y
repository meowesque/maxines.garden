{
module Parser 
  ( parseDocument
  )
where
 
import Parser.Error 
import Parser.Lexer
import Parser.Internal
import Syntax.Obj
import Syntax.Token
import Data.List.NonEmpty
}

%tokentype { Token }
%monad { P } { (>>=) } { pure }
%lexer { lexer } { Token _ TcEof }
%error { parseError }
%errorhandlertype explist 

%name parseDocument Document

%token
  '⟨'       { Token _ TcSLAngle }
  '⟩'       { Token _ TcSRAngle }
  '⟦'       { Token _ TcSLBracket }
  '⟧'       { Token _ TcSRBracket }
  '('       { Token _ TcLParen }
  ')'       { Token _ TcRParen }
  quote     { Token _ TcQuote }
  strlit    { Token _ (TcStrLit _) }
  intlit    { Token _ (TcIntLit _) }
  ident     { Token _ (TcIdent _) }
  span      { Token _ (TcSpan _) }
  codeblock { Token _ (TcCodeblock _) }

%%

Expr :: { Obj }
Expr : '(' Exprs1 ')' { Obj ($1.span <> $3.span) (OcList $2) }
     | quote Expr { Obj ($1.span <> $2.span) (OcQuote $2) }
     | ident { Obj $1.span (OcIdent $ tcContent $1.class_) }

Exprs :: { [Obj] }
Exprs : Exprs Expr { $1 <> [$2] }
      | Expr { [$1] }

Exprs1 :: { NonEmpty Obj }
Exprs1 : Expr Exprs { $1 :| $2 }  
       | Expr { $1 :| [] }

Directive :: { Obj }
Directive : '⟦' Exprs1 '⟧' { Obj ($1.span <> $3.span) (OcDirective $2) }

InlineList :: { Obj }
InlineList : '⟨' Exprs1 '⟩' { Obj ($1.span <> $3.span) (OcList $2) }

Toplevel :: { Obj }
Toplevel : Directive { $1 }
         | InlineList { $1 }
         | span { Obj $1.span (OcSpan $ tcContent $1.class_) }
         | codeblock { Obj $1.span (OcCodeblock $ tcContent $1.class_) }

Document :: { [Obj] }
Document : Document Toplevel { $1 <> [$2] }
         | Toplevel { [$1] }

{
lexer :: (Token -> P a) -> P a 
lexer = (lexOne >>=)

parseError :: (Token, [String]) -> P a 
parseError = throwError . uncurry PUnexpectedToken
}
