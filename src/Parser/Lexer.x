{
module Parser.Lexer
  ( lexOne 
  , lexAll
  )
where

import Data.Functor
import Data.Bifunctor
import Data.Function
import Data.Source
import Data.SourceSpan
import Syntax.Token
import Parser.Error 
import Parser.Internal
import Data.Text qualified as Text
}

%encoding "utf8"

$digit  = [0-9]
$alpha  = [a-zA-Z]
$symbol = [\. \, \! \@ \# \$ \% \^ \& \* \( \) \_ \+ \- \= \~ \` \' \" \< \> \? \/ \{ \} \[ \] \\ \|]

$ident     = [$alpha \-]
$span      = [$alpha $digit $symbol $white]
$codeblock = [\x00-\x10ffff] # \x22ef

tokens :- 
  <0> $white* "⟦"          { beginStmt }
  <0> "⟨"                  { beginExpr }
  <0> "⋯" $codeblock* "⋯" { codeblock }
  <0> $span+               { span' }

  -- Statement
  <stmt> $white+     { skip }
  <stmt> $ident+     { ident }
  <stmt> \'          { quote }
  <stmt> "⟧" $white* { endStmt }

  -- Expression
  <expr> $white+ { skip }
  <expr> $ident+ { ident }
  <stmt> \'      { quote }
  <expr> "⟩"     { endExpr }

{

skip :: L Token
skip = liftP lexOne

beginStmt, endStmt :: L Token
beginStmt = do
  modeL stmt 
  tokenL TcSLBracket >>= liftP . pushToken
  liftP lexOne
endStmt = do 
  modeL 0
  tokenL TcSRBracket >>= liftP . pushToken 
  liftP lexOne

ident :: L Token
ident = tokenL =<< TcIdent <$> excerptL

quote :: L Token
quote = tokenL TcQuote

codeblock :: L Token
codeblock = 
  tokenL 
    =<< ( TcCodeblock 
        . Text.strip 
        . Text.drop 1 
        . Text.dropEnd 1
        ) 
          <$> excerptL

beginExpr, endExpr :: L Token
beginExpr = do
  modeL expr 
  tokenL TcSLAngle >>= liftP . pushToken
  liftP lexOne
endExpr = do 
  modeL 0
  tokenL TcSRAngle >>= liftP . pushToken 
  liftP lexOne

span' :: L Token 
span' = tokenL =<< TcSpan <$> excerptL 

scanOne :: P Token
scanOne = do 
  input <- getAlexInput 
  mode <- getAlexMode 

  case alexScan input mode of 
    AlexEOF -> do
      pos' <- getAlexInput <&> pos
      mode' <- getAlexMode
      case mode' of 
        0 -> pure $ Token (mkSourceSpan pos' pos') TcEof
        _ -> throwError $ PUnexpectedEof pos'
    AlexError input' -> do
      throwError $ PUnexpected input'.pos
    AlexSkip input' _ -> do
      setAlexInput input'
      lexOne
    AlexToken input' _ lexlet -> do
      setAlexInput input' 
      runL 
        lexlet 
        input 
        (mkSourceSpan input.pos input'.pos) 
        (input'.offset - input.offset)

lexOne :: P Token
lexOne = popToken' >>= \case
  Just token -> pure token
  Nothing -> scanOne

lexAll :: Source -> Either PError [Token]
lexAll = second fst . runP go . mkPState
  where
    go = lexOne >>= \case
      token@(Token _ TcEof) -> pure [token]
      token -> (token :) <$> go
}