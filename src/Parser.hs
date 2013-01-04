module Parser where

import Ast

import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef = emptyDef
                { commentStart	 = "/*"
                , commentEnd	 = "*/"
                , commentLine	 = "//"
                , nestedComments = True
                , identStart	 = letter
                , identLetter	 = alphaNum <|> oneOf "_'"
                , reservedNames  = [ "class"
                                    ,"extends"
                                    ,"if"
                                    ,"else"
                                    ,"while"
                                    ,"return"
                                    ,"new"
                                    ,"this"
                                    ,"null"
                                    ,"true"
                                    ,"false"
                                    ,"void"
                                    ,"int"
                                    ,"bool"
                                   ]
                , reservedOpNames= [ "||","&&"
                                    ,"==","!="
                                    ,"<","<=",">",">="
                                    ,"+","-"
                                    ,"*","/"
                                    ,"!"
                                    ,"="
                                   ]
                , caseSensitive  = False
                }


lexer = Token.makeTokenParser languageDef

whiteSpace = Token.whiteSpace lexer
reserved   = Token.reserved   lexer
--reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
semi       = Token.semi       lexer
identifier = Token.identifier lexer
natural    = Token.natural    lexer
stringLit  = Token.stringLiteral lexer
commaSep  = Token.commaSep lexer
dot  = Token.dot lexer

reservedOpR :: String -> Parser String
reservedOpR op = do currentop <- lookAhead oper                   
                    let maxop = maximumValidOp currentop
                    if maxop == op then symbol op else unexpected maxop
  where oper = do{ c <- (opStart languageDef)
                 ; cs <- many (opLetter languageDef)
                 ; return (c:cs)
                 }
               <?> "operator"
        maximumValidOp op = foldr (\x a -> if (elem x reservedOps) then x else a) "" $ subOps op
        reservedOps = reservedOpNames languageDef
        subOps op = (reverse $ (inits op) ++ [op])
        symbol = Token.symbol lexer

reservedOp :: String -> Parser ()
--reservedOp op = (Token.lexeme lexer $ reservedOpR op) >> return ()
reservedOp op = reservedOpR op >> return ()

parseBinaryOp :: (String -> Expression -> Expression -> Expression) ->
                 [String] ->
                 Parser (Expression -> Expression -> Expression)
parseBinaryOp node ops = choice $ map (\op -> reservedOp op >> (return $ node op)) ops

parseBinaryExpression :: Parser Expression -> 
                         (String -> Expression -> Expression -> Expression) ->
                         [String] ->
                         Parser Expression
parseBinaryExpression parseSubExp node ops = 
  chainl1 parseSubExp (parseBinaryOp node ops)

parseLiteral :: Parser Expression
parseLiteral = liftM I (liftM fromInteger natural)
                <|> liftM S stringLit
                <|> (reserved "true" >> (return $ B True))
                <|> (reserved "false" >> (return $ B False))

parseVariable :: Parser Expression
parseVariable = (reserved "this" >> (return $ Var "this"))
                 <|> liftM Var identifier

parsePrimaryExpression :: Parser Expression
parsePrimaryExpression = parseLiteral
                         <|> parseVariable
                         <|> parens parseExpression

parsePostfixExpression :: Parser Expression
parsePostfixExpression =  liftM2 (foldl (flip ($))) parsePrimaryExpression parseMembersAccess
  where parseMembersAccess = (dot >> sepBy1 parseMemberAccess dot) <|> return []
        parseMemberAccess = do id <- identifier
                               option (FieldAccess id) $ liftM (MethodCall id) (parens $ commaSep parseExpression)

parsePrimitiveTypes :: Parser TypeName
parsePrimitiveTypes = 
  choice $ map (\t -> reserved t >> (return t)) ["bool","int","void"]

parseUnaryExpressionNotPlusMinus :: Parser Expression
parseUnaryExpressionNotPlusMinus = 
  (reservedOp "!" >> liftM Not (parseUnaryExpression))
  <|> liftM2 Cast (try $ parens parsePrimitiveTypes) parseUnaryExpression
  <|> (try $ liftM2 Cast (parens identifier) parseUnaryExpressionNotPlusMinus)
  <|> parsePostfixExpression

parseUnaryExpression :: Parser Expression
parseUnaryExpression = (reservedOp "-" >> liftM Negative (parseUnaryExpression))
                       <|> parseUnaryExpressionNotPlusMinus

parseMultiplicativeExpression :: Parser Expression
parseMultiplicativeExpression =
  parseBinaryExpression parseUnaryExpression Multiplicative ["*","/"]

parseAdditiveExpression :: Parser Expression
parseAdditiveExpression =
  parseBinaryExpression parseMultiplicativeExpression Additive ["+","-"]

parseRelationalExpression :: Parser Expression
parseRelationalExpression = 
  parseBinaryExpression parseAdditiveExpression Relational ["<","<=",">",">="]

parseEqualityExpression :: Parser Expression
parseEqualityExpression = 
  parseBinaryExpression parseRelationalExpression Equality ["==","!="]

parseBooleanAndExpression :: Parser Expression
parseBooleanAndExpression = 
  parseBinaryExpression parseEqualityExpression Boolean ["&&"]

parseBooleanOrExpression :: Parser Expression
parseBooleanOrExpression =
  parseBinaryExpression parseBooleanAndExpression Boolean ["||"]

parseExpression :: Parser Expression
parseExpression = parseBooleanOrExpression
