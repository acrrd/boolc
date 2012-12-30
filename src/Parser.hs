module Parser where

import Ast

import Control.Monad
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
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
semi       = Token.semi       lexer
identifier = Token.identifier lexer
decimal    = Token.decimal    lexer
stringLit  = Token.stringLiteral lexer

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
parseLiteral = liftM I (liftM fromInteger decimal)
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
parsePostfixExpression = parsePrimaryExpression

parseUnaryExpressionNotPlusMinus :: Parser Expression
parseUnaryExpressionNotPlusMinus = parsePostfixExpression

parseUnaryExpression :: Parser Expression
parseUnaryExpression = parseUnaryExpressionNotPlusMinus

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
