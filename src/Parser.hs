module Parser where

import Ast

import Control.Monad
import Text.ParserCombinators.Parsec hiding (string)
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
integer    = Token.integer    lexer
string     = Token.stringLiteral lexer


parseExpression :: Parser Expression
parseExpression = parseAdditiveExpression

parseLiteral :: Parser Expression
parseLiteral = liftM I (liftM fromInteger integer)
                <|> liftM S string
                <|> (reserved "true" >> (return $ B True))
                <|> (reserved "false" >> (return $ B False))

parseVariable :: Parser Expression
parseVariable = (reserved "this" >> (return $ Var "this"))
                 <|> liftM Var identifier

parsePrimaryExpression :: Parser Expression
parsePrimaryExpression = parseLiteral
                         <|> parseVariable
                         <|> parens parseExpression

makeBinaryNode :: (Expression -> Expression -> Expression) ->
                  Expression -> Expression -> Expression
makeBinaryNode _ node Empty  = node
makeBinaryNode _ Empty node = node
makeBinaryNode parentNode node1 node2  = parentNode node1 node2

parseBinaryOp :: Parser Expression -> 
                 Parser (Expression -> Expression -> Expression) ->
                 Parser Expression
parseBinaryOp parseSubExp parseOp = 
  chainl parseSubExp (liftM makeBinaryNode parseOp) Empty

parseMultiplicativeOp :: Parser (Expression -> Expression -> Expression)
parseMultiplicativeOp = (reservedOp "*" >> (return $ Multiplicative "*"))
                        <|> (reservedOp "/" >> (return $ Multiplicative "/"))

parseMultiplicativeExpression :: Parser Expression
parseMultiplicativeExpression =
  parseBinaryOp parsePrimaryExpression parseMultiplicativeOp

parseAdditiveOp :: Parser (Expression -> Expression -> Expression)
parseAdditiveOp = (reservedOp "+" >> (return $ Additive "+"))
                        <|> (reservedOp "-" >> (return $ Additive "-"))

parseAdditiveExpression :: Parser Expression
parseAdditiveExpression =
  parseBinaryOp parseMultiplicativeExpression parseAdditiveOp
