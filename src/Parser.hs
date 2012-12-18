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


parseLiteral :: Parser (Expression a)
parseLiteral = liftM I (liftM fromInteger integer)
                <|> liftM S string
                <|> (reserved "true" >> (return $ B True))
                <|> (reserved "false" >> (return $ B False))

parseVariable :: Parser (Expression String)
parseVariable = (reserved "this" >> (return $ B True))
                 <|> liftM Var identifier

parsePrimaryExpression :: Parser (Expression String)
parsePrimaryExpression = parseLiteral
                         <|> parseVariable


makeBinaryNode :: ((Expression a) -> (Expression a) -> (Expression a)) ->
                  (Expression a) -> (Expression a) -> (Expression a)
makeBinaryNode _ node Empty  = node
makeBinaryNode _ Empty node = node
makeBinaryNode parentNode node1 node2  = parentNode node1 node2

parseBinaryOp :: Parser (Expression a) -> 
                 Parser ((Expression a) -> (Expression a) -> (Expression a)) ->
                 Parser (Expression a)
parseBinaryOp parseSubExp parseOp = 
  chainl parseSubExp (liftM makeBinaryNode parseOp) Empty

parseMultiplicativeOp :: Parser ((Expression a) -> (Expression a) -> (Expression a))
parseMultiplicativeOp = (reservedOp "*" >> (return $ Multiplicative "*"))
                        <|> (reservedOp "/" >> (return $ Multiplicative "/"))

parseMultiplicativeExpression :: Parser (Expression String)
parseMultiplicativeExpression =
  parseBinaryOp parsePrimaryExpression parseMultiplicativeOp
