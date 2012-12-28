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
integer    = Token.integer    lexer
string     = Token.stringLiteral lexer


makeBinaryNode :: (Expression -> Expression -> Expression) ->
                  Expression -> Expression -> Expression
makeBinaryNode _ node Empty  = node
makeBinaryNode _ Empty node = node
makeBinaryNode parentNode node1 node2  = parentNode node1 node2

parseBinaryOp :: (String -> Expression -> Expression -> Expression) ->
                 [String] ->
                 Parser (Expression -> Expression -> Expression)
parseBinaryOp node ops = choice $ map (\op -> reservedOp op >> (return $ node op)) ops

parseBinaryExpression :: Parser Expression -> 
                         (String -> Expression -> Expression -> Expression) ->
                         [String] ->
                         Parser Expression
parseBinaryExpression parseSubExp node ops = 
  chainl parseSubExp (liftM makeBinaryNode (parseBinaryOp node ops)) Empty

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

parseMultiplicativeExpression :: Parser Expression
parseMultiplicativeExpression =
  parseBinaryExpression parsePrimaryExpression Multiplicative ["*","/"]

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
