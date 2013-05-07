module Parser where

import Ast

import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

type SourcePosInfo = SourcePos
type ExpressionSP = Expression SourcePosInfo
type StatementSP = Statement SourcePosInfo
type FieldDeclSP = FieldDecl SourcePosInfo
type MethodDeclSP = MethodDecl SourcePosInfo
type ClassDeclSP = ClassDecl SourcePosInfo
type ProgramSP = Program SourcePosInfo

data MemberSP = F FieldDeclSP | M MethodDeclSP

runBoolParser :: SourceName -> String -> Either ParseError ProgramSP
runBoolParser fn = (parse parseBool fn)

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
operator  = Token.operator lexer
braces =  Token.braces lexer

parseBool :: Parser ProgramSP
parseBool = do whiteSpace
               r <- parseProgram
               eof
               return r


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

parseBinaryOp :: (SourcePos -> String -> ExpressionSP -> ExpressionSP -> ExpressionSP) ->
                 [String] ->
                 Parser (ExpressionSP -> ExpressionSP -> ExpressionSP)
parseBinaryOp node ops = choice $ map (\op -> liftM3 (const.node) getPosition (reservedOp op) (return op)) ops

parseBinaryExpression :: Parser ExpressionSP -> 
                         (SourcePos -> String -> ExpressionSP -> ExpressionSP -> ExpressionSP) ->
                         [String] ->
                         Parser ExpressionSP
parseBinaryExpression parseSubExp node ops = 
  chainl1 parseSubExp (parseBinaryOp node ops)

parseLiteral :: Parser ExpressionSP
parseLiteral =  liftM2 I getPosition (liftM fromInteger natural)
                <|> liftM2 S getPosition stringLit
                <|> liftM2 B getPosition parseTrue
                <|> liftM2 B getPosition parseFalse
                <|> liftM2 (const.Null) getPosition (reserved "null")
  where parseTrue = reserved "true" >> (return True)
        parseFalse = reserved "false" >> (return False)

parseVariable :: Parser ExpressionSP
parseVariable = liftM2 Var getPosition (parseThis <|> identifier)
  where parseThis = reserved "this" >> (return "this")

parsePrimaryExpression :: Parser ExpressionSP
parsePrimaryExpression = parseLiteral
                         <|> parseVariable
                         <|> parens parseExpression
                         <|> (liftM4 (\p _ -> New p) getPosition (reserved "new")  identifier $ parens $ commaSep parseExpression)

parsePostfixExpression :: Parser ExpressionSP
parsePostfixExpression =  liftM2 (foldl (flip ($))) parsePrimaryExpression parseMembersAccess
  where parseMembersAccess = (dot >> sepBy1 parseMemberAccess dot) <|> return []
        parseMemberAccess = do pos <- getPosition
                               id <- identifier
                               option (FieldAccess pos id) $ liftM (MethodCall pos id) (parens $ commaSep parseExpression)

parsePrimitiveTypes :: Parser TypeName
parsePrimitiveTypes = 
  choice $ map (\t -> reserved t >> (return t)) ["bool","int","void"]

parseUnaryExpressionNotPlusMinus :: Parser ExpressionSP
parseUnaryExpressionNotPlusMinus = 
  liftM3 (\p _ -> Not p) getPosition (reservedOp "!") parseUnaryExpression
  <|> liftM3 Cast getPosition (try $ parens parsePrimitiveTypes) parseUnaryExpression
  <|> (try $ liftM3 Cast getPosition (parens identifier) parseUnaryExpressionNotPlusMinus)
  <|> parsePostfixExpression

parseUnaryExpression :: Parser ExpressionSP
parseUnaryExpression = liftM3 (const.Negative) getPosition (reservedOp "-") parseUnaryExpression
                       <|> parseUnaryExpressionNotPlusMinus

parseMultiplicativeExpression :: Parser ExpressionSP
parseMultiplicativeExpression =
  parseBinaryExpression parseUnaryExpression Multiplicative ["*","/"]

parseAdditiveExpression :: Parser ExpressionSP
parseAdditiveExpression =
  parseBinaryExpression parseMultiplicativeExpression Additive ["+","-"]

parseRelationalExpression :: Parser ExpressionSP
parseRelationalExpression = 
  parseBinaryExpression parseAdditiveExpression Relational ["<","<=",">",">="]

parseEqualityExpression :: Parser ExpressionSP
parseEqualityExpression = 
  parseBinaryExpression parseRelationalExpression Equality ["==","!="]

parseBooleanAndExpression :: Parser ExpressionSP
parseBooleanAndExpression = 
  parseBinaryExpression parseEqualityExpression Boolean ["&&"]

parseBooleanOrExpression :: Parser ExpressionSP
parseBooleanOrExpression =
  parseBinaryExpression parseBooleanAndExpression Boolean ["||"]

parseExpression :: Parser ExpressionSP
parseExpression = parseBooleanOrExpression

endStm :: Parser StatementSP -> Parser StatementSP
endStm stmparser = do x <- stmparser
                      semi
                      return x

parseType :: Parser TypeName
parseType = try parsePrimitiveTypes <|> identifier

parseStatement :: Parser StatementSP
parseStatement = choice [parseBlockStatement,
                         parseNoOpStatement,
                         try parseDeclarationStatement,
                         parseExpStmOrAssignStatement,
                         parseIfStatement,
                         parseWhileStatement,
                         parseReturnStatement
                        ]

parseNoOpStatement :: Parser StatementSP
parseNoOpStatement = liftM2 (const.NoOp) getPosition semi

parseDeclarationStatement :: Parser StatementSP
parseDeclarationStatement = endStm $ liftM3 Declaration getPosition parseType identifier

parseExpStmOrAssignStatement :: Parser StatementSP
parseExpStmOrAssignStatement = endStm $ do e1 <- parseExpression
                                           option (ExpStm e1) $ parseAssign e1
  where parseAssign e = do pos <- getPosition
                           reservedOp "="
                           liftM3 Assign (return pos) (return e) parseExpression

parseIfStatement :: Parser StatementSP
parseIfStatement = liftM4 If parsePos parseCond parseThen parseElse
  where parsePos = reserved "if" >> getPosition
        parseCond = parens parseExpression
        parseThen = parseStatement
        parseElse = do pos <- getPosition
                       option (NoOp pos) (reserved "else" >> parseStatement)

parseWhileStatement :: Parser StatementSP
parseWhileStatement = liftM3 While parsePos parseCond parseStatement
  where parsePos = reserved "while" >> getPosition
        parseCond = parens parseExpression

parseReturnStatement :: Parser StatementSP
parseReturnStatement = endStm $ liftM2 Return parsePos parseExpression
  where parsePos = reserved "return" >> getPosition

parseBlockStatement :: Parser StatementSP
parseBlockStatement = liftM Block $ braces $ many parseStatement

-- Reuse declaration parser
parseFieldDecl :: Parser FieldDeclSP
parseFieldDecl = do d <- parseDeclarationStatement
                    let (Declaration pos t v) = d
                    return $ FieldDecl pos t v

parseMethodDecl :: Parser MethodDeclSP
parseMethodDecl = liftM5 MethodDecl getPosition parseType identifier parseParameterDecl parseBody
  where parseParameterDecl = parens $ commaSep $ liftM3 ParameterDecl getPosition parseType identifier
        parseBody = parseBlockStatement

parseMember :: Parser MemberSP
parseMember =  (try $ liftM M parseMethodDecl)
               <|> liftM F parseFieldDecl

parseClassDecl :: Parser ClassDeclSP
parseClassDecl = liftM4 buildClassDecl getPosition parseClassName parseExtends parseClassBody
  where parseClassName = reserved "class" >> identifier
        parseExtends = option "" (reserved "extends" >> identifier)
        parseClassBody = braces $ many parseMember
        buildClassDecl i cn pn ms = let partms =  partition isField ms in
                                      ClassDecl i cn pn (map ef $ fst partms) (map em $snd partms)
        isField m = case m of 
                      F _ -> True
                      _ -> False
        ef (F f) = f
        em (M m) = m

parseProgram :: Parser ProgramSP
parseProgram = liftM Program $ many parseClassDecl

