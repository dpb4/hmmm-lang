module Hmmm.Parser (
    runParse,
    parseAbs,
    parseApp,
    parseAtom,
    parseExpr,
    parseId,
    parseIf,
    parseLet,
    parseLit,
) where

import Hmmm.Syntax

import Data.Char (isLetter)
import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String
import qualified Text.Parsec.Token as Tok

-- | parse a whole input string; leading whitespace is allowed, trailing input is not
runParse :: String -> Either String Expr
runParse input =
    case parse (whiteSpace *> parseExpr <* eof) "input" input of
        Left err -> Left $ show err
        Right expr -> Right expr

{- | every token parser built from the lexer consumes the whitespace after it,
so no other parser needs to deal with whitespace
-}
lexer :: Tok.TokenParser ()
lexer =
    Tok.makeTokenParser
        emptyDef
            { Tok.identStart = satisfy (\c -> isLetter c && c /= 'λ')
            , Tok.identLetter = alphaNum
            , Tok.reservedNames = ["if", "then", "else", "let", "in", "true", "false"]
            , Tok.reservedOpNames = ["+", "-", "->", "=", "\\", "λ"]
            }

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

opTable :: OperatorTable String () Identity Expr
opTable =
    [
        [ Infix (EOp OAdd <$ reservedOp "+") AssocLeft
        , Infix (EOp OSub <$ reservedOp "-") AssocLeft
        ]
    ]

-- | operators bind looser than application: f x + 1 = (f x) + 1
parseExpr :: Parser Expr
parseExpr = buildExpressionParser opTable parseApp

-- | application is left-associative: f x y = (f x) y
parseApp :: Parser Expr
parseApp = foldl1 EApp <$> many1 parseAtom

parseAtom :: Parser Expr
parseAtom =
    parseIf
        <|> parseLet
        <|> parseAbs
        <|> parseLit
        <|> (EVar <$> parseId)
        <|> parens parseExpr

-- | an identifier that is not a reserved word
parseId :: Parser Name
parseId = identifier

parseIf :: Parser Expr
parseIf =
    EIf
        <$> (reserved "if" *> parseExpr)
        <*> (reserved "then" *> parseExpr)
        <*> (reserved "else" *> parseExpr)

parseAbs :: Parser Expr
parseAbs =
    EAbs
        <$> ((reservedOp "\\" <|> reservedOp "λ") *> parseId)
        <*> (reservedOp "->" *> parseExpr)

parseLet :: Parser Expr
parseLet =
    ELet
        <$> (reserved "let" *> parseId)
        <*> (reservedOp "=" *> parseExpr)
        <*> (reserved "in" *> parseExpr)

parseLit :: Parser Expr
parseLit =
    ELit
        <$> ( (LInt <$> Tok.natural lexer)
                <|> (LBool True <$ reserved "true")
                <|> (LBool False <$ reserved "false")
                <|> (LStr <$> Tok.stringLiteral lexer)
            )
