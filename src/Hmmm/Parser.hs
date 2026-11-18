module Hmmm.Parser (
    runParse,
    parseAbs,
    parseApp,
    parseAtom,
    parseBinding,
    parseExpr,
    parseId,
    parseIf,
    parseLet,
    parseLit,
    parseType,
    runParseTopExprs,
    parseTopExpr,
    parseTopLet,
    parseTopFun,
) where

import Hmmm.Syntax
import Hmmm.Types

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

{- | every token parser consumes the whitespace after,
SO NO OTHER PARSER needs to deal with whitespace
-}
lexer :: Tok.TokenParser ()
lexer =
    Tok.makeTokenParser
        emptyDef
            { Tok.identStart = satisfy (\c -> isLetter c && c /= 'λ')
            , Tok.identLetter = alphaNum
            , Tok.reservedNames = ["if", "then", "else", "let", "in", "true", "false"]
            , Tok.reservedOpNames = ["+", "-", "->", "=", ":", "\\", "λ", ","]
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
        [ Infix (EOp OAdd <$ (continued *> reservedOp "+")) AssocLeft
        , Infix (EOp OSub <$ (continued *> reservedOp "-")) AssocLeft
        ]
    ]

-- | operators bind looser than application: f x + 1 = (f x) + 1
parseExpr :: Parser Expr
parseExpr = buildExpressionParser opTable parseApp

{- | succeeds only if the next token is not at the start of a line;
a token in column 1 begins a new top-level item, so an expression can only
continue onto the next line if that line is indented (a simplified offside rule)
-}
continued :: Parser ()
continued = do
    col <- sourceColumn <$> getPosition
    if col > 1 then return () else parserZero

-- | application is left-associative: f x y = (f x) y
parseApp :: Parser Expr
parseApp = foldl1 EApp <$> ((:) <$> parseAtom <*> many (continued *> parseAtom))

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

{- | an annotated lambda parameter needs parentheses;
@λx: int -> x@ is ambiguous, the arrow could belong to the type or the lambda
-}
parseAbs :: Parser Expr
parseAbs =
    EAbs
        <$> ((reservedOp "\\" <|> reservedOp "λ") *> (parens parseBinding <|> unannotated))
        <*> (reservedOp "->" *> parseExpr)
  where
    unannotated = (`Bind` Nothing) <$> parseId

parseLet :: Parser Expr
parseLet =
    ELet
        <$> (reserved "let" *> parseBinding)
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

-- | a name with an optional type annotation: @x@ or @x: T@
parseBinding :: Parser Binding
parseBinding = Bind <$> parseId <*> optionMaybe (reservedOp ":" *> parseType)

-- | function types are right-associative: int -> int -> int = int -> (int -> int)
parseType :: Parser Type
parseType = foldr1 (\a b -> TC (TCFun a b)) <$> sepBy1 parseTypeAtom (reservedOp "->")

parseTypeAtom :: Parser Type
parseTypeAtom = parens parseType <|> (concrete =<< identifier) <?> "type"
  where
    concrete name = case name of
        "int" -> return $ T CInt
        "bool" -> return $ T CBool
        "str" -> return $ T CStr
        _ -> unexpected $ "type name " ++ show name

{- | a top-level let is a function definition, a value definition, or,
if followed by @in@, an ordinary let expression
-}
parseTopLet :: Parser TopExpr
parseTopLet = do
    name <- reserved "let" *> parseId
    parseTopFun name <|> parseTopValue name

parseTopValue :: Name -> Parser TopExpr
parseTopValue name = do
    bind <- Bind name <$> optionMaybe (reservedOp ":" *> parseType)
    val <- reservedOp "=" *> parseExpr
    -- if val is followed by @in@, parse the body of the let
    option (TELet bind val) (TEExpr . ELet bind val <$> (reserved "in" *> parseExpr))

parseTopFun :: Name -> Parser TopExpr
parseTopFun name =
    returnTypeLast
        <$> parens (sepBy parseBinding (reservedOp ","))
        <*> optionMaybe (reservedOp ":" *> parseType)
        <*> (reservedOp "=" *> parseExpr)
  where
    returnTypeLast params rett = TEFun (Bind name rett) params

parseTopExpr :: Parser TopExpr
parseTopExpr = parseTopLet <|> (TEExpr <$> parseExpr)

runParseTopExprs :: String -> Either String [TopExpr]
runParseTopExprs input =
    case parse (whiteSpace *> many1 parseTopExpr <* eof) "input" input of
        Left err -> Left $ show err
        Right exprs -> Right exprs
