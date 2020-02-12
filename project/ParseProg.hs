module ParseProg where
import Control.Applicative

import Parser
type Name = String
type Def a = (a, Expr a)
type Alter a = (Int, [a], Expr a)
data IsRec = NonRecursive | Recursive deriving Show
type Program a = [ScDefn a]
type CoreProgram = Program Name
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

data Expr a
  =  Evar Name
   | Enum Int
   | EConstr Int Int
   | Eap (Expr a) (Expr a)
   | Elet
        IsRec
        [(a,Expr a)]
        (Expr a)
   | ECase 
        (Expr a)
        [Alter a]
   | Elam [a] (Expr a)
    deriving Show

parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do char ';'
                  ps <- parseProg
                  return (p:ps)
                <|> return [p]

parseScDef :: Parser (ScDefn Name)
parseScDef = do v <- parseVar
                pf <- many parseVar
                char '='
                body <- parseExpr -- call to parseExpr
                return (v, pf, body)

parseExpr :: Parser (Expr Name)
parseExpr = do x <- parseLet
               return x
            <|> do x <- parseLetrec
                   return x
            <|> do x <- parseCase
                   return x
            <|> do x <- parseLam
                   return x
            <|> do x <- parseAExpr
                   return x
            <|> do x <- parseExpr1
                   return x

parseAExpr :: Parser (Expr Name)
parseAExpr = do e <- parseVar
                return (Evar e)
             <|> do x <- natural
                    return (Enum x)
             <|> do symbol "Pack{"
                    n <- natural
                    symbol ","
                    m <- natural 
                    symbol "}"
                    return (EConstr n m) 
             <|> do symbol "("
                    e <- parseExpr
                    symbol ")"
                    return e
                 

parseLet :: Parser (Expr Name)
parseLet = do symbol "let"
              d <- parseDefns
              symbol "in"
              e <- parseExpr
              return (Elet NonRecursive d e)

parseLetrec :: Parser (Expr Name)
parseLetrec = do symbol "letrec"
                 d <- parseDefns
                 symbol "in"
                 e <- parseExpr
                 return (Elet Recursive d e)

parseDefn :: Parser (Def Name)
parseDefn = do v <- identifier
               symbol "="
               e <- parseExpr
               return (v,e)

parseDefns :: Parser ([Def Name])
parseDefns = do d <- parseDefn
                ds <- many (do symbol ";" 
                               parseDefn)
                return (d:ds)

parseCase :: Parser (Expr Name)
parseCase = do symbol "case"
               e <- parseExpr
               symbol "of"
               a <- parseAlts
               return (ECase e a)

parseAlt :: Parser (Alter Name)
parseAlt = do symbol "<"
              n <- natural
              symbol ">"
              as <- many (do identifier)
              symbol "->"
              e <- parseExpr
              return (n,as,e)

parseAlts :: Parser ([Alter Name])
parseAlts = do a <- parseAlt
               as <- many (do symbol ";"
                              parseAlt)
               return (a:as)

parseLam :: Parser (Expr Name)
parseLam = do symbol "\\"
              a <- identifier
              as <- many (do identifier)
              symbol "."
              e <- parseExpr
              return (Elam (a:as) e)

parseVar:: Parser String
parseVar = token var

var :: Parser String
var = do x <- lower
         xs <- many (alphanum <|> (char '_'))
         return (x:xs)

parseExpr1:: Parser (Expr Name)
parseExpr1 = do e2 <- parseExpr2 
                do symbol "|"
                   e1 <- parseExpr1
                   return (Eap (Eap (Evar "|") e2) e1)
                 <|> return e2

parseExpr2:: Parser (Expr Name)
parseExpr2 = do e3 <- parseExpr3 
                do symbol "&"
                   e2 <- parseExpr2
                   return (Eap (Eap (Evar "&") e3) e2)
                 <|> return e3

parseExpr3:: Parser (Expr Name)
parseExpr3 = do e4 <- parseExpr4
                do r <- parseRelop
                   e4' <- parseExpr4
                   return (Eap (Eap r e4) e4')
                 <|> return e4

parseRelop :: Parser (Expr Name)
parseRelop = do symbol "<"
                return (Evar "<")
             <|> do symbol "<="
                    return (Evar "<=")
             <|> do symbol "=="
                    return (Evar "==")
             <|> do symbol "~="
                    return (Evar "~=")
             <|> do symbol ">="
                    return (Evar ">=")
             <|> do symbol ">"
                    return (Evar ">")

parseExpr4:: Parser (Expr Name)
parseExpr4 = do e5 <- parseExpr5
                do symbol "+"
                   e4 <- parseExpr4
                   return (Eap (Eap (Evar "+") e5) e4)
                 <|> do symbol "-"
                        e5' <- parseExpr5
                        return (Eap (Eap (Evar "-") e5) e5')
                 <|> return e5

parseExpr5 :: Parser (Expr Name)
parseExpr5 = do e6 <- parseExpr6
                do symbol "*"
                   e5 <- parseExpr5
                   return (Eap (Eap (Evar "*") e6) e5)
                 <|> do symbol "/"
                        e6' <- parseExpr6
                        return (Eap (Eap (Evar "/") e6) e6')
                 <|> return e6

parseExpr6 :: Parser (Expr Name)
parseExpr6 = do ae <- parseAExpr
                do e6 <- parseExpr6
                   return (Eap ae e6)
                 <|> return ae

              