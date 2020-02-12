module Parser where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

instance Functor Parser where
    --fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\inp -> case parse p inp of 
                             [] -> []
                             [(v,out)] -> [(f v,out)])

instance Applicative Parser where
    --pure :: a -> Parser a
    pure x = P (\inp -> [(x,inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = P (\inp -> case parse pf inp of
                              [] -> []
                              [(f,out)] -> parse (fmap f px) out)

instance Monad Parser where
    -- >>= :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                            [] -> []
                            [(v,out)] -> parse (f v) out)

instance Alternative Parser where
     -- empty :: Parser a
     empty = P (\inp -> [])

     -- <|> :: Parser a -> Parser a -> Parser a
     p <|> q = P (\inp -> case parse p inp of
                             [] -> parse q inp
                             [(v,out)] -> [(v,out)])
item :: Parser Char
item = P (\inp -> case inp of 
                     [] -> []
                     (x:xs) -> [(x,xs)])

-- praticamente parse è equivalente a app applica la funzione restituendoti il risultato
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

three :: Parser (Char)
three = do x <- item
           item 
           z <- item
           return (x)

sat :: (Char -> Bool) -> Parser Char
sat f = do x <- item
           if f x then pure x else empty
--sat f = P (\inp -> let [(v,out)] = parse item inp
 --                  in case f v of
  --                       False -> []
   --                      True -> [(v,out)])


digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat ( == x)

-- ricordiamoci che return = pure quindi quando viene chiamato return non si fa altro che reinscatolare
-- ricordiamoci un'altra cosetta .. con le monadi quando si usa la do notation ogni istruzione è come se fosse in un relazione di >>= con la successiva
-- quindi lui esegue la prima istruzione e poi applica la seconda al risultato della prima?
string :: String -> Parser String
string [] = return []
--string (x:xs) = (char x) >>= (\i -> string xs >>= (\is -> return (i:is)))
string (x:xs) = do i <- char x
                   is <- string xs 
                   return (i:is)

--p >>= f = P (\inp -> case parse p inp of
 --                       [] -> []
  --                      [(v,out)] -> parse (f v) out)


--Parser (a -> [a] -> [a]) <*> Parser Char

--pf <*> px = P (\inp -> case parse pf inp of
 --                         [] -> []
 --                         [(f,out)] -> parse (fmap f px) out)

--ciao :: String -> Parser String
--ciao (x:xs) = (let i = item x) >>= (let is = ciao xs) >>= (return i:is) 


-- many and some 
-- class Applicative f => Alternative f where
-- empty :: f a 
-- <|> :: f a -> f a -> f a 
-- many :: f a -> f [a]
-- some :: f a -> f [a]

-- many x = some x <|> many []
-- some x = pure (:) <*> x <*> many x

ident :: Parser String 
--ident = lower >>= (\x -> many alphanum >>= (\xs -> return (x:xs)))
ident = do x <- lower
           xs <- many alphanum  
           return (x:xs)

nat :: Parser Int
--nat = some digit >>= (\x -> return (read x))
nat = do x <- some digit
         return (read x)

space :: Parser ()
--space = (many (sat isSpace)) >>= (\x -> return ())
space = do many (sat isSpace)
           return ()

int :: Parser Int
--int = ((char '-') >>= (\x -> nat >>= (\n -> return (-n)))) <|> nat
int = do char '-'
         x <- nat
         return (-x)
         <|>
         nat

token :: Parser a -> Parser a
--token p = (space >>= (\x -> p >>= (\y -> space >>= (\z -> return (y)))))
token p = do space 
             x <- p
             space 
             return x

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

{--
nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol "," 
                         natural)
          symbol "]"
          return (n:ns)

-- arithmetic expressions
-- expr ::= term (+ expr | e)
-- term ::= factor (* term | e)
-- factor ::= (expr) | nat
-- nat ::= 0 | 1 | 2 | ..

expr :: Parser Int
expr = do x <- term
          do symbol "+"
             y <- expr
             return (x + y)
           <|> return x


term :: Parser Int
term = do x <- factor
          do symbol "*"
             y <- term
             return (x * y)
           <|> return x

factor :: Parser Int
factor = do symbol "("
            x <- expr
            symbol ")"
            return x
          <|> natural 

eval :: String -> Int
eval xs = case (parse expr xs) of
               [(n,[])] -> n
               [(_,out)] -> error ("Unused input " ++ out)
               [] -> error ("Invalid syntax")
--}