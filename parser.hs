import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

item :: Parser Char
item = P (\inp -> case inp of 
                     [] -> []
                     (x:xs) -> [(x,xs)])

-- praticamente parse è equivalente a app applica la funzione restituendoti il risultato
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

instance Functor Parser where
    --fmap :: (a -> b) -> Parser a -> Parser b
    --fmap f p = P (\inp -> let [(x,xs)] = parse p inp in [(f x,xs)])
    fmap f p = P (\inp -> case parse p inp of
                             [] -> []
                             [(x,xs)] -> [(f x,xs)])

instance Applicative Parser where
    --pure :: a -> Parser a
    pure x = P (\inp -> [(x,inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = P (\inp -> case parse pf inp of 
                              [] -> []
                              [(f,out)] -> parse (fmap f px) out)

instance Monad Parser where
    --(>>=) :: Parser a -> (a -> Parser b) -> Parser b
    px >>= f = P (\inp -> case parse px inp of 
                             [] -> []
                             [(v,out)] -> parse (f v) out)

