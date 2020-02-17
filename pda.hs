type Stack = String
newtype PDA a = P(Stack -> [(a, Stack)])

app :: PDA a -> Stack -> [(a,Stack)]
app (P p) s = p s

instance Functor PDA where
    --fmap :: (a -> b) -> PDA a -> PDA b
    fmap f p = P(\s -> case app p s of
                          [] -> []
                          [(v,out)] -> [(f v,out)])

instance Applicative PDA where
    --pure :: a -> PDA a
    pure x = P(\s -> [(x,s)])

    -- <*> :: PDA (a -> b) -> PDA a -> PDA b
    pf <*> px = P(\s -> case app pf s of
                           [] -> []
                           [(f,s')] -> app (fmap f px) s')

instance Monad PDA where
    -- >>=:: PDA a -> (a -> PDA b) -> PDA b

    p >>= f = P(\s -> case app p s of
                         [] -> []
                         [(v,s')] -> app (f v) s')

pop :: PDA Char
pop = P(\s-> case s of
                [] -> []
                (x:xs) -> [(x,xs)])

push :: PDA ()
push = P(\s-> [((), '(':s)])

balance :: String -> PDA Bool
balance [] = P(\s -> case s of
                        [] -> [(True,s)])
                        _ -> [(False,s)]

balance ('(':xs) = do push 
                      balance xs

balance (')':xs) = P(\s -> case app pop s 
                              [] -> [(False,xs)]
                              [s,out] -> )
                       

def :: String -> PDA String
def [] =  pure []
def (x:xs) = (def xs) >>= (\vs -> if x == ')' then (push >>= (\v -> return (')':vs))) else (pop >>= (\v -> return ('(':vs))))

def' :: String -> PDA String
def' [] = pure []
def' (x:xs) = do vs <- def' xs
                 if x == ')' then do push 
                                     return (')':vs)
                             else do pop
                                     return ('(':vs)

exec :: String -> IO ()
exec xs = case app (balance xs) [] of
             [(True,"")] -> putStr ("Correct" ++ "\n")
             [(True,s)] -> putStr ("Fail :" ++ s ++ "\n")
             [(False,s)] -> putStr ("Fail" ++ "\n")

 