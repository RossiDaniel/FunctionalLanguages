import Data.Char

let2int:: Char ->Int
let2int c = ord c - ord 'a'

int2let:: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

freqitalian :: [Float]
freqitalian = [11.74,0.92,4.50,3.73,11.79,0.95,1.64,1.54,11.28,0.01,0.01,6.51,2.51,6.88,9.83,3.05,0.51,6.37,4.98,5.62,3.01,2.10,0.01,0.01,0.01,0.49]
alpha = ['a'..'z']

percent' a b = ( (fromIntegral a) / (fromIntegral b) ) * 100

currentfreq xs= [percent' (sum [1|x<-xs , x == y]) (sum [1|x<-xs]) | y <- alpha]

singleError :: Float -> Float -> Float
singleError esi osi =  ((osi - esi)^2 ) / esi

error' :: [Float] -> [Float] -> Float
error' [] [] = 0
error' (e:es) (o:os) = (singleError e o) + (error' es os)

error'' esi osi = sum [((o - e)^2 ) / e| (o,e) <- zip osi esi]

rotate :: [Float] -> [Float]
rotate xs = head(reverse xs) : reverse ( tail (reverse xs))  

min' (err1,s1) (err2,s2) | err1 > err2 = (err2,s2)
                        | otherwise   = (err1,s1)

general :: [Float] -> Int -> (Float,Int)
general _ 26 = (1000000000,26)
general xs s = min' ((error'' freqitalian xs),s) (general (rotate xs) (s+1))

example :: String
example = encode 1 "Nella crittoanalisi, l'analisi delle frequenze lo studio della frequenza di utilizzo delle lettere o gruppi di lettere in un testo cifrato. Questo metodo è utilizzato per violare i cifrari classici. Le indagini quantitative sui testi si servono spesso di qualche forma di analisi delle frequenze.Possono essere interessanti le analisi delle frequenze di caratteri, di parole, di gruppi di parole che si possono assegnare a lemmi o significati definiti; queste analisi possono riguardare un singolo testo (da un frammento epigrafico, a un'opera come la Divina commedia), un intero corpus letterario o un opportuno campione di un linguaggio specialistico o di un'intera lingua."

example1 :: String
example1 = encode 10 "Importanti furono anche i contatti con il gruppo degli amici bresciani, aperti alle influenze francesi e rivoluzionarie, e con Melchiorre Cesarotti, traduttore dei Canti di Ossian, per il quale Foscolo cominciò a nutrire una notevole ammirazione, giungendo ad intessere rapporti con i letterati che vedevano in lui il loro modello e padre spirituale, e contattandolo personalmente con una missiva del 28 settembre 1795.[19] Il 30 ottobre del 1795 inviò per un parere al Cesarotti, docente presso lo Studio padovano, il manoscritto della tragedia Tieste, di carattere alfieriano e viva di fervori giacobini (rappresentata poi con un certo successo al Teatro Sant'Angelo di Venezia, il 4 gennaio 1797).[20]Foscolo vide subito in Vittorio Alfieri un modello da seguire[21]; egli trasse il suo stile giovanile proprio da lui, e lo decantò in molte opere[22][23]. Foscolo inviò il testo del Tieste, con la dedica[24], alla residenza fiorentina del poeta astigiano. Foscolo preferì non visitare personalmente l'Alfieri, rispettando la sua estrema riservatezza degli ultimi anni, a quanto afferma nell'epistolario e nell'Ortis[25]; pare però che quest'ultimo, anche se non rispose alla lettera del Foscolo, avesse elogiato con alcuni conoscenti lo stile della tragedia, prevedendo il grande avvenire letterario dell'allora giovane ufficiale napoleonico (nonostante l'iniziale disparità di vedute su Napoleone, anche Foscolo poi converrà con Alfieri in un giudizio negativo del generale francese, chiamandolo tiranno[26]) e futuro primo vero poeta-vate dell'Italia risorgimentale. In particolare, avrebbe affermato che quel giovane l'avrebbe superato in quanto a gloria letteraria.[27]"

crack xs = encode (snd (general (currentfreq xs) 0)) xs
