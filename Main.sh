-- MALISH - Minimal Abbreviated Lisp Interpreter and SHell
-- wersja BARDZO WCZESNA ALPHA
-- © 2016 Michał "phoe" Herda
-- GNU GPL v3

------ Najpierw wytłumaczę się z importów.
-- System.IO     - oczywisty sposób porozumiewania się ze światem.
-- Data.Map      - podstawowa struktura danych używana w Env.
-- Data.Maybe    - monada używana w Env.
-- Control.Monad - unless z REPLa.
-- Debug.Trace   - zbawienie ufających w tobie. Znaczy, piszących w Haskellu.

import qualified Data.Map as Map
import System.IO
import Data.Maybe
import Control.Monad

import Debug.Trace
debug = flip trace -- ta sztuczka BARDZO ułatwiła mi pisanie

------ No to zaczynamy od teorii.
------ Każde wyrażenie jest atomem bądź listą.
data Expr = Atom Atom
          | List [Expr]

------ Wystarczą nam chyba trzy rodzaje atomów: symbol, liczba całkowita, napis.
data Atom = Symbol String
          | Number Int
          | String String

------ Potrzebujemy możliwości konwersji exprów na stringi.
instance Show Expr where show = showExpr

showExpr :: Expr -> String
showExpr (Atom (Symbol x)) = x
showExpr (Atom (Number x)) = show x
showExpr (Atom (String x)) = "\"" ++ x ++ "\""
showExpr (List x)   = "(" ++ unwords (map show x) ++ ")"

------ Potrzebujemy możliwości sprawdzania równości exprów.
instance Eq Expr where x == y = eqExpr x y

eqExpr :: Expr -> Expr -> Bool
       -- W przypadku atomów jest prosto. Porównujemy typy atomów i ich wartości.
eqExpr (Atom (Symbol x)) (Atom (Symbol y)) = x == y
eqExpr (Atom (Number x)) (Atom (Number y)) = x == y
eqExpr (Atom (String x)) (Atom (String y)) = x == y
       -- W przypadku list porównujemy wszystkie elementy, a przy okazji długość.
eqExpr (List [])         (List [])         = True
eqExpr (List (x:xs))     (List (y:ys))     = (x == y) && eqExpr (List xs) (List ys)
       -- W każdym przeciwnym przypadku dostajemy fałsz.
eqExpr _                 _                 = False












------ Potrzebujemy możliwości wczytywania exprów ze stringów. Do tego potrzebujemy parsera.
---- Parsowanie składni Lispa na dwie części. Pierwsza to podział na pojedyncze tokeny...
spacesAroundParens :: String -> String
spacesAroundParens [] = []                      
spacesAroundParens (x:xs) =
    if (x == '(') || (x == ')')
        then ' ':x:' ':spacesAroundParens xs -- to się jeszcze buguje, jeśli w lispowych stringach
        else x:spacesAroundParens xs         -- są nawiasy - ale przepiszę to kiedy indziej
             
tokenize :: String -> [String]
tokenize = words . spacesAroundParens

---- ...a druga to interpretacja i stworzenie właściwej struktury listowej.
dropQuotes :: String -> String
dropQuotes = drop 1 . reverse . drop 1 . reverse

isNumber :: String -> Bool
isNumber [x]    = x `elem` "1234567890"
isNumber (x:xs) = x `elem` "1234567890" && isNumber xs

------ Prosta funkcja przyporządkowująca odczytany atom do kategorii.
parseToken :: String -> Expr
parseToken string | head string == '"' = Atom (String $ dropQuotes string)
                  | isNumber string    = Atom (Number $ read string)
                  | otherwise          = Atom (Symbol string)

------ Serce całego parsera. Nad tą maleńką funkcją siedziałem bite cztery godziny.
------                                                                                     ;________;
parse :: [String] -> ([Expr], [String])
parse []              = ([], [])
parse (")":xs)        = ([], xs)
parse ("(":xs)        = (List (fParse xs):fsParse xs, ssParse xs)
parse (x  :xs)        = (parseToken x:fParse xs, sParse xs)

------ Nie będziemy używać gołego parse, bo ono zwraca parę.
------ Będą nas interesować przede wszystkim cztery specjalizowane funkcje poniżej.
fParse  = fst . parse
sParse  = snd . parse
fsParse = fParse . sParse
ssParse = sParse . sParse








------ Środowisko - tu trzymamy mapę symboli do sexpów.
data Env = EmptyEnv
         | Env (Map.Map String Expr) Env
         deriving Show

------ Ustaw symbol w środowisku.
envSet :: Env -> String -> Expr -> Env
envSet EmptyEnv _ _ = EmptyEnv
envSet (Env map parent) symbol value = Env (Map.insert symbol value map) parent

------ Pobierz symbol ze środowiska.
envGet :: Env -> String -> Expr
envGet EmptyEnv symbol = List []
envGet (Env map parent) symbol = fromMaybe (envGet parent symbol) (Map.lookup symbol map)

------ Rozszerz środowisko (choćby o zmienne lokalne lambdy).
extEnv :: Env -> [Expr] -> [Expr] -> Env
extEnv env params args = Env (Map.fromList (zip (map show params) args)) env

------ Funkcja pomocnicza: sprawdza równość dwóch wartości.
valueEq :: Env -> Expr -> Expr -> Bool
valueEq env a b = snd (eval env a) == snd (eval env b)








------ Potrzebujemy jedynego operatora w Lispie: operatora ewaluacji.
------ I to będzie GIGANTYCZNY operator.
eval :: Env -> Expr -> (Env, Expr)

eval env (Atom (Symbol "t")) = (env, Atom (Symbol "t")) -- t, atom prawdy
eval env (List [])           = (env, List [])           -- (), reprezentacja fałszu
eval env (Atom (Symbol x))   = (env, envGet env x)      -- symbol ewaluuje do przypisanego mu expra
eval env (Atom (Number x))   = (env, Atom (Number x))   -- liczby ewaluują same do siebie
eval env (Atom (String x))   = (env, Atom (String x))   -- stringi ewaluują same do siebie

------ Lista po ewaluacji woła funkcję. Albo jest to jeden z siedmiu aksjomatycznych funktorów...
eval env (List (Atom (Symbol x):xs)) = case x of
    -- (quote x) zwraca x.
    "quote" -> case xs of
        [a]                  -> (env, a)
        error                -> (env, List (Atom (String "Quote error!"):error)) `debug` show error
    -- (atom x) zwraca t, jeśli wartość x jest atomem, wpp zwraca ().
    "atom" -> case xs of
        [a] -> case snd (eval env a) of
            Atom _           -> (env, Atom (Symbol "t"))
            List []          -> (env, Atom (Symbol "t"))
            otherwise        -> (env, List [])
        error                -> (env, List (Atom (String "Atom error!"):error)) `debug` show error
    -- eq (x y) zwraca t, jeśli wartość x i wartość y to ten sam atom lub (), wpp zwraca ().
    "eq" -> case xs of
        [a, b]               -> if valueEq env a b
                                then (env, Atom (Symbol "t"))
                                else (env, List [])
        error                -> (env, List (Atom (String "Eq error!"):error)) `debug` show error

    ---- Poniższe trzy funkcje zakładają, że wartością x jest lista.
    -- (car x) zwraca pierwszy element wartości x.
    "car" -> case xs of
        [a] -> case snd (eval env a) of
            List []          -> (env, List [])
            List (a:_)       -> (env, a)
            error            -> (env, List [Atom (String "Car error!"), error]) `debug` show error
        error                -> (env, List (Atom (String "Car error!"):error)) `debug` show error
    -- (cdr x) zwraca wszystkie poza pierwszym elementem wartości x.
    "cdr" -> case xs of
        [a] -> case snd (eval env a) of
            List []          -> (env, List [])
            List (_:a)       -> (env, List a)
            error            -> (env, List [Atom (String "Cdr error!"), error]) `debug` show error
        error                -> (env, List (Atom (String "Cdr error!"):error)) `debug` show error
    -- (cons x y) zwraca nową listę z głową - wartością x i ogonem - wartością y.
    "cons" -> case xs of
        [a, b] -> case snd (eval env b) of
            List x           -> (env, List (snd (eval env a):x))
            error            -> (env, List [Atom (String "Cons error!"), error]) `debug` show error

    -- (cond (p1 e1) ... (pn en)) to konstrukt sterujący.
    -- Jeżeli p1 ma wartość t, to zwraca wartość e1, wpp przechodzi rekursywnie do p2.
    "cond" -> case xs of
        []                   -> (env, List [])
        (List [a, b]:xxs)    -> if snd (eval env a) == Atom (Symbol "t")
                                then (env, snd (eval env b))
                                else (env, snd (eval env (List (Atom (Symbol "cond"):xxs))))
        error                -> (env, List (Atom (String "Cond error!"):error)) `debug` show error

------ W każdym innym przypadku, albo lista jest lambdą...
    "lambda"                 -> (env, List (Atom (Symbol "lambda"):xs))

------ ...albo definiuje funkcję bądź zmienną...
    "define" -> case xs of
        [Atom (Symbol a), b] -> (envSet env a b, Atom (Symbol a)) --`debug` show (envSet env a b)
        error                -> (env, List (Atom (String "Define error!"):error)) `debug` show error

------ ...albo jest wywołaniem funkcji ze środowiska...
    _                        -> (env, snd (eval env (List (snd (eval env (Atom (Symbol x))):xs))))
    
------ ...albo cała lista jest wywołaniem lambdy.
eval env (List (List [Atom (Symbol "lambda"),List params, body]:args)) =
    let newEnv = extEnv env params (snd $ evalList env args)
    in  (env, snd (eval newEnv body)) 

------ W innym przypadku coś się, coś się popsuło i nie było mnie słychać, to powtórzę jeszcze raz...
eval env x = case x of        
    error                    -> (env, List [Atom (String "Eval error!"), x]) `debug` show error

------ Funkcja pomocnicza: ewaluuje wszystkie argumenty na liście.
evalList :: Env -> [Expr] -> (Env, [Expr])
evalList env []     = (env, [])
evalList env (x:xs) = (env, snd (eval env x) : snd (evalList env xs))











------ REPL, czyli Read-Eval-Print Loop.
loop :: Env -> IO ()
loop env = do putStr "MALISH> "
              hFlush stdout
              line <- getLine
              unless (line == "(quit)") $ do
                  let (newEnv, result) = eval env $ (flip (!!) 0 . fParse . tokenize) line
                  print result
                  hFlush stdout
                  loop newEnv

------ Komunikaty REPLa na powitanie i pożegnanie.
hello :: IO ()
hello = putStr $ "-- MALISH - Minimal Abbreviated Lisp\n" ++
                 "--          Interpreter and SHell\n" ++
                 "-- wersja BARDZO WCZESNA ALPHA\n" ++
                 "-- © 2016 Michał \"phoe\" Herda\n" ++
                 "-- GNU GPL v3\n" ++
                 "-------------------------------------------------\n" ++
                 "-- Na razie tylko jedno wyrażenie na linijkę.\n" ++
                 "-- Staraj się pisać poprawnie, bo się wywali.\n" ++
                 "-- Język jest PRAWIE kompletny w sensie Turinga.\n" ++
                 "-- Jeszcze nie ma przykładów ani wielu funkcji -\n" ++
                 "-- brak czasu mocno. ;_; Ale one się pojawią.\n" ++
                 "-------------------------------------------------\n" ++
                 "-- ZAIMPLEMENTOWANE POLECENIA:\n" ++
                 "-- (quote x)  (atom x) (eq x y)\n" ++
                 "-- (cons x y) (car x)  (cdr x)\n" ++
                 "-- (cond (p1 e1) ... (pn en))\n" ++
                 "-- (define x y) (lambda (x1 ... xn) e)\n" ++
                 "-- (quit)\n" ++
                 "-------------------------------------------------\n" ++
                 "-- JESZCZE DO ZROBIENIA:\n" ++
                 "-- (defun f args e), funkcje + - * / % > < >= <=\n" ++
                 "-- oraz spora ilość lispowych utilsów.\n" ++
                 "-------------------------------------------------\n"

goodbye :: IO ()
goodbye = putStr $ "-- Thank you for using MALISH!\n" ++
                   "-- More features will come. I promise. ;_;\n"

------ REPLa chcemy wywołać w czystym, ale nie pustym środowisku.
repl :: IO ()
repl = do
       hello
       loop (Env (Map.fromList []) EmptyEnv)
       goodbye

------ Najbardziej skomplikowana funkcja w całym calutkim projekcie.
main :: IO ()
main = repl
