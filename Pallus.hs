{-# LANGUAGE RecordWildCards #-}

import Data.Char (isUpper)
import Data.List (nub, isInfixOf)
import Data.List.Split (splitOn)
import Data.String.Utils (strip)
import Data.Maybe (mapMaybe, fromMaybe)

type Programa = [Regla]

type BaseConeixement = [Atom]

type Sustitucio = [(Term, Term)]

data Term = Var String | Sym String
   deriving (Eq, Show)

data Atom = Atom { _nomPredicat::String, _termes::[Term] }
  deriving (Eq, Show)

data Regla = Regla { _cap::Atom, _cos::[Atom] }
  deriving (Eq, Show)

-- Inicialitza un terme a variable o constant segons si el primer caràcter
-- del string paràmetre es majúscula o no
stringATerme :: String -> Term
stringATerme (x:xs)
  | isUpper x = Var (x:xs)
  | otherwise = Sym (x:xs)

-- Llegeix tots els termes fins el seguent separador de àtoms o final de línia
-- de la cadena de strings, retorna el seu parsing més arietat
llegeixTermes :: [String] -> ([Term],Int)
llegeixTermes [] = ([],0)
llegeixTermes (x:xs)
  |x == "=>" || x == "&" = ([],1)
  |otherwise = (((stringATerme x):termes), n+1)
    where
      res = llegeixTermes xs
      termes = fst res
      n = snd res

-- Llegeix el següent àtom de la cadena de strings, retorna el seu parsin més arietat
llegeixAtom :: [String] -> (Atom,Int)
llegeixAtom (x:xs) = (Atom x termes, n)
  where
    res = llegeixTermes xs
    termes = fst res
    n = snd res

-- Llegeix tots els àtoms de la cadena de strings, per avançar per la cadena fem
-- servir la arietat de cada àtom
llegeixAtoms :: [String] -> [Atom]
llegeixAtoms [] = []
llegeixAtoms (x:xs) = atom:(llegeixAtoms $ drop arietat xs)
  where
    res = llegeixAtom (x:xs)
    atom = fst res
    arietat = snd res

-- Llegeix la regla de la cadena de strings i la parseija
llegeixRegla :: [String] -> Regla
llegeixRegla entrada
  |not (elem "=>" entrada) = Regla x []
  |otherwise = Regla x ante
    where
      res = llegeixAtoms entrada
      (ante,(x:xs)) = splitAt (length res - 1) res
      -- amb el split separem la regla en antecedent i conseqüent per tenir acces a ambdues parts
      -- ja que el conseqüent és l'últim en ser llegit i no tindriem accés a ell altrament

-- Llegeix cada linia de l'entrada, la parseija i retorna el programa respectiu
llegeixPrograma :: IO Programa
llegeixPrograma =
  do
  entrada <- getLine
  if null entrada || entrada == "end."
    then return []
  else
    do
      programa <- llegeixPrograma
      return ((llegeixRegla $ splitOn " " (take (length entrada - 1) entrada)):programa)
      -- take elimina el caràcter '.' del final de cada línia llegida
sustitucioBuida :: Sustitucio
sustitucioBuida = []

-- Es comprova si els termes de dos àtoms son compatibles, es a dir comprovem les parelles de termes,
-- si els termes son compatibles es retorna la sustitució més true, altrament es retorna una sustitució i false
termesCompatibles :: [Term] -> [Term] -> (Sustitucio,Bool)
termesCompatibles [] [] = (sustitucioBuida,True) -- cas base
termesCompatibles ((Var x):xs) ((Sym y):ys) = (((Var x),(Sym y)):sus,b) -- Si tenim Variable x i Symbol y = ok
  where
    (sus,b) = termesCompatibles xs ys
termesCompatibles ((Sym x):xs) ((Sym y):ys) -- Si tenim Constant x i Constant y = ok si x == y, altrament no compatibles
  |x == y = (sus,b)
  |otherwise = (sustitucioBuida, False)
  where
    (sus,b) = termesCompatibles xs ys
termesCompatibles _ _ = (sustitucioBuida, False) -- Qualsevol altra situació incompatibles

-- Comprovem que cada variable esta aparellada en nomes una constant, es a dir comprovem
-- que passin coses del estil (X,jaume) i (X,pedro) en la mateixa sustitució
comprovaVariables :: Sustitucio -> Bool
comprovaVariables sus = (length res) == (length $ nub res) -- si fem nub de res i es eliminen variables això implica que havien
  where                                                    -- variables iguals aparellades amb constants diferents en la sustitucio
    res = [fst(x,y) | (x,y) <- nub $ sus] -- Obtenim totes les variables de la sustitució sense repetits

-- Es comprova que dos atoms unifiquen, si és dona el cas retornem la sustitucio, altrament Nothing
unifica :: Atom -> Atom -> Maybe Sustitucio
unifica a1 a2
  |(_nomPredicat a1 == _nomPredicat a2) && (length t1 == length t2) && b && (comprovaVariables sus) = Just $ nub sus
  |otherwise = Nothing  -- Els atoms unifiquen si el seu nom es el mateix, tenen la mateixa arietat, les constants son iguals
    where               -- i al aparellar variables amb constants, una variable només està aparellada a una constant
      t1 = _termes a1
      t2 = _termes a2
      (sus,b) = termesCompatibles t1 t2

-- Funció que s'encarrega de aplicar una sustitució de variable per terme
aplicaSustitucio :: (Term,Term) -> Term -> Term
aplicaSustitucio (x,y) (Var z) -- Cas on z és una variable
  |x == (Var z) = y            -- Si la variable de la sustitució coincideix amb
  |otherwise = Var z           -- la variable z s'aplica la sustitució
aplicaSustitucio sus z = z     -- Si z és una constant no apliquem cap sustitució

-- Donanda una sustitucio apliquem la funció aplicaSustitucio amb cadascun dels seus
-- elements sobre els termes passats com paràmetre, els termes es sustituiran progresivament
-- a mesura que ens endisem en la recusivitat
sustitueixTermes :: [Term] -> Sustitucio -> [Term]
sustitueixTermes ter [] = ter
sustitueixTermes ter (s:sus) = sustitueixTermes res sus
  where
    res = map (aplicaSustitucio s) ter

-- Funció que donats un àtom i una sustitució retorna un nou atom amb la sustitució aplicada
sustitueix :: Atom -> Sustitucio -> Atom
sustitueix at sus = Atom (_nomPredicat at) (sustitueixTermes (_termes at) sus)

-- Funció que avalua un atom, donada una llista de susticions, apliquem cadascuna
-- d'elles al atom, intenem unificar el nou atom amb la base de coneixmen i acumulem
-- totes les noves susticions generades per retornar-les
avaluaAtom :: BaseConeixement -> Atom -> [Sustitucio] -> [Sustitucio]
avaluaAtom bc at sus
  |sus == [] = map concatena (mapMaybe (unifica $ sustitueix at sustitucioBuida) bc)
  |length sus == 1 = map concatena (mapMaybe (unifica $ sustitueix at (head sus)) bc)
  |otherwise = (map concatena (mapMaybe (unifica $ sustitueix at (head sus)) bc)) ++ res
  where
    res = avaluaAtom bc at (tail sus)
    s = if sus /= [] then head sus else sustitucioBuida
    concatena = (\x ->  nub $ s ++ x) -- Aquesta funció anònima el que fa es concatenar a cada nova
                                      -- sustitució generada la que hem fet servir per obtenir el nou atom
                                      -- això simplifica el el tractament de les regles, ja que a mesura que
                                      -- avaluem atoms cada sustitució acumula els parells variable-constant
                                      -- que ha fet servir en la avaluació de àtoms anteriors per si els
                                      -- podriem necessitar més endavant

-- Avaluem cadascun dels àtoms de la regla fins arribar al últim, una vega avaluat aquest,
-- retornem les sustitucions acumulades per poder-les aplicar al conseqüent de la regla
obteSustitucionsConsequent :: BaseConeixement -> [Atom] -> [Sustitucio] -> [Sustitucio]
obteSustitucionsConsequent bc (a:as) s
  |(length (a:as)) == 1 = sus
  |otherwise = obteSustitucionsConsequent bc as sus
    where
      sus = avaluaAtom bc a s

-- Avaluem la regla per generar els atoms grounds conseqüents, per fer això avaluem
-- cadascun dels àtoms del antecedent, si la avaluació es correcta obtindrem una llista
-- de sustitucions per al conseqüent que aplicarem per obtenir els nous àtoms ground
avaluaRegla :: BaseConeixement -> Regla -> BaseConeixement
avaluaRegla bc reg
  |ante == [] = [conseq]                      -- Cas on la regla no te antecedent
  |bc == [] = bc                              -- Cas on la regla te antecedent pero la base de coneixemnt es buida, ergo no podem fer res
  |otherwise = map (sustitueix conseq) res    -- Cas on la regla te antecedent i la base de coneixment no és buida
    where
      conseq = _cap reg
      ante = _cos reg
      res = obteSustitucionsConsequent bc ante []

-- Es genera la base de coneixemnt conseqüencia de avaluar cadascuna de les regles
-- del programa amb la base de coneixemnt paràmetre
consequencia :: Programa -> BaseConeixement -> BaseConeixement
consequencia pr bc
  |pr == [] = [] -- Avaluem cada regla i concatenem les seves bases de coneixment generades
  |otherwise = (avaluaRegla bc (head pr)) ++ (consequencia (tail pr) bc)

-- Funció que va generant consequencies d'una base de coneixemnt fins que la base
-- coneixemnt resultant es igual a la anterior, en aquell moment ja haurem generat tots
-- els atoms grounds possibles amb les regles del programa
obteBaseConeixemntFinal :: Programa -> BaseConeixement -> BaseConeixement
obteBaseConeixemntFinal pr bc
  |res == bc = consequencia pr bc
  |otherwise = obteBaseConeixemntFinal pr res
    where
      res = nub (consequencia pr bc) -- El nub elimina atoms que podrien estar repetis
                                     -- a casua de regles repetides dins del programa

-- Si estem responent a una query de true o false, el resultat es ture si cadascun
-- dels atoms del antecendent està dins de la base de coneixemnt, altrament serà fals
comprovaAntecedents :: BaseConeixement -> [Atom] -> Bool
comprovaAntecedents bc ats
  |ats == [] = True
  |elem (head ats) bc = comprovaAntecedents bc (tail ats)
  |otherwise = False

-- S'avalua cada element de la sustitució, si la variable està present als termes
-- el element es preserva sinó es elimina de la sustitució
eliminaVariablesObsoletes :: [Term] -> Sustitucio -> Sustitucio
eliminaVariablesObsoletes terms sus
  |sus == [] = sustitucioBuida
  |elem var terms = s:res
  |otherwise = res
    where
      s = head sus
      var = fst s
      res = eliminaVariablesObsoletes terms (tail sus)

-- S'obtenen totes les sustitucions necessaries per respondre la query
obteSustitucionsPregunta :: BaseConeixement -> Regla -> [Sustitucio]
obteSustitucionsPregunta bc q = map (eliminaVariablesObsoletes term) $ obteSustitucionsConsequent bc atoms []
  where                           -- Una vegada obtingues les sustitucions, eliminem alguns elements obsolets d'aquestes
    term = _termes (_cap q)       -- que no necessitem per respondre la pregunta, per exepmle progenitor X Y & germa Z X => query Z Y
    atoms = _cos q                -- nomes volem sustitucions sobre Z Y pero la sustitucio tambe aporta resposta per X,
                                  -- perque durant la avaluacio del antecendent necessitem la sustitucio sobre X en ambdos atoms
                                  -- i segons el meu disseny les susticions contenen totes les variables de la regla

-- Es respon a cada query, es retorna un string on cada element es la resposta a una query
contestaPreguntes :: Programa -> BaseConeixement -> [String]
contestaPreguntes pr bc
  |(pr == []) = []                                                          -- Si no hi han preguntes no cal fer res
  |(_termes (_cap q)) == [] = (show $ comprovaAntecedents bc (_cos q)):res  -- Cas on la query es una reposta de true o false
  |otherwise = [ x | x <- resposta, not (elem x "\"")]:res                  -- Cas on la query requereix sustitucions
    where                                                                   -- La llista per comprehensió és el resultat de eliminar els
      resposta = show $ obteSustitucionsPregunta bc q                       -- caràcters \" del sting per poder llegir millor la resposta
      q = head pr
      res = contestaPreguntes (tail pr) bc

-- Entrada del programa, fa la crida a les funcions de lectura del programa, la funció
-- que retorna la base de coneixemnt final, la funció que respon les queries i finalment
-- escriu per terminal les resposes
main :: IO ()
main =
  do
    entrada <- llegeixPrograma
    preguntes <- llegeixPrograma
    let bc = obteBaseConeixemntFinal entrada []
    mapM_ print (contestaPreguntes preguntes bc)
    return ()
