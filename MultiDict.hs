module MultiDict where

import Data.Maybe
import Data.Char
import Data.List
import Data.Either

data MultiDict a b = Nil | Entry a b (MultiDict a b) | Multi a (MultiDict a b) (MultiDict a b) deriving Eq

padlength = 5

isNil Nil = True
isNil _ = False

padMD :: (Show a, Show b) => Int -> MultiDict a b -> String
padMD nivel t = initialPad ++ case t of
                    Nil -> ""
                    Entry k v m -> "\n" ++ initialPad ++ " " ++ show k ++": "++ show v ++ comma m ++ padMD nivel m
                    Multi k m1 m2 -> "\n" ++ initialPad ++ " " ++ show k ++": {"++ rec m1 ++ pad (padlength*nivel) ++"}" ++ comma m2 ++ padMD nivel m2
    where levelPad = (padlength*nivel)
          initialPad = pad levelPad
          rec = padMD (nivel+1)
          comma m = if isNil m then "\n" else ","

pad :: Int -> String
pad i = replicate i ' '

instance (Show a, Show b) => Show (MultiDict a b) where
  show x = "{" ++ padMD 0 x ++ "}"

{- Fold toma una funcion para el Entry, una funcion para el Multi, el MultiDic y devuelve algo de tipo c -}
foldMD :: c -> (a -> b -> c -> c) -> (a -> c -> c -> c) -> MultiDict a b -> c
foldMD f1 _ _ Nil = f1
foldMD f1 f2 f3 (Entry a b dic) = f2 a b (foldMD f1 f2 f3 dic)
foldMD f1 f2 f3 (Multi a dic1 dic2) = f3 a (foldMD f1 f2 f3 dic1) (foldMD f1 f2 f3 dic2)

recMD :: b  -> (a -> c -> MultiDict a c -> b -> b) -> (a -> MultiDict a c -> MultiDict a c -> b -> b -> b) -> MultiDict a c -> b
recMD b f1 f2 Nil = b
recMD b f1 f2 (Entry a val dic) = f1 a val dic (recMD b f1 f2 dic)
recMD b f1 f2 (Multi a dic1 dic2) = f2 a dic1 dic2 (recMD b f1 f2 dic1) (recMD b f1 f2 dic2)

profundidad :: MultiDict a b -> Integer
profundidad = foldMD  0 (\x b acum -> max 1 acum ) (\x acum1 acum2 -> (max (1 + acum1) acum2))

--Cantidad total de claves definidas en todos los niveles.
tamaño :: MultiDict a b -> Integer
tamaño = foldMD  0 (\x b acum -> 1 + acum) (\x acum1 acum2 -> 1 + acum1 + acum2 )

podarHasta = foldMD
          (\_ _ _ -> Nil)
          (\k v r l p lorig->cortarOSeguir l p $ Entry k v $ r (l-1) p lorig)
          (\k r1 r2 l p lorig ->cortarOSeguir l p $ Multi k (r1 lorig (p-1) lorig) (r2 (l-1) p lorig))
 {- m -}
 {- long -}
 {- proof  -}
 {- long -}
  where cortarOSeguir l p x = if l <= 0 || p <= 0 then Nil else x

-- Poda a lo ancho y en profundidad.
-- El primer argumento es la cantidad máxima de claves que deben quedar en cada nivel.
-- El segundo es la cantidad de niveles.
podar :: Integer -> Integer -> MultiDict a b -> MultiDict a b
podar long prof m = podarHasta m long prof long

--Agrega a un multidiccionario una cadena de claves [c1, ..., cn], una por cada nivel,
--donde el valor asociado a cada clave es un multidiccionario con la clave siguiente, y así sucesivamente hasta
--llegar a la última clave de la lista, cuyo valor es el dato de tipo b pasado como parámetro.
definir :: Eq a => [a] -> b -> MultiDict a b -> MultiDict a b
definir (x:xs) v d = (recMD (\ks -> cadena v ks)
       (\k1 v1 m r (k:ks)-> if k1 == k then armarDic ks k m (cadena v ks) else Entry k1 v1 (r (k:ks)))
       (\k1 m1 m2 r1 r2 (k:ks) -> if k1 == k then armarDic ks k m2 (r1 ks) else Multi k1 m1 (r2 (k:ks)))) d (x:xs)
  where armarDic ks k resto interior = if null ks then Entry k v resto else Multi k interior resto

-------------------------------------------------------- Ejercicio 3 - INICIO ------------------------------------------------
-- Dado un natural N, devuelve TODOS los pares de naturales (i,j), sin repeticiones, tales que i>=n.
todosLosParesDesde:: Integer -> [(Integer,Integer)]
todosLosParesDesde n = [ (i,j) | x <- [1..], i<-[n..x] , j<-[1..x],x==i+j]

-- Dado un a y un MultiDict a b, busca en el primer nivel de md un mdAnidado a la clave a. No contempla que no exista.
buscarMDParaI::Eq a => a -> MultiDict a b -> MultiDict a b    
buscarMDParaI elemBuscado Nil = Nil
buscarMDParaI elemBuscado (Entry k v md) = buscarMDParaI elemBuscado md
buscarMDParaI elemBuscado (Multi k mdAnidado mdLocatario) | k == elemBuscado = mdAnidado
                                                          | otherwise = buscarMDParaI elemBuscado mdLocatario

-- Función auxiliar de tablas: genera las tablas de multiplicar, con basura intercalada que hay que filtrar
tablasAux::Integer -> Integer -> MultiDict Integer Integer
tablasAux indexAux minI = Multi i (agregar i j $ buscarMDParaI i mdRec ) $ mdRec
  where mdRec = tablasAux (indexAux+1) minI
        agregar elemExterno elemInterno md = Entry elemInterno (elemInterno*elemExterno) $ md
        i = fst $ (todosLosParesDesde minI) !! (fromIntegral indexAux)
        j = snd $ (todosLosParesDesde minI) !! (fromIntegral indexAux)

-- Filtra la basura dada por tablasAux
filtrarTablas:: Integer ->  MultiDict Integer Integer -> MultiDict Integer Integer
filtrarTablas desde Nil = undefined
filtrarTablas desde (Entry _ _ _) = undefined
filtrarTablas desde (Multi i tabla rec) | i== desde = Multi i tabla $ filtrarTablas (desde+1) rec
                                        | otherwise = filtrarTablas desde rec

--Dado un entero n, define las claves de n en adelante, cada una con su tabla de multiplicar.
--Es decir, el valor asociado a la clave i es un diccionario con las claves de 1 en adelante, donde el valor de la clave j es i*j.
tablas :: Integer -> MultiDict Integer Integer
tablas desde = filtrarTablas desde $ tablasAux 0 desde

serialize :: (Show a, Show b) => MultiDict a b -> String
serialize = foldMD "[ ]" (\x b str -> ("[" ++ (show x) ++ ": " ++ (show b ) ++", " ++ str ++"]" )) (\x str1 str2  -> ("[" ++ ( show x ) ++ ": "++ str1 ++ ", " ++ str2 ++ "]") )
-------------------------------------------------------- Ejercicio 3 - FIN ---------------------------------------------------

-------------------------------------------------------- Ejercicio 7 - INICIO ------------------------------------------------
obtener :: Eq a => [a] -> MultiDict a b -> Maybe b
obtener rama md = if null rama then Nothing else last  $ snd $ obtenerAux rama md

obtenerAux::Eq a => [a] -> MultiDict a b -> (MultiDict a b,[Maybe b])
obtenerAux rama md = mapAccumL (\mdAcum clave -> (getMDRec clave mdAcum , getValor clave mdAcum)) md rama
    where getMDRec clave mdAcum = either (\_ -> Nil) (\mdRec -> mdRec) (getSubArbol clave mdAcum) 
          getValor clave mdAcum = either (\v -> v) (\_ -> Nothing) (getSubArbol clave mdAcum)

getSubArbol::Eq a=> a -> (MultiDict a b) -> (Either (Maybe b) (MultiDict a b))
getSubArbol clave md = recMD (Right Nil)
 (\k v mdRec appRec -> if (k==clave) then (Left $ Just v) else appRec) 
 (\k mdV mdRec appV appRec -> if(k==clave) then (Right mdV) else appRec)
 md
-------------------------------------------------------- Ejercicio 7 - FIN ---------------------------------------------------

mapMD :: (a->c) -> (b->d) -> MultiDict a b -> MultiDict c d
mapMD f g = foldMD Nil (\a b m -> Entry (f a) (g b) m) (\a m1 m2 -> Multi (f a) m1 m2 )

filterMD :: (a->Bool) -> MultiDict a b -> MultiDict a b
filterMD f = foldMD Nil (\a b m ->if (f a) then Entry a b m else m) (\a m1 m2 -> if (f a) then Multi a m1 m2 else m2)

enLexicon :: [String] -> MultiDict String b -> MultiDict String b
enLexicon l m = filterMD (`elem` l) (mapMD (map toLower) (\x -> x) m)

cadena :: Eq a => b ->  [a] -> MultiDict a b
cadena v claves = foldr (\x md -> Multi x md Nil) (Entry (last claves) v $ Nil) (init claves) 

