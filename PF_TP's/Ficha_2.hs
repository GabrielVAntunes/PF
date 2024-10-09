import Data.Char
import Data.List

-- Ex1: Indique como é que o interpretador de Haskell avalia as seguintes expressões. (Todos os passos desde o input até ao valor final)

-- a)

funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

-- funA [2,3,5,1]: 
-- funA [2,3,5,1] -> 4 + funA[3,5,1] -> 13 + funA[5,1] -> 38 + funA[1] -> 39 + funA[0] -> 39

-- b) 

funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t)
             else (funB t)

-- funB [8,5,12]
-- funB [8,5,12] -> [8, funB[5,12]] -> [8, funB[12]] -> [8, 12, funB[]] -> [8, 12]

-- c)

funC (x:y:t) = funC t -- Caso o input tenha no mínimo 3 elementos, ele efetua a chamada recursiva com a lista a partir do terceiro elemento inclusive
funC [x] = [x]
funC [] = []

-- funC [1,2,3,4,5]
-- funC [1,2,3,4,5] -> funC [3,4,5] -> funC[5] -> [5]

-- d)

funD l = g [] l             -- Esta função inverte a ordem de uma String
g acc [] = acc
g acc (h:t) = g (h:acc) t

-- funD "otrec"
-- funD "otrec" -> g [] "otrec" -> g "o" "trec" -> g "to" "rec" -> g "rto" "ec" -> g "erto" "c" -> g "certo" [] -> "certo"

-- Ex2: Defina recursivamente as seguintes funções

-- a) Recebe uma lista e retorna uma lista em que cada elemento corresponde ao dobro do valor na sua posição na lista de entrada

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2*h : dobros t

-- b) Calcula o número de vezes que um caracter ocorre numa String

numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre char (h:t) | char == h = 1 + numOcorre char t
                     | otherwise = numOcorre char t

-- c) Testa se uma lista só tem elementos positivos

positivos :: [Int] -> Bool
positivos [] = False
positivos [x]   | x >= 0 = True             -- (assumindo que 0 é positivo, fica á interpretação de cada um)
                | otherwise = False 
positivos (h:t) | h >= 0 = positivos t
                | otherwise = False 

-- d) Retira todos os elementos não positivos de uma lista

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h >= 0 = h : soPos t
            | otherwise = soPos t

-- e) Soma todos os números negativos de uma lista

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h < 0 = h + somaNeg t
              | otherwise = somaNeg t

-- f) Devolve os ultimos 3 elementos de uma lista 

tresUlt :: [a] -> [a]
tresUlt (h:t) | length(h:t) <= 3 = (h:t)
              | otherwise = tresUlt t

-- g) Ao receber uma lista de pares de elementos devolve uma lista com os segundos elementos dos pares

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((h1,h2):t) = h2 : segundos t

-- h) Recebendo uma lista de pares de elementos verifica se esse elemento aparece como 1ª componente de algum dos pares

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a ((h1,h2):t) | a == h1 = True
                           | otherwise = nosPrimeiros a t

-- i) Recebe uma lista de triplos e soma cada componente de uma mesma posição

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((h1,h2,h3):t) = (h1 + t1, h2 + t2, h3 + t3)
                                where (t1,t2,t3) = sumTriplos t

-- Ex3: Recorrendo ao módulo Data.Char, defina recursivamente as seguintes funções:

-- a) Recebe uma lista de caracteres e seleciona dessa lista os caracteres que são algarismos

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | isDigit h = h : soDigitos t
                | otherwise = soDigitos t

-- b) Recebe uma lista de caracteres e conta quantos destes são letras minúsculas

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) | isLower(h) = 1 + minusculas t
                 | otherwise = minusculas t

-- c) Recebe uma string e devolve uma lista com os algarismos que ocorrem nessa string pela mesma ordem

nums :: String -> [Int]
nums [] = []
nums (h:t) | isDigit(h) = digitToInt(h) : nums t
           | otherwise = nums t

-- Ex.4: Representemos os polinómios de uma variável da seguinte forma:

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a) Indica quantos monómios de grau n existem em p

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((h1,h2):t) | n == h2 = 1 + conta n t
                    | otherwise = conta n t

-- b) Indica o grau de um polinómio

grau :: Polinomio -> Int
grau [] = 0
grau poli = grauAux 0 poli

grauAux :: Int -> Polinomio -> Int
grauAux n [] = n
grauAux n ((h1,h2):t) | n < h2 = grauAux h2 t
                      | otherwise = grauAux n t

-- c) Seleciona os Monómios com um determinado grau

selGrau :: Int -> Polinomio -> Polinomio
selGrau _ [] = []
selGrau n ((h1,h2):t) | n == h2 = (h1,h2) : selGrau n t
                      | otherwise = selGrau n t

-- d) Calcula a derivada de um polinómio

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((h1,h2):t) = ((h1 * fromIntegral(h2)), (h2 -1)) : deriv t


-- e) Calcula o valor de um polinómio para um dado valor x

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((h1,h2):t) = h1*(x^h2) + calcula x t

-- f) Retira de um polinómio os monómios de coeficiente 0

simp :: Polinomio -> Polinomio
simp [] = []
simp ((h1,h2):t) | h1 == 0 = simp t
                 | otherwise = (h1,h2) : simp t

-- g) Calcula o resultado da multiplicação de um monómio por um polinómio

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (coef, expo) ((h1,h2):t) = ((coef * h1), (expo + h2)) : mult (coef, expo) t

-- h) Dado um polinómio calcula um polinómio equivalente que não contenha varios monomios do mesmo grau
    -- Analisem esta função com cuidado que pode ser um pouco confusa
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((h1,h2):t) | h1 == 0 = normaliza t  -- Esta condição retira os monomios de coeficiente 0, o enunciado não pede mas acrescentei esta condição para utilizar no ultimo exercicio
                      | hasEqualExponents h2 t && coef == 0 = normaliza (polWOsumMons (h1,h2) t)   -- Acrescentei esta linha para que caso a soma dos monómios de um determinado grau sejam = a 0 não apareça esse monómio de coeficiente 0 no polinómio final                                       
                      | hasEqualExponents h2 t = sumMons (h1,h2) t : normaliza (polWOsumMons (h1,h2) t)
                      | otherwise = (h1,h2) : normaliza t
                                    where (coef, expo) = sumMons (h1,h2) t
                                

hasEqualExponents :: Int -> Polinomio -> Bool
hasEqualExponents _ [] = False
hasEqualExponents expo ((h1,h2):t) | expo == h2 = True
                                   | otherwise = hasEqualExponents expo t

sumMons :: Monomio -> Polinomio -> Monomio      -- Esta função auxiliar vai calcular o monómio resultante da soma de todos os monómios de grau "expo"
sumMons m [] = m
sumMons (coef, expo) ((h1,h2):t) | expo == h2 = sumMons (coef + h1, expo) t
                                 | otherwise  = sumMons (coef,expo) t
                                      
-- Esta função auxiliar calculará o polinómio resultante da remoção dos monómios usados para calcular o monómio da função auxiliar anterior 
--de modo a que não sejam repetidos monomios que já foram somados 
polWOsumMons :: Monomio -> Polinomio -> Polinomio   
polWOsumMons _ [] = []                             
polWOsumMons (coef, expo) ((h1,h2):t) | expo == h2 = polWOsumMons (coef,expo) t
                                      | otherwise = (h1,h2) : polWOsumMons (coef, expo) t

-- i) Soma 2 Polinómios de modo que se os os polinómios que recebe estiverem normalizados, a soma também estará

-- Aproveitei me da forma que resolvi a "normaliza" para resolver esta,
--poderá não funcionar caso a função normaliza esteja estruturada de uma forma diferente da minha
soma :: Polinomio -> Polinomio -> Polinomio 
soma p [] = p
soma [] p = p
soma p1 p2 = normaliza (p1 ++ p2)

-- j) Calcula o produto de 2 polinómios

produto :: Polinomio -> Polinomio -> Polinomio
produto [] p = []
produto p [] = []
produto p1 p2 = normaliza (produtoAux p2 p1 p2) -- O uso da função "normaliza" aqui é opcional

produtoAux :: Polinomio -> Polinomio -> Polinomio -> Polinomio
produtoAux p2 [x] [] = []        -- O primeiro polinomio serve de back-up para que sempre que o segundo polinomio do input original chegar ao fim seja possivel reinicia lo
produtoAux p2 ((h1,h2):t) [] = produtoAux p2 t p2     
produtoAux p2 ((h1,h2):t) ((x1,x2):t2) = ((h1*x1),(h2+x2)) : produtoAux p2 ((h1,h2):t) t2

-- k) Orderna um Polinomio pela ordem crescente do grau dos seus monómios

-- Esta função é um pouco confuso, recomendo que vejam com atenção
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((coef, expo): t) = ordenaAux ((coef, expo):t) expo ((coef, expo): t)

-- Os inputs desta auxiliar representam a lista Ordenada (resposta), o expoente que vamos comparar com o expoente do próximo elemento, e o polinómio que estamos a percorrer
ordenaAux :: Polinomio -> Int -> Polinomio -> Polinomio
ordenaAux lOrd _ [] = lOrd               -- Quando conseguirmos percorrer a lista até ao fim é porque os elementos já estão todos ordenados
ordenaAux lOrd x ((coef,expo):t) | expo >= x = ordenaAux lOrd expo t
                                 | otherwise = ordenaAux list expo list
                                     where list = ([(coef,expo)] ++ (delete (coef,expo) lOrd))

-- l) Esta função testa se 2 polinomios são equivalentes

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena(normaliza p1) == ordena(normaliza p2)   -- Testei bastantes casos e parece me que funciona em todas as situações
                                                             -- Tive de fazer alterações à função "normaliza" para ser mais restritiva para a poder utilizar desta forma