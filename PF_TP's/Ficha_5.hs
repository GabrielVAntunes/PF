import Data.List

-- Ex1: Apresente definições das seguintes funções de ordem superior, já pré-definidas no Prelude ou no Data.List:

-- a) Função que testa se um predicado é verdade para algum elemento de uma lista

_any :: (a -> Bool) -> [a] -> Bool
_any f [] = False
_any f (h:t) | f h = True
             | otherwise = _any f t

-- b) Função que combina os elementos de 2 listas usando uma função específica
    -- Vou assumir que as listas têm de ter o mesmo tamanho

_zipWith :: (a->b->c) -> [a] -> [b] -> [c]
_zipWith f [] _ = []
_zipWith f _ [] = []
_zipWith f (h1:t1) (h2:t2) = (f h1 h2) : _zipWith f t1 t2

-- c) Função que retorna o maior prefixo da lista em que os elementos são válidos para uma dada função

_takeWhile :: (a->Bool) -> [a] -> [a]
_takeWhile f [] = []
_takeWhile f (h:t) | f h = h : _takeWhile f t
                   | otherwise = []

-- d) Função que retorna a lista sem o maior prefixo da lista em que os elementos são válidos para uma dada função

_dropWhile :: (a->Bool) -> [a] -> [a]
_dropWhile f [] = []
_dropWhile f (h:t) | f h = _dropWhile f t
                   | otherwise = (h : t)

-- e) Função que calcula o resultado das duas anteriores
    -- ou seja: span p l = (takeWhile p l, dropWhile p l)

_span :: (a-> Bool) -> [a] -> ([a],[a])
_span f [] = ([],[])
_span f (h:t) | f h = (h:take, drop)
              | otherwise = ([], h:t)
            where (take,drop) = _span f t

-- f) Função que elimina a primeira ocorrência de um elemento "igual" a um dado elemento de acordo com os parametros da função dada
    -- _deleteBy (\x y -> snd x == snd y) (1,2) [(3,3),(2,2),(4,2)], resultaria em: [(3,3),(2,2),(4,2)]

_deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
_deleteBy f a [] = []
_deleteBy f a (h:t) | f a h = t
                    | otherwise = h : _deleteBy f a t

-- g) Função que ordena uma lista de acordo com o parametro definido na função dada
    -- Esta é uma função confusa vejam com atenção

_sortOn :: Ord b => (a -> b) -> [a] -> [a]
_sortOn f [] = []
_sortOn f (h:t) = sortAux f h (_sortOn f t)
-- Notar que como esta função recorre a uma chamada recursiva, o primeiro elemento inserido pela auxiliar vai ser o ultimo elemento da função original, e este será inserido numa lista vazia

-- A função sortAux é uma função auxiliar que tem o proposito de inserir ordenamente um elemento numa lista de acordo com o parametro definido
sortAux :: Ord b => (a -> b) -> a -> [a] -> [a]
sortAux f a [] = [a]                                -- Caso a lista seja vazia não há necessidade de verificar nada, basta inserir
sortAux f a (h:t) | f a <= f h = a : h : t          -- Caso o elemento que estamos a inserir for menor ou igual ao primeiro elemento da lista de acordo com o parametro definido, podemos coloca lo antes
                  | otherwise = h : sortAux f a t   -- Caso contrário vamos avançando os elementos até encontrarmos a posição certa para inserir esse elemento

-- Desenvolvimento da função com o exemplo: _sortOn fst [(3,1),(1,2),(2,5)].
    -- 1: chegamos á 3ª linha da função original e percorremos todas as suas chamadas recursivas, pois a auxiliar precisa de uma lista para operar
    -- 2: quando finalmente chegamos a uma lista em concreto, a lista vazia ([]), estamos na ultima chamada recursiva e com o ultimo elemento
    -- 3: chamamos então a auxiliar -- sortAux fst (2,5) [], que resulta em [(2,5)]
    -- 4: voltamos para a chamada recursiva onde vamos comparar o penultimo elemento da lista original a esta lista
    -- 5: chamamos -- sortAux fst (1,2) [(2,5)], que resulta em [(1,2),(2,5)]
    -- 6: por fim fazemos a ultima chamada recursiva e vamos comparar o primeiro elemento da lista original
    -- 7: chamamos -- sortAux fst (3,1) [(1,2),(2,5)], que resulta em [(1,2),(2,5),(3,1)]
    -- 8: Termina a função

-- Ex2: Relembre a questão sobre polinómios introduzida na Ficha 3, onde um polinómio era
--representado por uma lista de monómios representados por pares (coeficiente, expoente)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- Redefina as funções pedidas nessa ficha, usando agora funções de ordem
--superior (definidas no Prelude ou no Data.List) em vez de recursividade explícita:

-- a) Retorna um polinómio com os monómios de grau n

selgrau :: Int -> Polinomio -> Polinomio
selgrau n pol = filter (\(coef,expo) -> n == expo) pol

-- b) Retorna o número de monómios de grau n

conta :: Int -> Polinomio -> Int
conta n pol = length (filter (\(coef,expo) -> n == expo) pol)

-- c) Função que indica o grau de um polinómio

grau :: Polinomio -> Int
grau pol = foldr (\(coef,expo) -> max expo) 0 pol

-- A função "foldr" pré definida em haskell, pega numa função e num argumento e numa lista e aplica essa função ao argumento e a todos 
--os valores da lista do fim para o inicio, avançando com o valor resultante, por exemplo:

-- foldr (/) 2 [8,12,24,4] -> foldr (4/2 = 2) [8,12,24] -> foldr (24/2 = 12) [8,12] -> foldr (12/12 = 1) [8] -> foldr (8/1 = 8) [] -> 8

-- Neste exercicio vamos comparar o coeficiente de todos os polinomios e apenas trocamos caso encontremos um maior que o que temos guardado atualmente 


--d) Calcula a Derivada de um Polínomio

-- Para este exercício vamos utilizar a função map que vai aplicar uma função a todos os elementos de uma lista

deriv :: Polinomio -> Polinomio
deriv pol = map (\(coef,expo) -> if expo > 0 then (coef * fromIntegral(expo), expo - 1) else (0, 0)) pol

-- e) Calcula o polinómio para um dado valor de x

calcula :: Float -> Polinomio -> Float
calcula x pol = sum (map (\(coef, expo) -> if expo > 0 then (coef *(x^expo)) else (coef)) pol)

-- f) retira de um polinómio os monómios de coeficiente 0

simp :: Polinomio -> Polinomio
simp pol = filter (\(coef,expo) -> coef /= 0) pol

-- g) Calcula o resultado da multiplicação de um monómio por um polinómio

mult :: Monomio -> Polinomio -> Polinomio
mult (coefm, expom) pol = map (\(coef, expo) -> (coefm * coef, expom + expo)) pol

-- h) Ordena um polinómio por ordem crescente do grau dos seus monómios

ordena :: Polinomio -> Polinomio
ordena pol = sortOn (\(coef,expo) -> expo) pol

-- i) função em que não aparecem mais que um monómio do mesmo grau
--Vejam este com atenção

-- Nota*: Esta versão não elimina monómios de coeficiente 0, por exemplo (0,2)
normaliza :: Polinomio -> Polinomio --{o foldr soma os coeficientes e mantém o expoente em cada lista (l)}
normaliza pol = map (\l -> foldr (\(coef,expo) (c,e)-> (c + coef, max e expo)) (0,0) l) (groupBy mesmoGrau (ordena pol)) 
--  {Aplicamos o foldr a todos os grupos}              {(0,0) para não alterar valores}    {o groupBy junta os elementos numa lista de listas agrupadas consoante o critério "mesmoGrau"}
                            where mesmoGrau (_,g1)(_,g2) = g1 == g2

-- groupBy mesmoGrau [(3.0,1),(1.0,1),(1.0,2),(3.0,2)] -> [[(3.0, 1), (1.0, 1)], [(1.0, 2), (3.0, 2)]]

-- j) Faz a soma de dois polinómios e deve produzir um polinómio normalizado

soma :: Polinomio -> Polinomio -> Polinomio
soma pol1 pol2 = normaliza ((++) pol1 pol2)
-- Basta normalizar a concatenação de ambos os polinómios

-- k) Calcula o produto de dois polinómios
produto :: Polinomio -> Polinomio -> Polinomio
produto pol1 pol2 = normaliza $ concatMap (\x -> mult x pol2) pol1
-- A função concatMap é similar à map mas tem a propriedade extra de concatenar as listas resultantes numa única lista
-- Nota*: o "$" substitui o uso de parentesis -> func1 (func2 x) é igual a func1 $ func2 x

-- l) Testa se dois polinómios são equivalentes
equiv :: Polinomio -> Polinomio -> Bool
equiv pol1 pol2 = filter (\(coef, expo) -> coef /= 0) (ordena (normaliza pol1))
                == filter (\(coef, expo) -> coef /= 0) (ordena (normaliza pol2))
-- Este filter serve para retirar monómios de coeficiente 0, visto que [(0,2),(5,1)] é quivalente a [(5,1)]

-- Ex.3: Considere a sequinte definição para representar matrizes:

type Mat a = [[a]]

--Por exemplo, a matriz (triangular superior)

--    1 2 3
--    0 4 5
--    0 0 6

-- seria representada por [[1,2,3], [0,4,5], [0,0,6]]

-- Defina as seguintes funções sobre matrizes (use, sempre que achar apropriado, funções
--de ordem superior).

-- a) Testa se uma matriz está bem construída (todas as linhas têm a mesma dimensão)

dimOK :: Mat a -> Bool
dimOK [x] = True
dimOK (h:t) | length h == length (head t) = dimOK t
            | otherwise = False

-- Resolução alternativa utilizando funções de ordem superior

_dimOK :: Mat a -> Bool
_dimOK (h:t) = all (\l -> length l == length h) t
-- A função all verifica se todos os elementos de uma lista verificam uma dada propriedade

-- b) Calcula a dimensão de uma Matriz (vou assumir que esta função apenas recebe matrizes válidas)

dimMat :: Mat a -> (Int,Int)
dimMat l = (length l, length (head l))

-- c) Realiza a dição de duas matrizes

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat m1 m2 = zipWith (zipWith (+)) m1 m2
-- Utilizando a função de ordem superior definida no exercício 1, usamos a função zipWith para unir as duas matrizes e como critério
--usamos novamente a função zipWith com o critério (+) de modo a que as linhas de ambas as matrizes vão ser combinadas
--somando os seus elementos.

-- d) Calcula a trasnposta de uma matriz

_transpose :: Mat a -> Mat a
_transpose ([]:_) = []
_transpose m = map head m : _transpose (map tail m)

-- e) Calcula o produto de duas matrizes 
-- Atenção a este exercício

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = [[sum (zipWith (*) lin col) | col <- _transpose m2] | lin <- m1]
-- Usando compreensão de listas como vimos na ficha 4, torna se mais fácil resolver este exercício
--desta forma vamos percorrer cada elemento de cada lista da transposta de m2 e vamos multiplica-lo
--com cada elemento de cada lista de m1 utilizando a zipWith e somamos todos os elementos da lista resultante

-- f) Contruir uma versão da zipWith mas para matrizes, depois utilizar esta versão para construir uma função que adicione matrizes 

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f m1 m2 = zipWith (zipWith f) m1 m2

addMatv2 :: Num a => Mat a -> Mat a -> Mat a
addMatv2 m1 m2 = zipWMat (+) m1 m2

-- g) Testa se uma matriz quadrada é triangular superior

-- O "Eq a" define que os elementos "a" são comparáveis, uma vez que os operadores "==" e "/=" estão associados a operações numéricas
triSup :: (Eq a, Num a) => Mat a -> Bool
triSup [] = True
triSup (h:t) = all (\x -> x == 0) col && triSup (map tail t)
            where col = map head t

-- h) Roda uma matriz 90º para a esquerda

rotateLeft :: Mat a -> Mat a
rotateLeft ([]:_) = []
rotateLeft m = map last m : rotateLeft (map init m)
