import Data.Char

-- Ex.1: Defina uma função que dada uma String com digitos e letras retorna um par em que um elemento tem todas as letras e o outro tem todos os números
    -- Relembremos a existencia das funções isDigit e isAlpha do módulo Data.Char que verificam se um char é digito ou letra, respetivamente

digitAlpha :: String -> (String, String)
digitAlpha [] = ("","")
digitAlpha l = (getDigits l, getAlphas l)

getDigits :: String -> String
getDigits [] = ""
getDigits (h:t) | isDigit h = [h] ++ getDigits t
                | otherwise = getDigits t

getAlphas :: String -> String
getAlphas [] = ""
getAlphas (h:t) | isAlpha h = [h] ++ getAlphas t
                | otherwise = getAlphas t     

------ Versão sem auxiliares -----
-- Apercebi me depois que secalhar o objetivo desta deve ser habituarmo nos a mexer com este formato

digitAlpha2 :: String -> (String, String)
digitAlpha2 [] = ([],[])
digitAlpha2 (h:t) | isDigit h = (h:digit, alpha)
                  | otherwise  = (digit, h:alpha)
                    where (digit,alpha) = digitAlpha2 t

-- Ex.2: Dada uma lista de inteiros conta o números de valores negativos, =0 e positivos, respetivamente

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h < 0     = (1+n, z, p)
          | h == 0    = (n, 1+z, p)
          | otherwise = (n, z, 1+p)
                where (n,z,p) = nzp t

-- Ex.3: Calcula Simultaneamente a divisão e o resto da divisão inteira por subtrações sucessivas

_divMod :: Integral a => a -> a -> (a,a)
_divMod resto div | resto-div >= 0 = (d+1,r) 
                  | otherwise = (0,resto)
                        where (d,r) = _divMod (resto-div) div

-- Ex.4: Utilizando uma função auxiliar com acumulador, otimize a definição de "fromDigits"


-- Versão do enunciado:
-- fromDigits :: [Int] -> Int
-- fromDigits [] = 0
-- fromDigits (h:t) = h*10^(length t) + fromDigits t
--                      ^^^^^^^^^^^^^

-- Nota*: Um acumulador neste caso vai ser um valor que vai guardar o resultado para que não tenhamos de calcular as potencia de 10 em todas as iterações

_fromDigits :: [Int] -> Int
_fromDigits [] = 0
_fromDigits l = _fromDigitsAux l 0

_fromDigitsAux :: [Int] -> Int -> Int
_fromDigitsAux [] c = c                                 -- neste caso a acumulador seria a variável c, daí o seu retorno quando a lista termina
_fromDigitsAux (h:t) c = _fromDigitsAux t ((c*10) + h) 

-- Ex.5: Utilizando uma função auxiliar com acumuladores, optimize seguinte definição que determina a soma do segmento inicial de uma lista com soma máxima.

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit [] = 0
maxSumInit l = maxSumInitAux 0 0 l

maxSumInitAux :: (Num a, Ord a) => a -> a -> [a] -> a -- Avariavel max vai guardar a maior soma que for registada, que será a resposta final e a variável vai registar a soma na posição atual da lista
maxSumInitAux max currSum [] = max
maxSumInitAux max currSum (h:t) | (currSum + h) > max = maxSumInitAux (currSum + h) (currSum + h) t
                        | otherwise = maxSumInitAux max (currSum + h) t

-- Ex.6: Defina a função recursiva da função que calcula o n-ésimo número da sequencia de Fibonacci, recorrendo a uma função auxiliar com 2 acumuladores (o "n" e o "n+1" ésimo termo)

fib :: Int -> Int
fib n = fibAux 0 1 n

fibAux :: Int -> Int -> Int -> Int
fibAux n nPlus1 0 = n
fibAux n nPlus1 x | x > 0 = fibAux nPlus1 (n + nPlus1) (x-1) 

-- Ex.7**: Defina a função que converte um inteiro numa string recorrendo a uma função auxiliar com um acumulador onde a string resposta é construída
    -- Para fazer esta função tive de ir pesquisar algumas funções pré-definidas parecidas com as finais da ficha 1 (essas não funcionam por causa da tipagem das variáveis)

intToString :: Integer -> String
intToString number = intToStringAux "" number

intToStringAux :: String -> Integer -> String
intToStringAux resp 0 = resp
intToStringAux resp num = intToStringAux (toDigit (mod num 10) : resp) (div num 10)

toDigit :: Integer -> Char
toDigit n = toEnum (fromEnum '0' + fromIntegral n)

-- Ex.8: Para cada uma das expressões seguintes, exprima por enumeração a lista correspondente. Tente ainda, para cada caso, descobrir uma outra forma de obter o mesmo resultado.

-- a) [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]

    -- [1..20] (números de 1 a 20)
    -- mod x 2 == 0 (números pares)
    -- mod x 3 == 0 (números divisiveis por 3)

    -- [6, 12, 18] Lista final

    -- [x | x <- [6,12..20]] (alternativa 1.)
    -- [x | x <- [1 .. 20], mod x 6 == 0] (alternativa 2.)


-- b) [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]

    --  [y | y <- [1..20], mod y 2 == 0] (números de 1 a 20, múltiplos de 2) [2,4,6,8,10,12,14,16,18,20]
    --  [x | x <- [y |  ... , mod x 3 == 0] (na lista anterior vai buscar os múltiplos de 3)

    -- [6, 12, 18] Lista final

    -- [x | x <- [6,12..20]] (alternativa 1.)
    -- [x | x <- [1 .. 20], mod x 6 == 0] (alternativa 2.)    
    -- A alínea a) e b) são equivalentes


-- c) [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]

    -- x <- [0..20] // y <- [0..20] (Tanto x como y assumem valores de 0 a 20)
    -- x+y == 30] (A soma de ambos os elementos é igual a 30)

    -- [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)] Lista final

    -- [(x,y) | x <- [10..20], y <- [10..20], x+y == 30] (alternativa 1.)
    -- [(x,30-x) | x <- [10..20]] (alternatica 2.)


-- d) [sum [y | y <- [1..x], odd y] | x <- [1..10]]

    -- x <- [1..10]] (Na expressão da esquerda o x assume valores de 1 a 10)
    -- [y | y <- [1..x], odd y] (Em cada iteração o valor de y assume os valores de 1 a x que sejam ímpares)
    -- [sum "expressão" ] (Soma os valores de cada lista de valores ímpares de 1 a x)

    -- Explicação -- Para x = 1: y = [1]; sum [1] = 1 
    --            -- Para x = 2: y = [1]; sum [1] = 1
    --            -- Para x = 3: y = [1,3]; sum [1,3] = 4
    --            -- Para x = 4: y = [1,3]; sum [1,3] = 4
    --                       . . .
    --            -- Para x = 8: y = [1,3,5,7]; sum [1,3,5,7] = 16

    -- [1,1,4,4,9,9,16,16,25,25] Lista final

    -- [x^2 | x <- [1..5], _ <- [1..2]] (alternativa)

-- Ex.9: Defina cada uma das listas seguintes por compreensão

    -- a) [2^x | x <- [0..10]]
 
    -- b) [(x,6-x) | x<- [1..5]]

    -- c) [[1..x] | x <- [1..5]]

    -- d) [replicate 1 x | x <- [1..5]]

    -- e) [product [1..x] | x <- [1..6]]