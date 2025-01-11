
import Data.List 

-- 1. Constroi uma lista com todos os números entre os limites definidos

_enumFromTo :: Int -> Int -> [Int]
_enumFromTo x y | x < y = x : enumFromTo (x+1) y -- Enquanto o primeiro valor for menor que o segundo o primeiro será escrito na lista constantemente 
                | x == y = [x]                   -- Quando os 2 valores se igualarem o código vai escrever o limite inferior uma ultima vez deixando de fazer a chamada recursiva
                | otherwise = []                 -- Caso o intervalo introduzido esteja invertido (Ex: _enumFromTo 7 3) (Opcional)

-- 2. Constroi uma lista com todos os números entre os limites defindos espaçados por um valor constante

_enumFromThenTo :: Int -> Int -> Int -> [Int]
_enumFromThenTo x y z | x < z = x : _enumFromThenTo (x+y) y z  -- Mesmo raciocinio do exercício anterior mas agora em vez de somar 1 ao x...
                      | x == z = [x]                           -- ...somamos o valor constante pré-definido
                      | otherwise = []

-- 3. Concatena 2 listas

plusplus :: [a] -> [a] -> [a]            -- Apesar de no enunciado ser dado um exemplo com números esta função funciona para variáveis de qualquer tipo
plusplus l [] = l                        -- Caso a segunda lista seja vazia basta escrever a primeira 
plusplus [] (h:t) = h : plusplus [] t    -- Quando acabamos a primeira lista começamos a escrever a segunda 
plusplus (h:t) l = h : plusplus t l      -- Quando ainda temos 2 listas vamos escrevendo os elementos da primeira até que esta acabe

-- 4. Indica o elemento que está na posição pretendida

exclpExclp :: [a] -> Int -> a
exclpExclp (h:t) 0 = h                  -- Quando o valor da posição que estamos à procura chegar a 0 quer dizer que o encontramos 
exclpExclp (h:t) x = exclpExclp t (x-1) -- Vai percorrer a lista enquanto diminui o valor da posição que estamos a procurar 


-- 5. Inverte a ordem dos elementos de uma lista

_reverse :: [a] -> [a]
_reverse [] = []                      -- Caso a lista seja vazia, ou quando já tivermos percorrido todos os elementos o programa termina.
_reverse (h:t) = _reverse t ++ [h]    -- Neste caso não poderiamos fazer "_reverse t : h" porque o operador ":" espera a cabeça da lista 
                                    --seguida dos restantes elementos da lista, e neste caso estamos a por uma lista à cabeça o que
                                    --não corresponde com o tipo esperado: [a]. Para resolver utilizamos a função definida no ex3.
                                    --que junta 2 listas.

-- 6. Retorna uma lista com os n primeiros elementos da lista dada

_take :: Int -> [a] -> [a]
_take _ [] = []                     -- Quando a lista acabar ou caso seja vazia já de inicio é escusado continuar a chamada recursiva
_take 0 _ = []                      -- Quando percorrermos as "n" posições e por consequencia o n chegar a 0, não escrevemos mais nenhum valor 
_take n (h:t) = h : _take (n-1) t   -- Enquanto o "n" for maior que 0 e existir valores na lista para percorrer, vamos escrevendo a lista
                                  --até que a lista acabe ou que sejam escritas as "n" primeiras poisções da lista

-- 7. Retorna uma lista sem os n primeiros elementos da lista dada

_drop ::  Int -> [a] -> [a]
_drop _ [] = []                             -- Quando a lista acabar é escusado continuar a percorre-la
_drop n (h:t) | n > 0 = _drop (n-1) t       -- Vamos avançar as "n" primeiras posições sem escrever nada na lista
              | otherwise = h : _drop 0 t   -- Apenas no fim de avançarmos as "n" primeiras posições é que começamos a escrever os restantes
                                          --elementos da lista até que esta acabe

-- 8. Dadas duas listas retorna uma lista com os pares de cada posição das listas (Assumamos que as listas dadas são do mesmo tamanho)

_zip :: [a] -> [b] -> [(a,b)]
_zip [] _ = []                                -- Como estamos a assumir que as listas são do mesmo tamanho, basta verificar se a primeira é vazia para terminar o programa
_zip (x : xs) (y : ys) = (x,y) : _zip xs ys   -- Basta juntar as cabeças de ambas as listas num par e seguir com a chamada recursiva 
              
-- 9. Cria uma lista que contém o valor x "n" vezes

_replicate :: Int -> a -> [a]
_replicate 0 x = []                         -- Quando o valor já tiver sido replicado "n" vezes o "n" estará a 0 e neste momento o programa termina 
_replicate n x = x : _replicate (n-1) x     -- Vamos colocar o x á cabeça da lista enquanto o "n" for maior que 0


-- 10. Dados um valor e uma lista, retorna uma lista com esse valor intercalado com todos os elementos da lista 

_intersperse :: a -> [a] -> [a]
_intersperse x [] = []                              -- Quando a lista terminar não escrevemos mais nada
_intersperse x [a] = [a]                            -- Esta linha verifica se a lista restante consiste num unico elemento, nesse caso escrevemos apenas esse elemento
_intersperse x (h:t) = h : x : _intersperse x t     -- Enquanto a lista ainda tiver vários elementos escrevemos o que estiver á cabeça seguido do valor x, 
                                                --notemos que sem a segunda linha, este código iria escrever o valor x no fim da lista que não é o objetivo pretendido

-- 11. Dada uma lista, retorna uma lista de listas com todos os números iguais consecutivos agrupados

_group :: Eq a => [a] -> [[a]]
_group [] = []  
_group (h:t) = (h:takeWhile (==h) t) : _group (dropWhile (==h) t) -- Para não termos de recorrer ao uso excessivo de funções auxiliares, utilizei
                                    --as funções pré-definidas em haskell "takeWhile" e "dropWhile" que funcionam de forma similar ás funções 
                                    --já construídas "take" e "drop" que em vez de receberem um número de elementos, vão delimitar o número
                                    --de elementos tirados através da condição dada, neste caso "(==h)" serem iguais ao primeiro elemento da lista

-- 12. Dada uma lista de listas, retorna uma lista apenas com todos os elementos 

_concat :: [[a]] -> [a]
_concat [] = []                     
_concat (h:t) = h ++ _concat t    -- Aproveitando a função pré-definida ++ para juntar listas numa só, percorremos todas as listas na lista principal
                                --e simplesmente unimos todas numa só

-- 13. Dada uma lista, devolve a lista dos seus prefixos

_inits :: [a] -> [[a]]
_inits [] = [[]]                    -- No ultimo elemento colocamos a lista vazia no inicio do resultado e terminamos a execução
_inits l = _inits (init l) ++ [l]   -- A função init replica a lista sem o ultimo elemento. Esta linha vai colocar a
                                --entrada da função no inicio da lista e vai chamar recursivamente sem o ultimo elemento


-- 14. Dada uma lista, devolve a lista dos seus sufixos

_tails :: [a] -> [[a]]
_tails [] = [[]]                    -- Quando chegarmos ao ultimo elemento colocamos a lista vazia no fim do resultado e terminamos a execução
_tails (h:t) = (h:t) : _tails t     -- De forma muito similar ao exercício anterior, vamos colocar a entrada no inicio da lista 
                                --e depois chamamos recursivamente com a cauda dessa mesma entrada da função


-- 15. Dada uma lista de listas devolve uma lista com o primeiro elemento de cada lista

heads :: [[a]] -> [a]
heads [] = []                                   -- Quando encontrarmos um elemento vazio terminamos a execução com uma lista vazia
heads (h:t) | null h = heads t                  -- A função null verificará se h == [] (notemos que "h == []" é uma expressão inválida), caso seja não escrevemos esta posição
            | otherwise = head h : heads t      -- Caso a cabeça da lista não seja vazia usamos a função "head h" para escrever apenas a cabeça da lista  e depois
                                            --continuamos a chamada recursiva

-- 16. Dada uma lista de listas conta os elementos de todas as listas 

total :: [[a]] -> Int
total [] = 0                        -- Quando chegarmos ao fim da lista fechamos a soma dos elementos somando 0 e cancelando a chamada recursiva
total (h:t) = length h + total t    -- Ao percorrer cada lista, usamos a função "length h" para calcular o número de elementos dessa lista e 
                                --somamos com a chamada recursiva das próximas posições

-- 17. Dada uma lista de triplos devolve uma lista de duplos com o primeiro e o ultimo elemento de cada posição

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []                             -- Análogo aos anteriores
fun ((h1,h2,h3):t) = (h1,h3) : fun t    -- Simplesmente decompomos a cabeça da lista no formato pretendido e reorganizamos de modo a ficar apenas com
                                    --o primeiro e o ultimo elemento de cada triplo

-- 18. Dada uma lista de triplos onde o primeiro elemento de cada triplo é uma String, une todas as strings numa única

cola :: [(String,b,c)] -> String
cola [] = ""                            -- Análogo aos anteriores (note-se que agora como o output da função é uma String adaptamos o resultado para esse formato "")
cola ((h1,h2,h3):t) = h1 ++ cola t      -- De forma similar á função "fun" apenas reorganizamos o triplo, notemos que neste caso tivemos de utilizar
                                    -- "++" para concatenar ambas as strings visto que não se trata da lista convencional []

-- 19. A função recebe um Ano, uma idade e uma lista de pares [(Nome,AnoDeNascimento)] e devolve uma lista com os nomes que já atingiram/ultrapassaram essa idade
--no ano indicado

idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []                                                           -- Caso de paragem (Nota*: "_" significa anything)
idade year age ((h1,h2):t) | (year - h2) >= age = h1 : idade year age t     -- Se ao subtrair o ano de nascença ao ano indicado o resultado for maior ou igual á idade pretendida, escrevemos esse nome na lista
                           | otherwise = idade year age t                   -- C.C. (caso contrário), sem escrever o nome efetuamos apenas a chamada recursiva para verificar os próximos elementos

-- 20. A função recebe um elemento e o tamanho da lista final e devolve uma lista das potencias desse elemento

powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 1 = [1]                                 -- Quando chegarmos ao ultimo elemento da lista, o valor de x vai ser igual a 1, nesse caso vamos terminar a recursividade com o elemento n^0 (1)
powerEnumFrom n x = powerEnumFrom n (x-1) ++ [n^(x-1)]  -- Mais uma vez temos de usar "++" porque estamos a construir a lista do fim para o inicio, tendo em conta que a lista vai de [n^0, ..., n^(x-1)]
                                                    --temos de subtrair 1 em todas as iterações

-- 21. Função que dado um valor, indica se é primo ou não

isPrime :: Int -> Bool
isPrime n 
    | n < 2     = True  -- Números menores que 2 são primos
    | otherwise = not (hasDivisors n 2)

-- Função auxiliar que verifica se n tem divisores entre 2 e √n
hasDivisors :: Int -> Int -> Bool
hasDivisors n m
    | m * m > n      = False             
    | n `mod` m == 0 = True             
    | otherwise      = hasDivisors n (m + 1) 

-- 22. Testa se a primeira lista é prefixo da segunda

_isPrefixOf :: Eq a => [a] -> [a] -> Bool
_isPrefixOf [] _ = True                                  -- Independentemente da segunda lista, a lista vazia é prefixo de qualquer lista
_isPrefixOf _ [] = False                                 -- Caso cheguemos ao fim da segunda lista quer dizer que não encontramos nenhuma correspondência, logo a afirmação é falsa
_isPrefixOf l1 l2 | l1 == l2 = True                      -- Se ambas as listas forem iguais a afirmação é verdadeira
                  | otherwise = _isPrefixOf l1 (init l2) -- C.C. efetuamos a chamada recursiva sem o ultimo elemento da lista da direita
                                                     -- a ver se em algum momento ambas as lsitas se igualam

-- 23. Testa se a primeira lista é sufixo da segunda 

_isSufixOf :: Eq a => [a] -> [a] -> Bool
_isSufixOf [] _ = True                             -- Análogo ao anterior
_isSufixOf _ [] = False
_isSufixOf l1 (h:t) | l1 == (h:t) = True  
                 | otherwise = _isSufixOf l1 t

-- 24. Verifica se os elementos da primeira lista estão na segunda pela mesma ordem (podem ser intercalados por outros elementos)

_isSubSequenceOf :: Eq a => [a] -> [a] -> Bool
_isSubSequenceOf [] _ = True                                                -- Caso a primeira lista chegue ao fim antes da segunda é porque encontramos todos os elementos pela ordem correta 
_isSubSequenceOf _ [] = False                                               -- C.C. É porque percorremos a segunda lista sem encontrar todos os elementos
_isSubSequenceOf (h1:t1) (h2:t2) | h1 == h2 = _isSubSequenceOf t1 t2        -- Caso ambos os elementos à cabeça da lista sejam iguais, ambas as listas avançam 1 elemento
                                 | otherwise = _isSubSequenceOf (h1:t1) t2  -- C.C. apenas a segunda lista avança pois ainda temos de encontrar uma correspondencia com o outro elemento

-- 25. Função que devolve uma lista com as posições em que um determinado elemento aparece numa lista

_elemIndices :: Eq a => a -> [a] -> [Int]
_elemIndices _ [] = []
_elemIndices n l = elemIndicesAux 0 n l     -- Esta função apenas vai ter o proposito de chamar a função auxiliar que tem capacidade de registar as posições

-- Função auxiliar que vai contando os indices para que possamos escrever a posição que estavas a ver com precisão

elemIndicesAux :: Eq a => Int -> a -> [a] -> [Int]
elemIndicesAux _ _ [] = []                                           -- Quando a lista ficar vazia quer dizer que já acabamos de a percorrer
elemIndicesAux x n (h:t) | n == h = x : elemIndicesAux (x+1) n t     -- Caso o elemento que estamos á procura e o elemnento á cabeça da lista sejam iguais podemos registar essa posição                         
                         | otherwise = elemIndicesAux (x+1) n t      -- C.C. avançamos a lista sem registar essa posição

-- 26. Dada uma lista retorna uma lista com os primeiros elementos de uma lista sem repetir elementos 

_nub :: Eq a => [a] -> [a]
_nub [] = []
_nub l = nubAux l []             -- Vamos precisar de chamar uma função auxiliar para verificar se um determinado elemento já pertence à lista final

nubAux :: Eq a => [a] -> [a] -> [a]
nubAux [] resp = resp                                   -- Quando chegarmos ao fim da lista inicial devolvemos a lista "resposta"
nubAux (h:t) resp | (elem h resp) = nubAux t resp       -- A função "elem x list" verifica se um determinado elemento está contido na lista
                  | otherwise = nubAux t (resp ++ [h])  -- Apenas caso não esteja contido vamos acrescenta-lo à lista "resposta" 

-- 27. Função que remove a primeira ocorrência do elemento selecionado na lista

_delete :: Eq a => a -> [a] -> [a]
_delete x [] = []
_delete x (h:t) | x == h = t                        -- Quando encontrar o valor indicado terminamos a chamada recursiva escrevendo o resto da lista 
                | otherwise = h : _delete x t      

-- 28. Função que remove as primerias ocorrências dos elementos selecionados numa lista

bSlashBSlash :: Eq a => [a] -> [a] -> [a]
bSlashBSlash [] _ = []
bSlashBSlash l [] = l
bSlashBSlash (h:t) l | elem h l =  bSlashBSlash t (delete h l) -- Recorremos à função definida anteriormente para facilitar caso os elementos sejam repetidos ou não estejam pela mesma ordem
                     | otherwise = h : bSlashBSlash t l -- Basicamente aplicamos a função "_delete" para cada elemento da lista                         

-- 29. Função que retorna a união entre 2 listas (acrescenta à primeira lista os elementos da segunda que não constem na mesma)

_union :: Eq a => [a] -> [a] -> [a]
_union [] l2 = l2
_union l1 [] = l1
_union l1 (h2:t2) | (elem h2 l1) = _union l1 t2
                  | otherwise = _union (l1 ++ [h2]) t2  -- Apenas temos te acrescentar os elementos da segunda lista no fim da primeira caso eles já não cosntem na mesma 

-- 30. Função que retorna a interseção entre 2 listas (remove à primeira os elementos que não constam na segunda)
 
_intersect :: Eq a => [a] -> [a] -> [a]                 -- Análogo ao anterior
_intersect [] _ = []
_intersect l [] = l
_intersect (h:t) l | (elem h l) = h : _intersect t l
                   | otherwise = _intersect t l

-- 31. Função que insere um elemento numa lista Ordenada

_insert :: Ord a => a -> [a] -> [a]
_insert n [] = [n]                          -- Caso o elemento que queremos inserir seja o maior elemento da lista este só será colocado quando percorrermos a lista completa
_insert n (h:t) | n > h = h : _insert n t   -- Estas condições verificam quando é que um elemento é menor que o que estamos a verificar para então poder inseri-lo na lista ordenada
                | otherwise = n : h : t

-- 32. Função que recebe uma lista de strings e junta todas numa só separadas por um " "

_unwords :: [String] -> String              -- Resolução análoga à Função "_intersperse" (nº10)
_unwords [] = ""                          --apenas tivemos de adaptar para o contexto de Strings
_unwords [w] = w
_unwords (h:t) = h ++ " " ++ _unwords t

-- 33. Função que junta todas as Strings de uma lista colocando "\n" no fim de cada uma

_unlines :: [String] -> String              -- Igual á anterior apenas retiramos a linha que verifica se a lista tem um unico elemento pois neste caso também colocamos "\n"
_unlines [] = ""                          --no final da ultima String
_unlines (h:t) = h ++ "\n" ++ _unlines t

-- 34. Função que retorna o índice do maior elemento da lista

pMaior :: Ord a => [a] -> Int
pMaior [] = 0
pMaior (h:t) = pMaiorAux 0 0 h t

pMaiorAux :: Ord a => Int -> Int -> a -> [a] -> Int
pMaiorAux i curr x [] = i                                              -- Legenda variáveis: i - índice do maior elemento / curr - posição atual na lista
pMaiorAux i curr x (h:t) | x < h = pMaiorAux (curr+1) (curr+1) h t   --                      x - maior elemento
                         | otherwise = pMaiorAux i (curr+1) x t        -- Este algoritmo limita se a chamar a auxiliar que tem a capacidade de armazena mais informação para
                                                                     --para que seja possivel ter sempre acesso às posições e ao maior elemento por exemplo
 
-- 35. Função que dada uma lista de pares retorna os elementos que satisfazem a condição dada
    -- Ex: lookup ’a’ [(’a’,1),(’b’,4),(’c’,5)] corresponde `a lista Just 1

_lookup :: Eq a => a -> [(a,b)] -> Maybe b              -- Não entendi bem o proposito desta função, acho que foi colocada aqui apenas para nos habituar às notações "Maybe", "Just" e "Nothing"
_lookup a [] = Nothing
_lookup a ((h1,h2) : t) | a == h1 = Just h2     
                        | otherwise = (_lookup a t)

-- 36. Calcula o maior prefixo crescente de uma lista dada 

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente (h:t) = preCrescenteAux [h] t                  -- Chamamos a função auxiliar que tem a capacidade de guardar o maior prefixo

preCrescenteAux :: Ord a => [a] -> [a] -> [a]
preCrescenteAux pref [] = pref                                                  -- Caso a lista termine quer dizer que o maior prefixo crescente é a própria lista
preCrescenteAux pref (h:t) | (last pref) < h = preCrescenteAux (pref ++ [h]) t  -- Verificamos se o próximo elemento a ser colocado mantém a ordem crescente e inserimo-lo
                           | otherwise = pref                                  --ou não de acordo com a condição

-- 37. Função que ordena uma lista por ordem crescente 

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort l = iSortAux l l

iSortAux :: Ord a => [a] -> [a] -> [a]
iSortAux l [] = l
iSortAux l [x] = l 
iSortAux l (h:t) | h < (head t) = iSortAux l t
                 | otherwise = iSortAux list list           -- Caso o elemento que estamos a verificar não se encontre ordenado vamos retira lo da lista 
                 where list = (_insert h (_delete h l))     --e depois usamos a função insert para voltar a inserir de forma ordenada

-- 38. Dadas 2 Strings verifica se a primeira é menor que a segunda por ordem lexicográfica

menor :: String -> String -> Bool
menor _ [] = False
menor [] _ = True
menor (h1:t1) (h2:t2) | h1 < h2 = True              -- Verificamos letra a letra qual é que tem o menor valor (lembrando que uma letra é maior que outra se vier depois na ordem alfabética)
                      | h1 == h2 = menor t1 t2      -- Caso seja igual avançamos para a proxima letra
                      | otherwise = False           -- Caso seja maior é porque é falso e não precisamos de verificar mais letras

-- 39. Verifica se um elemento pertence a um multi conjunto

elemMSet :: Eq a => a -> [(a,Int)] -> Bool          -- Análogo ao lookup, mas em vez de retornarmos o elemento do para retornamos o valor bool correspondente
elemMSet a [] = False
elemMSet a ((x, n):t) | a == x = True               
                      | otherwise = elemMSet a t

-- 40. Converte uma lista de instruções de letras numa String

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,0):t) = converteMSet t                      -- Quando um elemento tiver 0 de multiplicidade podemos passar para o próximo elemento
converteMSet ((a,n):t) = [a] ++ converteMSet ((a,(n-1)):t)   -- Vamos subtraindo a multiplicidade e acrescentando à lista

-- 41. Função que acrescenta um elemento a um multi-conjunto

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]                                       -- Caso cheguemos ao fim da lista é porque o valor não existe 
insereMSet a ((x,n):t) | a == x = (x,(n+1)) : t                 -- Caso a letra exista na lista de multi-conjuntos vamos somar 1 ao seu valor
                       | otherwise = (x,n) : (insereMSet a t)   -- Percorremos a lista à procura do elemento

-- 42. Função que remove um elemento a um multi-conjunto

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]               -- Análogo ao anterior 
removeMSet a [] = []
removeMSet a ((x, n):t) | a == x && n == 1 = t
                        | a == x = (x,(n-1)) : t
                        | otherwise = (x,n) : removeMSet a t

-- 43. Função que converte uma String de letras para uma lista de multi-conjuntos

constroiMSet :: Ord a => [a] -> [(a,Int)]                           -- Não tenho a certeza se é suposto manter a ordem das letras caso elas apareçam intercaladas (Ex "aba" resultaria em [(a,2),(b,1)])
constroiMSet l = constroiMSetAux [] l

constroiMSetAux :: Ord a => [(a,Int)] -> [a] -> [(a,Int)]           -- Basta chamar a função insereMSet para cada elemento da String
constroiMSetAux l [] = l
constroiMSetAux l (h2:t2) = constroiMSetAux (insereMSet h2 l) t2

-- 44. Função que divide uma lista de Eithers em 2 listas

_partitionEithers :: [Either a b] -> ([a],[b])
_partitionEithers [] = ([],[])
_partitionEithers (h:t) =
                     let (leftL, rightL) = _partitionEithers t
                     in case h of
                        Left a -> (a:leftL, rightL)
                        Right b -> (leftL, b:rightL)

-- 45. Coleciona os elementos do tipo a de uma lista 

_catMaybes :: [Maybe a] -> [a]                  -- Elementos do tipo "Maybe a" podem ser "Just a" e "Nothing"
_catMaybes [] = []                              
_catMaybes (Nothing:t) = _catMaybes t           -- Esta Função apenas seleciona os elementos do tipo "Just a"
_catMaybes (Just a : t) = a : _catMaybes t

-- 46. Dadas as coordenadas iniciais e finais a função gera a lista de movimentos necessários para ir do ponto inicial ao final

data Movimento = Norte | Sul | Este | Oeste
    deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) | xi == xf && yi == yf = []                       -- Apenas verificamos as coordenadas depois de cada avanço numa determinada direção e registamos a direção desse avanço
                        | xi < xf = Este : caminho ((xi+1),yi) (xf,yf)
                        | xi > xf = Oeste : caminho ((xi-1),yi) (xf,yf)
                        | yi < yf = Norte : caminho (xi,(yi+1)) (xf,yf)
                        | otherwise = Sul : caminho (xi,(yi-1)) (xf,yf)

-- 47. Dadas as coordenadas iniciais e uma lista de movimentos verifica se a função alguma vez volta a passar pela posição inicial

hasloops :: (Int,Int) -> [Movimento] -> Bool
hasloops _ [] = False
hasloops (xc,yc) (Norte:t) = hasloopsAux (xc,yc) (xc,(yc+1)) t
hasloops (xc,yc) (Sul:t)   = hasloopsAux (xc,yc) (xc,(yc-1)) t
hasloops (xc,yc) (Este:t)  = hasloopsAux (xc,yc) ((xc+1),yc) t
hasloops (xc,yc) (Oeste:t) = hasloopsAux (xc,yc) ((xc-1),yc) t

hasloopsAux :: (Int,Int) -> (Int,Int) -> [Movimento] -> Bool
hasloopsAux _ _ [] = False
hasloopsAux x y _ | x == y = True
hasloopsAux i (xc, yc) (Norte:t) = hasloopsAux i (xc,(yc+1)) t
hasloopsAux i (xc, yc) (Sul:t)   = hasloopsAux i (xc,(yc-1)) t
hasloopsAux i (xc, yc) (Este:t)  = hasloopsAux i ((xc+1),yc) t
hasloopsAux i (xc, yc) (Oeste:t) = hasloopsAux i ((xc-1),yc) t

-- 48. Dada uma lista de retangulos calcula quantos deles são quadrados

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (a1,a2) (b1,b2)) : t) 
                                | (a1-b1) == (a2-b2) = 1 + contaQuadrados t  -- Um retangulo será um quadrado caso a diferença de alturas e de larguras seja igual
                                | otherwise = 0 + contaQuadrados t 

-- 49. Dada uma lista de retangulos calcula a área de todos os retangulos somados

areaTotal :: [Rectangulo] -> Float                        
areaTotal [] = 0
areaTotal (h : t) = areaRet h + areaTotal t               -- Chamamos a função que calcula a área do retangulo atual e somamos ao resto da lista

areaRet :: Rectangulo -> Float                            -- Função auxiliar que calcula a área de um unico retangulo
areaRet (Rect (a1,a2) (b1,b2)) = abs((a1-b1)*(a2-b2))

-- 50. Função que determina a quantidade de equipamentos quen não necessitam de reparação

data Equipamento = Bom | Razoavel | Avariado
    deriving Show

naoReparar :: [Equipamento] -> Int              -- Função simples, que se limita a verificar o elemento à cabeça da lista e soma ou não consoante precise de reparação
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (h:t) = 0 + naoReparar t



