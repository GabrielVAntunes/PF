-- Ex1: Considere o seguinte tipo para representar expressões inteiras

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

-- Os termos deste tipo ExpInt podem ser vistos como árvores cujas folhas são inteiros e cujos nodos (não folhas) são operadores.

exemploExp :: ExpInt
exemploExp = Menos
                (Mult (Const 3)
                      (Mais (Const 5) 
                            (Simetrico (Const 2))))
                (Const 4)

exp2 :: ExpInt
exp2 = (Mais (Const 3) (Menos (Const 2) (Const 5)))                

-- Representa a expressão: 3*(5+(−2))−4   
                        
-- a) Construa uma função que dadas estas expressões calcula o seu valor

calcula :: ExpInt -> Int
calcula (Const n)     = n
calcula (Simetrico n) = -1 * calcula n
calcula (Mais l r)    = calcula l + calcula r
calcula (Menos l r)   = calcula l - calcula r
calcula (Mult l r)    = calcula l * calcula r 

-- b) Construa uma função que escreva a árvore ExpInt por extenso (no formato de equação)

infixa :: ExpInt -> String
infixa (Const n)     = show n
infixa (Simetrico n) = "(-" ++ infixa n ++ ")"
infixa (Mais l r)    = "(" ++ infixa l ++ " + " ++ infixa r ++ ")"
infixa (Menos l r)   = "(" ++ infixa l ++ " - " ++ infixa r ++ ")"
infixa (Mult l r)    = "(" ++ infixa l ++ " * " ++ infixa r ++ ")" 

-- c) Construa uma função que escreve a expressão num outro formato:
--(Mais (Const 3) (Menos (Const 2) (Const 5))) -> "3 2 5 - +"

posfixa :: ExpInt -> String
posfixa (Const n)     = show n ++ " "
posfixa (Simetrico n) = "(-" ++ posfixa n ++ ") "
posfixa (Mais l r)    = posfixa l ++ posfixa r ++ "+ "
posfixa (Menos l r)   = posfixa l ++ posfixa r ++ "- "
posfixa (Mult l r)    = posfixa l ++ posfixa r ++ "* " 

-- Esta versão deixa um espaço no fim da String, não consegui remover esse espaço sem usar uma função auxiliar

-- Ex.2: Considere o seguinte tipo para representar árvores irregulares (Rose trees).

data RTree a = R a [RTree a]

exemploRTree :: RTree Int
exemploRTree = R 1 [
                    R 2 [R 5 [],
                         R 6 []],
                    R 3 [R 7 []],
                    R 4 [R 8 [R 9 []]]
                   ]

--                    1
--                 /  | \
--                2   3   4
--              /  \  |    \
--             5    6 7     8
--                          |
--                          9


-- a) Função que soma todos os elementos da árvore

soma :: Num a => RTree a -> a
soma (R n []) = n
soma (R n l) = n + sum (map soma l)

-- b) Função que calcula a altura de uma árvore

altura :: RTree a -> Int
altura (R _ []) = 1
altura (R n l) = 1 + maximum (map altura l)
-- A função maximum tem o mesmo propósito que a função max, mas opera numa lista

-- c) Remove todos os elementos a partir de uma dada profundidade

prune :: Int -> RTree a -> RTree a
prune _ (R n []) = (R n [])
prune 0 (R n l) = (R n [])
prune x (R n l) = R n (map (prune (x-1)) l)
-- Consideremos que a raíz da árvore é o nível 0

-- d) Gera a árvore simétrica

mirror :: RTree a -> RTree a 
mirror (R n []) = (R n [])
mirror (R n l)  = R n (reverse (map mirror l))

-- e) faz a travessia postOrder da árvore
-- A travessia da exemploRTree seria [5,6,2,7,3,9,8,4,1]
-- Vai se o mais profundo possivel para a esquerda e depois vamos subindo até percorrer a árvore toda

postorder :: RTree a -> [a] 
postorder (R n []) = [n]
postorder (R n l) = concatMap postorder l ++ [n]

-- Ex3: Relembre a definição de árvores binárias apresentada na ficha anterior

data BTree a = Empty | Node a (BTree a) (BTree a)

-- Nestas árvores a informação está nos nodos, mas existe outra definição em que 
--a informação está apenas nas extremidades (leaf trees):

data LTree a = Tip a | Fork (LTree a) (LTree a)

exemploLTree :: LTree Int
exemploLTree = (Fork
                 (Fork (Tip 1) (Tip 2)) 
                 (Fork (Tip 3) (Tip 4)) 
                )

-- a) Soma as folhas de uma árvore

ltSum :: Num a => LTree a -> a 
ltSum (Tip n) = n 
ltSum (Fork l r) = ltSum l + ltSum r

-- b) Lista as folhas da árvore, da esquerda para a direita

listaLT :: LTree a -> [a]
listaLT (Tip n)    = [n]
listaLT (Fork l r) = listaLT l ++ listaLT r

-- c) Calcula a altura de uma árvore

ltHeight :: LTree a -> Int
ltHeight (Tip n) = 1
ltHeight (Fork l r) = 1 + max (ltHeight l) (ltHeight r)

-- Ex.4: Podemos agrupar estes 2 conceitos nas full trees onde tanto os nodos como as folhas 
--guardam informação, notar que o tipo de informação não tem de ser o mesmo

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

exemploFTree :: FTree Int String
exemploFTree = No 1 
                   (No 2 (Leaf "A") (Leaf "B"))
                   (No 3 (Leaf "C") (Leaf "D"))

--                         No 1
--                      /        \
--                 No 2           No 3
--                /    \         /    \
--          Leaf "A"  Leaf "B" Leaf "C" Leaf "D"



-- a) Separa uma full tree numa binary tree e numa leaf tree

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty, Tip b)
splitFTree (No a l r) = ((Node a btreel btreer),Fork ltreel ltreer)
                where (btreel, ltreel) = splitFTree l
                      (btreer, ltreer) = splitFTree r  

-- b) Faz o processo inverso da função anterior, dadas uma BTree e uma LTree constroi um FTree

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip n) = Just (Leaf n)
joinTrees (Node n bl br) (Fork ll lr) = Just (No n ftl ftr)
                                            where Just ftl = joinTrees bl ll
                                                  Just ftr = joinTrees br lr
joinTrees _ _= Nothing
