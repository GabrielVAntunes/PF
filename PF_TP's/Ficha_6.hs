-- Ex1: Considere o seguinte tipo para representar árvores binárias

data BTree a = Empty
            | Node a (BTree a) (BTree a)
                deriving Show

exampleTree :: BTree Int
exampleTree = Node 6 
                 (Node 3 
                     (Node 2 Empty Empty) 
                     (Node 5 Empty Empty)) 
                 (Node 9
                     (Node 8 
                        (Node 7 Empty Empty) 
                        Empty) 
                     (Node 15 Empty Empty))

tree1 :: BTree (Int, Int, Int)
tree1 = Node (1,2,3)
           (Node (1,2,3) Empty Empty) 
           (Node (1,2,3) Empty Empty) 

-- Para testar alguma função:

main :: IO()
main = print("func" turmaExemplo)

-- a) Calcula a altura de uma árvore

altura :: BTree a -> Int
altura Empty = 0
altura (Node n l r) = 1 + max (altura l) (altura r)

-- b) Conta o número de nodos de uma árvore

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node n l r) = 1 + (contaNodos l) + (contaNodos r)

-- c) Conta o número de folhas de uma árvore

contaFolhas :: BTree a -> Int
contaFolhas Empty = 1
contaFolhas (Node n l r) = 0 + (contaFolhas l) + (contaFolhas r)

-- d) Remove todos os elementos a partir de uma determinada profundidade

prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune _ Empty = Empty
prune x (Node n l r) = Node n (prune (x-1) l) (prune (x-1) r)

-- e) Dado um caminho ([Bool], False: esquerda, True: direita) retorna a lista dos Nodos por onde esse caminho passou

path :: [Bool] -> BTree a -> [a]
path [] (Node n l r) = [n]
path list Empty = []
path (h:t) (Node n l r) | h = n : path t r
                        | otherwise = n : path t l

-- f) Dada uma árvore, retorna a sua simétrica

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node n l r) = (Node n (mirror r) (mirror l))

-- g) Adapta a função zipWith para o formato "BTree a"

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node n1 l1 r1) (Node n2 l2 r2) = (Node (f n1 n2) (zipWithBT f l1 l2) (zipWithBT f r1 r2))

-- h) Generaliza  a função unzip que dada uma árvore de triplos gera um tripo de árvores

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (n1,n2,n3) l r) = ((Node n1 l1 r1), (Node n2 l2 r2), (Node n3 l3 r3))
                            where (l1,l2,l3) = unzipBT l
                                  (r1,r2,r3) = unzipBT r

-- Ex.2: Defina as seguintes funções assumindo que agora as árvores são binárias de procura
--Uma árvore binária de procura é uma árvore binária em que os elementos estão organizados de modo que todos os elementos 
--à esquerda de um nodo são menores que o próprio e todos os elementos á direita são maiores que o próprio:

--                 6
--               /   \
--              3      9
--             / \    / \
--            2   5  8   15
--                  /
--                 7

-- a) Determina o menor elemento de uma árvore não vazia

minimo :: Ord a => BTree a -> a
minimo (Node n Empty _) = n
minimo (Node _ l _) = minimo l

-- b) Remove o menor elemento de uma árvore não vazia

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node n Empty Empty) = Empty
semMinimo (Node n Empty r) = r
semMinimo (Node n l r) = (Node n (semMinimo l) r)

-- c) Numa única travessia calcula o resultado das 2 funções anteriores

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node n Empty Empty) = (n , Empty)
minSmin (Node n Empty r) = (n, r)
minSmin (Node n l r) = (min, Node n left r)
                where (min, left) = minSmin l

-- d) Defina a função que remove um elemento x de uma árvore usando a função anterior
-- !!! Não faz sentido usar a função anterior neste exercício !!!
-- Muito foda, não consegui fazer, Boa sorte! (O do Necc não está completamente certo)

-- Ex3. 3. Considere agora que guardamos a informa ̧c ̃ao sobre uma turma de alunos na seguinte estrutura de dados:

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int | Rep | Faltou
        deriving Show

type Turma = BTree Aluno --  ́arvore binária de procura (ordenada por número)

turmaExemplo :: Turma
turmaExemplo = Node (123, "Joao Silva", ORD, Aprov 16)
                (Node (100, "Maria Pereira", TE, Rep) 
                      Empty
                      (Node (110, "Carlos Santos", MEL, Aprov 12) Empty Empty))
                (Node (150, "Ana Costa", ORD, Faltou) 
                      (Node (140, "Luisa Ferreira", TE, Aprov 18) Empty Empty)
                      Empty)


--                       (123, "Joao Silva", ORD, Aprov 16)
--                      /                                   \
--       (100, "Maria Pereira", TE, Rep)      (150, "Ana Costa", ORD, Faltou)
--                \                              /
--       (110, "Carlos Santos", MEL, Aprov 12) (140, "Luisa Ferreira", TE, Aprov 18)


-- a) Dado um número, verifica se esse aluno está inscrito

inscNum :: Numero -> Turma -> Bool
inscNum x Empty = False
inscNum x (Node (n,_,_,_) l r) | x == n = True
                               | x > n = inscNum x r
                               | otherwise = inscNum x l

-- b) Dado um nome, verifica se esse aluno está inscrito

inscNome :: Nome -> Turma -> Bool
inscNome x Empty = False
inscNome x (Node (_,name,_,_) l r) | x == name = True
                                   | otherwise = inscNome x l || inscNome x r
                                   
-- c) Lista o número e nome de todos os alunos TE ordenados por número  

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,name,TE,_) l r) = trabEst l ++ [(num,name)] ++ trabEst r
trabEst (Node (num,name,_,_) l r) = trabEst l ++ trabEst r

-- d) Retorna a classificação de um aluno, dado o seu número

nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota x (Node (num,name,_,classif) l r) | x == num = Just classif
                                       | x > num = nota x r
                                       | x < num = nota x l

-- e) Calcula a percentagem de alunos que faltaram à avaliação

percFaltas :: Turma -> Float 
percFaltas t = contaFaltas t / fromIntegral (contaNodos t)

contaFaltas :: Turma -> Float
contaFaltas Empty = 0
contaFaltas (Node (_,_,_,Rep) l r) = 1 + contaFaltas l + contaFaltas r
contaFaltas (Node info l r) = contaFaltas l + contaFaltas r

-- f) calcula a média dos alunos que passaram

mediaAprov :: Turma -> Float
mediaAprov t = fromIntegral (somaNotas t) / contaAprovs t

contaAprovs :: Turma -> Float 
contaAprovs Empty = 0
contaAprovs (Node (_,_,_,Aprov _) l r) = 1 + contaAprovs l + contaAprovs r
contaAprovs (Node (_,_,_,_) l r) = contaAprovs l + contaAprovs r

somaNotas :: Turma -> Int
somaNotas Empty = 0
somaNotas (Node (_,_,_,Aprov grade) l r) = grade + somaNotas l + somaNotas r
somaNotas (Node (_,_,_,_) l r) = somaNotas l + somaNotas r

-- g) Calcula o rácio de alunos aprovados por avaliados, com apenas uma travessia

aprovAv :: Turma -> Float
aprovAv t = aprov / aval
            where (aprov, aval) = aprovAvAux t

aprovAvAux :: Turma -> (Float, Float)
aprovAvAux Empty = (0,0)
aprovAvAux (Node (_,_,_,Aprov _) l r) = (1 + aprovL + aprovR, 1 + avalL + avalR)
                            where (aprovL, avalL) = aprovAvAux l
                                  (aprovR, avalR) = aprovAvAux r
aprovAvAux (Node (_,_,_,Rep) l r)     = (aprovL + aprovR, 1 + avalL + avalR)
                            where (aprovL, avalL) = aprovAvAux l
                                  (aprovR, avalR) = aprovAvAux r
aprovAvAux (Node (_,_,_,Faltou) l r)  = (aprovL + aprovR, avalL + avalR)
                            where (aprovL, avalL) = aprovAvAux l
                                  (aprovR, avalR) = aprovAvAux r