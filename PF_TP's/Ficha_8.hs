import Data.List

-- Ex.1: Considere o seguinte tipo de dados para representar frações

data Frac = F Integer Integer
--           ^^^^^^^^ ^^^^^^^
--          Numerador Denominador

-- a) Dada uma fração calcula essa fração na forma irredutível com denominador positivo
--Sugestão: começar por calcular o maximo divisor comum pela propriedade de Euclides

mdc :: Integer -> Integer -> Integer
mdc x y | x == y = x
        | x > y = mdc (x-y) y
        | otherwise = mdc (y-x) x

normaliza :: Frac -> Frac
normaliza (F nume deno) = normalizaAux (mdc (abs nume) (abs deno)) nume deno

normalizaAux :: Integer -> Integer -> Integer -> Frac
normalizaAux mdcval nume deno | deno > 0 = (F (nume `div` mdcval) (deno `div` mdcval))
                              | deno < 0 = (F (nume*(-1) `div` mdcval) (deno*(-1) `div` mdcval)) 
                         
-- Temos de usar `div` em vez "/" uma vez que estamos a lidar com "Integer"'s então temos de garantir que 
--o resultado da divisão é também um Integer, (`div` realiza a operação inteira)

-- b) Defina Frac como uma instância da classe Eq.

instance Eq Frac where
        (==) (F nume1 deno1) (F nume2 deno2) = nume1 `div` deno1 == nume2 `div` deno2

-- c) Defina Frac como uma instância da classe Ord

instance Ord Frac where
        compare (F nume1 deno1) (F nume2 deno2) | nume1 `div` deno1 == nume2 `div` deno2 = EQ
                                                | nume1 `div` deno1 > nume2 `div` deno2 = GT
                                                | nume1 `div` deno1 < nume2 `div` deno2 = LT

-- d) Defina Frac como uma instância da classe Show

instance Show Frac where
        show (F nume 1) = show nume
        show (F nume deno) = show nume ++ "/" ++ show deno

-- e) Defina Frac como uma instancia da classe Num
--Relembremos a definição:

-- class (Eq a, Show a) => Num a where
-- (+), (*), (-) :: a -> a -> a
-- negate, abs, signum :: a -> a
-- fromInteger :: Integer -> a

instance Num Frac where
        (+) (F nume1 deno1) (F nume2 deno2) = normaliza (F ((nume1 * deno2) + (nume2 * deno1)) (deno1 * deno2))
        (-) (F nume1 deno1) (F nume2 deno2) = normaliza (F ((nume1 * deno2) - (nume2 * deno1)) (deno1 * deno2))
        (*) (F nume1 deno1) (F nume2 deno2) = normaliza (F (nume1 * nume2) (deno1 * deno2))
        negate (F nume deno) = normaliza (F (-1*nume) (deno))
        abs (F nume deno) = (F (abs nume) (abs deno))
        signum (F nume deno) | nume == 0 = (F 0 1)
                             | nume * deno > 0 = (F 1 1)
                             | otherwise = (F (-1) 1)             -- Não tenho a certeza se o signum tinha de retornar no formato dado, assumi que sim
        fromInteger n = (F n 1)

-- f) Defina uma função que dada uma fração e uma lista de frações retorna os elementos da lista que são maiores que o dobro que a fração inicial

gtDoubleFrac :: Frac -> [Frac] -> [Frac]
gtDoubleFrac f l = filter (\x -> x > 2*f) l

-- Ex.2: Relembre o tipo definido na Ficha 7 para representar expressões inteiras Uma possível generalização será considerar expressões cujas constantes são 
--de um qualquer tipo numérico

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

-- Reaproveitemos as funções calcula e infixa já definidas nas ficha 7.

calcula :: Num a => Exp a -> a
calcula (Const n)     = n
calcula (Simetrico n) = -1 * calcula n
calcula (Mais l r)    = calcula l + calcula r
calcula (Menos l r)   = calcula l - calcula r
calcula (Mult l r)    = calcula l * calcula r 

-- b) Construa uma função que escreva a árvore ExpInt por extenso (no formato de equação)

infixa :: Show a => Exp a -> String
infixa (Const n)     = show n
infixa (Simetrico n) = "(-" ++ infixa n ++ ")"
infixa (Mais l r)    = "(" ++ infixa l ++ " + " ++ infixa r ++ ")"
infixa (Menos l r)   = "(" ++ infixa l ++ " - " ++ infixa r ++ ")"
infixa (Mult l r)    = "(" ++ infixa l ++ " * " ++ infixa r ++ ")" 

-- a) Declare Exp a como uma instância da classe Show

instance Show a => Show (Exp a) where
        show x = infixa x

-- b) Declare Exp a como uma instância da classe Eq

instance (Eq a, Num a) => Eq (Exp a) where
        (==) exp1 exp2 = calcula exp1 == calcula exp2

-- c) Declare Exp a como uma instância da classe Num

instance Num a => Num (Exp a) where
        (+) exp1 exp2 = Const (calcula exp1 + calcula exp2)
        (-) exp1 exp2 = Const (calcula exp1 - calcula exp2)
        (*) exp1 exp2 = Const (calcula exp1 * calcula exp2)
        negate exp = Const (negate (calcula exp))
        abs exp = Const (abs (calcula exp))
        signum exp = Const (signum (calcula exp))
        fromInteger exp = Const (fromInteger exp)

-- Ex.3: Relembre o exercício da Ficha 3 sobre contas bancárias, com a seguinte declaração de tipos

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

extratoExemplo :: Extracto
extratoExemplo = Ext 1000.0 -- Saldo inicial
    [ (D 15 11 2024, "Salário", Credito 2000.0),
      (D 10 11 2024, "Supermercado", Debito 150.0),
      (D 20 10 2024, "Aluguel", Debito 800.0),
      (D 5 11 2024, "Jantar", Debito 120.0),
      (D 1 11 2024, "Reembolso", Credito 50.0)
    ]

-- a) Defina Data como instância de classe Ord

instance Eq Data where
    (==) (D d1 m1 y1) (D d2 m2 y2) = d1 == d2 && m1 == m2 && y1 == y2

instance Ord Data where
        compare (D d1 m1 y1) (D d2 m2 y2) = compare (y1,m1,d1) (y2,m2,d2)

-- b) Defina Data como uma instância da classe Show

instance Show Data where
        show (D d m y) = show d ++ "/" ++ show m ++ "/" ++ show y

-- c) Defina uma função que transforma um extrato de modo a que a lista de movimentos apareça ordenada por ordem crescente de data

ordenaExtrato :: Extracto -> Extracto
ordenaExtrato (Ext initVal lext) = (Ext initVal (sortOn (\(date,_,_) -> date) lext))

-- d) Defina Extracto como instância da classe Show de modo a que a apresentação do extrato seja por ordem de data do movimento e que siga o seguinte formato

--     Saldo anterior: 300
--     ---------------------------------------
--     Data Descricao Credito Debito
--     ---------------------------------------
--     2010/4/5 DEPOSITO 2000
--     2010/8/10 COMPRA 37,5
--     2010/9/1 LEV 60
--     2011/1/7 JUROS 100
--     2011/1/22 ANUIDADE 8
--     ---------------------------------------
--     Saldo actual: 2294,5

-- O exercício pede para apresentar a data no formato "ano/mes/dia", mas como já tinha definido no formato "dia/mes/ano" vou manter a minha versão

-- Aproveitemos a função definida na "Ficha_3.hs" que calcula o valor final do Extracto

saldo :: Extracto -> Float
saldo (Ext final []) = final
saldo (Ext init ((_, _, Debito value):t)) = saldo (Ext (init - value) t)
saldo (Ext init ((_, _, Credito value):t)) = saldo (Ext (init + value) t)

instance Show Extracto where
        show (Ext initVal lext) = "Saldo anterior: " ++ show initVal ++ "\n"
                                ++"---------------------------------------\n"
                                ++"Data       Descricao   Credito   Debito\n"
                                ++"---------------------------------------\n"
                                ++ concatMap showMoves ordLext
                                ++"---------------------------------------\n"
                                ++"Saldo actual: " ++ show (saldo (Ext initVal lext))
                                where
                                 Ext _ ordLext = (ordenaExtrato (Ext initVal lext))

showMoves :: (Data, String, Movimento) -> String
showMoves (date, descr, Credito val) = pad (show date) 11 ++ pad descr 12 ++ show val ++ "\n"
showMoves (date, descr, Debito val)  = pad (show date) 11 ++ pad descr 12 ++ replicate 10 ' ' ++ show val ++ "\n"

-- Fui pesquisar esta função apenas para formatar melhor o extracto, é opcional
pad :: String -> Int -> String
pad str width = str ++ replicate (width - length str) ' '

main :: IO()
main = print (extratoExemplo)