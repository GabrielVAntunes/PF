import Data.List

-- Ex.1: Assumamos que uma hora é representada por um par de inteiros 

data Hora = H Int Int
        deriving Show

-- Assumamos também que uma viagem é representada por uma lista de etapas, que por sua vez consistem num par de horas

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- Exemplo de como se representaria uma viagem por este formato

-- [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]

-----------------------------------------------------------------------------------------------

-- Funções de Manipulação / Controlo de Horas neste mesmo formato construídas na "Ficha_1.hs"

validTime :: Hora -> Bool
validTime (H h m )| h >= 0 && h < 24 && m >= 0 && m < 60 = True
                   | otherwise = False

-- b) Testa se a primeira hora é depois da segunda

afterThen :: Hora -> Hora -> Bool
afterThen (H h1 m1) (H h2 m2) | h1 > h2 = True
                              | h1 == h2 && m1 > m2 = True
                              | otherwise = False  

-- c) Converte um valor em horas para minutos

hourToMinutes :: Hora -> Int
hourToMinutes (H h m) = (h*60) + m

-- d) Converter um valor de horas para minutos

minutesToHours :: Int -> Hora
minutesToHours m = (H (div m 60) (mod m 60))

-- e) Calcula a diferença entre 2 horas

timeBetween :: Hora -> Hora -> Int
timeBetween t1 t2 = abs(hourToMinutes t1 - hourToMinutes t2)

-- f) Adicionar um determinado nº de minutos a uma hora

addMinutes :: Hora -> Int -> Hora
addMinutes t m = minutesToHours(hourToMinutes t + m)

-----------------------------------------------------------------------------------------------

-- Devemos assumir que todas as viagens se realizam no mesmo dia

-- a) Testa se uma etapa está bem construída

validStep :: Etapa -> Bool
validStep (h1, h2) = afterThen h2 h1

-- b) Testa se uma viagem está bem construída (todas as etapas são válidas e há tempo entre etapas)

validTrip :: Viagem -> Bool
validTrip [(h1, h2)] = validStep (h1, h2)
validTrip ((h1,h2):(snd1,snd2):t) | (validStep (h1,h2)) && (afterThen snd1 h2) = validTrip ((snd1, snd2): t)
                    | otherwise = False

-- c) Calcular a hora de partida e de chegada de uma determinada viagem  

departureAndArrival :: Viagem -> Etapa
departureAndArrival trip = (h1,l2)
                                where (h1,h2) = head trip
                                      (l1,l2) = last trip

-- d) Dada uma viagem válida calcular o tempo total de viagem efetiva

-- Como usei a função timeBetween, esta função vai retornar o tempo de viagem em minutos
travelTime :: Viagem -> Int 
travelTime [] = 0
travelTime ((h1,h2):t) = (timeBetween h1 h2) + travelTime t

-- e) Dada uma viagem válida calcula o tempo de espera entre viagens 

awaitedTime :: Viagem -> Int
awaitedTime [(h1,h2)] = 0
awaitedTime ((h1,h2):(snd1,snd2):t) = (timeBetween h2 snd1) + awaitedTime ((snd1,snd2):t)

-- f) Dada uma viagem válida calcula o tempo total da viagem

totalTime :: Viagem -> Int
totalTime trip = timeBetween h1 h2
                where (h1,h2) = departureAndArrival trip


-- Ex.2: Considere a seguinte definição de um tipo para representar linhas poligonias


type Poligonal = [Ponto]

-- Consideremos a definição do tipo "Ponto" definidas na "Ficha_1.hs"

data Ponto = Cartesiano Double Double | Polar Double Double
            deriving (Show,Eq)

-----------------------------------------------------------------------------------------------

-- Funções de Manipulação / Controlo de pontos neste mesmo formato construídas na "Ficha_1.hs"

-- a) Calcula a Distancia de um ponto ao eixo dos x's

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar dist angle) = dist * (cos angle)

-- b) Calcula a Distancia de um ponto ao eixo dos y's

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar dist angle) = dist * (sin angle)

-- c) Calcula a Distancia de um ponto à origem

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2 + y^2)
raio (Polar dist angle) = dist

-- d) Calcula o angulo do vetor que liga a origem ao ponto

angulo :: Ponto -> Double
angulo (Cartesiano x y) | x > 0            = atan (y / x)       -- Estas condições definem a função "atan2" em haskell que tem exatamente o mesmo propósito
                        | x < 0  && y >= 0 = atan (y / x) + pi
                        | x < 0  && y < 0  = atan (y / x) - pi
                        | x == 0 && y > 0  = pi / 2
                        | x == 0 && y < 0  = - pi / 2
angulo (Polar dist angle) = angle    

-- e) Função que calcula a distancia entre 2 pontos

distFunc :: Ponto -> Ponto -> Double
distFunc (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)
distFunc (Cartesiano x y) (Polar dist angle) = sqrt((posx (Polar dist angle) - x)^2 + (posy (Polar dist angle) - y)^2)
distFunc (Polar dist angle) (Cartesiano x y) = sqrt(( x - posx (Polar dist angle))^2 + (y - posy (Polar dist angle))^2)
distFunc (Polar dist1 angle1) (Polar dist2 angle2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)
                                        where x2 = posx (Polar dist2 angle2)
                                              x1 = posx (Polar dist1 angle1)
                                              y2 = posy (Polar dist2 angle2)
                                              y1 = posy (Polar dist1 angle1)

-----------------------------------------------------------------------------------------------

-- a) Função que calcula o comprimento de uma linha poligonal

pLineSize :: Poligonal -> Double
pLineSize [x] = 0
pLineSize (h:t) = (distFunc h (head t)) + pLineSize t

-- b) Teste se uma dada linha poligonal é ou não fechada

pLineClosed :: Poligonal -> Bool
pLineClosed [] = False
pLineClosed p | posx (head p) == posx (last p) && posy (head p) == posy (last p) = True
             | otherwise = False

-- c) Dada uma linha poligonal fechada convexa, retorna uma lista de triangulos cuja soma das áreas seja igual à área delimitada pela Linha Poligonal

-- Consideremos o tipo "Figura" definido na "Ficha_1.hs"

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

triangula :: Poligonal -> [Figura]
triangula [] = []
triangula (h:snd:t) | length ((h:snd:t)) <= 3 = []
                    | otherwise = triangulaAux h (snd:t)

-- Esta auxiliar é um bocado manhosa, ter atenção
triangulaAux :: Ponto -> Poligonal -> [Figura]
triangulaAux p [x,y] = []
triangulaAux p (h:snd:t) = (Triangulo p h snd) : triangulaAux p (snd:t)  

-- d) Função que calcula a área delimitada por uma linha poligonal fechada e convexa

-- Consideremos a função já definida na "Ficha_1.hs"

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = distFunc p1 p2
        b = distFunc p2 p3
        c = distFunc p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
        in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Circulo p1 r) = pi * r^2
area (Retangulo p1 p2) = abs((posx p1 - posx p2) * (posy p1 - posy p2))

-----------------------------------------------------------------------

pLineTotalArea :: Poligonal -> Double
pLineTotalArea p = pLTAAux (triangula p)

pLTAAux :: [Figura] -> Double
pLTAAux [] = 0
pLTAAux (h:t) = (area h) + pLTAAux t

-- e) Dada uma linha Poligonal e um ponto dá como resultado uma linha Poligonal identica á primeira mas com inicio nesse ponto

mover :: Poligonal -> Ponto -> Poligonal
mover [] _ = []
mover (h:t) p = moverAux (subtractPoints h p) (h:t)

moverAux :: Ponto -> Poligonal -> Poligonal
moverAux p [] = []
moverAux p (h:t) = (addPoints p h) : moverAux p t

subtractPoints :: Ponto -> Ponto -> Ponto
subtractPoints p1 p2 = (Cartesiano ((posx p2)-(posx p1)) ((posy p2)-(posy p1)))

addPoints :: Ponto -> Ponto -> Ponto
addPoints p1 p2 = (Cartesiano ((posx p2)+(posx p1)) ((posy p2)+(posy p1)))

-- f) Dada uma linha poligonal e um fator de escala, retorna essa linha Poligonal, com inicio no mesmo ponto mas as distancias entre cada ponto são multiplicadas pelo tal fator

zoom :: Double -> Poligonal -> Poligonal
zoom 0 (h:t) = replicate (length (h:t)) h
zoom n [x] = [x]
zoom n (h:t) = h : zoomAux h (vectorFactor n (h:t))

zoomAux :: Ponto -> Poligonal -> Poligonal
zoomAux _ [] = []
zoomAux p (h2 : t2) = sum : zoomAux sum (t2)
                        where sum = (addPoints p h2)

vectorFactor :: Double -> Poligonal -> Poligonal
vectorFactor _ [x] = []
vectorFactor n (h:t) = (Cartesiano (n*x) (n*y)) : (vectorFactor n (t))
                        where (Cartesiano x y) = (subtractPoints h (head t))          

-- Ex.3: Consideremos os seguintes tipos de dados para armazenar Contactos (telefónicos e Correio eletronico)

data Contacto = Casa Integer| Trab Integer| Tlm Integer| Email String
                  deriving Show

-- Consideremos também os seguintes tipos para definir um nome e uma Agenda, que associa uma lista de contactos a um determinado nome

type Nome = String
type Agenda = [(Nome, [Contacto])]

-- a) Função que acrescenta um email a uma pessoa na agenda
      -- Vou assumir que se o nome da pessoa não constar na agenda não é para acrescentar nada

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail _ _ [] = []
acrescEmail name email ((entry,contacts):t) | name == entry = (entry, (contacts++[(Email email)])) : t
                                            | otherwise = (entry,contacts) : acrescEmail name email t

-- b) Dado um nome e uma agenda retorna uma lista dos emails dessa pessoa

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails name ((entry,contacts):t) | name == entry = Just (listEmails contacts)
                                    | otherwise = verEmails name t

listEmails :: [Contacto] -> [String]
listEmails [] = []
listEmails ((Email mail):t) = (mail : listEmails t)
listEmails (h:t) = listEmails t

-- c) Dada uma lista de contactos, retorna uma lista com todos os contactos telefonicos

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Casa num):t) = num : consTelefs t
consTelefs ((Tlm num):t) = num : consTelefs t
consTelefs ((Trab num):t) = num : consTelefs t
consTelefs (h:t) = consTelefs t

-- d) Dado um Nome e uma Agenda retorna o número do telefone de casa

casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa name ((entry,contacts):t) | name == entry && hasHomeNum contacts = Just (searchHomeNum contacts)
                               | otherwise = casa name t

searchHomeNum :: [Contacto] -> Integer
searchHomeNum ((Casa num):t) = num
searchHomeNum (h:t) = searchHomeNum t

hasHomeNum :: [Contacto] -> Bool
hasHomeNum  [] = False
hasHomeNum ((Casa num):t) = True
hasHomeNum (h:t) = hasHomeNum t

-- Ex.4: Criaram se as seguintes estruturas para registar  informação sobre aniversários

type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String   (Já tinha sido declarada para o exercicio anterior)

data Data = D Dia Mes Ano
      deriving Show

type TabDN = [(Nome,Data)]

-- a) Dado um nome e uma Tabela com Datas de Nascimento, devolve a Data de Nascimento dessa pessoa

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura name ((key,date):t) | name == key = Just date
                            | otherwise = procura name t

-- b) Calcula a idade de uma pessoa numa determinada data 

-- Não tenho a certeza se as condições acertam a idade em todas as datas mas o raciocinio é esse
idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade (D day month year) name ((key,(D day_k month_k year_k)):t) | name == key && ((month_k > month) || (day_k > day && month_k == month)) = Just (year - year_k - 1)
                                                                 | name == key = Just (year - year_k )
                                                                 | otherwise = idade (D day month year) name t

-- c) Uma função que testa se uma data é antes da outra
      -- Vamos assumir que o programa recebe sempre 2 datas válidas

anterior :: Data -> Data -> Bool
anterior (D d1 m1 y1) (D d2 m2 y2) | y1 > y2 = False
                                   | y1 == y2 && m1 > m2 = False
                                   | y1 == y2 && m1 == m2 && d1 >= d2 = False
                                   | otherwise = True

-- d) Ordena uma tabela de datas de nascimento por ordem crescente das datas de nascimento

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((key, D day month year):t) = ordenaAux ((key, D day month year):t) ((key, D day month year):t)

ordenaAux :: TabDN -> TabDN -> TabDN
ordenaAux lOrd [x] = lOrd
ordenaAux lOrd ((key, d1):(key2, d2):t) | anterior d1 d2 = ordenaAux lOrd ((key2, d2):t)
                                        | otherwise = ordenaAux list list
                                                where list = ([(key2, d2)] ++ (deleteDate (key2, d2) lOrd))

deleteDate :: (Nome,Data) -> TabDN -> TabDN
deleteDate _ [] = []
deleteDate (name, d)  ((name1, d1):t) | name == name1 = t
                                      | otherwise = (name1,d1) : deleteDate (name, d) t   

-- e) Apresenta o nome e a idade das pessoas numa determinada data por ordem crescente de idade   

porIdade :: Data -> TabDN -> [(Nome, Int)]
porIdade d l = porIdadeAux d (reverse (ordena l))

porIdadeAux :: Data -> TabDN -> [(Nome,Int)]
porIdadeAux _ [] = []
porIdadeAux d ((name,D day month year): t) = (name, (idadeDate d (D day month year))) : porIdadeAux d t

idadeDate :: Data -> Data -> Int
idadeDate (D day month year) (D day1 month1 year1) | (month1 > month) || (day1 > day && month1 == month) = (year - year1 - 1) -- Ainda não fez
                                                   | otherwise = (year - year1 )  -- Já fez

-------------------------------------------------------------------------------------------------------------------------

-- Consideremos os seguintes tipos de dados para descrever informações de um extrato bancário

-- O extracto é representado pelo saldo inicial e uma lista de movimentos
-- Cada movimento é representado por um triplo que indica a data da operação, a sua descrição e a quantia movimentada
data Extracto = Ext Float [(Data, String, Movimento)]
            deriving Show

-- Quantia movimentada (Os valores são sempre positivos)
data Movimento = Credito Float | Debito Float
            deriving Show

-- a) Produz uma lista como todos os movimentos superiores a um determinado valor

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext init ((date, descr, Debito value):t)) min | value >= min = (Debito value) : extValor (Ext init t) min
                                                        | otherwise = extValor (Ext init t) min
extValor (Ext init ((date, descr, Credito value):t)) min | value >= min = (Credito value) : extValor (Ext init t) min
                                                         | otherwise = extValor (Ext init t) min    

-- b) Retorna apenas informações relativas aos movimentos cuja descrição esteja incluída na lista fornecida

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext init ((date, descr, move):t)) descrList | elem descr descrList = (date, move) : filtro (Ext init t) descrList
                                                    | otherwise = filtro (Ext init t) descrList

-- c) retorna o total de créditos e de débitos de um extracto no primeiro e segundo elemento  de um par respetivamente

creDeb :: Extracto -> (Float,Float)
creDeb ext = (sumCreds ext, sumDebs ext)

sumCreds :: Extracto -> Float 
sumCreds (Ext _ []) = 0
sumCreds (Ext init ((date, descr, Credito value):t)) = value + sumCreds (Ext init t)
sumCreds (Ext init (h:t)) = sumCreds (Ext init t)

sumDebs :: Extracto -> Float 
sumDebs (Ext _ []) = 0
sumDebs (Ext init ((date, descr, Debito value):t)) = value + sumDebs (Ext init t)
sumDebs (Ext init (h:t)) = sumDebs  (Ext init t)

-- d) devolve o saldo final resultande de todos os movimentos no extrato sobre o saldo inicial

saldo :: Extracto -> Float
saldo (Ext final []) = final
saldo (Ext init ((date, descr, Debito value):t)) = saldo (Ext (init - value) t)
saldo (Ext init ((date, descr, Credito value):t)) = saldo (Ext (init + value) t)