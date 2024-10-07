import Data.Char

-- Ex1: Defina as seguintes funções e os respetivos tipos


-- a) Calcula o perímetro de uma circunferencia dado o comprimento do seu raio

perimetro :: Float -> Float
perimetro r = 2*pi*r

-- b) Calcula a distancia entre 2 pontos no plano Cartesiano (Cada ponto é um par de valores tipo double)

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

-- c) Recebe uma lista e devolve um par com o primeiro e o ultimo elemento dessa lista

primUlt :: [a] -> (a,a)
primUlt l = (head l,last l)

-- d) Dado um "m" e um "n" testa se m é multiplo de n

multiplo :: Int -> Int -> Bool
multiplo m n | mod m n == 0 = True
             | otherwise = False

-- e) Recebe uma lista, caso essa lista seja par retira o primeiro elemento, C.C (Caso Contrário) devolve a própria lista           

truncaImpar :: [a] -> [a]
truncaImpar l | mod (length l) 2 == 0 = l
              | otherwise = tail l

-- f) Calcula o maior de 2 números inteiros

max2 :: Int -> Int -> Int 
max2 a b | a > b = a
         | otherwise = b

-- g) Calcula o maior de 3 números inteiros 

max3 :: Int -> Int -> Int -> Int 
max3 a b c | a >= b && a >= c = a
           | b >= a && b >= c = b
           | otherwise = c

-- Ex2: Defina as seguintes funções sobre polinómios de 2º grau

-- a) A função recebe os 3 coeficientes de um polinomio de 2º grau e que calcula o nº de raízes reais desse polinómio

nRaizes :: Float -> Float -> Float -> Int
nRaizes x y z | delta < 0 = 0
              | delta == 0 = 1
              | delta > 0 = 2
             where delta = (y^2) - 4 * x * z

-- b) Utilizando a função anterior, construa uma função que devolve a lsita das raízes reais do polinómio

raizes :: Float -> Float -> Float -> [Float]
raizes x y z | (nRaizes x y z) == 0 = []
             | (nRaizes x y z) == 1 = [root]
             | (nRaizes x y z) == 2 = [root, root_1]
             where 
                root = (((-y) + sqrt((y^2) - 4 * x * z)) / (2*x))
                root_1 = (((-y) - sqrt((y^2) - 4 * x * z)) / (2*x))

-- Ex3: Representemos as horas através de um par de inteiros

type Hora = (Int, Int)

-- a) Testar se um par representa uma hora válida do dia

validTime :: Hora -> Bool
validTime (h,m) | h >= 0 && h < 24 && m >= 0 && m < 60 = True
                | otherwise = False

-- b) Testar se a primeira hora é depois da segunda

afterThen :: Hora -> Hora -> Bool
afterThen (h1,m1) (h2,m2) | h1 > h2 = True
                          | h1 == h2 && m1 > m2 = True
                          | otherwise = False

-- c) Converter hora para o tempo em minutos

hourToMinutes :: Hora -> Int
hourToMinutes (h,m) = (h*60) + m

-- d) Converter um valor em minutos para horas

minutesToHours :: Int -> Hora
minutesToHours m = (div m 60, mod m 60)

-- e) Calcula a diferença entre 2 horas

timeBetween :: Hora -> Hora -> Int
timeBetween t1 t2 = abs(hourToMinutes t1 - hourToMinutes t2)

-- f) Adicionar minutos a uma hora

addMinutes :: Hora -> Int -> Hora
addMinutes t m = minutesToHours(hourToMinutes t + m)

-- Ex4: Repetir o Ex3. Mas agora com a seguinte estrutura de dados

data Hora2 = H Int Int deriving (Show,Eq)

-- a) Verifica se uma hora do dia é válida

validTime2 :: Hora2 -> Bool
validTime2 (H h m )| h >= 0 && h < 24 && m >= 0 && m < 60 = True
                   | otherwise = False

-- b) Testa se a primeira hora é depois da segunda

afterThen2 :: Hora2 -> Hora2 -> Bool
afterThen2 (H h1 m1) (H h2 m2) | h1 > h2 = True
                          | h1 == h2 && m1 > m2 = True
                          | otherwise = False  

-- c) Converte um valor em horas para minutos

hourToMinutes2 :: Hora2 -> Int
hourToMinutes2 (H h m) = (h*60) + m

-- d) Converter um valor de horas para minutos

minutesToHours2 :: Int -> Hora2
minutesToHours2 m = (H (div m 60) (mod m 60))

-- e) Calcula a diferença entre 2 horas

timeBetween2 :: Hora2 -> Hora2 -> Int
timeBetween2 t1 t2 = abs(hourToMinutes2 t1 - hourToMinutes2 t2)

-- f) Adicionar um determinado nº de minutos a uma hora

addMinutes2 :: Hora2 -> Int -> Hora2
addMinutes2 t m = minutesToHours2(hourToMinutes2 t + m)

-- Ex5: Conmsidere o seguinte tipo para representar os possíveis estados de um semáforo

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

-- a) Defina a função que retorna o próximo estado do semáforo

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

-- b) Defina a função que dita se é OBRIGATÓRIO parar num semáforo

stop :: Semaforo -> Bool
stop Verde = False
stop Amarelo = False
stop Vermelho = True

-- c) Testa se o estado de 2 semáforos num cruzamento é seguro

safe :: Semaforo -> Semaforo -> Bool
safe _ Vermelho = True
safe Vermelho _ = True
safe _ _ = False

-- Ex6: Consideremos que um Ponto pode ser representado por:
    -- Coordenadas Cartesianas
    -- Coordenadas Polares

data Ponto = Cartesiano Double Double | Polar Double Double
            deriving (Show,Eq)

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

-- Ex7: Considere o seguinte tipo de dados para representar figuras geométricas num plano

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

-- a) Função que testa de uma figura é um polígono

poligono :: Figura -> Bool 
poligono (Circulo _ _) = False
poligono _ = True

-- b) Calcula a lista dos vértices de uma figura

vertice :: Figura -> [Ponto]
vertice (Circulo _ _) = []
vertice (Retangulo p1 p2) = [p1 , p2 , (Cartesiano (posx p1) (posy p2)) , (Cartesiano (posx p2) (posy p1))]
vertice (Triangulo p1 p2 p3) = [p1 , p2 , p3]

-- c) Complete a definição da função que calcula a área de uma figura

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = distFunc p1 p2
        b = distFunc p2 p3
        c = distFunc p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
        in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
--      ...
area (Circulo p1 r) = pi * r^2
area (Retangulo p1 p2) = abs((posx p1 - posx p2) * (posy p1 - posy p2))

-- d) Definir a função que calcula o perimetro de uma figura

perimetroFigs :: Figura -> Double 
perimetroFigs (Circulo p r) = 2 * pi * r
perimetroFigs (Retangulo p1 p2) = (abs(posx p1 - posx p2) * 2) + (abs(posy p1 - posy p2) * 2)
perimetroFigs (Triangulo p1 p2 p3) = let a = distFunc p1 p2
                                         b = distFunc p2 p3
                                         c = distFunc p3 p1
                                         in a + b + c

-- Ex8: Utilizando as funçoes "ord :: Char -> Int" e "chr :: Int -> Char", defina as seguintes funçoes:          
    -- Nota*: Neste exercicio poupei nas verificações, por exemplo na função "_intToDigit" assumi que o utilizador irá apenas chamar a função para números de 0 a 9


-- a) Função que testa se um Char é uma minúscula

_isLower :: Char -> Bool
_isLower x | val <= 122 && val >= 97 = True
           | otherwise = False
                where val = ord x

-- b) Função que testa se um Char é um dígito

_isDigit :: Char -> Bool
_isDigit x | val <= 57 && val >= 48 = True
           | otherwise = False
                where val = ord x

-- c) Função que testa se um Char é uma letra  

_isAlpha :: Char -> Bool
_isAlpha x | (val <= 122 && val >= 97) || (val >= 65 && val <= 90)= True
           | otherwise = False
                where val = ord x

-- d) Função que converte uma letra para a respetiva maiuscula

_toUpper :: Char -> Char
_toUpper x = chr((ord x) - 32)

-- e) Função que converte um número para o respetivo dígito

_intToDigit :: Int -> Char
_intToDigit x = chr (x + 48)

-- f) Função que converte um dígito para o respetivo número

_digitToInt :: Char -> Int
_digitToInt x = (ord x) - 48