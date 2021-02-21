module Map(
    Map,
    generateMaps,
    subMatrixFilter,
    getCoin,
    getWeapon,
    coinsLeft,
    walls,
    map2pic
) where


import Data.Array
import System.Random
import CodeWorld




{-----DEFINICION TIPO MAPA-------------}


--El mapa sera un array bidimensional con valores que indicaran el tipo de la celda que representa 
type Map=  Array (Int,Int) Int

{-----FUNCIONES PARA GENERAR MAPAS----------------}

--Dado un mapa y un int e, devuelve una lista de la posiciones en las que su valor es igual a e
mFilter:: Map -> Int -> [(Int,Int)]
mFilter m e = [   (i,j)     | i <-[1..25] , j <-[1..19] , m!(i,j)==e ]


--A partir del mapa y de unas posiciones x y obtendremos las posiciones (validas) de la submatriz n x n centrada en x y
subMatrix:: Map -> Int -> Int -> Int ->  [(Int,Int)]
subMatrix m x y n = [(i,j)| i <-[x-r..x+r] , j <-[y-r..y+r] ,  (i>=1 && i <=25 ) && (j>=1 && j <=19 ) ]
    where r = floor ((fromIntegral n)/2)


--Metodo para obtener la lista de las posiciones de la submatrix n x n cuyo valor es == e usando la funcion subMatrix
subMatrixFilter:: Map -> Int -> Int -> Int -> Int -> [(Int,Int)]
subMatrixFilter m x y n e = filter (\(x,y)-> m!(x,y)==e) sm
    where sm = subMatrix m x y n

--Función generadora de la lista de los mapas que se usaran en la partida 
--(El primer mapa debe de ser generado manualmente, será la pantalla principal del juego)
generateMaps:: StdGen -> Int-> [Map]
generateMaps seed numberOfLv = [generateEmptyMap]  ++ [head maps] ++ [generateWeaponMap] ++  tail maps ++ [generateFinalMap]
    where rolls = (randomRs (0,100) seed) :: [Int]  --Lista de probabilidades que usaremos para generar la matriz aleatoria.
          maps = generateMap rolls (numberOfLv-2)

--Genera el mapa vacio:
generateEmptyMap::Map
generateEmptyMap = array ((1,1),(25,19)) 
            [ ((i,j), 0   )   | i <-[1..25] , j <-[1..19] ]

--Genera el mapa para el último lv:
generateFinalMap::Map
generateFinalMap = array ((1,1),(25,19))
        [ ((i,j), f i j   )   | i <-[1..25] , j <-[1..19] ]
    where f i j
            |( (i == 4 )  || (i == 22))  && j/=9 && j/=11  && j < 17 && j >3   = 1-- && ( j)
            |( (j == 4 )  || (j == 16))  && i/=12 && i/=14 && i < 23 && i >3   = 1
            | i == 11 && (j == 8 || j == 12) = 1
            | i == 15 && (j == 8 || j == 12) = 1
            | i == 2  && (j == 2 || j == 18) = 1
            | i == 24 && (j == 2 || j == 18) = 1
            | otherwise = 0
--Genera el mapa del lv2, donde se encuentran las armas 
generateWeaponMap:: Map
generateWeaponMap = array ((1,1),(25,19)) 
            [ ((i,j), f i j   )   | i <-[1..25] , j <-[1..19] ]
    where f i j 
            | i==6 && j==6 = 3
            | i==13 && j ==15 = 4
            | i==20 && j==6 = 5
            | otherwise = 0

--Funcion que dado un Int que representa el nivel, te genera la matriz que representa dicho nivel
generateMap:: [Int] -> Int -> [Map]
generateMap rolls lv
    | lv == 0 = []
    | otherwise = generateMap futureRolls (lv-1) ++ [validMap lv matrix]  --Cambiar testMap por validMap
    where (ourRolls,futureRolls) = splitAt (25*19) rolls       --Esto es para que en cada iteracion utilice nº distintos sin repetir en total para cada mapa necesitamos 24*18 valores aleatorios
          matrix = array ((1,1),(25,19)) 
            [ ((i,j), randomCell i j ourRolls  )   | i <-[1..25] , j <-[1..19] ]


--Dado un mapa generado aleatoriamente y el nivel de este mapa, devuelve un mapa válido a partir del anterior.
validMap:: Int-> Map -> Map
validMap lv map = array ((1,1),(25,19)) [ ((i,j),f i j) | i <-[1..25] , j <-[1..19]]
    where goldAmount = length $ mFilter map 2
          f i j 
            | i == 1 || j == 1 || j== 19 || i == 25 = 0 --En las posiciones colindantes a los limites del mapa solo saldrán casillas vacías
            | elem (i,j) (subMatrix map 13 10 3) = 0    --En el centro del mapa sólo saldrán casillas vacías 
            | (length $ subMatrixFilter map i j 3 1) > 5 = 0  --Si hay más de 5 muros en la misma submatriz 3x3 eliminamos el muro central de esa submatrix (evitamos callejones sin salida)
            | (map!(i,j) == 2 ) && (length $ subMatrixFilter map i j 3 1) >3 = 1   --Si una moneda esta rodeada de más de 4 muros se cambiará por un muro (evita monedas inalcanzables)
            | lv == 1 && goldAmount <10 && (length $ subMatrixFilter map i j 3 2 )<2 = 2  --Si estamos en el lv 1,hay menos de 10 monedas y la submatrix 3x3 de esa casilla no tiene más de 1moneda, ponemos 1 moneda  
            | otherwise = map!(i,j)  --Dejamos la posición como se ha generado aleatoramente

--Funcion para generar un mapa testing:
testMap:: Int -> Map -> Map
testMap _ map = array ((1,1),(25,19)) [ ((i,j),f i j) | i <-[1..25] , j <-[1..19]]
    where f i j 
            | elem (i,j) (subMatrix map 13 10 3) = 0
            | otherwise = 1

--Dada la posicion de la celda y la lista de probabilidades nos da el tipo de la celda
randomCell:: Int -> Int -> [Int] -> Int
randomCell i j ourRolls 
    | roll < 75 = 0 -- 75% casilla vacia
    | roll < 98 = 1 -- 23% casilla muro
    | otherwise = 2 -- 2% casilla moneda
    where pos = ( (i-1) *19) + (j-1) --Función con la cual a partir de la posicion de la casilla obtener su probabilidad 
          roll = ourRolls!!pos


{------INTERACCIONES CON EL MAPA---------}

--Dada la lista de mapas, el lv, y la posicion de la moneda, crea una nueva lista de mapas actualizada sin la moneda que se haya cogido en el mapa del nivel X
getCoin:: [Map] -> Int -> (Int,Int) -> [Map]
getCoin [] _ _ = []
getCoin (map:maps) lv tp@(x,y)
    | lv >0 = [map] ++ getCoin maps (lv-1) tp
    | otherwise =[nMap] ++ maps
     where f i j = if (i==x && j==y) then 0 else map!(i,j)
           nMap = array ((1,1),(25,19)) [ ((i,j),f i j) | i <-[1..25] , j <-[1..19]]

--Dada la lista de mapas y el nivel, devuelve el mapa del nivel en el que estemos sin que haya armas
getWeapon:: [Map] -> Int -> [Map]
getWeapon [] _ = []
getWeapon (map:maps) lv 
    | lv > 0 = [map] ++ getWeapon maps (lv-1)
    | otherwise = [nMap] ++ maps
     where f i j = if map!(i,j)>2 then 0 else map!(i,j)
           nMap = array ((1,1),(25,19)) [ ((i,j), f i j) | i <-[1..25] , j <-[1..19]]


--Dado la lista de mapas y el nivel , te da las monedas que quedan en ese nivel
coinsLeft:: [Map] -> Int -> Int
coinsLeft maps lv = length [1  | i <-[1..25] , j <-[1..19], (maps!!lv)!(i,j) ==2 ]


--Dado la lista de mapas y el nivel, obtenemos el número de muros en ese nivel
walls:: [Map] -> Int ->Int
walls maps lv = length [1  | i <-[1..25] , j <-[1..19], (maps!!lv)!(i,j) ==1 ]


{------RENDERIZADO-----------------------}


--Lista de cada tipo de celda:

cells::[Picture]  --0: casilla estandar , 1: muro , 2: moneda , 3: wp 0 , 4: wp1 ,5: wp2
cells= [ r,   coloured black sr, coin & r ,bullets & r, aura & r ,prism & r]
    where r= rectangle 1 1
          sr =  solidRectangle 1 1
          coin =  scaled 1.25 0.80   ( colored  yellow (solidCircle 0.3) )
          prism = translated (-0.5) (-0.5) $solidPolygon [(0,0),(0.5,1),(1,0)]
          c = colored blue $solidCircle 0.2
          bullets = translated (-0.5) (-0.5) $ translated 0.2 0.2 c & translated 0.5 0.8 c & translated 0.8 0.2 c  
          aura = colored blue $thickCircle 0.25 0.4

--Dado un mapa, da una lista de Pictures que representa cada casilla del mapa
cellList:: Map -> [Picture]
cellList  map = [ translated (fromIntegral i) (fromIntegral j) (cells!!( map!(i,j)  ))    | i <-[1..25],   j<-[1..19]]


--Dado un Map, devuelve la Picture que lo representa.
map2pic:: Map -> Picture
map2pic map = foldr (\pic acum -> acum & pic) blank (cellList map )