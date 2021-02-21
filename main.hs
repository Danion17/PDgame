{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Map 
import Data.Maybe
import Data.Array
import Data.Text (Text,pack)
import System.Random


-- Constantes del juego:
anchoTablero::Double 
anchoTablero = 24

altoTablero::Double
altoTablero = 18

numberOfLvl :: Int 
numberOfLvl = 6
--tiempo que pasa el personaje sin ser golpeado despues de un golpe
invulnerabilityTime :: Int 
invulnerabilityTime = 30

speedPJ::Double
speedPJ = 0.3

attKeys :: [Text]
attKeys = ["Up","Down","Left","Right"]


--Clase del personaje controlable  : 

---La puntuación tendrá: La semilla(esta aqui por utilidad) una acumulacion de dt, el número de monedas obtenidas, y el número de enemigos derrotados
type Scoreboard = ( StdGen, Int, Int ,Int) 
--Clase del Estado del juego  WIP:
-- Player  keys enemigos ataques(actuales en el tablero) Mapa Nivel Tiempo Scoreboard (Condiciones para pasar de lv)
data State = State Pj [Text] [Enemy] [Attack] [Map] Int Int Scoreboard Bool
    deriving Show

--Clase para determinar la dirección de elementos que se generen y no cambien

data Aiming = Izq | Der | Arr | Abj
    deriving (Show,Eq)


-- double para posicion integer para tipo arma Aiming para posicion donde mira Integer como vidas Int como dt para determinar hitbox
data Pj = Pj (Double,Double) Int Aiming Int Int
    deriving Show

--Tipos ataque y enemigo, ataque determina la posición del ataque, la dirección  y un integer con su tipo
--tipo 0, proyectil básico con una direccion
--tipo 1  aura, se mantiene siempre activa
--tipo 2 rayo laser, atraviesa todo
--tipo 3 proyectiles de enemigos
type Attack = ((Double,Double),Aiming, Int) 

--Enemigo guarda la información de su posición y un integer para determinar su tipo, y sus vidas
--Tipo 0 , enemigo basico que te persigue 
--Tipo 1 , enemigo mas rapido pero con menos vida
--Tipo 2 , enemigo mas lento pero con mas vida
--Tipo 3 , como el 0 pero dispara proyectiles
type Enemy = ((Double,Double),Int,Int) 

--lista de hitbox de los enemigos
enemyHitboxes:: [Double]
enemyHitboxes = [0.25,0.15,0.45,0.25]

--lista de enemigos con su velocidad y su tamaño de render
enemyList :: [(Enemy,Double,Double)]
enemyList = [( ((1,1),0,2) ,speedPJ/2,0.7),  ( ( (1,1) ,1,1) ,speedPJ/2,0.4) , (((1,1),2,4),speedPJ/4,0.9) , (((1,1),0,2),speedPJ/2,0.7)] 


--Estado inicial con el que se iniciará el juego:

initialState:: StdGen ->State 
initialState gen = (State (Pj (13,10) (-1) Arr 5 0) [] [] [] (generateMaps gen numberOfLvl) (0) 0 (gen,0,0,0) False)
--initialState gen = (State (Pj (13,10) (-1) Arr 3 15) [] [((10,1),3,6)] [] (generateMaps gen) 2 0 (gen,0,0,0) False)
-- Player  keys enemigos ataques(actuales en el tablero) Mapa Nivel Tiempo Scoreboard (Condiciones para pasar de lv)
--Enemigos: [((1,1),0,5),((1,2),0,6)] ; [((1,1),0,6),((2,1),1,3),((3,1),2,10),((10,1),3,6)]
-- ((1,1),0,6),((1,1),1,3),((1,1),2,10),((1,1),3,6)



--Handler: actualizacion del estado dado 1 evento.
--toma el evento y el estado actual y devuelve el resultado del estado al aplicar dicho evento
handler::Event->State -> State
handler trigger@(KeyPress key) status@(State (Pj (i,j) wp ai hp ht) ls xs ys m lv t sb end) = if (elem key ls ) then status 
    else (State (Pj (i,j) wp (checkAim ai key) hp ht) (ls++[key]) xs ys m lv t sb end)   
handler trigger@(KeyRelease key) status@(State p ls xs ys m lv t sb end) = (State p (filter (/=key) ls) xs ys m lv t sb end)
handler trigger@(TimePassing dt) status = updateStatus status 
handler _ status = status

--funcion para determina donde mira el personaje despues de pulsar una tecla
checkAim :: Aiming -> Text -> Aiming 
checkAim dir key 
    | key == "Up" = Arr 
    | key == "Left" = Izq 
    | key == "Down" = Abj 
    | key == "Right" = Der 
    | otherwise = dir


--Actualización del entorno llamando a cada función que modifique el estado

--dado un estado devuelve el proximo estado

updateStatus :: State  -> State 

    
updateStatus status@(State pj@(Pj (i,j) wp ai hp ht) keys ens atts m (-1) t sb@(gen,pT,c,ek) end) --Pantalla de GameOver/Victoria 
    | elem "Enter" keys = backToZero gen
    | otherwise = status 
updateStatus status@(State pj@(Pj (i,j) wp ai hp ht) keys ens atts m 0 t sb end) --Pantalla de inicio
    | elem "Enter" keys = nextLv status
    | otherwise = status 
updateStatus status@(State pj@(Pj (i,j) wp ai hp ht) keys ens atts m lv t sb@(gen,pT,c,ek) end) 
    | hp == 0 = (State (Pj (13,10) (-1) Arr 3 0) [] [] [] (m) (-1) 0 sb False)
    | end && (round i)==13 && (round j)==10 = nextLv status   --Si el personaje esta en la casilla de salida y cumple las condiciones, pasas al siguiente lv
    | (t<20) = (State pj keys ens atts m lv newTime sb end)
    | otherwise =  if lv == numberOfLvl && mod t 100 == 0 && t<401 then (State newPj keys (newEnemies++(generateEnemies status)) newAttacks2 newMaps lv newTime newScb newEnd)  else (State newPj keys newEnemies newAttacks2 newMaps lv newTime newScb newEnd) 
    where nPj (Pj (i,j) wp ai hp ht) = if ht == 0 then  (Pj (i,j) wp ai hp ht) else (Pj (i,j) wp ai hp (ht-1)) --va modificando ht para disminuirlo y que el personaje pueda ser golpeado
          pj'= pjGetDmg (nPj (Pj (movPJ (i,j) keys (m!!lv)) wp ai hp ht)) ens atts --mueve el personaje y comprueba si ha sido golpeado por un enemigo
          (newEnemies,newAttacks1) = --if wp ==1 then filterEnemyAndAttack (updateEnemy (movEnemy (i,j) ens (m!!lv)) ( [updateAttackStatus w (m!!lv) newPj | w <- (attackKey pj keys atts t)] ++[((i,j),Arr,1)])t) pj m lv keys t
             filterEnemyAndAttack ( spawnEnemyRangedAttack (updateEnemy (movEnemy (i,j) ens (m!!lv)) ( [updateAttackStatus w (m!!lv) newPj | w <- (attackKey pj keys atts t)] )t) t) pj m lv keys t  --actualiza enemigos y ataques
          newTime = t + 1 --va incrementando el tiempo
          scb1 = (gen,pT+1,c,ek + (length ens - length newEnemies)) --actualiza scoreboard
          (newMaps,newScb,newPj,newAttacks2) =  processMap  pj' m lv scb1 newAttacks1 
          newEnd = checkEnd lv newPj newEnemies newMaps  --En el primer lv la condicion para avanzar se cumple si consigues todas las monedas, si no la condicion es matar a los enemigos




--funcion para obtener daño de un enemigo segun su posicion
--tiene como entrada el pj y la lista de enemigos y devuelve el pj actualizado (si ha sido golpeado o no)
getDmgFromEnemy :: Pj -> [Enemy] -> Pj
getDmgFromEnemy pj [] = pj
getDmgFromEnemy (pj@(Pj (i,j) wp ai hp ht)) (((x,y),te,hpe):es)  
    | ht == 0 = if ((f x y || f x (y+enemyHitboxes!!te)  ||  f x (y-enemyHitboxes!!te) || f (x+enemyHitboxes!!te) y || f (x-enemyHitboxes!!te) y )
        || f (x-enemyHitboxes!!te) (y-enemyHitboxes!!te) || f (x-enemyHitboxes!!te) (y+enemyHitboxes!!te) || f (x+enemyHitboxes!!te) (y - enemyHitboxes!!te)
        || f (x+enemyHitboxes!!te) (y+enemyHitboxes!!te)) then (Pj (i,j) wp ai (hp-1) invulnerabilityTime) else getDmgFromEnemy pj es
    | otherwise = pj
        where f x y = (x < i+ 0.4) && (x > i - 0.4) && (y < j + 0.4) && (y> j - 0.4)

--tiene como entrada el pj y la lista de ataques, devuelve el personaje actualizado (si ha sido golpeado por un proyectil enemigo)
getDmgFromAttack :: Pj -> [Attack] -> Pj 
getDmgFromAttack pj [] = pj 
getDmgFromAttack (pj@(Pj (i,j) wp ai hp ht)) (((x,y),aia,tp):as) 
    | ht == 0 && tp == 3=  if (f x y || f x (y+0.2)  ||  f x (y-0.2) || f (x+0.2) y || f (x-0.2) y ) then (Pj (i,j) wp ai (hp-1) invulnerabilityTime) else getDmgFromAttack pj as
    | otherwise = pj
        where f x y = (x < i+ 0.4) && (x > i - 0.4) && (y < j + 0.4) && (y> j - 0.4)

-- funcion para determinar si un ataque o un enemigo golpea al personaje 
-- tiene como entrada el pj lista de enemigos y lista de ataques y devuelve el pj actualizado
pjGetDmg :: Pj -> [Enemy] -> [Attack] -> Pj
pjGetDmg (pj@(Pj (i,j) wp ai hp ht)) es as = getDmgFromAttack ( getDmgFromEnemy pj es ) as


--Funcion para comprobar si se cumple las condiciones para que aparezca la salida:
-- dado el nivel en el que estamos , el pj, la lista de enemigos y la lista de mapas , devuelve si se cumplen las condiciones para pasar al siguiente nivel
checkEnd:: Int -> Pj ->[Enemy]-> [Map]-> Bool
checkEnd lv (Pj _ wp _ _ _) ens maps
    | lv == 1 =coinsLeft maps lv == 0
    | lv == 2 = wp /= -1
    | otherwise = null ens


--Funcion para pasar al siguiente lv 
--dado un estado, da un estado del siguiente nivel
nextLv:: State -> State
nextLv status@(State pj@(Pj (i,j) wp ai hp ht) keys ens atts m lv t sb end) 
    | lv+1 == length m = State pj keys ens atts m (-1) 0 sb True
    |  wp == 1 = (State (Pj (13,10) wp Arr hp ht) [] (generateEnemies status ) [((13,10),Arr,1)] m (lv+1) 0 sb  False)
    | otherwise = (State (Pj (13,10) wp Arr hp ht) [] (generateEnemies status ) [] m (lv+1) 0 sb  False)


--Función para reiniciar el juego:
--dada la semilla devuelve un estado inicial
backToZero:: StdGen -> State
backToZero gen = (State (Pj (13,10) (-1) Arr 5 0) [] [] [] (generateMaps (snd $ split gen) numberOfLvl) 0 0 (gen,0,0,0) False )


--Función para generar enemigos: 
--dado un estado devuelve una lista de enemigos para el estado
generateEnemies:: State -> [Enemy]
generateEnemies status@(State pj@(Pj (i,j) wp ai hp ht ) keys ens atts m lv t sb end) = case lv of
    0 -> if coinsLeft m 1  < 10 then  [ enemy0 1 (fromIntegral (coinsLeft m 1`mod` 10 )+1) , enemy0 25 (fromIntegral (coinsLeft m 1`mod` 10 )+1)  , enemy0 (fromIntegral (coinsLeft m 1`mod` 15 )+6) 1, enemy0 (fromIntegral (coinsLeft m 1`mod` 15 )+6) 19]
     else [ enemy2 1 (fromIntegral (coinsLeft m 1`mod` 10 )+1) , enemy2 25 (fromIntegral (coinsLeft m 1`mod` 10 )+1)  , enemy2 (fromIntegral (coinsLeft m 1`mod` 15 )+6) 1, enemy2 ( fromIntegral (coinsLeft m 1`mod` 15 )+6) 19 ]
    1 -> []--[enemy3 1 1,enemy3 1 18, enemy3 25 18, enemy3 25 1]
    6 -> [enemy1  (13)  19, enemy1 (13 )  1 , enemy2 1 (10) , enemy2 25  (10) , enemy3 1 1, enemy3 1 19, enemy3 25 1, enemy3 25 19]
    _ -> if (nCoins==0 && nWalls==0) then concat [ f i  | i<-[1..5]] else concat [ f (nCoins + nWalls) , f (nCoins*nWalls), f (nCoins*nWalls - (nCoins + nWalls))]
    

    where enemy0 x y =((x,y),0,2) --EL basico
          enemy1 x y =((x,y),1,1) --El chico rapido
          enemy2 x y =((x,y),2,4) --El gordo lento
          enemy3 x y =((x,y),3,2) --El rangued
          nCoins = coinsLeft m (lv+1)
          nWalls = walls m (lv+1)
         
          f i 
            | i+nCoins `mod` 3 == i+nWalls `mod` 3 = [enemy0 1 (fromIntegral (i `mod` 18 )+1) , enemy1 (fromIntegral(((25-i) `mod` 24)+1)) 1 , enemy2 25 (fromIntegral (i `mod` 18 )+1), enemy3 (fromIntegral (((25-i) `mod` 25)+1)) 19]
            | nWalls `mod` 2 == 0 = if (nCoins `mod` 2 == 0) then [enemy0 1(fromIntegral ((20-i) `mod` 19 )+1) , enemy2 25 (fromIntegral ((20-i) `mod` 19 )+1)] else [enemy0 (fromIntegral (i `mod` 25)+1) 1, enemy3 (fromIntegral (i `mod` 25)+1) 19]
            | otherwise = if (nCoins `mod` 2 == 0) then [enemy1 1 (fromIntegral ((20-i )`mod` 19 )+1) , enemy2 25 (fromIntegral ((20-i) `mod` 19 )+1)] else [enemy1 (fromIntegral (i `mod` 25)+1) 1, enemy3 (fromIntegral (i `mod` 25)+1) 19]

 




--funciones para ver si un enemigo ha sido golpeado por un ataque

--filtramos según la vida y con tipo positivo (los negativos se usan para los ataques que se destruyen con el tiempo)


--dado una lista de enemigos y ataques, el personaje, el mapa , el nivel , las teclas y el tiempo, devolvemos la lsita de enemigos y ataques filtradas segun las condiciones de eliminacion
filterEnemyAndAttack :: ([Enemy],[Attack]) -> Pj->  [Map] -> Int -> [Text]-> Int -> ([Enemy],[Attack])
filterEnemyAndAttack (ens,atts) pj@(Pj (i,j) wp ai hp ht) m lv keys t= (newEns,newAtts)
                where newEns = filter (\((x,y),tp,hp) -> hp > 0) ens
                      newAtts = (filter (\((x,y),ai,tp) -> (x>=0 && y >=0)) atts)
                   

--dado una lista de ataques, de enemigos y un bool , se devuelve la lista de ataques y enemigos actualizada, un ataque solo puede golpear a un enemigo (tipo 0)
hitEnemyRanged :: [Attack] -> [Enemy] -> Bool -> ([Attack],[Enemy])
hitEnemyRanged [] [] _ = ([],[])
hitEnemyRanged [] es _ = ([],es)
hitEnemyRanged as [] _ = (as,[])
hitEnemyRanged as es True = (as,es)
hitEnemyRanged ats@(((x,y),ai,tp):as) ens@(((i,j),te, hpe):es) False  = if f x y || f x (y+0.2)  ||  f x (y-0.2) || f (x+0.2) y || f (x-0.2) y 
    then hitEnemyRanged [] ([((i,j),te, hpe-1)]++es) True else (nats,[((i,j),te, hpe)]++nenm)
                where (nats,nenm) =  hitEnemyRanged ats es False
                      f x y = (x < i+ enemyHitboxes!!te) && (x > i - enemyHitboxes!!te) && (y < j + enemyHitboxes!!te) && (y> j - enemyHitboxes!!te)



--dado una lista de ataque y una de enemigo se devuelve la lista de ataques y enemigos actualizada, golpea a todos los enemigos en el rango
hitEnemyAura :: [Attack] -> [Enemy]  -> ([Attack],[Enemy])
hitEnemyAura ats@(((x,y),ai,tp):as) ens = (ats, map (\ ((i,j),te, hpe) -> if ((  i + enemyHitboxes!!te )+2>=x && (i - enemyHitboxes!!te)-2<=x && (j+enemyHitboxes!!te)+2>=y && (j-enemyHitboxes!!te)-2<=y) then ((i,j),te,hpe-1) else ((i,j),te, hpe) ) ens )

--dado una lista de ataques y enemigos, devuelve la lista de ataques y enemigos, actualizando los enemigos golpeados por el rayo
hitEnemyLaser :: [Attack] -> [Enemy] -> ([Attack],[Enemy])
hitEnemyLaser ats@(((x,y),ai,tp):as) ens  
    | ai == Arr = (ats, map (\ ((i,j),te, hpe) -> if ((  j + enemyHitboxes!!te )>=y && (i+enemyHitboxes!!te)+0.5>=x && (i-enemyHitboxes!!te)-0.5<=x) then ((i,j),te,hpe-1) else ((i,j),te, hpe) ) ens )
    | ai == Der = (ats, map (\ ((i,j),te, hpe) -> if ((  i + enemyHitboxes!!te )>=x && (j+enemyHitboxes!!te)+0.5>=y && (j-enemyHitboxes!!te)-0.5<=y) then ((i,j),te,hpe-1) else ((i,j),te, hpe) ) ens )
    | ai == Izq = (ats, map (\ ((i,j),te, hpe) -> if ((  i + enemyHitboxes!!te )<=x && (j+enemyHitboxes!!te)+0.5>=y && (j-enemyHitboxes!!te)-0.5<=y) then ((i,j),te,hpe-1) else ((i,j),te, hpe) ) ens )
    | ai == Abj = (ats, map (\ ((i,j),te, hpe) -> if ((  j + enemyHitboxes!!te )<=y && (i+enemyHitboxes!!te)+0.5>=x && (i-enemyHitboxes!!te)-0.5<=x) then ((i,j),te,hpe-1) else ((i,j),te, hpe) ) ens )
    | otherwise = (ats,ens)


--devuelve el ataque y enemigo actualizado segun su tipo , tiene como entrada un bool para determinar si el enemigo ya ha sido golpeado, el tipo de ataque como int y dt como int determinado el tiempo
getAttack :: [Attack] -> [Enemy] -> Bool -> Int -> Int -> ([Attack],[Enemy])
getAttack as e b t dt
        | t == 0 = hitEnemyRanged as e b  --el tipo 0 se asocia al ranged
        | t == 1 && mod dt 15 == 0 = hitEnemyAura as e -- tipo 1 se asocia al aura, que hace daño cada cierto tiempo
        | t >= 60 && t <= 90 && mod dt 5 == 0 = hitEnemyLaser as e --el rango de 60 a 90 determina el tiempo del laser activo que hace daño cada cierto tiempo
        | otherwise = (as,e)

--funcion que dado una posicion devuelve la lisla de ataques de un enemigo a distancia
genEnemyRangedAttack :: Double -> Double -> [Attack]
genEnemyRangedAttack i j = [((i,j),Arr,3) , ((i,j),Abj,3) , ((i,j),Der,3) , ((i,j),Izq,3)]

--dado el tiempo y una lista de enemigos , devuelve la lista de ataques actualizada (comprueba si hay enemigos con proyectiles y actualiza los ataques)
genRangedAttackIfEnemy ::  Int -> [Enemy]  -> [Attack]
genRangedAttackIfEnemy dt [] = []
genRangedAttackIfEnemy dt (e@((i,j),te,hp):es) 
    | te == 3 && mod dt 30 == 0 = (genEnemyRangedAttack i j) ++ genRangedAttackIfEnemy dt es
    | otherwise = genRangedAttackIfEnemy dt es


--update del enemigo y de la lista de ataques general
-- dado una lista de enemigos, de ataques y del tiempo, hace uso de las funciones anteriores para actualizar ambas listas

updateEnemy :: [Enemy] -> [Attack] -> Int -> ([Enemy],[Attack])
updateEnemy es [] dt = (es,[])
updateEnemy [] as dt = ([],as)
updateEnemy ess@(e@((x,y),te,hp):es) (a@((i,j),ai,tp):as) dt = (newEns2,newAtts2++newAtts) 
        where (newEns2,newAtts2) = updateEnemy newEns as dt
              (newAtts,newEns) = getAttack [a] ess False tp dt


--funcion que actualiza la lista de ataques segun los ataque de rango de enemigos, tiene como entrada enemigos y ataques y el tiempo , devuelve la lista de ataques actualizada
spawnEnemyRangedAttack :: ([Enemy],[Attack]) -> Int -> ([Enemy],[Attack])
spawnEnemyRangedAttack (es,as) dt= (es,as++(concat [genEnemyRangedAttack x y | ((x,y),te,hp) <- es, te == 3 && mod dt 30 == 0])) 
                      
           
          




--Dado la posicion nueva del pj, la lista de mapas, el nivel, y la Scoreboard; obtiene los mapas actualizados,  la SB actualizada y el pj actualizado
processMap:: Pj-> [Map] -> Int -> Scoreboard ->[Attack]-> ([Map],Scoreboard,Pj,[Attack])
processMap pj@(Pj (i,j) wp ai hp ht) maps lv scb@(gen,dt,coins,kills) attacks
    | isNothing ls ||( null coinCells && null weaponCells) = (maps,scb,newPj,attacks)
    | otherwise = (newMaps,newScb,newPj,newAttacks)
    where ls = collidesWithCell (i,j) (maps!!lv)   ---Lista de elementos con los que colisiona el pj
          coinCells = filter (\(x,y)-> (maps!!lv)!(x,y) == 2) (fromJust ls)  --Casillas de monedas con las que colisiona el pj
          weaponCells = filter (\(x,y)-> (maps!!lv)!(x,y) >2) (fromJust ls)  --Casillas de armas con las que colisiona el pj
          newScb = if null coinCells then scb else (gen,dt,coins+length (coinCells), kills )       --Nueva puntuacion en funcion de las monedas cogidas
          newWp =(maps!!lv)!(head weaponCells) -3
          maps' = foldr (\tp acum-> getCoin acum lv tp ) maps (coinCells) --Mapa actualizado dps de coger las monedas
          newAttacks = if wp == -1 && lv == 2 && isJust ls && not (null weaponCells) && newWp == 1 then attacks ++ [((i,j),Arr,1)] else attacks
          (newPj,newMaps) = if  isNothing ls || null weaponCells || wp >=0 then (pj,maps') 
            else (Pj (i,j) newWp ai hp ht, getWeapon maps' lv )
          
          

--funcion para comprobar si hay un laser en pantalla y no poder realizar dos ataques a la vez
--dada una lista de ataques devuelve si hay un rayo para poder crear otro o no
checkLaserTiming :: [Attack] -> Bool
checkLaserTiming [] = False
checkLaserTiming (a@((i,j),ai,tp):as) 
    | tp >= 45 && tp <= 105 = True 
    | otherwise = checkLaserTiming as

--comprueba la tecla para determinar si se añade un ataque
--dado el pj una lista de teclas , una lista de ataques, y el tiempo y devuelve la lista de ataques actualizada
attackKey:: Pj -> [Text] -> [Attack] -> Int -> [Attack] 
attackKey (Pj (i,j) tw dir hp ht) keys atts tm 
    | any (\x -> elem x keys) attKeys && tw== 0 = if mod tm 5 == 0 then atts++[((i,j),dir,0)] else atts 
    | any (\x -> elem x keys) attKeys && tw== 1  = atts 
    | any (\x -> elem x keys) attKeys && tw == 2 = if checkLaserTiming atts then atts else atts++[((i,j),dir,105)] 
    | otherwise = atts


--funcion para actualizar el estado de un arma segun su tipo
--dado un ataque , el mapa y el pj actualiza el ataque 
updateAttackStatus :: Attack -> Map -> Pj -> Attack 
updateAttackStatus a@((i,j),dir,tp) m pj@(Pj (x,y) wp ai hp ht)
    | tp == 0 = ((movFromDirection (i,j) dir m),dir,0)
    | tp == 3= ((movFromDirection (i,j) dir m),dir,3) -- proyectil, se sigue moviendo
    | (tp > 45 && tp <=105) = ((x,y),dir,tp-1)
    | tp == 1 = ((x,y),dir,tp)
    | otherwise = ((-1,-1),Arr,tp)


--Función para determinar hacia que zona se mueve un elemento dependiendo de su dirección
--dado una posicion , la direccion a la que apunta y el mapa, devuelve la posicion en el siguiente estado 
movFromDirection :: (Double,Double) -> Aiming -> Map -> (Double,Double)
movFromDirection (i,j) dir m
    | dir == Izq = validMov2 (-1,-1) (i-speedPJ*2,j) m 0.15
    | dir == Der = validMov2 (-1,-1) (i+speedPJ*2,j) m 0.15
    | dir == Arr = validMov2 (-1,-1) (i,j+speedPJ*2) m 0.15
    | dir == Abj = validMov2 (-1,-1) (i,j-speedPJ*2) m 0.15



--Funcion para determinar el movimiento del enemigo, toma tu posición, la lista de enemigos y el mapa, y actualiza la posicion de cada enemigo 
movEnemy :: (Double,Double) -> [Enemy] -> Map -> [Enemy] 
movEnemy  (x,y) ens m =  [((obt (x,y) (i,j)) te,te,hp) | ((i,j),te,hp) <- ens] 
  where obt (x,y) (i,j) te = validMov2 (i,j) (horizontal (vertical (i,j))) m (enemyHitboxes!!te)
                    where vertical (i',j')= if i'<x then validMov2 (i',j') ( (i'+(speedEn (enemyList!!te))) ,j') m (enemyHitboxes!!te) else validMov2 (i',j') ((i'-(speedEn (enemyList!!te))),j') m (enemyHitboxes!!te)
                          horizontal (i',j')= if j<y then validMov2 (i',j') (i',(j'+(speedEn (enemyList!!te))) ) m (enemyHitboxes!!te) else validMov2 (i',j') (i',(j'-(speedEn (enemyList!!te)))) m (enemyHitboxes!!te)
                          speedEn (e,sp,rn)= sp


--movimiento del personaje
--dado la posicion del personaje , las teclas y el mapa, devuelve la posicion actualizada
movPJ :: (Double,Double) -> [Text] -> Map -> (Double,Double)
movPJ (i,j) keys map= pos2
    where (i',j') = foldr (\key acum -> processKey key acum) (i,j) keys
          (x1,y2) =  (validMov2 (i,j) (i',j) map 0.3)
          pos2 =  (validMov2 (i,j) (x1,j') map 0.3) 
          
--Dada una tecla en formato Text y unas coordenadas i,j ; devuelve la posición resultante de aplicar el movimiento que indica dicha tecla:
processKey:: Text -> (Double,Double) -> (Double,Double)
processKey key (i,j) = case key of
    "W" -> (i,j+speedPJ)
    "D" ->(i+speedPJ,j)
    "S" ->(i,j-speedPJ)
    "A" ->(i-speedPJ,j)
    _->(i,j)



 --Metodo para PJ  que dadas unas coordenada, comprueba si colisiona con algun elemento del mapa de tipo distinto al por defecto
--Devuelve (Just casillas con la que colisiona) o Nothing
collidesWithCell::(Double,Double) -> Map -> Maybe [(Int,Int)]
collidesWithCell (i,j) map 
    | null conflict = Nothing
    | otherwise = Just ((conflict))
    where cells = [ (x,y)  | x <-[1..25] , y <-[1..19] , map!(x,y) /= 0 ] --Lista de casillas especiales
          rounding n x y = ((round (i +n)) ==  x   )  &&   ((round (j + n)) == y)  || ((round (i)) ==  x   )  &&   ((round (j + n)) == y) || 
            ((round (i +n)) ==  x   )  &&   ((round (j )) == y)  --comprobacion de si choca con las casillas x y dada un valor, que jutno a i,j se aproximará.
            -- || ((round (i -n)) ==  x   )  &&   ((round (j - n)) == y) ||  ((round (i)) ==  x   )  &&   ((round (j - n)) == y)  || ((round (i -n)) ==  x   )  &&   ((round (j )) == y)l
          conflict = filter (\(x,y)-> rounding 0 (x) y || rounding 0.3 x y || rounding (-0.3) x y) cells --Lista de casillas conflictivas


--Igual que collidesWithCell , pero tambien recibe el radio de la hitbox de la entidad como parámetro.
collidesWithCell2::(Double,Double) -> Map -> Double-> Maybe [(Int,Int)]
collidesWithCell2 (i,j) map hitbox
    | null conflict = Nothing
    | otherwise = Just ((conflict))
    where cells = [ (x,y)  | x <-[1..25] , y <-[1..19] , map!(x,y) /= 0 ] --Lista de casillas especiales
          rounding n x y = ((round (i +n)) ==  x   )  &&   ((round (j + n)) == y)  || ((round (i)) ==  x   )  &&   ((round (j + n)) == y) || 
            ((round (i +n)) ==  x   )  &&   ((round (j )) == y)  --comprobacion de si choca con las casillas x y dada un valor, que jutno a i,j se aproximará.
        --    || ((round (i -n)) ==  x   )  &&   ((round (j - n)) == y) ||  ((round (i)) ==  x   )  &&   ((round (j - n)) == y)  || ((round (i -n)) ==  x   )  &&   ((round (j )) == y)
          conflict = filter (\(x,y)-> rounding 0 (x) y || rounding hitbox x y || rounding (-hitbox) x y) cells --Lista de casillas conflictivas

--Version  del CollidesWithCell exclusiva para muros; toma una posición i,j , el mapa en el que estamos y la hitbox del objeto en la posición i j, y devolvemos o bien una lista con los muros con los que colisiona o bien Nothing 
collidesWithCell3::(Double,Double) -> Map -> Double-> Maybe [(Int,Int)]
collidesWithCell3 (i,j) map hitbox 
    | null collisions = Nothing
    | otherwise = Just (collisions)
    where cells = subMatrixFilter map (round i) (round j) 3 1 --Casillas colindantes que sean muros
          f x y = (filter (\(i',j')-> round x == i' && j' ==round y) cells)
          r x y =  (f x y)   ++ (f (x+hitbox) y) ++ (f (x-hitbox) y ) ++ (f x (y+hitbox)) ++ (f x (y-hitbox)) ++(f (x+hitbox) (y+hitbox)) ++ (f (x-hitbox) (y-hitbox)) ++ (f (x+hitbox) (y-hitbox)) ++ (f (x-hitbox) (y+hitbox))
          collisions= r (  i) ( j) 

--Metodo para controlar los movimientos: (Aquí habría que añadir en el futuro la matrix del mapa):

--Metodo para PJ, dado posicion original, posible nueva posicion y el mapa, da la siguiente posicion válida
validMov:: (Double,Double) -> (Double,Double) -> Map -> (Double,Double)
validMov (i,j) (i',j') map 
    | (isJust collisions) && collisionWithWall = (i,j)  --Ha colisionado con un muro?
    | otherwise = validMov'
    where iCond = (i' < 0.75) || (i' > anchoTablero +1.25 )  --Choca contra un muro en el eje y
          jCond =  (j' < 0.75) || (j' > altoTablero +1.25)    --Choca contra un muro en el eje x
          validMov'                                         --Comprobacion de si choca con los limites
            | iCond = if ( jCond ) then (i,j) else (i,j')
            | otherwise = if (jCond) then (i',j) else (i',j')
          collisions= collidesWithCell (i',j') map       --Colisiona con alguna casilla especial ¿?
          collisionWithWall = not . null $ filter (\(x,y)-> map!(x,y) ==1) (fromJust collisions) 


--Igual que validMov, pero tambien recibe el radio de la hitbox del personaje como parámetro.
validMov2:: (Double,Double) -> (Double,Double) -> Map -> Double ->  (Double,Double)
validMov2 (i,j) (i',j') map hitbox
    | (isJust collisions) && collisionWithWall = (i,j)  --Ha colisionado con un muro?
    | otherwise = validMov'
    where iCond = (i' < 0.75) || (i' > anchoTablero +1.25 )  --Choca contra un muro en el eje y
          jCond =  (j' < 0.75) || (j' > altoTablero +1.25)    --Choca contra un muro en el eje x
          validMov'                                         --Comprobacion de si choca con los limites
            | iCond = if ( jCond ) then (i,j) else (i,j')
            | otherwise = if (jCond) then (i',j) else (i',j')
          collisions= collidesWithCell3 (i',j') map hitbox      --Colisiona con alguna casilla especial ¿?
          collisionWithWall = not . null $ filter (\(x,y)-> map!(x,y) ==1) (fromJust collisions) 


{----------RENDERIZADO------------------------------}



--Transforma el estado dado en la imagen

render:: State->Picture
render (State pj@(Pj (i,j) wp dir hp ht) _ xs ys map (-1) t sb end) = gameOverScreen sb end
render (State pj@(Pj (i,j) wp dir hp ht) _ xs ys map 0 t sb end) = tittleScreen --Inicio de partida 
render (State pj@(Pj (i,j) wp dir hp ht) _ xs ys map lv t sb end) 
    | end =  pjPict &   attacksPict & enemiesPict & exit & pict
    | otherwise = if (t <20) then loadingScreen else pjPict & enemiesPict &  attacksPict & pict 
    where x = anchoTablero +3
          y = altoTablero  +3
          exit = colored green  $solidRectangle 1 1
          pjPict=  renderPj pj
          loadingScreen = renderLS t
          enemiesPict = pictures [renderEnemy (x-13) (y-10) tp| ((x,y),tp,hp) <-xs]
          attacksPict = pictures [renderAttack (x-13) (y-10) dir tp | ((x,y),dir,tp) <- ys]
          pict = translated (-13) (-10) (map2pic (map!!lv))
         
--renderiza el laser en su forma pequeña (carga) dado su posicion y su dirección
renderSmallLaserAttack :: Double -> Double -> Aiming -> Picture 
renderSmallLaserAttack i j dir 
    | dir == Arr = coloured blue $ solidPolygon [(i-0.05,j),(i+0.05,j),(i+0.05,altoTablero -8.5),(i-0.05,altoTablero -8.5)]   --translated i (j+altoTablero*(2/3)) $ coloured blue (solidRectangle (0.1) (anchoTablero-j))
    | dir == Abj = coloured blue $ solidPolygon [(i-0.05,j),(i+0.05,j),(i+0.05, -9.5),(i-0.05, -9.5)]  --translated i (j-altoTablero*(2/3)) $ coloured blue (solidRectangle (0.1) (anchoTablero))
    | dir == Der = coloured blue $ solidPolygon [(i,j+0.05),(i,j-0.05),(anchoTablero-11.5,j-0.05),(anchoTablero-11.5,j+0.05)]     --translated (i+anchoTablero*(1/2)) j $ rotated (pi/2) $ coloured blue (solidRectangle (0.1) (anchoTablero))
    | dir == Izq = coloured blue $ solidPolygon [(i,j+0.05),(i,j-0.05),(-12.5,j-0.05),(-12.5,j+0.05)] --translated (i-anchoTablero*(1/2)) j   $ rotated (pi/2) $ coloured blue (solidRectangle (0.1) (anchoTablero))
 
--renderiza el laser en su forma grande (mientras hace daño) , dado su posicion y su dirección
renderBigLaserAttack :: Double -> Double -> Aiming -> Picture 
renderBigLaserAttack i j dir 
     | dir == Arr = coloured blue $ solidPolygon [(i-0.25,j),(i+0.25,j),(i+0.25,altoTablero -8.5),(i-0.25,altoTablero -8.5)]   --translated i (j+altoTablero*(2/3)) $ coloured blue (solidRectangle (0.1) (anchoTablero-j))
    | dir == Abj = coloured blue $ solidPolygon [(i-0.25,j),(i+0.25,j),(i+0.25, -9.5),(i-0.25, -9.5)]  --translated i (j-altoTablero*(2/3)) $ coloured blue (solidRectangle (0.1) (anchoTablero))
    | dir == Der = coloured blue $ solidPolygon [(i,j+0.25),(i,j-0.25),(anchoTablero-11.5,j-0.25),(anchoTablero-11.5,j+0.25)]     --translated (i+anchoTablero*(1/2)) j $ rotated (pi/2) $ coloured blue (solidRectangle (0.1) (anchoTablero))
    | dir == Izq = coloured blue $ solidPolygon [(i,j+0.25),(i,j-0.25),(-12.5,j-0.25),(-12.5,j+0.25)] --translated (i-anchoTablero*(1/2)) j   $ rotated (pi/2) $ coloured blue (solidRectangle (0.1) (anchoTablero))
 

--renderiza los ataques a distancia, dado su posicion, su dirección, el color y si es circular o cuadrado
renderRangedAttack :: Double -> Double -> Aiming -> Colour ->Bool-> Picture 
renderRangedAttack i j dir c flag
    | dir == Izq =  if flag then translated (i-speedPJ) j $ coloured c (solidCircle 0.2) else translated (i-speedPJ) j $ coloured c (solidRectangle 0.3 0.3)
    | dir == Der =  if flag then translated (i+speedPJ) j $ coloured c (solidCircle 0.2) else translated (i+speedPJ) j $ coloured c (solidRectangle 0.3 0.3)
    | dir == Arr =  if flag then translated i (j+speedPJ) $ coloured c (solidCircle 0.2) else translated i (j+speedPJ) $ coloured c (solidRectangle 0.3 0.3)
    | dir == Abj =  if flag then translated i (j-speedPJ) $ coloured c (solidCircle 0.2) else translated i (j-speedPJ) $ coloured c (solidRectangle 0.3 0.3)


--Generacion del sprite de los ataques, segun su tipo
renderAttack::Double -> Double -> Aiming -> Int -> Picture --cambiar a Case of
renderAttack i j dir tp 
    | tp == 0 = renderRangedAttack i j dir (dark blue ) True
    | tp == 1 = translated i j $ coloured (light blue) (thickCircle 0.5 1.5)
    | (tp >= 45 && tp <= 60) || (tp >=90 && tp <=105) = renderSmallLaserAttack i j dir
    | tp >60 && tp < 90 = renderBigLaserAttack i j dir
    | tp == 3 = renderRangedAttack i j  dir (dark $mixed [red,yellow,red]) False
    | otherwise = translated i j $ coloured yellow (solidCircle 1.7)




--Generacion de imagen de enemigo, dado su posicion y su tipo
renderEnemy::Double -> Double -> Int -> Picture
renderEnemy i j tp = case tp of
    0 -> translated i j $ coloured red (solidRectangle (rn (enemyList!!tp)) (rn (enemyList!!tp)) )
    1 -> translated i j $ coloured (light purple  ) (solidRectangle (rn (enemyList!!tp)) (rn (enemyList!!tp)) )
    2 -> translated i j $ coloured (dark $ dark green) (solidRectangle (rn (enemyList!!tp)) (rn (enemyList!!tp)) )
    3 -> translated i j $ coloured (dark $ mixed [red,yellow,red]) (solidRectangle (rn (enemyList!!tp)) (rn (enemyList!!tp)) )
    where rn (e,sp,r) = r

--Renderiza la pantalla de carga
renderLS:: Int -> Picture
renderLS t= (lettering msg) & blank 
    where msg = pack $ foldr (\x acum -> acum ++ [x] )  "Loading" [ '.'  | i <-[0..t], i `mod` 2 == 0]

--renderiza el personaje
renderPj::Pj-> Picture
renderPj (Pj (i,j) wp dir hp ht) =pjSprite & healthBar 
    where pjSprite= translated (-13) (-10) $ translated i j   $ ( coloured cl (solidCircle 0.5))
          healthBar = if hp < 10 then pictures [ translated (13.5) (9 + (fromIntegral i)*(-2))  heart |  i <-[0..hp-1] ] else translated (13.5) 9 $ colored yellow heart
          cl = if ht == 0 then red else blue

--renderiza un corazon para mostrar las vidas
heart::Picture
heart = colored red $ solidClosedCurve   [pt1,pt12,pt2,pt3,pt4,pt5,pt6,pt61]
pt1::Point
pt1 = (0,-1)
pt12::Point
pt12 = (-0.6,-0.6)
pt2::Point
pt2 = (-1,0)
pt3::Point
pt3 = (-0.5,0.5)
pt4::Point
pt4 = (0,0.25)
pt5::Point
pt5 = (0.5,0.5)
pt6::Point
pt6 = (1,0)
pt61::Point
pt61 = (0.6,-0.6)

--renderiza la pantalla de inicio
tittleScreen::Picture
tittleScreen = (translated 0 5 $ dilated 2.0 $ styledLettering  Bold Monospace "POLYGON DISASTER" ) 
  & translated 0 0 (dilated (0.75) $ styledLettering Bold Monospace "PRESS ENTER TO START A GAME")
  & background 

--renderiza el fondo
background::Picture
background =  colored black (solidPolygon [(-15,-15),(-13,-15),(-15,-13)] )
  & colored ( dark gray)  $solidPolygon [(-15,-15),(15,-15),(15,15),(-15,15)] 

--renderiza la pantalla de fin de juego dado el scoreboard y si ha acabado o no el juego
gameOverScreen:: Scoreboard ->Bool-> Picture
gameOverScreen (_, time,coins,kills) b= if b then (translated 0 5 $ dilated 2.0 $ styledLettering  Bold Monospace "YOU HAVE WON" )
  & translated 0 3 (styledLettering Bold Monospace ( pack ("SCORE: "  ++ show ( (coins*250 + kills*500 -  ( (time `div` 1000)))   + 1000)) )) -- - ( (time `div` 1000))
  & translated 0 0 ( styledLettering Bold Monospace "PRESS ENTER TO RETURN TO THE TITLE SCREEN")
  & background else (translated 0 5 $ dilated 2.0 $ styledLettering  Bold Monospace "YOU HAVE BEEN DEFEATED" )
  & translated 0 3 (styledLettering Bold Monospace ( pack ("SCORE: "  ++ show ( (coins*250 + kills*500 -  ( (time `div` 1000))  ) )) ) )  -- -  ( (time `div` 1000))
  & translated 0 0 ( styledLettering Bold Monospace "PRESS ENTER TO RETURN TO THE TITLE SCREEN")
  & background 
    
          
{-----MAIN--------------------------------------}

main:: IO()
main = do
    gen <- newStdGen 
    activityOf (initialState gen)handler render
