# PDgame
Game made using Haskell for the subject PD (programación dinámica/dynamic programming) at Computer Science / Ingeniería Informática,Tecnologías informáticas degree at Seville University   together with _____.

# Requirements 
  -Haskell v8.6.5
  -Modules needed (use command "cabal install" with each module as follows:
      array, data-default , codeworld-api
 
# Gameplay
 To start playing just load main.hs and call the function main. If you want to compile the program write "ghc main.hs -o nombreEjecutable -threaded" in a terminal where the files are located.
 
 Controls: "wasd" keys for movement; arrow keys for aiming and shooting. 
 
 In this game you are a circle fighting against polygons in a series of randomly generated maps. First you need to run away from them collecting all the coins to scape the first level. Next you will have to choose between 3 diferents weapons: a "classic" proyectile that hits the first thing it collides with, a powerful deathly ray that pierces through the whole map. From this point until the last level, you will need to kill all enemies to proceed to the next level, you may also keep collecting coins as you will get a better score at the end.
