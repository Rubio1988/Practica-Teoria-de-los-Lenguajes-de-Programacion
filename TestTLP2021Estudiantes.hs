module TestTLP2021Estudiantes where

import System.IO
import FourColors

type Test = (String,Map,[Adjacency],Bool)

tfile :: String
tfile = "TestTLP2021Estudiantes.map"

--- FILTER EMPTY ADJACENCIES FROM THE LIST ---
filterAdjacencies :: Map -> [Adjacency]
filterAdjacencies map = filter (\(x,y) -> y/=[]) (adjacencies map)

--- CHECK A TEST ---
checkTest :: Test -> String
checkTest (name,map,adj,hasSol) = "Test \"" ++ name ++ "\": " ++ (if ( ((filterAdjacencies map) == adj) && (isSolution == hasSol) ) then "Ok!" else "Fail!") ++ "\n"
  where isSolution :: Bool
        isSolution
          | solutions == [] = False
          | otherwise       = checkSolution adj (head solutions)
        solutions :: [Node]
        solutions = solution map

--- CHECKS IF A SOLUTION VERIFIES THE THEOREM ---
checkSolution :: [Adjacency] -> Node -> Bool
checkSolution adj (_,_,_,sol) = and (map checkZone adj)
  where checkZone :: Adjacency -> Bool
        checkZone (zone,adjZone) = and (map differentColor adjZone)
          where differentColor :: Zone -> Bool
                differentColor aZ = (colorZone zone) /= (colorZone aZ)
                colorZone :: Zone -> Color
                colorZone z = sol!!(z-1)

--- MAIN LOOP ---
loop :: [String] -> IO ()
loop []     = do { putStr "End of test file\n" }
loop (t:ts) = do { let test = (read t)::Test in do {
                     putStr (checkTest test);
                     loop ts;
                   }
                 }

--- FUNCTION MAIN ---
main :: IO ()
main = do { putStr ("Loading test file: " ++ tfile ++ "\n");  -- INDICAMOS QUE VAMOS A LEER EL FICHERO CON LOS TESTS
            hFlush stdout;                                    -- PROVOCA QUE LA SALIDA SE VUELQUE EN PANTALLA
            fcontent <- (readFile tfile);                     -- LEEMOS EL CONTENIDO DEL FICHERO
            let tests = (lines fcontent) in do {              -- CADA LÍNEA ES UN TEST
              loop tests;                                     -- EJECUTA EL BUCLE PRINCIPAL SOBRE LOS TESTS
            }
          }

