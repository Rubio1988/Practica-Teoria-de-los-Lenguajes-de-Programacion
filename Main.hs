module Main where

import System.IO
import FourColors

-- LOAD A MAP FROM A TEXT FILE AND BUILDS THE MAP ---
readMap :: FilePath -> IO Map
readMap f = do { putStr ("Loading map from file " ++ f ++ "\n");
                 fcontent <- (readFile f);                             -- LEEMOS EL CONTENIDO DEL FICHERO
                 putStr fcontent;                                      -- MOSTRAMOS EL CONTENIDO DEL FICHERO
                 hFlush stdout;                                        -- PROVOCA QUE LA SALIDA SE VUELQUE EN PANTALLA
                 let map = read(concat (lines fcontent))::Map in do {  -- CONSTRUIMOS EL MAPA
                     return map;                                       -- DEVOLVEMOS EL MAPA
                 }
               }

--- MAIN LOOP ---
loop :: IO ()
loop = do { putStr "Enter mapfile to load: ";       -- ESCRIBE LA PREGUNTA
            hFlush stdout;                          -- PROVOCA QUE LA SALIDA SE VUELQUE EN PANTALLA
            fileName <- getLine;                    -- LEE EL NOMBRE DEL FICHERO A CARGAR
            if ( fileName /= "" )                   -- SI EL NOMBRE ES NO VACÍO
               then do { map <- readMap fileName;      -- LEEMOS EL FICHERO
                         putStr (color map);           -- ESCRIBIMOS EL RESULTADO DE COLOREARLO
                         loop;                         -- VOLVEMOS A EJECUTAR EL BUCLE PRINCIPAL
                       }
               else do putStr "End of Line.\n";     -- SI ES VACÍO FIN DEL PROGRAMA
          }

--- FUNCTION MAIN ---
main :: IO ()
main = do { hSetBuffering stdin LineBuffering;      -- PERMITE QUE SE PUEDA EDITAR LA LÍNEA
            loop;                                   -- EJECUTA EL BUCLE PRINCIPAL
          }

