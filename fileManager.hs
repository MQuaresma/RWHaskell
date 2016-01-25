{-|Module: fileManager
Description: Simple file manager written in Haskell
Copyright: Miguel Quaresma <miguelquaresma97@gmail.com>
-}


import System.Directory
import ElfMagic

main:: IO ()
main = do
  putStrLn "Would you like to move(m) or delete(d) a file: "
  o <- getChar
  case o of
    'm' -> mover
    'd' -> apagar

-- |Func responsible for deleting the file specified by the user
apagar :: IO ()
apagar = do
  putStrLn "\nInsert the path to the filw you wish to delete: "
  h <-getLine
  vl <- isElfFile h
  case vl of
    False -> do removeFile h
                putStrLn "File removed with sucess"
    True -> do putStrLn "You're trying to delete a program executable, are you sure about this?"
               r <- getChar
               case r of
                 'Y' -> do
                          removeFile h
                          putStrLn "File removed with sucess"
                 'O' -> do
                          putStrLn "Exiting..."
                          return ()

  -- |Func responsible for moving/renaming the file specified by the user
mover :: IO ()
mover = do
  putStrLn "\nInsert the name of the file you wish to move: "
  f <- getLine
  putStrLn "Insert the new path of the file: "
  p <- getLine
  renameFile f p
  putStrLn "File moved with sucess"
