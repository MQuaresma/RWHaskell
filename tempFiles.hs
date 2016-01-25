import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception(finally, catch)

main :: IO()
main = withTempFie "mytemp.txt" myAction

myAction :: FilePath -> Handle -> IO()
myAction tempname temph =
  do
    putStrLn "Welcome to tempfile.hs"
    putStrLn $ "I have a temporary file at" ++ tempname
    pos <- hTell temph
    putStrLn $ "My current position is " ++ show pos
    let tempdata = show [1..10]
    putStrLn $ "Writing one line containing " ++ show(length tempdata) ++ "bytes: " ++ tempdata
    hPutStrLn temph tempdata
    pos <- hTell temph
    putStrLn $ "After writing, my new position is " ++ show pos
    putStrLn $ "The file content is "
    hSeek temph AbsoluteSeek 0
    c <- hGetContents temph
    putStrLn c
    print c


withTempFie :: FilePath -> (FilePath -> Handle -> IO ()) -> IO ()
withTempFie tempname f =
  do
    tempdir <-getTemporaryDirectory
    (tempfile, temph) <- openTempFile tempdir tempname
    finally (f tempfile temph)
            (do hClose temph
                removeFile tempfile)
