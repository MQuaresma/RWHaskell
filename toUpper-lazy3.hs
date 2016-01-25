import Data.Char (toUpper)

main = do
	inp <- readFile "inp.txt"
	writeFile "out.txt" (map toUpper inp)
