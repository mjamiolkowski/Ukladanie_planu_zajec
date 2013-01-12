module Utils where

import Data.String.Utils
import DataTypes

--Sta³e 
justification :: Int
justification = 30

fill :: Char
fill = ' '

-- Wczytanie obiekt Maybe z ci¹gu znaków
readMaybe :: (Read a) => String -> Maybe a
readMaybe s =
	case reads s of
	[(x, "")] -> Just x
	_ -> Nothing
  
readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

  
-- Aliasy u¿ywanych funkcji wbudowanych
printString = putStr
printLine = putStrLn
printNewLine = printLine ""
printSeparator = printLine "---"
printList list = mapM_ printLine list
tryCatch catchBody exceptBody =
	catch (catchBody)
	(\_ -> exceptBody)

-- £adowanie z pliku modelu danych
loadFromFile :: FilePath -> (String -> b) -> IO b
loadFromFile filePath readFunction = do
	raw <- readFile filePath
	return (readFunction (strip raw))

-- Zapisywanie do pliku modelu danych
saveToFile :: (Show a) => a -> FilePath -> IO ()
saveToFile object filePath = writeFile filePath (show object)

-- Ponumerowanie elementów listy
enumerate :: [a] -> Int -> [(Int, a)]
enumerate [] _ = []
enumerate (x:xs) start = [(start, x)] ++ (enumerate xs (start + 1))

-- Usniêcie elementu z listy
removeItem :: (Eq t) => t -> [t] -> [t]
removeItem _ [] = []
removeItem x (y:ys) | x == y    = ys    
                    | otherwise = y : removeItem x ys
                    
                    
-- Wyœrodkowanie ci¹gu znaków
center :: Int -> Char -> String -> String
center width fill string =
	if width <= stringLength then
		string
	else
		centeredString
	where
		stringLength = length string
		fillLength = width - stringLength
		centeredString = beginning ++ string ++ ending
		partition = divide fillLength
		beginning = takeRepeat (fst partition) fill
		ending = takeRepeat (snd partition) fill
		divide number = if mod number two == one then
							(part, complement)
						else
							(half, half)
						where
							part = half + one
							complement = number - part
							half = div number two
							two = 2
							one = 1

-- Powtórzenie znaku podan¹ iloœæ razy							
takeRepeat :: Int -> a -> [a]
takeRepeat number item = take number (repeat item)

-- Otoczenie listy danym elementem
surround :: [a] -> a -> [a]
surround list item = [item] ++ list ++ [item]

-- Pobranie œrodkowego elementu listy
middle :: [a] -> [a]
middle [] = []
middle [x] = []
middle (x:xs) = init xs

-- Aliasy funkcji formatowania
centerEach justification fill list = map (center justification fill) list
format justification fill fields = concat (centerEach justification fill fields)
formatField = format justification fill