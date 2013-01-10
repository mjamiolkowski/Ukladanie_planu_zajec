module Utils where

import Data.String.Utils
import DataTypes

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