module Ui where 

import Utils
import Data.Maybe
import Data.String.Utils
import Strings

-- Stworzenie obramowanego komunikatu
messageBox "" = ""
messageBox message =
	unlines (surround centeredMessageLinesWithBars verticalBar)
	where
		messageLines = map (surroundWith ' ') (lines message)
		messageLengths = map length messageLines
		maximumMessageLength = maximum messageLengths
		verticalBar = surround (take maximumMessageLength (repeat '-')) ' '
		centeredMessageLines = map (center maximumMessageLength ' ') messageLines
		centeredMessageLinesWithBars = map (surroundWith '|') centeredMessageLines
		surroundWith item = flip (surround) item

-- Wyświetlenie komunikatu
showMessageBox message = printString (messageBox message)
    
-- Wyświetlenie menu
showMenuBox menuItems = do
	printString (menuBox menuItems)
	function <- showMenuOptionInputBox menuItems
	return function
  
-- Stworzenie menu
menuBox menuItems =
	unlines numberedMenuItemTexts
	where
		menuItemTexts = map fst menuItems
		numberedMenuItemTexts = map (\(x, y) -> (show x) ++ ". " ++ y) (enumerate menuItemTexts 1)
					
-- Wyświetlenie polecenia zachęty dla wyboru pozycji z menu		
showMenuOptionInputBox menuItems = do
	input <- showInputBox "Podaj numer opcji"
	let optionNumber = readMaybe input
	if (isNothing optionNumber) || (notElem (fromJust optionNumber) [1..(length menuItems)]) then
		do
			printLine "Nieprawidłowy numer opcji"
			showMenuOptionInputBox menuItems
	else
		return (snd (menuItems !! ((fromJust optionNumber) - 1)))
                
-- Wyświetlenie polecenia zachęty
inputBox message = message ++ ": "
showInputBox message = do
	printLine (inputBox message)
	input <- getLine
	let strippedInput = strip input
	if null strippedInput then
		showInputBox message
	else
		return strippedInput
    
-- Wyświetlenie polecenia zachęty dla wczytania pliku
showFileInputBox = showInputBox "Podaj ścieżkę do pliku"

-- Wyświetlenie zapytania o ponowienie operacji 
showConfirmationInputBox message = do
	input <- showInputBox (message ++ " [T/N]")
	return ((input == "T") || (input == "t"))

-- Wyświetlenie polecenia zachęty dla ponowienia
showRepeatInputBox = showConfirmationInputBox "Jeszcze raz?"
									
-- Ponowienie polecenia zachęty
interactRepeatInputBox message callback = do
	showMessageBox message
	repeat <- showRepeatInputBox
	if repeat then
		callback
	else
		return Nothing
									
-- Wyświetlenie polecenie zachęty i wczytanie wielu obiektów
getListOfObjects readFunction message = do
	rawObjects <- showInputBox message
	let stringObjects = words rawObjects
	let maybeObjects = map readFunction stringObjects
	if any isNothing maybeObjects then
		interactRepeatInputBox invalidFormatErrorString (getListOfObjects readFunction message)
	else do
		let objects = map fromJust maybeObjects
		return (Just objects)
		
-- Wyświetlenie polecenia zachęty i wczytanie pojedynczego obiektu
getSingletonObject readFunction message = do
	stringObject <- showInputBox message
	let maybeObject = readFunction stringObject
	if isNothing maybeObject then
		interactRepeatInputBox invalidFormatErrorString (getSingletonObject readFunction message)
	else
		return maybeObject
		
-- Pobranie nazwy obiektu z funkcją weryfikującą
getObjectName objectNames checkFunction message = do
	objectName <- showInputBox message
	if checkFunction objectName objectNames then
		return (Just objectName)
	else
		return Nothing




    
