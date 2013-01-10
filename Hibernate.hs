module Hibernate where
 
import DataTypes
import Data.Maybe
import Utils
import Gui
import Strings
-- Pusty Model
createModel = Model (Classes []) (Classrooms []) (Groups []) (Schedule [])

-- Zapis modelu do pliku
saveModel model = do 
  filePath <- showFileInputBox
  saveToFile model filePath
  showMessage successfulOperationString
  return model

-- Wczytanie modelu z pliku
loadModel model =
  tryCatch (doLoadModel model) (showError model)
  where 
    doLoadModel model = do
      filePath <- showFileInputBox
      maybeModel <- loadFromFile filePath readMaybeModel
      if isNothing maybeModel then do
        showMessage invalidFormatErrorString
        return model
      else do 
          showMessage successfulOperationString
          let newModel = fromJust maybeModel
          return newModel
			where
				readMaybeModel :: String -> Maybe Model
				readMaybeModel = readMaybe
        
    showError model = do
      showMessage cannotOpenFileErrorString
      return model
		

-- Wyczyszczenie modelu
clearModel model = do
	showMessage successfulOperationString
	return createModel