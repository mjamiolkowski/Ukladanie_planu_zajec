module Hibernate where
 
import DataTypes
import Data.Maybe
import Utils
import Ui
import Strings
-- Pusty Model
createModel = Model (Classes []) (Classrooms []) (Groups []) (Schedule [])

-- Zapis modelu do pliku
saveModel model = do 
  filePath <- showFileInputBox
  saveToFile model filePath
  showMessageBox successfulOperationString
  return model

-- Wczytanie modelu z pliku
loadModel model =
  tryCatch (doLoadModel model) (showError model)
  where 
    doLoadModel model = do
      filePath <- showFileInputBox
      maybeModel <- loadFromFile filePath readMaybeModel
      if isNothing maybeModel then do
        showMessageBox invalidFormatErrorString
        return model
      else do 
          showMessageBox successfulOperationString
          let newModel = fromJust maybeModel
          return newModel
			where
				readMaybeModel :: String -> Maybe Model
				readMaybeModel = readMaybe
        
    showError model = do
      showMessageBox cannotOpenFileErrorString
      return model
		

-- Wyczyszczenie modelu
clearModel model = do
	showMessageBox successfulOperationString
	return createModel