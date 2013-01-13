module Main where

import System.Exit
import DataTypes
import Hibernate
import Utils
import Ui
import Logic

--Główna funkcja progrmau
main = do
      showMainMenuInLoop createModel
      
--Główna pętla 
showMainMenuInLoop model = do
	printNewLine
	function <- showMainMenu
	model <- function model
	showMainMenuInLoop model

      
showMainMenu =
	showMenuBox [("Zarządzanie przedmiotami", showMenuSubjectInLoop),
        ("Zarządzanie grupami", showMenuGroupInLoop),
        ("Zarządzanie salami", showMenuClassroomInLoop),
        ("Zapis danych do pliku",  saveModel),
        ("Odczyt danych z pliku", loadModel),
        ("Wyczyszczenie danych", clearModel),
        ("Manualne ukladanie planu zajec",showManualScheduleInLoop), 
				("Zakończ", exit)]
        
        
--- Menu przedmiotów,grup i sal
showMenuSubjectInLoop model = do
  printNewLine
  model <- menuSubject model
  showMenuSubjectInLoop model
  
showMenuGroupInLoop model = do
  printNewLine
  model <- menuGroup model
  showMenuGroupInLoop model

showMenuClassroomInLoop model = do
  printNewLine
  model <- menuClassroom model
  showMenuClassroomInLoop model
  
showManualScheduleInLoop model = do
  printNewLine
  model <- menuManualSchedule model
  showManualScheduleInLoop model


        
--Wyświetla podmenu przedmioty
menuSubject model = do
  function <- showMenuBox[("Wyświetl wprowadzone przedmioty", listSubjects),
    ("Dodaj przedmiot", addSubject),
    ("Usuń przedmiot", removeSubject),
    ("Powrót do głównego menu", showMainMenuInLoop)]
  model <- function model
  return model

--Wyświetla podmenu grup
menuGroup model = do
  function <- showMenuBox[("Wyświetl wprowadzone grupy", listGroups),
    ("Dodaj grupę", addGroup),
    ("Usuń grupę", removeGroup),
    ("Powrót do głównego menu", showMainMenuInLoop)]
  model <- function model
  return model  
  

--Wyświetla podmenu sal
menuClassroom model = do
  function <- showMenuBox[("Wyświetl wprowadzone sale", listClassrooms),
    ("Dodaj salę", addClassroom),
    ("Usuń salę", removeClassroom),
    ("Powrót do głównego menu", showMainMenuInLoop)]
  model <- function model
  return model    
--Wyświetlanie manualnego układanie rozkładu
menuManualSchedule model = do 
  function <- showMenuBox[
    ("Wyświetl plan dla przedmiotów", showSchedule),
    ("Dodaj zajęcia do planu", addCourse),
--    ("Usuń zajęcia z planu", removeCourse),
    ("Powrót do głównego menu", showMainMenuInLoop)]
  model <- function model
  return model
  


returnToMainMenu model = do 
   return model
   
-- Wyjście z programu
exit _ = exitWith ExitSuccess
