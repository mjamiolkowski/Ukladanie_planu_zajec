module Logic where 

import DataTypes
import Hibernate
import Utils
import Gui
import Data.Maybe
import Strings

--Przydało by się wykorzystać funkcję Modelu, żeby wybierać pola a nie cały model deklarować

--Logika związana z przedmiotami
----Wyświetlenie przedmiotów
listSubjects model = do 
  printSeparator
  print $ getClasses model
  return model

--- Wczytanie nazwy nowego przedmiotu
getNewSubjectName subject = getObjectName subject notElem "Podaj nazwę nowego przedmiotu"


--- Wczytanie liczby godzin zajęć (została ogranicznona do max 5 w tygodniu)
getNewDurationTime = getSingletonObject readMaybeTime "Podaj liczbę godzin w tygodniu 1-7"

--- Wczytanie istniejącego przedmiotu
getExisitingSubjectName subject = getObjectName subject elem "Podaj nazwę przedmiotu do usuniecia"

readMaybeTime :: String -> Maybe Int
readMaybeTime s =
	case reads s of
	[(x, "")] -> 
                if x > 0 && x < 8 then 
                  Just x
                else 
                  Nothing
	_ -> Nothing
  
---Dodanie przedmiotu do listy
addSubject (Model classes  classrooms groups schedule)  = do 
  maybeSubjectName <- getNewSubjectName ( getSubjects classes )
  if isNothing maybeSubjectName then do 
    showMessage subjectExistErrorString
    return (Model classes classrooms groups schedule)
  else do
    maybeDurationTime <- getNewDurationTime
    let newClasses = doAddSubject ( fromJust maybeSubjectName) ( fromJust maybeDurationTime) classes
    showMessage successfulOperationString
    return (Model newClasses  classrooms groups schedule)

doAddSubject subjectName durationTime (Classes c) = Classes ([(Class subjectName durationTime)] ++ c)
    
--- Usuwanie przedmiotu
removeSubject (Model classes classrooms groups schedule ) = do
  maybeSubjectName <- getExisitingSubjectName ( getSubjects classes )
  if isNothing maybeSubjectName then do 
    showMessage subjectNonExistingErrorString
    return (Model classes classrooms groups schedule)
  else do
    -- TODO dopisanie usuwianie usuniętego obiektu z już ułożonego planu albo jakieś inne rozwiązanie
    let newClasses = doRemoveSubject ( fromJust maybeSubjectName) classes
    showMessage successfulOperationString
    return (Model (Classes newClasses)  classrooms groups schedule)
    
doRemoveSubject subjectName (Classes c) = removeClass subjectName c

removeClass:: Subject -> [Class] -> [Class]
removeClass _ [] = []
removeClass s (c:cs) | (getSubject c) == s    = cs    
                     | otherwise              = [c] ++ removeClass s cs
  
--Logika związana z grupami

--- Wczytanie nazwy nowej grupy
getNewGroupName group = getObjectName group notElem "Podaj nazwę nowej grupy"

--- Wczytanie istniejącej grupy
getExisitingGroupName group = getObjectName group elem "Podaj nazwę grupy do usuniecia"

----Wyświetlenie grup
listGroups model = do 
  printSeparator
  print $ getGroups model
  return model
---Dodanie grupy do listy
addGroup (Model classes  classrooms groups schedule)  = do 
  maybeGroupName <- getNewGroupName (getGroupList groups)
  if isNothing maybeGroupName then do 
    showMessage groupExistErrorString
    return (Model classes classrooms groups schedule)
  else do
    let newGroups = doAddGroup ( fromJust maybeGroupName) groups
    showMessage successfulOperationString
    return (Model classes  classrooms newGroups schedule)

doAddGroup groupName (Groups g) = Groups ([groupName] ++ g)
    
--- Usuwanie grupy
removeGroup (Model classes classrooms groups schedule ) = do
  maybeGroupName <- getExisitingGroupName (getGroupList groups)
  if isNothing maybeGroupName then do  
    showMessage groupNonExistingErrorString
    return (Model classes classrooms groups schedule)
  else do
    -- TODO dopisanie usuwianie usuniętego obiektu z już ułożonego planu albo jakieś inne rozwiązanie
    let newGroups = doRemoveGroup ( fromJust maybeGroupName) groups
    showMessage successfulOperationString
    return (Model classes  classrooms ( Groups newGroups) schedule)
    
doRemoveGroup groupName (Groups g) = removeItem groupName g

--Logika związana z salami

--- Wczytanie nazwy nowej sali
getNewClassroomName classroom = getObjectName classroom notElem "Podaj nazwę nowej sali"

--- Wczytanie istniejącej sali
getExisitingClassroomName classroom = getObjectName classroom elem "Podaj nazwę sali do usuniecia"

----Wyświetlenie zdefiniowanych sal w modelu
listClassrooms model = do 
  printSeparator
  print $ getClassrooms model
  return model
---Dodanie sali do modelu
addClassroom (Model classes  classrooms groups schedule)  = do 
  maybeClassroomName <- getNewClassroomName (getClassroomList classrooms)
  if isNothing maybeClassroomName then do 
    showMessage classroomExistErrorString
    return (Model classes classrooms groups schedule)
  else do
    let newClassrooms = doAddClassroom ( fromJust maybeClassroomName) classrooms
    showMessage successfulOperationString
    return (Model classes  newClassrooms groups schedule)

doAddClassroom classroomName (Classrooms c) = Classrooms ([classroomName] ++ c)
    
--- Usuwanie sali z modelu
removeClassroom (Model classes classrooms groups schedule ) = do
  maybeClassroomName <- getExisitingClassroomName (getClassroomList classrooms)
  if isNothing maybeClassroomName then do  
    showMessage classroomNonExistingErrorString
    return (Model classes classrooms groups schedule)
  else do
    -- TODO dopisanie usuwianie usuniętego obiektu z już ułożonego planu albo jakieś inne rozwiązanie
    let newClassrooms = doRemoveClassroom ( fromJust maybeClassroomName) classrooms
    showMessage successfulOperationString
    return (Model classes ( Classrooms newClassrooms) groups schedule)
    
doRemoveClassroom classroomName (Classrooms c) = removeItem classroomName c


---Manualne ukladanie planu



addCourse model = do
  listGroups model
  maybeGroupName <- getExisitingGroupName (getGroupList groups)
  if isNothing maybeGroupName then do  
    showMessage groupNonExistingErrorString
    return model
  else do
    listSubjects model
    maybeSubjectName <- getExisitingSubjectName ( getSubjects classes )
    if isNothing maybeSubjectName then do 
      showMessage subjectNonExistingErrorString
      return model
    else do
      listClassrooms model
      maybeClassroomName <- getExisitingClassroomName (getClassroomList classrooms)
      if isNothing maybeClassroomName then do  
        showMessage classroomNonExistingErrorString
        return model
      else do
        maybeStartTime <- getStartTime
        maybeEndTime <- getEndTime
        maybeDay <- getDayGui
        if isNothing maybeStartTime || isNothing maybeEndTime || isNothing maybeDay then do 
          showMessage hourErrorString
          return model
        else do
          let newSubject = fromJust maybeSubjectName
          let newClassroom = fromJust maybeClassroomName
          let newGroup = fromJust maybeGroupName
          let newDay = (toEnum (fromJust maybeDay)) :: Day
          let dayTime = DayTime newDay ( fromJust maybeStartTime) (fromJust maybeEndTime)
          let course = Course newSubject newGroup newClassroom dayTime
          if not $ checkCourse model course then do
            showMessage collisionErrorString
            return model
          else do
            return model
          
          
    
  where
    classes = getClasses model
    classrooms = getClassrooms model
    groups = getGroups model
    schedule = getSchedule model
    
    
getStartTime = getSingletonObject readMaybeHour ("Podaj podaj godzinę rozpoczęcia zajęć" ++ timeWarningString)

getEndTime = getSingletonObject readMaybeHour ("Podaj podaj godzinę zakończenia zajęć" ++ timeWarningString)

getDayGui = getSingletonObject readMaybeDay ("Podaj dzień tygodnia 1-Pon, 2-Wt ... 5-Pt")
readMaybeHour :: String -> Maybe Int
readMaybeHour s =
	case reads s of
	[(x, "")] -> 
                if x >= 8 && x <= 20 then 
                  Just x
                else 
                  Nothing
	_ -> Nothing
  
readMaybeDay :: String -> Maybe Int
readMaybeDay s =
	case reads s of
	[(x, "")] -> 
                if x >= 1 && x <= 7 then 
                  Just x
                else 
                  Nothing
	_ -> Nothing

checkCourse :: Model -> Course -> Bool
checkCourse model course = True
