module Logic where 

import DataTypes
import Hibernate
import Utils
import Ui
import Data.Maybe
import Strings
import Data.List

--Przydało by się wykorzystać funkcję Modelu, żeby wybierać pola a nie cały model deklarować

--Logika związana z przedmiotami
----Wyświetlenie przedmiotów
listSubjects model = do 
  showMessageBox "Przedmiot - liczba godzin w tyg."
  showMessageBox subjects
  printSeparator 
  return model
  where 
    classes = getClasses model
    subjects = unlines $ map (\x -> (getSubject x) ++  sep ++ show (getDurationTime x) ) classes
    sep = " - "

--- Wczytanie nazwy nowego przedmiotu
getNewSubjectName subject = getObjectName subject notElem "Podaj nazwę nowego przedmiotu"


--- Wczytanie liczby godzin zajęć (została ogranicznona do max 5 w tygodniu)
getNewDurationTime = getSingletonObject readMaybeTime "Podaj liczbę godzin w tygodniu 1-7"

--- Wczytanie istniejącego przedmiotu
getExisitingSubjectName subject msg = getObjectName subject elem msg

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
    showMessageBox subjectExistErrorString
    return (Model classes classrooms groups schedule)
  else do
    maybeDurationTime <- getNewDurationTime
    let newClasses = doAddSubject ( fromJust maybeSubjectName) ( fromJust maybeDurationTime) classes
    showMessageBox successfulOperationString
    return (Model newClasses  classrooms groups schedule)

doAddSubject subjectName durationTime classes = [(Class subjectName durationTime)] ++ classes
    
--- Usuwanie przedmiotu
removeSubject (Model classes classrooms groups schedule ) = do
  maybeSubjectName <- getExisitingSubjectName ( getSubjects classes ) "Podaj nazwę przedmiotu do usuniecia"
  if isNothing maybeSubjectName then do 
    showMessageBox subjectNonExistingErrorString
    return (Model classes classrooms groups schedule)
  else do
    -- TODO dopisanie usuwianie usuniętego obiektu z już ułożonego planu albo jakieś inne rozwiązanie
    let newClasses = doRemoveSubject ( fromJust maybeSubjectName) classes
    showMessageBox successfulOperationString
    return (Model newClasses  classrooms groups schedule)
    
doRemoveSubject subjectName classes = removeClass subjectName classes

removeClass:: Subject -> [Class] -> [Class]
removeClass _ [] = []
removeClass s (c:cs) | (getSubject c) == s    = cs    
                     | otherwise              = [c] ++ removeClass s cs
  
--Logika związana z grupami

--- Wczytanie nazwy nowej grupy
getNewGroupName group = getObjectName group notElem "Podaj nazwę nowej grupy"

--- Wczytanie istniejącej grupy
getExisitingGroupName group msg = getObjectName group elem msg

----Wyświetlenie grup
  
listGroups model = do 
  showMessageBox "Grupy"
  showMessageBox groups
  printSeparator 
  return model
  where 
    groups = unlines $ getGroups model


---Dodanie grupy do listy
addGroup (Model classes  classrooms groups schedule)  = do 
  maybeGroupName <- getNewGroupName ( groups)
  if isNothing maybeGroupName then do 
    showMessageBox groupExistErrorString
    return (Model classes classrooms groups schedule)
  else do
    let newGroups = doAddGroup ( fromJust maybeGroupName) groups
    showMessageBox successfulOperationString
    return (Model classes  classrooms newGroups schedule)

doAddGroup groupName (g) = ([groupName] ++ g)
    
--- Usuwanie grupy
removeGroup (Model classes classrooms groups schedule ) = do
  maybeGroupName <- getExisitingGroupName ( groups) "Podaj nazwę grupy do usuniecia"
  if isNothing maybeGroupName then do  
    showMessageBox groupNonExistingErrorString
    return (Model classes classrooms groups schedule)
  else do
    -- TODO dopisanie usuwianie usuniętego obiektu z już ułożonego planu albo jakieś inne rozwiązanie
    let newGroups = doRemoveGroup ( fromJust maybeGroupName) groups
    showMessageBox successfulOperationString
    return (Model classes  classrooms  newGroups schedule)
    
doRemoveGroup groupName g = removeItem groupName g

--Logika związana z salami

--- Wczytanie nazwy nowej sali
getNewClassroomName classroom = getObjectName classroom notElem "Podaj nazwę nowej sali"

--- Wczytanie istniejącej sali
getExisitingClassroomName classroom msg = getObjectName classroom elem msg

----Wyświetlenie zdefiniowanych sal w modelu

listClassrooms model = do 
  showMessageBox "Sale wykladowe"
  showMessageBox $ unlines $ getClassrooms model
  printSeparator 
  return model
---Dodanie sali do modelu
addClassroom (Model classes  classrooms groups schedule)  = do 
  maybeClassroomName <- getNewClassroomName classrooms
  if isNothing maybeClassroomName then do 
    showMessageBox classroomExistErrorString
    return (Model classes classrooms groups schedule)
  else do
    let newClassrooms = doAddClassroom ( fromJust maybeClassroomName) classrooms
    showMessageBox successfulOperationString
    return (Model classes  newClassrooms groups schedule)

doAddClassroom classroomName classrooms = [classroomName] ++ classrooms
    
--- Usuwanie sali z modelu
removeClassroom (Model classes classrooms groups schedule ) = do
  maybeClassroomName <- getExisitingClassroomName (classrooms) "Podaj nazwę sali do usuniecia"
  if isNothing maybeClassroomName then do  
    showMessageBox classroomNonExistingErrorString
    return (Model classes classrooms groups schedule)
  else do
    -- TODO dopisanie usuwianie usuniętego obiektu z już ułożonego planu albo jakieś inne rozwiązanie
    let newClassrooms = doRemoveClassroom ( fromJust maybeClassroomName) classrooms
    showMessageBox successfulOperationString
    return (Model classes newClassrooms groups schedule)
    
doRemoveClassroom classroomName classrooms  = removeItem classroomName classrooms


---Manualne ukladanie planu



addCourse model = do
  listGroups model
  maybeGroupName <- getExisitingGroupName (groups) "Podaj nazwę grupy"
  if isNothing maybeGroupName then do  
    showMessageBox groupNonExistingErrorString
    return model
  else do
    listSubjects model
    maybeSubjectName <- getExisitingSubjectName ( getSubjects classes ) "Podaj nazwę przedmiotu"
    if isNothing maybeSubjectName then do 
      showMessageBox subjectNonExistingErrorString
      return model
    else do
      listClassrooms model
      maybeClassroomName <- getExisitingClassroomName ( classrooms) "Podaj nazwę sali"
      if isNothing maybeClassroomName then do  
        showMessageBox classroomNonExistingErrorString
        return model
      else do
        maybeStartTime <- getStartTime
        maybeEndTime <- getEndTime
        maybeDay <- getDayGui
        if isNothing maybeStartTime || isNothing maybeEndTime || isNothing maybeDay then do 
          showMessageBox hourErrorString
          return model
        else do
          if ( fromJust maybeStartTime) > (fromJust maybeEndTime) then do
            showMessageBox timeOrderTimeErrorString
            return model
          else do
            let newSubject = fromJust maybeSubjectName
            let newClassroom = fromJust maybeClassroomName
            let newGroup = fromJust maybeGroupName
            let newDay = (toEnum (fromJust maybeDay)) :: Day
            let dayTime = DayTime newDay ( fromJust maybeStartTime) (fromJust maybeEndTime)
            let course = Course newSubject newGroup newClassroom dayTime
            if  checkCourse model course then do
              showMessageBox collisionErrorString
              return model
            else do
              showMessageBox successfulOperationString
              return ( addCourseToModel model course) 

  where
    classes = getClasses model
    classrooms = getClassrooms model
    groups = getGroups model
    courses = getCourses model
    
    
getStartTime = getSingletonObject readMaybeHour ("Podaj podaj godzinę rozpoczęcia zajęć" ++ timeWarningString)

getEndTime = getSingletonObject readMaybeHour ("Podaj podaj godzinę zakończenia zajęć" ++ timeWarningString)

getDayGui = getSingletonObject readMaybeDay ("Podaj dzień tygodnia 0-Pon, 2-Wt ... 4-Pt")
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
                if x >= 0 && x <= 4 then 
                  Just x
                else 
                  Nothing
	_ -> Nothing


-- True oznacza, że w nie można umieścić w planie tego kursu  
checkCourse :: Model -> Course -> Bool
checkCourse model course = 
  checkSubject model course || checkGroup model course || checkClassroom model course

checkSubject model course = 
  any (checkDayTimeCollision  dayTime)  (map snd selectedSubjectList) 
  where 
    
    dayTime = getDayTime course
    subject = getSubjectCourse course
    coursesList = getCourses model
    subjectList = zip (map getSubjectCourse coursesList) (map getDayTime coursesList)
    selectedSubjectList = map fromJust $ filter isJust  $ map (\x -> if fst x == subject then Just (fst x,snd x) else Nothing) subjectList
    
    
checkGroup model course = 
  elem dayTime (map snd selectedGroupList) 
  where  
    
    dayTime = getDayTime course
    group = getGroup course
    coursesList = getCourses model
    groupList = zip (map getGroup coursesList) (map getDayTime coursesList)
    selectedGroupList = map fromJust $ filter isJust  $ map (\x -> if fst x == group then Just (fst x,snd x) else Nothing) groupList
    
checkClassroom model course = 
  elem dayTime (map snd selectedGroupList) 
  where  
    
    dayTime = getDayTime course
    classroom = getClassroom course
    coursesList = getCourses model
    classroomList = zip (map getClassroom coursesList) (map getDayTime coursesList)
    selectedGroupList = map fromJust $ filter isJust  $ map (\x -> if fst x == classroom then Just (fst x,snd x) else Nothing) classroomList
    
checkDayTimeCollision ::  DayTime -> DayTime -> Bool
checkDayTimeCollision  dt1 dt2 = 
  if d1 /= d2 then 
    True
  else 
    if et1 < st2 || et2 < st1 then
      True
    else
      False
  where
    d1  = getDay dt1
    st1 = getClassStartTime dt1
    et1 = getClassEndTime dt1
    
    d2  = getDay dt2
    st2 = getClassStartTime dt2
    et2 = getClassEndTime dt2
--- Wyświtlanie planów dla przedmiotów  
showSchedule model = do
  showMessageBox "Poniedziałek"
  showMessageBox (listSchedule model Pon)
  showMessageBox "Wtorek"
  showMessageBox (listSchedule model Wt)
  showMessageBox "Środa"
  showMessageBox (listSchedule model Sr)
  showMessageBox "Czwartek"
  showMessageBox (listSchedule model Czw)
  showMessageBox "Piątek"
  showMessageBox (listSchedule model Pt)
  return model

listSchedule model day =
  unlines $ map (\x -> (show (getClassStartTime $  getDayTime x)) ++ seph ++   (show (getClassEndTime $  getDayTime x)) ++ tab ++ (getSubjectCourse x) ++ sep ++ (getGroup x) ++ sep ++  show (getClassroom x)) sortedList
  
  where 
    courses = getCourses model
--    objectList = map getCourses courses
    filteredList = filter (\x -> if (getDay $ getDayTime x) == day then True else False ) courses
    sortedList = sortBy compareDays filteredList
    sep  = " - " 
    seph = " - "
    tab  = "\t"
    

  
compareDays c1 c2 = 
  if d1  > d2 then GT
  else LT
  where
    d1 = getClassStartTime   (getDayTime c1)
    d2 = getClassStartTime $ getDayTime c2
