module Strings where


errorStr msg = "Błąd: " ++ msg

--Błędy ogólne
successfulOperationString = "Pomyślnie wykonano operację"

--Błędy dot. przedmiotów
subjectExistErrorString = errorStr "Przedmiot o podanej nazwie już istnieje"
subjectNonExistingErrorString = errorStr "Przedmiot o podanej nazwie nie istnieje i nie może zostać usnięty"
--Błędy dot. grup
groupExistErrorString = errorStr "Grupa o podanej nazwie już istnieje"
groupNonExistingErrorString = errorStr "Grupa o podanej nazwie nie istnieje"
--Błędy dot. sal 
classroomExistErrorString = errorStr "Sala o podanej nazwie juz istnieje"
classroomNonExistingErrorString = errorStr "Sala o podanej nazwie nie istnieje w modelu"



--Błędy IO
invalidFormatErrorString =  errorStr "nieprawidłowy format danych"
cannotOpenFileErrorString = errorStr "Nie można odczytać pliku"

-- Ostrzenie o preferowanym oknie czasowoym
timeWarningString =  "\n Zajęcia mogą się odbywać od godziny 8 - 20"
hourErrorString = errorStr "Nieprawidłowy format wprowadzonej daty"
timeOrderTimeErrorString = errorStr "Wprowadzona godzinna początku zajęć jest późniejsza niż godzinna końca zajęć"

-- Błędy układania planu
collisionErrorString = errorStr "Nie można dodać do planu wprowadzonych zajęć z powodu, że użyte zasoby są w tym czasie już wykorzystywane!"

classroomBusyErrorString = errorStr "Podana sala jest już zajęta w zdefiniownym czasie"
groupBusyErrorString = errorStr "Podana grupa jest już przypisana do zajęć w zdefiniowanym czasie"
subjectBusyErrorString = errorStr "Podany przedmiot jest już wykładany w zdefiniowanym czasie"
