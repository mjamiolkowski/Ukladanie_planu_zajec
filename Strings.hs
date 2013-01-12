module Strings where


errorStr msg = "Błąd: " ++ msg

--Błędy ogólne
successfulOperationString = "Pomyślnie wykonano operację"

--Błędy dot. przedmiotów
subjectExistErrorString = "Przedmiot o podanej nazwie już istnieje"
subjectNonExistingErrorString = "Przedmiot o podanej nazwie nie istnieje i nie może zostać usnięty"
--Błędy dot. grup
groupExistErrorString = "Grupa o podanej nazwie już istnieje"
groupNonExistingErrorString = "Grupa o podanej nazwie nie istnieje"
--Błędy dot. sal 
classroomExistErrorString = "Sala o podanej nazwie juz istnieje"
classroomNonExistingErrorString = "Sala o podanej nazwie nie istnieje w modelu"


--Błędy IO
invalidFormatErrorString =  errorStr "nieprawidłowy format danych"
cannotOpenFileErrorString = "Nie możn odczytać pliku"

-- Ostrzenie o preferowanym oknie czasowoym
timeWarningString = "\n Zajęcia mogą się odbywać od godziny 8 - 20"
hourErrorString = "Nieprawidłowy format wprowadzonej daty"

-- Błędy układania planu
collisionErrorString = "Nie można dodać do planu wprowadzonych zajęć z powodu, że użyte zasoby są w tym czasie już wykorzystywane!"

classroomBusyErrorString = "Podana sala jest już zajęta w zdefiniownym czasie"
groupBusyErrorString = "Podana grupa jest już przypisana do zajęć w zdefiniowanym czasie"
subjectBusyErrorString = "Podany przedmiot jest już wykładany w zdefiniowanym czasie"
