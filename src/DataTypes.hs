module DataTypes where

-- Obiekt reprezentujący dzień tygodnia
data Day = Pon | Wt | Sr | Czw | Pt | Sob | Nd deriving (Eq, Show, Enum, Read)

-- Typy

-- Zakałada się zajecia mogą zaczyna się i kończyć tylko o pełnych godzinach zegarowych
type ClassStartTime = Int
type ClassEndTime = Int
type DurationTime = Int

type Classroom = String
type Group = String
type Subject = String

-- Zajęcia
data Class = Class Subject DurationTime deriving (Show,Read,Eq)

getSubject :: Class -> Subject
getSubject (Class s _ ) = s
getDurationTime (Class _ dt) = dt

getSubjects :: [Class] -> [Subject]
getSubjects [] = []
getSubjects (c:cs) = [(getSubject c)] ++ (getSubjects cs)

data DayTime = DayTime Day ClassStartTime ClassEndTime deriving (Show,Read,Eq)
getDay (DayTime dt _ _ ) = dt
getClassStartTime (DayTime  _ st _ ) = st
getClassEndTime (DayTime _ _ et ) = et
-- Zajecia
data Course = Course Subject Group Classroom DayTime deriving (Show, Read)

getSubjectCourse (Course s _ _ _ ) = s
getGroup (Course _ g _  _) = g
getClassroom (Course _ _ c _) = c
getDayTime (Course _ _ _ d) = d


data Model = Model [Class] [Classroom] [Group] [Course] deriving (Show, Read)
getClasses (Model c _ _ _) =  c
getClassrooms (Model _ c _ _) = c
getGroups (Model _ _ g _) = g
getCourses (Model _ _ _ c) = c


addCourseToModel :: Model -> Course -> Model
addCourseToModel (Model c1 c2 g c3) course = Model c1 c2 g ( ([course] ++ c3))