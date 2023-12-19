module Student 
    ( Student(..)
    , createStudent
    , addStudent
    , findStudent
    , updateStudent
    , deleteStudent
    , listStudents
    ) where
import Data.List (find, delete)

-- Student data type
data Student = Student {
    studentId :: Int,
    studentName :: String,
    studentEmail :: String
} deriving (Show, Eq)

-- Function to create a new student
createStudent :: Int -> String -> String -> Student
createStudent id name email = Student id name email

-- Function to display student information
displayStudent :: Student -> String
displayStudent student =
    "Student ID: " ++ show (studentId student) ++
    ", Name: " ++ studentName student ++
    ", Email: " ++ studentEmail student

-- Add a student to the list (Create)
addStudent :: [Student] -> Student -> [Student]
addStudent students newStudent = newStudent : students

-- Find a student by ID (Read)
findStudent :: [Student] -> Int -> Maybe Student
findStudent students sid = find (\student -> studentId student == sid) students

-- Update a student's details (Update)
updateStudent :: [Student] -> Student -> [Student]
updateStudent students updatedStudent = updatedStudent : delete updatedStudent students

-- Delete a student by ID (Delete)
deleteStudent :: [Student] -> Int -> [Student]
deleteStudent students sid = filter (\student -> studentId student /= sid) students

-- List all students
listStudents :: [Student] -> String
listStudents = unlines . map displayStudent
