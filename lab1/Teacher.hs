module Teacher
    ( Teacher(..)
    , createTeacher
    , addTeacher
    , findTeacher
    , updateTeacher
    , deleteTeacher
    , listTeachers
    ) where

import Data.List (find, delete)

-- Teacher data type
data Teacher = Teacher {
    teacherId :: Int,
    teacherName :: String,
    teacherEmail :: String
} deriving (Show, Eq)

-- Function to create a new teacher
createTeacher :: Int -> String -> String -> Teacher
createTeacher id name email = Teacher id name email

-- Function to display teacher information
displayTeacher :: Teacher -> String
displayTeacher teacher =
    "Teacher ID: " ++ show (teacherId teacher) ++
    ", Name: " ++ teacherName teacher ++
    ", Email: " ++ teacherEmail teacher

-- Add a teacher to the list (Create)
addTeacher :: [Teacher] -> Teacher -> [Teacher]
addTeacher teachers newTeacher = newTeacher : teachers

-- Find a teacher by ID (Read)
findTeacher :: [Teacher] -> Int -> Maybe Teacher
findTeacher teachers tid = find (\teacher -> teacherId teacher == tid) teachers

-- Update a teacher's details (Update)
updateTeacher :: [Teacher] -> Teacher -> [Teacher]
updateTeacher teachers updatedTeacher = updatedTeacher : delete updatedTeacher teachers

-- Delete a teacher by ID (Delete)
deleteTeacher :: [Teacher] -> Int -> [Teacher]
deleteTeacher teachers tid = filter (\teacher -> teacherId teacher /= tid) teachers

-- List all teachers
listTeachers :: [Teacher] -> String
listTeachers = unlines . map displayTeacher
