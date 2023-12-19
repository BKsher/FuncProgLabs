{-# LANGUAGE OverloadedStrings #-}

import qualified Student
import qualified Teacher
import qualified Topic
import qualified Schedule
import qualified QA
import Database (runMigrations, addStudent, listAllStudents, addTeacher, listAllTeachers, addTopic, listAllTopics, addQuestion, listAllQuestions, addAnswer, listAllAnswers)

import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    putStrLn "Starting the application..."

    -- Run database migrations
    runMigrations

    -- Example operations
    -- Adding entities
    let newStudent = Student.createStudent "John Doe" "john@example.com"
    addStudent newStudent

    let newTeacher = Teacher.createTeacher "Jane Smith" "jane@example.com"
    addTeacher newTeacher

    let newTopic = Topic.createTopic "Functional Programming" "Introduction to Haskell"
    addTopic newTopic

    today <- utctDay <$> getCurrentTime
    let newSchedule = Schedule.createSchedule today "10:00 AM" "Room 101"
    Schedule.addSchedule newSchedule

    let newQuestion = QA.createQuestion 1 "What is a Monad?"
    QA.addQuestion newQuestion

    let newAnswer = QA.createAnswer 1 1 "A Monad is a design pattern used in functional programming..."
    QA.addAnswer newAnswer

    -- Listing entities
    putStrLn "Students:"
    listAllStudents >>= print

    putStrLn "Teachers:"
    listAllTeachers >>= print

    putStrLn "Topics:"
    listAllTopics >>= print

    putStrLn "Questions:"
    listAllQuestions >>= print

    putStrLn "Answers:"
    listAllAnswers >>= print

    putStrLn "Application finished."
