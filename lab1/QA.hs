module QA
    ( Question(..)
    , Answer(..)
    , addQuestion
    , addAnswer
    , findQuestion
    , listQuestions
    , listAnswers
    ) where

import Data.List (find)

-- Question data type
data Question = Question {
    questionId :: Int,
    studentId :: Int, -- Assuming Student ID is linked to the question
    questionText :: String
} deriving (Show, Eq)

-- Answer data type
data Answer = Answer {
    answerId :: Int,
    questionId :: Int, -- Linking the answer to a question
    teacherId :: Int, -- Assuming Teacher ID for the answer
    answerText :: String
} deriving (Show, Eq)

-- Add a question (Create)
addQuestion :: [Question] -> Question -> [Question]
addQuestion questions newQuestion = newQuestion : questions

-- Add an answer (Create)
addAnswer :: [Answer] -> Answer -> [Answer]
addAnswer answers newAnswer = newAnswer : answers

-- Find a question by ID (Read)
findQuestion :: [Question] -> Int -> Maybe Question
findQuestion questions qid = find (\question -> questionId question == qid) questions

-- List all questions
listQuestions :: [Question] -> String
listQuestions = unlines . map displayQuestion

-- List all answers for a specific question
listAnswers :: [Answer] -> Int -> String
listAnswers answers qid = unlines . map displayAnswer . filter (\answer -> questionId answer == qid) answers

-- Display function for Question
displayQuestion :: Question -> String
displayQuestion question =
    "Question ID: " ++ show (questionId question) ++
    ", Student ID: " ++ show (studentId question) ++
    ", Text: " ++ questionText question

-- Display function for Answer
displayAnswer :: Answer -> String
displayAnswer answer =
    "Answer ID: " ++ show (answerId answer) ++
    ", Question ID: " ++ show (questionId answer) ++
    ", Teacher ID: " ++ show (teacherId answer) ++
    ", Text: " ++ answerText answer
