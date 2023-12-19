{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Database where

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH
import qualified Student
import qualified Teacher
import qualified Topic

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Student
    Id Int
    name String
    email String
    UniqueStudentId Id
    deriving Show

Teacher
    Id Int
    name String
    email String
    UniqueTeacherId Id
    deriving Show

Topic
    Id Int
    name String
    description String
    UniqueTopicId Id
    deriving Show

-- Add other entities like Schedule, Question, Answer, etc.
|]

-- Database connection information
connInfo :: ConnectInfo
connInfo = defaultConnectInfo {
    connectHost = "127.0.0.1",
    connectDatabase = "lab6",
    connectUser = "root",
    connectPassword = "240303vova"
    }

-- Function to run database migrations
runMigrations :: IO ()
runMigrations = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        runMigration migrateAll

addNewStudent :: Student.Student -> IO ()
addNewStudent student = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        studentId <- insert $ Student (Student.studentId student) (Student.studentName student) (Student.studentEmail student)
        liftIO $ print ("Inserted student with ID: " ++ show (studentId))

findStudent :: Int -> IO (Maybe Student)
findStudent studentId = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        student <- get (StudentKey $ SqlBackendKey $ fromIntegral studentId)
        return student

updateStudent :: Int -> String -> String -> IO ()
updateStudent studentId newName newEmail = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        update (StudentKey $ SqlBackendKey $ fromIntegral studentId) [StudentName =. newName, StudentEmail =. newEmail]
        liftIO $ putStrLn "Student updated successfully"

deleteStudent :: Int -> IO ()
deleteStudent studentId = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        delete (StudentKey $ SqlBackendKey $ fromIntegral studentId)
        liftIO $ putStrLn "Student deleted successfully"

listAllStudents :: IO [Student]
listAllStudents = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        students <- selectList [] []
        return $ map entityVal students

addTeacher :: Teacher.Teacher -> IO ()
addTeacher teacher = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        _ <- insert $ Teacher (Teacher.teacherName teacher) (Teacher.teacherEmail teacher)
        liftIO $ putStrLn "Teacher added successfully"

findTeacher :: Int -> IO (Maybe Teacher)
findTeacher teacherId = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        teacher <- get (TeacherKey $ SqlBackendKey $ fromIntegral teacherId)
        return teacher

updateTeacher :: Int -> String -> String -> IO ()
updateTeacher teacherId newName newEmail = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        update (TeacherKey $ SqlBackendKey $ fromIntegral teacherId) [TeacherName =. newName, TeacherEmail =. newEmail]
        liftIO $ putStrLn "Teacher updated successfully"

deleteTeacher :: Int -> IO ()
deleteTeacher teacherId = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        delete (TeacherKey $ SqlBackendKey $ fromIntegral teacherId)
        liftIO $ putStrLn "Teacher deleted successfully"

listAllTeachers :: IO [Teacher]
listAllTeachers = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        teachers <- selectList [] []
        return $ map entityVal teachers

addTopic :: Topic.Topic -> IO ()
addTopic topic = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        _ <- insert $ Topic (Topic.topicName topic) (Topic.topicDescription topic)
        liftIO $ putStrLn "Topic added successfully"

findTopic :: Int -> IO (Maybe Topic)
findTopic topicId = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        topic <- get (TopicKey $ SqlBackendKey $ fromIntegral topicId)
        return topic

updateTopic :: Int -> String -> String -> IO ()
updateTopic topicId newName newDescription = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        update (TopicKey $ SqlBackendKey $ fromIntegral topicId) [TopicName =. newName, TopicDescription =. newDescription]
        liftIO $ putStrLn "Topic updated successfully"

deleteTopic :: Int -> IO ()
deleteTopic topicId = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        delete (TopicKey $ SqlBackendKey $ fromIntegral topicId)
        liftIO $ putStrLn "Topic deleted successfully"

listAllTopics :: IO [Topic]
listAllTopics = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        topics <- selectList [] []
        return $ map entityVal topics

addSchedule :: Schedule.Schedule -> IO ()
addSchedule schedule = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        _ <- insert $ Schedule (Schedule.scheduleDate schedule) (Schedule.scheduleTime schedule) (Schedule.scheduleLocation schedule)
        liftIO $ putStrLn "Schedule added successfully"

findSchedule :: Int -> IO (Maybe Schedule)
findSchedule scheduleId = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        schedule <- get (ScheduleKey $ SqlBackendKey $ fromIntegral scheduleId)
        return schedule

updateSchedule :: Int -> Day -> String -> String -> IO ()
updateSchedule scheduleId newDate newTime newLocation = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        update (ScheduleKey $ SqlBackendKey $ fromIntegral scheduleId) [ScheduleDate =. newDate, ScheduleTime =. newTime, ScheduleLocation =. newLocation]
        liftIO $ putStrLn "Schedule updated successfully"

deleteSchedule :: Int -> IO ()
deleteSchedule scheduleId = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        delete (ScheduleKey $ SqlBackendKey $ fromIntegral scheduleId)
        liftIO $ putStrLn "Schedule deleted successfully"

listAllSchedules :: IO [Schedule]
listAllSchedules = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        schedules <- selectList [] []
        return $ map entityVal schedules

addQuestion :: Question.Question -> IO ()
addQuestion question = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        _ <- insert $ Question (Question.studentId question) (Question.questionText question)
        liftIO $ putStrLn "Question added successfully"

findQuestion :: Int -> IO (Maybe Question)
findQuestion questionId = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        question <- get (QuestionKey $ SqlBackendKey $ fromIntegral questionId)
        return question

deleteQuestion :: Int -> IO ()
deleteQuestion questionId = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        delete (QuestionKey $ SqlBackendKey $ fromIntegral questionId)
        liftIO $ putStrLn "Question deleted successfully"

listAllQuestions :: IO [Question]
listAllQuestions = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        questions <- selectList [] []
        return $ map entityVal questions

addAnswer :: Answer.Answer -> IO ()
addAnswer answer = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        _ <- insert $ Answer (Answer.questionId answer) (Answer.teacherId answer) (Answer.answerText answer)
        liftIO $ putStrLn "Answer added successfully"

findAnswersForQuestion :: Int -> IO [Answer]
findAnswersForQuestion questionId = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        answers <- selectList [AnswerQuestionId ==. questionId] []
        return $ map entityVal answers

deleteAnswer :: Int -> IO ()
deleteAnswer answerId = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        delete (AnswerKey $ SqlBackendKey $ fromIntegral answerId)
        liftIO $ putStrLn "Answer deleted successfully"

listAllAnswers :: IO [Answer]
listAllAnswers = runStdoutLoggingT $ withMySQLConn connInfo $ \backend ->
    flip runReaderT backend $ do
        answers <- selectList [] []
        return $ map entityVal answers


