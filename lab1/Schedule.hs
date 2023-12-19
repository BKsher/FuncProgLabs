module Schedule
    ( Schedule(..)
    , createSchedule
    , addSchedule
    , findSchedule
    , updateSchedule
    , deleteSchedule
    , listSchedules
    ) where

import Data.List (find, delete)
import Data.Time (Day)

-- Schedule data type
data Schedule = Schedule {
    scheduleId :: Int,
    scheduleDate :: Day,
    scheduleTime :: String,
    scheduleLocation :: String
} deriving (Show, Eq)

-- Function to create a new schedule
createSchedule :: Int -> Day -> String -> String -> Schedule
createSchedule id date time location = Schedule id date time location

-- Function to display schedule information
displaySchedule :: Schedule -> String
displaySchedule schedule =
    "Schedule ID: " ++ show (scheduleId schedule) ++
    ", Date: " ++ show (scheduleDate schedule) ++
    ", Time: " ++ scheduleTime schedule ++
    ", Location: " ++ scheduleLocation schedule

-- Add a schedule to the list (Create)
addSchedule :: [Schedule] -> Schedule -> [Schedule]
addSchedule schedules newSchedule = newSchedule : schedules

-- Find a schedule by ID (Read)
findSchedule :: [Schedule] -> Int -> Maybe Schedule
findSchedule schedules sid = find (\schedule -> scheduleId schedule == sid) schedules

-- Update a schedule's details (Update)
updateSchedule :: [Schedule] -> Schedule -> [Schedule]
updateSchedule schedules updatedSchedule = updatedSchedule : delete updatedSchedule schedules

-- Delete a schedule by ID (Delete)
deleteSchedule :: [Schedule] -> Int -> [Schedule]
deleteSchedule schedules sid = filter (\schedule -> scheduleId schedule /= sid) schedules

-- List all schedules
listSchedules :: [Schedule] -> String
listSchedules = unlines . map displaySchedule
