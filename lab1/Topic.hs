module Topic
    ( Topic(..)
    , createTopic
    , addTopic
    , findTopic
    , updateTopic
    , deleteTopic
    , listTopics
    ) where

import Data.List (find, delete)

-- Topic data type
data Topic = Topic {
    topicId :: Int,
    topicName :: String,
    topicDescription :: String
} deriving (Show, Eq)

-- Function to create a new topic
createTopic :: Int -> String -> String -> Topic
createTopic id name description = Topic id name description

-- Function to display topic information
displayTopic :: Topic -> String
displayTopic topic =
    "Topic ID: " ++ show (topicId topic) ++
    ", Name: " ++ topicName topic ++
    ", Description: " ++ topicDescription topic

-- Add a topic to the list (Create)
addTopic :: [Topic] -> Topic -> [Topic]
addTopic topics newTopic = newTopic : topics

-- Find a topic by ID (Read)
findTopic :: [Topic] -> Int -> Maybe Topic
findTopic topics tid = find (\topic -> topicId topic == tid) topics

-- Update a topic's details (Update)
updateTopic :: [Topic] -> Topic -> [Topic]
updateTopic topics updatedTopic = updatedTopic : delete updatedTopic topics

-- Delete a topic by ID (Delete)
deleteTopic :: [Topic] -> Int -> [Topic]
deleteTopic topics tid = filter (\topic -> topicId topic /= tid) topics

-- List all topics
listTopics :: [Topic] -> String
listTopics = unlines . map displayTopic
