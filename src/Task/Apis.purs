module Hasyr.Task.Apis where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as Format
import Data.Argonaut (Json, decodeJson)
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Hasyr.Task.Types (Task)

-- class Monad m <= ManageTasks m where
--   getAllTasks :: m (Either String (Array Task))

-- instance manageTasksNow :: ManageTasks Now where
--   getAllTasks = runAffNow $ getTodosFromFakeServer

tasksFromJson :: Json -> Either String (Array Task)
tasksFromJson = decodeJson

getTodosFromFakeServer :: Aff (Either String (Array Task))
getTodosFromFakeServer = do
  let fakeUrl = "https://my-json-server.typicode.com/dewey92/hasyr/todos"
  res <- AX.get Format.json fakeUrl
  delay (Milliseconds 3000.0) -- not too fast, I want to show loading indicator
  pure $ (bimap AX.printError _.body res) >>= tasksFromJson
