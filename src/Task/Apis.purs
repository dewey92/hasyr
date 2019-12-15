module Hasyr.Task.Apis where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as Format
import Data.Argonaut (Json, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Hasyr.Task.Types (Task)

tasksFromJson :: Json -> Either String (Array Task)
tasksFromJson = decodeJson

getTodosFromFakeServer :: Aff (Either String (Array Task))
getTodosFromFakeServer = do
  let fakeUrl = "https://my-json-server.typicode.com/dewey92/hasyr/todos"
  rawResult <- AX.get Format.json fakeUrl
  delay (Milliseconds 3000.0) -- not too fast, I want to show loading indicator

  pure $ do
    { body } <- lmap AX.printError rawResult
    tasks <- tasksFromJson body
    pure tasks
