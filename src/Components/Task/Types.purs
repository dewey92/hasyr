module Hasyr.Component.Task.Types where

import Data.Argonaut (Json, decodeJson)
import Data.Either (Either)

type Task = { id :: Number, name :: String }

taskFromJson :: Json -> Either String Task
taskFromJson = decodeJson
