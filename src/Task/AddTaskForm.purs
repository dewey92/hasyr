module Hasyr.Task.AddTaskForm (addTaskForm) where

import Prelude

import Data.String (trim)
import Hareactive.Combinators (snapshot, time)
import Hareactive.Combinators as H
import Hareactive.Types (Behavior, Stream)
import Hasyr.Task.Types (Task)
import Turbine (Component, modelView, use, withStatic)
import Turbine.HTML as E
import Web.UIEvent.KeyboardEvent as KE

isKey :: String -> KE.KeyboardEvent -> Boolean
isKey key event = (KE.key event) == key

addTaskForm :: Component { taskName :: Behavior String, submit :: Stream Task } {}
addTaskForm = modelView model view where
  model { keyup, value } = do
    let onEnterPressed = H.filter (isKey "Enter") keyup
    let onSubmit = H.filter (trim >>> (_ /= "")) (snapshot value onEnterPressed)
    let newTask = H.snapshotWith (\name id -> { id, name }) time onSubmit

    taskName <- H.stepper "" (H.changes value <> (onSubmit $> ""))

    pure { taskName, submit: newTask }
  view input =
    E.section {} (
      E.input (
        { value: input.taskName, class: pure "input" } `withStatic`
        { placeholder: "What needs to be done?", autofocus: true }
      ) `use` (\o -> { value: o.value, keyup: o.keyup })
    )
