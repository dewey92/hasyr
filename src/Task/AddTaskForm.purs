module Hasyr.Task.AddTaskForm (addTaskForm) where

import Prelude

import Data.String (null, trim)
import Hareactive.Combinators as H
import Hareactive.Types (Behavior, Stream)
import Hasyr.Task.Types (Task)
import Haysr.Utils (isKey)
import Turbine (Component, modelView, use, withStatic)
import Turbine.HTML as E

addTaskForm :: Component { taskNameB :: Behavior String, submitS :: Stream Task } {}
addTaskForm = modelView model view where
  model { keyup, value } = do
    let enterPressedS = H.filter (isKey "Enter") keyup
    let submitS = H.filter (not <<< null <<< trim) (H.snapshot value enterPressedS)
    let newTaskS = H.snapshotWith (\name id -> { id, name }) H.time submitS

    taskNameB <- H.stepper "" (H.changes value <> (submitS $> ""))

    pure { taskNameB, submitS: newTaskS }
  view input =
    E.section {} (
      E.input (
        { value: input.taskNameB, class: pure "input" } `withStatic`
        { placeholder: "What needs to be done?", autofocus: true }
      ) `use` (\o -> { value: o.value, keyup: o.keyup })
    )
