module Hasyr.Task.TaskList where

import Prelude

import Data.Array (filter, fold, snoc)
import Data.Either (either)
import Hareactive.Combinators (changes, runAffNow, stepTo)
import Hareactive.Combinators as H
import Hasyr.Task.AddTaskForm (addTaskForm)
import Hasyr.Task.Apis (getTodosFromFakeServer)
import Hasyr.Task.TaskItem (taskItem)
import Network.RemoteData (RemoteData(..), fromEither)
import Turbine (Component, component, list, output, use, (</>))
import Turbine.HTML as E

taskList :: Component {} {}
taskList = component \on -> do
  -- Delete stream from individual item
  let foldedStream = map (map _.deleteItem >>> fold) on.actions
  let deleteItem = H.shiftCurrent foldedStream

  -- Fetch stream on init
  fromServer <- runAffNow getTodosFromFakeServer
  let futureStatus = fromServer <#> (either (const $ Failure "Unknown error") fromEither)
  let tasksRemoteStatus = stepTo NotAsked futureStatus

  items <- H.accum identity [] (
    (
      on.addItem <#>
      (\task arr -> snoc arr task)
    ) <>
    (
      deleteItem <#>
      (\id arr -> filter ((_.id) >>> (_ /= id)) arr)
    ) <>
    (
      (changes tasksRemoteStatus) <#>
      (\status arr -> case status of
        Success tasks -> arr <> tasks
        _ -> arr
      )
    )
  )

  E.section {} (
    addTaskForm `use` (\o -> { addItem: o.submit }) </>
    E.div { class: pure "content" } (
      E.ul {} (
        list (\item -> taskItem item `use` (\o -> { deleteItem: o.delete })) items (_.id)
          `use` (\o -> { actions: o })
      )
    )
  ) `output` {}

