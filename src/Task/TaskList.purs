module Hasyr.Task.TaskList (taskList) where

import Prelude

import Data.Array (filter, fold, snoc)
import Data.Either (either)
import Hareactive.Combinators as H
import Hareactive.Types (Stream)
import Hasyr.Component.Message as Message
import Hasyr.Task.AddTaskForm (addTaskForm)
import Hasyr.Task.Apis (getTodosFromFakeServer)
import Hasyr.Task.TaskItem (taskItem)
import Network.RemoteData (RemoteData(..), fromEither, isFailure)
import Turbine (Component, component, dynamic, list, output, use, (</>))
import Turbine.HTML as E

taskList :: Component {} {}
taskList = component \on -> do
  -- Delete stream from individual item
  let foldedStream = map (map _.deleteItem >>> fold) on.actions
  let deleteItem = H.shiftCurrent foldedStream

  -- Fetch stream on init
  fromServer <- H.runAffNow getTodosFromFakeServer
  let futureStatus = fromServer <#> (either (const $ Failure "Unknown error") fromEither)
  let tasksRemoteStatus = H.stepTo NotAsked futureStatus

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
      (H.changes tasksRemoteStatus) <#>
      (\status arr -> case status of
        Success tasks -> arr <> tasks
        _ -> arr
      )
    )
  )

  -- Error message
  let toggleOpen = H.filter isFailure (H.changes tasksRemoteStatus)
  let toggleClose = on.closeMsg
  isError <- H.toggle false toggleOpen toggleClose

  let alert = dynamic (isError <#>
    if _ then
      Message.message { type: Message.Danger, body: tasksRemoteStatus <#> getErrorMsg }
      `use` (\o -> { close: o.close })
    else
      E.empty `use` (\o -> { close: mempty :: Stream Unit })
  )

  E.section {} (
    addTaskForm `use` (\o -> { addItem: o.submit }) </>
    alert `use` (\bhvr -> { closeMsg: H.shiftCurrent (bhvr <#> _.close) }) </>
    E.div {} (
      E.ul {} (
        list (\item -> taskItem item `use` (\o -> { deleteItem: o.delete })) items (_.id)
          `use` (\o -> { actions: o })
      )
    )
  ) `output` {}

getErrorMsg :: ∀ a. RemoteData String a -> String
getErrorMsg (Failure f) = f
getErrorMsg _ = ""
