module Hasyr.Task.TaskList (taskList) where

import Prelude

import Data.Array (filter, fold, snoc)
import Data.Either (either)
import Hareactive.Combinators as H
import Hareactive.Types (Behavior, Now, Stream)
import Hasyr.Component.Message as Message
import Hasyr.Task.AddTaskForm (addTaskForm)
import Hasyr.Task.Apis (getTodosFromFakeServer)
import Hasyr.Task.TaskItem (taskItem)
import Hasyr.Task.Types (Task)
import Network.RemoteData (RemoteData(..), fromEither, isFailure)
import Turbine (Component, component, dynamic, list, output, use, (</>))
import Turbine.HTML as E

type TasksRemoteStatus = Behavior (RemoteData String (Array Task))

getErrorMsg :: ∀ a. RemoteData String a -> String
getErrorMsg (Failure f) = f
getErrorMsg _ = ""

useDeleteItem :: Behavior (Array { deleteItem :: Stream Number }) -> Now (Stream Number)
useDeleteItem actions = do
  let foldedStream = map (map _.deleteItem >>> fold) actions
  let deleteItem = H.shiftCurrent foldedStream
  pure deleteItem

useGetTodos :: Now TasksRemoteStatus
useGetTodos = do
  fromServer <- H.runAffNow getTodosFromFakeServer
  let futureStatus = fromServer <#> (either (const $ Failure "Unknown error") fromEither)
  let tasksRemoteStatus = H.stepTo NotAsked futureStatus
  pure tasksRemoteStatus

useErrorMessage ::
  { tasksRemoteStatus :: TasksRemoteStatus
  , closeMsg :: Stream Unit
  } ->
  Now (Component (Behavior { close :: Stream Unit }) {})
useErrorMessage deps = do
  let toggleOpen = H.filter isFailure (H.changes deps.tasksRemoteStatus)
  let toggleClose = deps.closeMsg
  isError <- H.toggle false toggleOpen toggleClose

  let alertComponent = dynamic (isError <#>
    if _ then
      Message.message { type: Message.Danger, body: deps.tasksRemoteStatus <#> getErrorMsg }
      `use` (\o -> { close: o.close })
    else
      E.empty `use` (\o -> { close: mempty :: Stream Unit })
  )
  pure alertComponent

taskList :: Component {} {}
taskList = component \on -> do
  deleteItem <- useDeleteItem on.actions
  tasksRemoteStatus <- useGetTodos
  alert <- useErrorMessage { tasksRemoteStatus, closeMsg: on.closeMsg }

  items <- H.accum identity [] (
    ( on.addItem <#> flip snoc ) <>
    ( deleteItem <#> (\id arr -> filter ((_.id) >>> (_ /= id)) arr) ) <>
    (
      (H.changes tasksRemoteStatus) <#>
      (\status arr -> case status of
        Success tasks -> arr <> tasks
        _ -> arr
      )
    )
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
