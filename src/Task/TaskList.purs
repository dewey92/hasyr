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
import Network.RemoteData (RemoteData(..), fromEither, isFailure, isLoading)
import Turbine (Component, component, dynamic, list, output, use, (</>))
import Turbine.HTML as E

type TasksRemoteStatus = Behavior (RemoteData String (Array Task))

getErrorMsg :: ∀ a. RemoteData String a -> String
getErrorMsg (Failure f) = f
getErrorMsg _ = ""

useDeleteItem :: Behavior (Array { deleteItemS :: Stream Number }) -> Now (Stream Number)
useDeleteItem actions = do
  let foldedS = map (map _.deleteItemS >>> fold) actions
  let deleteItemS = H.shiftCurrent foldedS
  pure deleteItemS

useGetTodos :: Now TasksRemoteStatus
useGetTodos = do
  fromServer <- H.runAffNow getTodosFromFakeServer
  let remoteStatusF = fromServer <#> (either (const $ Failure "Unknown error") fromEither)
  let remoteStatusB = H.stepTo Loading remoteStatusF
  pure remoteStatusB

useErrorMessage ::
  { tasksRemoteStatusB :: TasksRemoteStatus
  , closeMsgS :: Stream Unit
  } ->
  Now (Component {} { closeMsgS :: Stream Unit })
useErrorMessage deps = do
  let toggleOpenS = H.filter isFailure (H.changes deps.tasksRemoteStatusB)
  let toggleCloseS = deps.closeMsgS
  isErrorB <- H.toggle false toggleOpenS toggleCloseS

  let alertComponent = dynamic (isErrorB <#>
    if _ then
      Message.message { type: Message.Danger, body: getErrorMsg <$> deps.tasksRemoteStatusB }
      `use` identity
    else
      E.empty `use` (\_ -> { closeS: mempty :: Stream Unit })
  )
  pure $ alertComponent `use` (\bhvr -> { closeMsgS: H.shiftCurrent (bhvr <#> _.closeS) })

useLoading :: { tasksRemoteStatusB :: TasksRemoteStatus } -> Now (Component {} {})
useLoading { tasksRemoteStatusB } = do
  let loadingC = dynamic (tasksRemoteStatusB <#> \r ->
    if isLoading r then
      E.div
        { class: pure "has-text-weight-semibold is-size-5 has-margin-15 has-text-centered" }
        (E.text "Loading")
    else E.div {} E.empty
  )
  pure $ loadingC `use` (\_ -> {})

taskList :: Component {} {}
taskList = component \on -> do
  deleteItemS        <- useDeleteItem on.actions
  tasksRemoteStatusB <- useGetTodos
  loadingC           <- useLoading { tasksRemoteStatusB }
  errMsgC            <- useErrorMessage { tasksRemoteStatusB, closeMsgS: on.closeMsgS }

  items <- H.accum identity [] (
    ( on.addItemS <#> flip snoc ) <>
    ( deleteItemS <#> (\id arr -> filter ((_.id) >>> (_ /= id)) arr) ) <>
    (
      (H.changes tasksRemoteStatusB) <#>
      (\status arr -> case status of
        Success tasks -> arr <> tasks
        _ -> arr
      )
    )
  )

  E.section {} (
    addTaskForm `use` (\o -> { addItemS: o.submitS }) </>
    errMsgC </>
    E.div {} (
      E.ul {} (
        list (\item -> taskItem item `use` (\o -> { deleteItemS: o.deleteS })) items (_.id)
          `use` (\o -> { actions: o })
      ) </>
      loadingC
    )
  ) `output` {}
