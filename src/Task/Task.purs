module Hasyr.Task.Task where

import Prelude

import Data.Array (filter, fold, snoc)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Hareactive.Combinators (changes, runAffNow, snapshot, stepTo, time)
import Hareactive.Combinators as H
import Hareactive.Types (Behavior, Stream)
import Hasyr.Task.Apis (getTodosFromFakeServer)
import Hasyr.Task.Types (Task)
import Turbine (Component, component, list, modelView, output, use, withStatic, (</>))
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

taskItem :: Task -> Component { delete :: Stream Number } {}
taskItem props = modelView model view where
  model input = pure { delete: input.delete $> props.id }
  view input =
    E.li {} (
      E.div { class: pure "card" } (
        E.div { class: pure "card-content" } (
          E.p { class: pure "title" } (E.text props.name)
        ) </>
        E.footer { class: pure "card-footer" } (
          E.span { class: pure "card-footer-item" } (E.text "Edit") </>
          E.span { class: pure "card-footer-item" } (E.text "Delete") `use` (\o -> { delete: o.click })
        )
      )
    )

taskList :: Component {} {}
taskList = component \on -> do
  -- Delete stream from individual item
  let foldedStream = map (map _.deleteItem >>> fold) on.actions
  let deleteItem = H.shiftCurrent foldedStream

  -- Fetch stream on init
  fromServer <- runAffNow getTodosFromFakeServer
  let fetchResult = stepTo Nothing (fromServer <#> hush)

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
      (changes fetchResult) <#>
      (\maybeTasks arr -> case maybeTasks of
        Nothing -> arr
        Just eitherTasks -> case eitherTasks of
          Left _ -> arr
          Right tasks -> arr <> tasks
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
