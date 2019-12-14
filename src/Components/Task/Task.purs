module Hasyr.Component.Task.Task where

import Prelude

import Data.Array (filter, fold, snoc)
import Data.String (trim)
import Hareactive.Combinators (snapshot, time)
import Hareactive.Combinators as H
import Hareactive.Types (Behavior, Stream)
import Hasyr.Component.Task.Types (Task)
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
  let foldedStream = map (map _.deleteItem >>> fold) on.actions
  let deleteItem = H.shiftCurrent foldedStream
  items <- H.accum identity [] (
    (on.addItem <#> (\task arr -> snoc arr task)) <>
    (deleteItem <#> (\id arr -> filter ((_.id) >>> (_ /= id)) arr))
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
