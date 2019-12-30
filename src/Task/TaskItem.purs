module Hasyr.Task.TaskItem where

import Prelude

import Data.String (null, trim)
import Hareactive.Combinators as H
import Hareactive.Types (Behavior, Now, Stream)
import Hasyr.Task.Types (Task)
import Haysr.Utils (isKey)
import Turbine (Component, component, dynamic, output, use, (</>))
import Turbine.HTML as E
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

useTextOrInput ::
  { isEditingB :: Behavior Boolean
  , valueB :: Behavior String
  , taskNameB :: Behavior String
  } ->
  Now (Component {} { keyup :: Stream KeyboardEvent , value :: Behavior String })
useTextOrInput deps = do
  let comp = dynamic (deps.isEditingB <#>
    if _ then
      E.input { value: deps.valueB } `use` (\o -> { keyup: o.keyup, value: o.value })
    else
      E.p { class: pure "title" } (
        E.textB deps.taskNameB
      ) `use` (\_ -> { keyup: mempty :: _, value: deps.taskNameB })
  )
  pure $ comp `use` (\b -> { keyup:  H.shiftCurrent (_.keyup <$> b), value: b >>= _.value })

taskItem :: Task -> Component { deleteS :: Stream Number, submitEditS :: Stream Task } {}
taskItem task = component \on -> do
  let enterPressedS = H.filter (isKey "Enter") on.keyup
  let escPressedS   = H.filter (isKey "Escape") on.keyup
  let submitS       = H.filter (not <<< null <<< trim) (H.snapshot on.value enterPressedS)

  valueB     <- H.stepper task.name ((H.changes on.value) <> submitS)
  taskNameB  <- H.stepper task.name submitS
  isEditingB <- H.toggle false (on.editS) ((escPressedS $> "") <> submitS)

  textOrInputC <- useTextOrInput { isEditingB, valueB, taskNameB }

  E.li {} (
    E.div { class: pure "card has-margin-top-20" } (
      E.div { class: pure "card-content" } (
        textOrInputC
      ) </>
      E.footer { class: pure "card-footer" } (
        E.span { class: pure "card-footer-item" } (E.text "Edit") `use` (\o -> { editS: o.click }) </>
        E.span { class: pure "card-footer-item" } (E.text "Delete") `use` (\o -> { deleteS: o.click })
      )
    )
  ) `output` {
    deleteS: on.deleteS $> task.id,
    submitEditS: submitS <#> \name -> { id: task.id, name }
  }