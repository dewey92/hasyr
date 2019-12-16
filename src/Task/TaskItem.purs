module Hasyr.Task.TaskItem where

import Prelude

import Hareactive.Types (Stream)
import Hasyr.Task.Types (Task)
import Turbine (Component, modelView, use, (</>))
import Turbine.HTML as E

taskItem :: Task -> Component { delete :: Stream Number } {}
taskItem props = modelView model view where
  model input = pure { delete: input.delete $> props.id }
  view input =
    E.li {} (
      E.div { class: pure "card has-margin-top-20" } (
        E.div { class: pure "card-content" } (
          E.p { class: pure "title" } (E.text props.name)
        ) </>
        E.footer { class: pure "card-footer" } (
          E.span { class: pure "card-footer-item" } (E.text "Edit") </>
          E.span { class: pure "card-footer-item" } (E.text "Delete") `use` (\o -> { delete: o.click })
        )
      )
    )
