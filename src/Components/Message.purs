module Hasyr.Component.Message (MessageType(..), MessageProps, message) where

import Prelude

import Hareactive.Types (Behavior, Stream)
import Turbine (Component, component, output, use, (</>))
import Turbine.HTML as E

data MessageType = Dark | Primary | Link | Info | Warning | Danger | Success

toClass :: MessageType -> String
toClass = case _ of
  Dark    -> "is-dark"
  Primary -> "is-primary"
  Link    -> "is-link"
  Info    -> "is-info"
  Warning -> "is-warning"
  Danger  -> "is-danger"
  Success -> "is-success"

type MessageProps = {
  type :: MessageType,
  body :: Behavior String
}

message :: MessageProps -> Component { close :: Stream Unit } {}
message props = component \on -> do
  E.div { class: pure $ "message " <> toClass props.type } (
    E.div { class: pure "message-header" } (
      E.p {} ( E.text "Error" ) </>
      E.button { class: pure "delete" } E.empty `use` (\o -> { close: o.click })
    ) </>
    E.div { class: pure "message-body" } (E.textB props.body)
  ) `output` { close: on.close }
