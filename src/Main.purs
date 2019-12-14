module Main where

import Prelude

import Effect (Effect)
import Hasyr.Component.HeaderTime (headerTime)
import Hasyr.Component.Task.Task (taskList)
import Turbine (Component, component, output, runComponent, (</>))
import Turbine.HTML as E

app :: Component {} {}
app = component \on -> do
  E.div { class: pure "root" } (
    E.div { class: pure "columns" } (
      E.div { class: pure "column" } ( E.text "") </>
      E.div { class: pure "column is-half" } (
        E.section {} (
          headerTime </>
          taskList
        )
      ) </>
      E.div { class: pure "column" } ( E.text "" )
    )
  ) `output` {}

main :: Effect Unit
main = runComponent "#app" app
