module Main where

import Prelude

import Effect (Effect)
import Hasyr.Component.HeaderTime (headerTime)
import Hasyr.Task.TaskList (taskList)
import Turbine (Component, component, output, runComponent, (</>))
import Turbine.HTML as E

app :: Component {} {}
app = component \on -> do
  E.div { class: pure "root" } (
    E.div { class: pure "columns" } (
      E.div { class: pure "column" } E.empty </>
      E.div { class: pure "column is-half" } (
        E.section {} (
          headerTime </>
          taskList
        )
      ) </>
      E.div { class: pure "column" } E.empty
    )
  ) `output` {}

main :: Effect Unit
main = runComponent "#app" app
