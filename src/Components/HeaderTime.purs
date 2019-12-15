module Hasyr.Component.HeaderTime (headerTime) where

import Prelude

import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (formatDateTime)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Effect.Timer (setInterval)
import Hareactive.BehaviorRef (new', write)
import Turbine (Component, component, output)
import Turbine.HTML as E

formatCurrTime :: DateTime -> String
formatCurrTime now = case formatDateTime "ddd, DD MMMM YYYY HH:mm:ss" now of
  Left _ -> "Invalid date"
  Right r -> r

headerTime :: Component {} {}
headerTime = component \on -> do
  initNow <- liftEffect $ nowDateTime <#> formatCurrTime
  { ref, behavior } <- liftEffect $ new' initNow

  _ <- liftEffect $ setInterval 100 do
    now <- nowDateTime <#> formatCurrTime
    write now ref

  E.section {} (
    E.h1 { class: pure "title" } (E.textB $ behavior)
  ) `output` {}
