module Haysr.Utils where

import Prelude

import Web.UIEvent.KeyboardEvent as KE

isKey :: String -> KE.KeyboardEvent -> Boolean
isKey key event = (KE.key event) == key
