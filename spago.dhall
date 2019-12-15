{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "console"
    , "effect"
    , "formatters"
    , "hareactive"
    , "js-timers"
    , "psci-support"
    , "remotedata"
    , "turbine"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
