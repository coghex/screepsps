{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "argonaut-core"
  , "console"
  , "effect"
  , "foreign-object"
  , "maybe"
  , "ordered-collections"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
