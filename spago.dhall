{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "console"
  , "control"
  , "debug"
  , "dom-indexed"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-store"
  , "halogen-subscriptions"
  , "integers"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "parsing"
  , "parsing-dataview"
  , "prelude"
  , "safe-coerce"
  , "strings"
  , "transformers"
  , "tuples"
  , "uint"
  , "web-encoding"
  , "web-events"
  , "web-socket"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
