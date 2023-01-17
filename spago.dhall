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
{ name = "chatterbox"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "erl-atom"
  , "erl-binary"
  , "erl-cowboy"
  , "erl-kernel"
  , "erl-lists"
  , "erl-logger"
  , "erl-pinto"
  , "erl-process"
  , "erl-stetson"
  , "erl-test-eunit"
  , "erl-test-eunit-discovery"
  , "erl-tuples"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "free"
  , "heterogeneous"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "record"
  , "routing-duplex"
  , "simple-json"
  , "simple-json-generics"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unicode"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "Common/**/*.purs" ]
, backend = "purerl"
}
