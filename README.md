# Beeline

A routing library.

Example
-------

Assume the following sitemap:

+ left/
  - foo
  - bar
  - baz
+ right/
  - foo
  - bar
  - baz

An example URL would be e.g. `left/foo`.

Beeline helps with encoding the sitemap as one value.
The value is used both for generating URLs and for matching on parsed routes.
That way, dead links can be avoided.

The leaf nodes could be modelled with a sum type like

    data SumType = RouteFoo | RouteBar | RouteBaz

and the branching at the root could use `Either SumType SumType`.

An URL like `left/foo` would then be represented by the value `Left RouteFoo`.

However, Beeline can't work with these encodings. It uses
[Shrubbery](https://github.com/flipstone/shrubbery)
instead of vanilla sum types.

The Beeline analogue for the Foo/Bar/Baz type is shown in `test/fixtures/FooBarBaz.hs`, and it is called `fooBarBazRouter`.

This router can be 'interpreted' with strings to build URLs, see `fooBarBazToText`.

The router can also be embedded in other routers, see
`subrouter` in `test/Fixtures/Subrouter.hs` which
corresponds to the Either.

An example route for the URL `left/foo` is `exampleRouteLeftFoo` in
`Subroutes.hs`. This can be transformed into a String URL using
`subrouteToText`:

    >>> subrouteToText exampleRouteLeftFoo
    "left/foo"

You can convert the parsed route into the previously mentioned `Either SumType SumType` by writing a custom router interpreter like e.g.:

    parser =
      Shrubbery.dissect
      $ Shrubbery.branchBuild
      $ Shrubbery.branch (\(Subrouter.LeftSubroute fbb)  -> (Right $ FBB.handleFBB RouteFoo RouteBar RouteBaz fbb))
      $ Shrubbery.branch (\(Subrouter.RightSubroute fbb) -> (Left  $ FBB.handleFBB RouteFoo RouteBar RouteBaz fbb))
      $ Shrubbery.branchEnd

    parsed :: Either SumType SumType
    parsed = parser route

In practice, the `route` the previous example would be a parsed route from `recognizeRoute`, see `prop_subrouter` in `test/Test/RouteRecognizer.hs`.
