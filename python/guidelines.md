# Guidelines for Python Code

## Packaging

* Do not constrain dependency versions except where necessary.

  Avoiding vulnerable versions of dependencies is the user's responsibility.

* Within one organization/team, prefer a monorepo with custom packaging.

  Different repos: decouples development, but requires coordination across repos
  (e.g. function/method definitions and call sites), and leads to dependency
  hell (matching compatible version ranges) among team packages and for third-
  party ones.

  One repo with separate packages: simplifies development and testing, but does
  not address dependency hell. Dependencies between these packages also requires
  either installing the same version of different team packages, which usually
  requires pinning.

  One repo with one package in different "flavors": simplifies development and
  testing, ensures compatibility of third-party dependencies. You can build a
  single "everything" package with all sub-packages and modules, and provide
  different "extras" so that the user can choose to install only the third-party
  dependencies required for their use case. And/or, you can build a custom
  package for each use case, which includes only the modules required and their
  third party dependencies.

## Testing

* Include a self-test capability so that libraries and/or apps can be tested in
  the user's runtime environment.

  For instance, include tests in the package, provide an "extra" to install any
  testing dependencies, and an example command to run the tests.

* Save a transcript of successful testing, including system, Python and
  dependency versions.
