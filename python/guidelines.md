# Guidelines for Python Code

* Do not constrain dependency versions except where necessary.

  Avoiding vulnerable versions of dependencies is the user's responsibility.

* Include a self-test capability so that libraries and/or apps can be tested in
  the user's runtime environment.

  For instance, include tests in the package, provide an "extra" to install any
  testing dependencies, and an example command to run the tests.

* Save a transcript of successful testing, including system, Python and
  dependency versions.
