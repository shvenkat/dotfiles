# A Summary Of Python Tools

## What I need

When I develop Python libraries or programs, I want to:

  * Run linters (mypy, flake8, pylint, bandit) - one OS, one Python version.
  * Run formatters (black, isort, pyupgrade) - one OS, one Python version.
  * Run tests under different Python versions, dependency versions, and different OSes.
  * Build and publish for different Python versions (and different OSes).
  * Run arbitrary commands using different virtual environments.

Recommendation: tox

When I install/use Python libraries or programs, I need to:

  * Check resolved dependency versions for vulnerabilities. Apply version constraints.
  * Record resolved dependency versions under different Python versions and OSes.

Recommendation: pip-audit to scan for vulnerabilities. `pip freeze` to record dependencies?
https://stackoverflow.com/questions/77244199/how-do-i-freeze-the-requirements-of-a-tox-test-environment

## pyenv

Provides different versions of the Python interpreter.

Remembers and automatically uses one Python version for each project.

Competes with OS package managers (apt, brew) and custom builds (./configure &&
make) of Python.

## uv

Provides different versions of Python.

Manages pyproject.toml and a (single) project virtual environment.

Generates a universal lock file, across Python version and OSes.

## tox

Builds virtual environments and runs specific commands in each.

Useful for continuous integration, and testing across different Python versions.

Competes with task runners (make, just). Needs to be installed once.

Re-uses virtual environments between runs, but re-installs package being
developed/tested for each run (can be skipped).
