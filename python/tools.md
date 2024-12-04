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

## Installing multiple versions of Python

Multiple versions of Python are useful when testing your package and ensuring
support for specific versions. When installing multiple versions, do not change
the default Python symlinks, `python` and `python3`, as this may break an OS
script. Instead, always invoke Python using `python3.x`.

For Ubuntu, depending on the OS version, your desired version(s) of Python may
be available in the universe "component" of the default package repo. If not,
you can use the "deadsnakes" PPA.

    sudo add-apt-repository universe OR ppa:deadsnakes/ppa  # Optional.

For Python 3.12 and above, remove the "-distutils". This package has been
removed.

    sudo apt install python3.x{,-dev,-venv,-tk,-distutils}

If you need Python to use a recent version of libsqlite3, check it with:

    python3.x -c 'import sqlite3; print(sqlite3.sqlite_version);'

For MacOS, you can try to install multiple versions using homebrew. But there
many be issues when different versions of Python use different versions of some
libraries, such as libtk. You can try `export
DYLD_FALLBACK_LIBRARY_PATH=/opt/homebrew/opt/tcl-tk@8/lib`, for instance. If
that doesn't work, you may need to uninstall some versions of Python. The
alternatives are pyenv and uv, and other package managers, such as macports
and nix (with nix-darwin), which may do a better job than homebrew at handling
versioned dependencies.

### pyenv

Provides different versions of the Python interpreter.

Remembers and automatically uses one Python version for each project.

Competes with OS package managers (apt, brew) and custom builds (./configure &&
make) of Python.

### uv

Provides different versions of Python.

Manages pyproject.toml and a (single) project virtual environment.

Generates a universal lock file, across Python version and OSes.

## Managing virtual environments

To manage virtual environments with tox, you can install it with `sudo apt
install tox`. If your tox config is in TOML format (tox.toml, pyproject.toml),
you will need a newer version, which you can install with:

    # Set this variable if you have `require-virtualenv = true` in pip.conf.
    PIP_REQUIRE_VIRTUALENV=false \
    python3.12 -m pip install --user --break-system-packages 'tox >= 4.21.2'

Alternatively, use pipx to install Python programs in isolated virtualenvs:

    sudo apt install pipx
    pipx install 'python-pypi-mirror'

In both cases above (pip install --user OR pipx install), executables will be in
`~/.local/bin`, so ensure that this directory is in your PATH.

For a new project or new machine, create virtual environments in `/tmp` using
symlinks as follows. By changing `user-dir/.tox` symlink, you can redirect
virtual environments for all projects to a new location.

    user-dir/path-to/project-dir/.tox -> ../../.tox/project-dir
    user-dir/.tox -> /tmp/user/.tox  # Location appropriate to machine.
    mkdir -p $(readlink -f user-dir/path-to/project-dir/.tox)

### tox

Builds virtual environments and runs specific commands in each.

Useful for continuous integration, and testing across different Python versions.

Competes with task runners (make, just). Needs to be installed once.

Re-uses virtual environments between runs, but re-installs package being
developed/tested for each run (can be skipped).
