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

When I read data, I want to parse and validate it.

Recommendation: attrs + cattrs. Avoid Pydantic, which defaults to type coersion
and couples classes to their serialization (default JSON). See
https://threeofwands.com/why-i-use-attrs-instead-of-pydantic/. attrs + cattrs is
also a smaller codebase.

> Pydantic has a ton of validation logic magically baked in, whereas attrs gives
> you the tools to set it up yourself. Hence, less surprises.

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

## Web app framework

### Panel

Use different packages/modules to separate domain logic from app GUI and glue.

Use either pn.rx/pn.bind (function-based) or Parameter subclasses (class-based)
to bind domain logic to inputs. The main choice is not between FP or OOP style.
Rather, it is between an imperative style where updates are managed manually
with pn.depends(..., watch=True, on_init=True), or a declarative style
(pn.rx/pn.bind) where updates are handled _automatically and efficiently_.

Read and understand the docs for the param package (https://param.holoviz.org).

Reactive values/expressions update when their dependencies change, triggered by
user input or data sources or domain logic. In contrast, plain values are
static, and must be updated manually/imperatively. Distinguish between
widget.value (instantaneous value), and widget or widget.param.value (reference
to reactive value).

    text = pn.widgets.TextInput()
    text.value  # 👈 The current value of the widget
    text.param.value  # 👈 A reference to the "value" Parameter, used in Panel to bind to the "value"

Where possible, avoid creating new GUI objects (panes, widgets) on each call of
a reactive function. Instead, "reuse" GUI objects by creating them explicitly.

    def square(x):
        return f'{x} squared is {x**2}'

    def styles(background):
        return {'background-color': background, 'padding': '0 10px'}

    x = pn.widgets.IntSlider(name='x', start=0, end=100)
    background = pn.widgets.ColorPicker(name='Background', value='lightgray')
    pn.Column(
        x,
        background,
        pn.pane.Markdown(pn.bind(square, x), styles=pn.bind(styles, background))
    )

Use reactive expressions (pn.rx or widget.rx) to simplify some boilerplate.
Prefer pn.rx over pn.bind (its predecessor) where possible as it is more
flexible and efficient.

    wind_speed = pn.widgets.FloatSlider(
        value=5, start=0, end=20, step=1, name="Wind Speed (m/s)"
    )
    efficiency = pn.widgets.FloatInput(
        value=0.3, start=0.0, end=1.0, name="Efficiency (kW/(m/s))"
    )
    submit = pn.widgets.Button(name="Submit", button_type="primary")

    power = wind_speed.rx() * efficiency.rx()
    power_text = pn.rx(
        "Wind Speed: {wind_speed} m/s, "
        "Efficiency: {efficiency}, "
        "Power Generation: {power:.1f} kW"
    ).format(
        wind_speed=wind_speed,
        efficiency=efficiency,
        power=power
    ).rx.when(submit)

    pn.Column(
        wind_speed, efficiency, submit, pn.pane.Markdown(power_text)
    ).servable()

Reactive expressions can also be used to react to events (such as button click)
that are not widget value changes. Instead of directly mutating the GUI elements
state, bind GUI elements to app state using reacive expressions.

    is_stopped = pn.rx(True)  # Declare state of application
    rx_name = is_stopped.rx.where("Start the wind turbine", "Stop the wind turbine")
    submit = pn.widgets.Button(name=rx_name)

    def toggle_wind_turbine(clicked):
        is_stopped.rx.value = not is_stopped.rx.value

    submit.rx.watch(toggle_wind_turbine)
    pn.Column(submit).servable()

Select the appropriate pane type for access to events and efficiency. The Vega
pane provides access to selection events (click, hover, brush) and uses binary
serialization (faster than JSON).

Using templates:

    import numpy as np
    import matplotlib.pyplot as plt
    import panel as pn

    pn.extension(template='fast')

    freq = pn.widgets.FloatSlider(
        name='Frequency', start=0, end=10, value=5
    ).servable(target='sidebar')

    ampl = pn.widgets.FloatSlider(
        name='Amplitude', start=0, end=1, value=0.5
    ).servable(target='sidebar')

    def plot(freq, ampl):
        fig = plt.figure()
        ax = fig.add_subplot(111)
        xs = np.linspace(0, 1)
        ys = np.sin(xs*freq)*ampl
        ax.plot(xs, ys)
        return fig

    # See pn.pane.Vega(chart, sizing_mode="stretch_both", max_height=800, margin=20)
    mpl = pn.pane.Matplotlib(
        pn.bind(plot, freq, ampl)
    )

    pn.Column(
        '# Sine curve', mpl
    ).servable(target='main')

Keep the UI responsive with threads or processes.

- Use `panel serve ... --num-threads N` to automatically use a thread pool to
  call reactive functions in response to events.

Cache expensive computations, and use a persistent cache. If you install the
diskcache package, pn.cache will cache to a local SQLite database file.

    @pn.cache  # Global cache shared across sessions.
    def expensive_func(arg): ...

    @pn.cache(..., per_session=True)  # Session-specific cache.
    def expensive_func(arg): ...

diskcache claims to be process safe, so this cache may be shared with other
apps, for instance cron jobs that pre-populate the cache ahead of anticipated
use. See also `panel serve ... --setup` to pre-warm the cache.

However, if the cache is meant to be shared by a number of applications, it may
be better to use a separate cache in a more transparent format, perhaps one that
can be synced to S3 or between machines. An in-use SQLite database is not safe
to directly copy.

What is re-executed and when:

- Imported modules are executed once when they are first imported. Objects
  defined in these modules are shared across all user sessions. If using `panel
  serve ... --dev`, imported modules are also re-executed.

- The app.py script (from `panel serve app.py`) is executed each time the app is
  loaded, which is once for every user session (each distinct combination of
  user, browser and tab). Objects defined in app.py are shared within the single
  user session only (unless cached).

- Only specific, bound functions are re-executed upon user interactions, not the
  entire app.py script.

Provide feedback to the user during a slow computation. Use pn.panel(...,
loading_indicator=True) to provide minimal feedback. A progress bar is better.
Provide results progressively where appropriate.

Use throttling to "debounce" user input if real-time interactivity is not
desirable.

When using a class-based approach, you could use pn.cache to memoise methods.
Better yet, if only a single "dependent" method will be executed per update, use
@param.depends(...). If there are multiple dependent methods and some depend on
others, then some methods will be executed multiple times! Avoid this using
pn.rx which enables a declarative and efficient solution, instead of
@param.depends(..., watch=True, on_init=True) which provides an imperative
approach to update instance attributes that are Parameter instances.

    import pandas as pd
    import panel as pn
    import param
    from panel.viewable import Viewer

    pn.extension("tabulator")
    data_url = "https://assets.holoviz.org/panel/tutorials/turbines.csv.gz"
    turbines = pn.cache(pd.read_csv)(data_url)

    class DataExplorer(Viewer):
        data = param.DataFrame(doc="Stores a DataFrame to explore")
        columns = param.ListSelector(
            default=["p_name", "t_state", "t_county", "p_year", "t_manu", "p_cap"]
        )
        year = param.Range(default=(1981, 2022), bounds=(1981, 2022))
        capacity = param.Range(default=(0, 1100), bounds=(0, 1100))
        filtered_data = param.Parameter()
        number_of_rows = param.Parameter()

        def __init__(self, **params):
            super().__init__(**params)
            self.param.columns.objects = self.data.columns.to_list()

            dfrx = self.param.data.rx()
            p_year_min = self.param.year.rx().rx.pipe(lambda x: x[0])
            p_year_max = self.param.year.rx().rx.pipe(lambda x: x[1])
            p_cap_min = self.param.capacity.rx().rx.pipe(lambda x: x[0])
            p_cap_max = self.param.capacity.rx().rx.pipe(lambda x: x[1])

            self.filtered_data = dfrx[
                dfrx.p_year.between(p_year_min, p_year_max)
                & dfrx.p_cap.between(p_cap_min, p_cap_max)
            ][self.param.columns]

            self.number_of_rows = pn.rx("Rows: {len_df}").format(len_df=pn.rx(len)(dfrx))

        def __panel__(self):
            return pn.Column(
                pn.Row(
                    pn.widgets.MultiChoice.from_param(self.param.columns, width=400),
                    pn.Column(self.param.year, self.param.capacity),
                ),
                self.number_of_rows,
                pn.widgets.Tabulator(self.filtered_data, page_size=10, pagination="remote"),
            )

    DataExplorer(data=turbines).servable()

Reuse GUI components using Viewers and Views (aka DataStore pattern):

    # ---- module data_store ----

    CARD_STYLE = """
    :host {{
    box-shadow: rgba(50, 50, 93, 0.25) 0px 6px 12px -2px, rgba(0, 0, 0, 0.3) 0px 3px 7px -3px;
    padding: {padding};
    }} """

    TURBINES_URL = "https://assets.holoviz.org/panel/tutorials/turbines.csv.gz"

    @pn.cache(ttl=15 * 60)
    def get_turbines():
        return pd.read_csv(TURBINES_URL)

    class DataStore(Viewer):
        data = param.DataFrame()

        filters = param.List(constant=True)

        def __init__(self, **params):
            super().__init__(**params)
            dfx = self.param.data.rx()
            widgets = []
            for filt in self.filters:
                dtype = self.data.dtypes[filt]
                if dtype.kind == "f":
                    widget = pn.widgets.RangeSlider(
                        name=filt, start=dfx[filt].min(), end=dfx[filt].max()
                    )
                    condition = dfx[filt].between(*widget.rx())
                else:
                    options = dfx[filt].unique().tolist()
                    widget = pn.widgets.MultiChoice(name=filt, options=options)
                    condition = dfx[filt].isin(widget.rx().rx.where(widget, options))
                dfx = dfx[condition]
                widgets.append(widget)
            self.filtered = dfx
            self.count = dfx.rx.len()
            self.total_capacity = dfx.t_cap.sum()
            self.avg_capacity = dfx.t_cap.mean()
            self.avg_rotor_diameter = dfx.t_rd.mean()
            self.top_manufacturers = (
                dfx.groupby("t_manu").p_cap.sum().sort_values().iloc[-10:].index.to_list()
            )
            self._widgets = widgets

        def filter(
            self,
        ):
            return

        def __panel__(self):
            return pn.Column(
                "## Filters",
                *self._widgets,
                stylesheets=[CARD_STYLE.format(padding="5px 10px")],
                margin=10
            )

    # ---- module views ----

    from panel.viewable import Viewer

    class View(Viewer):
        data_store = param.ClassSelector(class_=DataStore)

    class Table(View):
        columns = param.List(
            default=["p_name", "p_year", "t_state", "t_county", "t_manu", "t_cap", "p_cap"]
        )

        def __panel__(self):
            data = self.data_store.filtered[self.param.columns]
            return pn.widgets.Tabulator(
                data,
                pagination="remote",
                page_size=13,
                stylesheets=[CARD_STYLE.format(padding="10px")],
                margin=10,
            )

    class Histogram(View):
        def __panel__(self):
            df = self.data_store.filtered
            df = df[df.t_manu.isin(self.data_store.top_manufacturers)]
            fig = (
                pn.rx(alt.Chart)(
                    (df.rx.len() > 5000).rx.where(df.sample(5000), df),
                    title="Capacity by Manufacturer",
                )
                .mark_circle(size=8)
                .encode(
                    y="t_manu:N",
                    x="p_cap:Q",
                    yOffset="jitter:Q",
                    color=alt.Color("t_manu:N").legend(None),
                )
                .transform_calculate(jitter="sqrt(-2*log(random()))*cos(2*PI*random())")
                .properties(
                    height=400,
                    width=600,
                )
            )
            return pn.pane.Vega(
                fig, stylesheets=[CARD_STYLE.format(padding="0")], margin=10
            )

    class Indicators(View):
        def __panel__(self):
            style = {"stylesheets": [CARD_STYLE.format(padding="10px")]}
            return pn.FlexBox(
                pn.indicators.Number(
                    value=self.data_store.total_capacity / 1e6,
                    name="Total Capacity (GW)",
                    format="{value:,.2f}",
                    **style
                ),
                pn.indicators.Number(
                    value=self.data_store.count,
                    name="Count",
                    format="{value:,.0f}",
                    **style
                ),
                pn.indicators.Number(
                    value=self.data_store.avg_capacity,
                    name="Avg. Capacity (kW)",
                    format="{value:,.2f}",
                    **style
                ),
                pn.indicators.Number(
                    value=self.data_store.avg_rotor_diameter,
                    name="Avg. Rotor Diameter (m)",
                    format="{value:,.2f}",
                    **style
                ),
            )

    # ---- module app ----

    from data_store import DataStore, CARD_STYLE
    from data_store import DataStore, get_turbines
    from views import Indicators, Histogram, Table

    pn.extension("tabulator", "vega", throttled=True)

    class App(Viewer):
        data_store = param.ClassSelector(class_=DataStore)

        title = param.String()

        views = param.List()

        def __init__(self, **params):
            super().__init__(**params)
            updating = self.data_store.filtered.rx.updating()
            updating.rx.watch(
                lambda updating: pn.state.curdoc.hold()
                if updating
                else pn.state.curdoc.unhold()
            )
            self._views = pn.FlexBox(
                *(view(data_store=self.data_store) for view in self.views), loading=updating
            )
            self._template = pn.template.MaterialTemplate(title=self.title)
            self._template.sidebar.append(self.data_store)
            self._template.main.append(self._views)

        def servable(self):
            if pn.state.served:
                return self._template.servable()
            return self

        def __panel__(self):
            return pn.Row(self.data_store, self._views)

    data = get_turbines()
    ds = DataStore(data=data, filters=["p_year", "p_cap", "t_manu"])
    App(
        data_store=ds, views=[Indicators, Histogram, Table], title="Windturbine Explorer"
    ).servable()

Use pn.hold to batch updates, either as a context manager or decorator.

Component-level binding is more efficient than function-level binding.
https://panel.holoviz.org/explanation/api/reactivity.html

User experience recommendations: https://panel.holoviz.org/how_to/best_practices/user_experience.html


### param

Subclass param.Parametrized and use param.Parameter instances as class
attributes. In place of Parameter instances, you can also use reactive functions
(param.bind), reactive expressions (param.rx), sync or async generators,
functions and methods with dependencies and/or watchers (param.depends)

    class Example(Parametrized):
        threads = Parameter(default=3, doc="How many threads to use")
        upload = Parameter(default=True, doc="Whether to upload to S3", constant=True)
        strict = Parameter(default=True, doc="Whether to tolerate errors?", readonly=True)

    ex = Example(threads=8, upload=False)
    ex.threads = 4
    ex.upload = True  # Raises an exception since .upload cannot be modified.

    ex.threads  # 4
    ex.param  # A namespace providing access to parameters of ex.
    ex.param["threads"]  # Parameter instance
    ex.param.threads  # Same parameter instance
    ex.param.threads.default  # 3

    ex.param.update(threads=8)  # Delays dependency updates and watchers until all values are set.

Custom Parameter subclasses can specify the _validate_value() method if needed.

    class CustomParameter(Parameter):
        def _validate_value(self, val, allow_None: bool) -> None:
            super()._validate_value(val, allow_None)
            ...  # Custom validation code

Use param.DataFrame() to specify the required min/max number of rows, column
names, and optionally column order.

See param.Callable (wraps a callable), param.Action (callable with no arguments,
fully curried), param.Event (boolean flag used to trigger watchers).

Avoid param.Composite which may not properly handle dependencies and watchers.

Use Parameter(..., allow_refs=True) to have some parameters linked to others, or
even contain others (nested_refs=True). This can reduce tight coupling between
providers and consumers in the class definitions. The following parameter values
need allow_refs=True:

- Class and instance Parameter objects
- Functions or methods with dependencies added using param.depends
- Reactive Functions using param.bind
- Reactive expressions declared using param.rx
- (Asynchronous) generators

For lazy updates (aka "get" model), use @param.depends(..., watch=False) on
functions and methods. This declares dependencies and leaves the update trigger
to an external library such as panel (which can inspect the dependency
relationships).

For eager updates (aka "push" model) in an imperative style: use
@param.depends(..., watch=True) to call a function or method whenever its
dependencies are updated. Avoid this as much as possible.

> Be sure not to set watch=True for dependencies for any method you pass to an
> external library like Panel to handle, or else that method will get invoked
> twice, once by Param itself (discarding the output) and once by the external
> library (using the output). Typically you will want watch=True for a
> side-effecty function or method (typically not returning a value), and
> watch=False (the default) for a function or method with a return value, and
> you’ll need an external library to do something with that return value.

Ideally, use reactive functions (param.bind) and/or expressions (param.rx) to
specify dependencies and computation in a declarative style. This focuses on
"what to update", while leaving "when and how to update" to the library. Of
these, param.rx is recommended because it is more flexible and easier to make
efficient (avoid recomputation).

    URL = 'https://datasets.holoviz.org/penguins/v1/penguins.csv'
    nrows = rx(2)  # Reactive integer value
    df = rx(pd.read_csv(URL))  # Reactive dataframe
    df.head(nrows)  # Regular method call on reactive instance with reactive arg

    nrows.rx.value += 2  # Automatically updates the displayed output of .head()

Functions need to be explicitly made reactive

    url = rx(URL)
    df = rx(pd.read_csv)(url)

Operators are automatically overloaded, returning reactive values that track the
operation and operands. This allows updating results when dependencies change.

    i = rx(1)
    j = i + 1
    i.rx.value = 7
    type(i), i._obj, j._operation

    # (param.reactive.rx,
    # 7,
    # {'fn': <function _operator.add(a, b, /)>,
    # 'args': (1,),
    # 'kwargs': {},
    # 'reverse': False})

The following operations cannot be overloaded, so they have explicit
alternatives in the .rx namespace of reactive operands.

- Python requires the len operation to return an integer, not a deferred
  reactive integer. Use .rx.len() instead.
- The Python is statement always checks the immediate identity of its two
  operands, so it cannot be deferred reactively. Use .rx.is_() instead.
- Logical operators like and, or, not, and in are required to return Boolean
  types rather than deferred, reactive Boolean types. Use .rx.and_() etc.
- No overloading is available for control flow keywords like if, elif, and else
  or ternary conditional expressions (i.e. a if condition else b), and so those
  actions cannot be captured for later reactive execution. Use .rx.where()
- Iteration keywords like for or while can only be overloaded to some extent,
  specifically for fixed-length collections; other types of iteration cannot be
  captured for later reactive execution.

See also: .rx.bool, .rx.in_, .rx.map (for reactive iterables), .rx.pipe
(reactive function application), .rx.updating (useful for progress spinners),
.rx.when (delaying recomputation until a button is clicked), .rx.where (ternary
expression), .rx.watch(callback)

param.bind allows you to define functions that are automatically invoked when
their input Parameters change. This serves as a bridge between the reactive rx
model and the lower-level ‘push’ model. Unlike the ‘push’ model, where you would
explicitly set up watchers and callbacks, param.bind simplifies the process by
letting Param manage the mechanics, but also making the dependencies more
transparent than in a purely rx approach.

In essence, param.bind offers the declarative nature of reactive expressions and
the explicitness of the ‘push’ model. This makes it particularly useful for
complex applications where you might want the clarity of explicit function calls
for key parts of your pipeline, but also wish to retain the high-level,
declarative relationships offered by reactive expressions.

param.bind follows the semantics of Python’s functools.partial, and so if you
only partially bind the required arguments, you’ll get a function of the
remaining arguments:

    def add(a, b):
        print(f'add: {a}+{b}={a+b}')
        return a + b

    reactive_add = param.bind(add, p.param.a, p.param.b)
    p.a += 4
    reactive_add  # Reactive return value (a function)
    reactive_add()  # Non-reactive return value (a number)

Bound functions update their outputs reactively when displayed, but what if you
want to use one in a reactive expression? You can easily do that if you call
.rx() on a fully bound function to get a reactive expression to work with:

    param.bind(add, p.param.a, p.param.b).rx() / 2

Functions can be made reactive using param.bind(func, ...) or param.rx(func).
The former makes the dependency on arguments more explicit. The latter can also
be used to express computation without writing (explicit) functions. The two can
also be combined.

Generators can be used as parameter values:

    def poll_sensor(timeout=0.5):
        while True:
            yield tidal_gauge()
            time.sleep(timeout)

    class TidalGauge(param.Parameterized):
        height = param.Number(allow_refs=True)  # allow_refs=True needed here!

    gauge = TidalGauge(height=poll_sensor)  # Non-reactive value
    gauge.param.height.rx()  # Reactive expression, updates automatically

Alternatively:

    gauge_rx = param.rx(poll_sensor)  # Reactive expression
    corrected = gauge_rx + 8

    (corrected > 10).rx.where('Risk of flooding', 'Everything is normal')

    param.bind(lambda value: 'Risk of flooding' if value > 10 else 'Everything is normal', corrected).rx()

Async generators:

    import asyncio

    async def poll_sensor_async(timeout=0.5):
        while True:
            yield tidal_gauge()
            await asyncio.sleep(timeout)

    async_gauge = TidalGauge(height=poll_sensor_async)
    async_gauge.param.height.rx()

You can use @param.output to indicate which value(s) are "outputs" of a
Parametrized instance. This can be used to programmatically determine whether
outputs of one instance can serve as inputs to another.

## Local Database

Here we look at embedded databases that don't require a network connection or
even a separate process.

### SQLite

Available in stdlib. Reliable, fast (single core), ACID. Multiple processes can
read and write concurrently. Simple to configure, backup, share as a file.
Streaming backup and replication (Litestream) is available.

Choose SQLite if any of the following is true:
- You need multiple concurrent writers, or concurrent readers and writers.
- You are mostly querying up to a few rows at a time, and these queries use
  indices not table scans.

**Cursors: to use or not to use?** Cursors in the Python `sqlite3` module are
semantically distinct from those in the underlying SQLite library. In SQLite,
each connections has (prepared) statements, each statement creates and uses
cursors internally (not exposed in the C API) in the SQLite bytecode engine. In
Python's DB-API2, each connection has cursors, each cursor can execute
statements. This may be borrowed from MySQL or PostgreSQL terminology. So,
whether `sqlite3.Cursor` instances are reused or not, does not affect efficiency
in `libsqlite`. The only impact is only on the Python side, where it might be a
bit more efficient, particularly if a large number of fast queries are made.
However, be careful to complete iteration over the results of one query _before_
executing another.

### DuckDB

Well tested, fast (multi-core), ACID. Only one process can write, and is
mutually exclusive with multiple readers. Streaming aggregation for datasets too
big to fit in RAM. Fast, integrated export to NumPy arrays, Pandas dataframes.
Simple to configure, backup, share as a file. Streaming backup and replication
(Duckstream) is available, but may not be well supported.

Choose SQLite if all the following are true:
- You have a single process, or read-only database.
- Your queries scan substantial portions of tables.

> The interpretation seems to be clear: DuckDB does not seem to work with
> indices as well as SQLite does (see also the section below) but is blazingly
> fast when scanning large parts of the table. SQLite is great at using indices
> but takes its time when it has to look at the whole table.

> Since my workload mainly consists of simple queries which should be well
> answerable from an index (and it is feasible to just create as many indices as
> it takes…), I will stay with SQLite for the project.

> - https://www.lukas-barth.net/blog/sqlite-duckdb-benchmark/:
